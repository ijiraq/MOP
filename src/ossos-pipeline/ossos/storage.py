"""OSSOS VOSpace storage convenience package"""
import cStringIO
import errno
import fnmatch
from glob import glob
import os
import sys
import re
import logging

from astropy.io import ascii
import vos
from vos.vos import URLparse
from astropy.io import fits
import requests

import coding
from mpc import Time


logger = logging

MAXCOUNT = 30000

CERTFILE = os.path.join(os.getenv('HOME'),
                        '.ssl',
                        'cadcproxy.pem')

DBIMAGES = 'vos:OSSOS/dbimages'
MEASURE3 = 'vos:OSSOS/measure3'

DATA_WEB_SERVICE = 'https://www.canfar.phys.uvic.ca/data/pub/'
VOSPACE_WEB_SERVICE = 'https://www.canfar.phys.uvic.ca/vospace/nodes/'
TAP_WEB_SERVICE = 'http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/tap/sync'

OSSOS_TAG_URI_BASE = 'ivo://canfar.uvic.ca/ossos'
OBJECT_COUNT = "object_count"

vospace = vos.Client(cadc_short_cut=True)
vlog = logging.getLogger('vos')
vlog.setLevel(logging.ERROR)
sh = logging.StreamHandler(sys.stderr)
vlog.addHandler(sh)

SUCCESS = 'success'

### some cache holders.
mopheaders = {}
astheaders = {}


def cone_search(ra, dec, dra=0.01, ddec=0.01, mjdate=None):
    """Do a QUERY on the TAP service for all observations that are part of OSSOS (*P05/*P016)
    where taken after mjd and have calibration 'observable'.

    :param ra: float degrees
    :param dec: float degrees
    :param dra: float degrees
    :param ddec: float degrees
    """

    data = dict(QUERY=(" SELECT Observation.observationID as collectionID, "
                       " Plane.time_bounds_cval1 AS mjdate "
                       " FROM caom2.Observation AS Observation "
                       " JOIN caom2.Plane AS Plane "
                       " ON Observation.obsID = Plane.obsID "
                       " WHERE  ( Observation.collection = 'CFHT' ) "
                       " AND Plane.calibrationLevel=2 "
                       " AND ( Observation.proposal_id LIKE '%P05' or Observation.proposal_id LIKE '%P06' )"),
                REQUEST="doQuery",
                LANG="ADQL",
                FORMAT="tsv")

    data["QUERY"] += (" AND  "
                      " INTERSECTS( BOX('ICRS', {}, {}, {}, {}), "
                      " Plane.position_bounds ) = 1 ").format(ra, dec, dra, ddec)
    if mjdate is not None:
       data["QUERY"] += " AND Plane.time_bounds_cval1 < {} AND Plane.time_bounds_cval2 > {} ".format(mjdate+1.0/24.0, mjdate-1/24.0)

    result = requests.get(TAP_WEB_SERVICE, params=data, verify=False)
    assert isinstance(result, requests.Response)
    logger.debug("Doing TAP Query using url: %s" % (str(result.url)))

    table_reader = ascii.get_reader(Reader=ascii.Basic)
    table_reader.header.splitter.delimiter = '\t'
    table_reader.data.splitter.delimiter = '\t'
    table = table_reader.read(result.text)

    logger.debug(str(table))
    return table


def populate(dataset_name,
             data_web_service_url=DATA_WEB_SERVICE + "CFHT"):
    """Given a dataset_name created the desired dbimages directories
    and links to the raw data files stored at CADC."""

    data_dest = get_uri(dataset_name, version='o', ext='fits.fz')
    data_source = "%s/%so.fits.fz" % (data_web_service_url, dataset_name)

    try:
        vospace.mkdir(os.path.dirname(data_dest))
    except IOError as e:
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    try:
        vospace.link(data_source, data_dest)
    except IOError as e:
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    header_dest = get_uri(dataset_name, version='o', ext='head')
    header_source = "%s/%so.fits.fz?cutout=[0]" % (
        data_web_service_url, dataset_name)
    try:
        vospace.link(header_source, header_dest)
    except IOError as e:
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    header_dest = get_uri(dataset_name, version='p', ext='head')
    header_source = "%s/%s/%sp.head" % (
        'http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub', 'CFHTSG', dataset_name)
    try:
        vospace.link(header_source, header_dest)
    except IOError as e:
        if e.errno == errno.EEXIST:
            pass
        else:
            raise e

    return True


def get_uri(expnum, ccd=None,
            version='p', ext='fits',
            subdir=None, prefix=None):
    """
    Build the uri for an OSSOS image stored in the dbimages
    containerNode.

    :rtype : basestring
    expnum: CFHT exposure number
    ccd: CCD in the mosaic [0-35]
    version: one of p,s,o etc.
    dbimages: dbimages containerNode.
    @type subdir: str
    @param expnum: int
    @param ccd:
    @param version:
    @param ext:
    @param subdir:
    @param prefix:
    """
    if subdir is None:
        subdir = str(expnum)
    if prefix is None:
        prefix = ''
    uri = os.path.join(DBIMAGES, subdir)

    if ext is None:
        ext = ''
    elif len(ext) > 0 and ext[0] != '.':
        ext = '.' + ext

    # if ccd is None then we send uri for the MEF
    if ccd is not None:
        ccd = str(ccd).zfill(2)
        uri = os.path.join(uri,
                           'ccd{}'.format(ccd),
                           '%s%s%s%s%s' % (prefix, str(expnum),
                                           version,
                                           ccd,
                                           ext))
    else:
        uri = os.path.join(uri,
                           '%s%s%s%s' % (prefix, str(expnum),
                                         version,
                                         ext))
    logger.debug("got uri: " + str(uri))
    return uri


dbimages_uri = get_uri


def _set_tags(expnum, keys, values=None):
    uri = os.path.join(DBIMAGES, str(expnum))
    node = vospace.getNode(uri)
    if values is None:
        values = []
        for idx in range(len(keys)):
            values.append(None)
    assert (len(values) == len(keys))
    for idx in range(len(keys)):
        key = keys[idx]
        value = values[idx]
        tag = tag_uri(key)
        node.props[tag] = value
    return vospace.addProps(node)


def set_tags(expnum, props):
    """Assign the key/value pairs in props as tags on on the given expnum.

    @param expnum: str
    @param props: dict
    @return: success
    """
    # first clear all the props
    _set_tags(expnum, props.keys())

    # now set all the props 
    return _set_tags(expnum, props.keys(), props.values())


def set_tag(expnum, key, value):
    """Assign a key/value pair tag to the given expnum containerNode.

    @param expnum:  str
    @param key: str
    @param value: str
    @return: success
    """

    return set_tags(expnum, {key: value})


def tag_uri(key):
    """Build the uri for a given tag key. 

    key: (str) eg 'mkpsf_00'

    """
    if OSSOS_TAG_URI_BASE in key:
        return key
    return OSSOS_TAG_URI_BASE + "#" + key.strip()


def get_tag(expnum, key):
    """given a key, return the vospace tag value."""

    if tag_uri(key) not in get_tags(expnum):
        get_tags(expnum, force=True)
    logger.debug("%s # %s -> %s" % (
        expnum, tag_uri(key), get_tags(expnum).get(tag_uri(key), None)))
    return get_tags(expnum).get(tag_uri(key), None)


def get_process_tag(program, ccd, version='p'):
    """make a process tag have a suffix indicating which ccd its for.

    """
    return "%s_%s%s" % (program, str(version), str(ccd).zfill(2))


def get_tags(expnum, force=False):
    uri = os.path.join(DBIMAGES, str(expnum))
    return vospace.getNode(uri, force=force).props


def get_status(expnum, ccd, program, version='p', return_message=False):
    """Report back status of the given program.

    """
    key = get_process_tag(program, ccd, version)
    status = get_tag(expnum, key)
    logger.debug('%s: %s' % (key, status))
    if return_message:
        return status
    else:
        return status == SUCCESS


def set_status(expnum, ccd, program, status, version='p'):
    """set the processing status of the given program.

    """

    return set_tag(expnum, get_process_tag(program, ccd, version), status)


def get_image(expnum, ccd=None, version='p', ext='fits', subdir=None, prefix=None, cutout=None):
    """Get a FITS file for this expnum/ccd  from VOSpace.


    @param expnum: CFHT exposure number (int)
    @param ccd:     @param ccd:
    @param version: [p, s, o]  (char)
    @param ext:
    @param subdir:
    @param prefix:
    @return: basestring
    """

    # the filename is based on the Simple FITS images file.
    filename = os.path.basename(get_uri(expnum, ccd, version, ext=ext, subdir=subdir, prefix=prefix))
    if os.access(filename, os.F_OK):
        logger.debug("File already on disk: {}".format(filename))
        return filename

    if not subdir:
        subdir = str(expnum)

    logger.debug("Building list of possible uri locations")
    ## here is the list of places we will look, in order
    locations = [(get_uri(expnum, ccd, version, ext=ext, subdir=subdir, prefix=prefix), cutout)]
    logger.debug(str(locations))
    if ccd is not None:
        try:
            ext_no = int(ccd) + 1
            flip = (cutout is None and (ext_no < 19 and "[-*,-*]" or "[*,*]")) or cutout
            locations.append((get_uri(expnum, version=version, ext=ext, subdir=subdir),
                              "[{}]{}".format(ext_no, flip)))
            logger.debug(str(locations))
            locations.append((get_uri(expnum, version=version, ext=ext + ".fz", subdir=subdir),
                              "[{}]{}".format(ext_no, flip)))
        except Exception as e:
            logger.error(str(e))
            pass
    while len(locations) > 0:
        (uri, cutout) = locations.pop(0)
        try:
            if cutout is not None:
                hdu_list = get_hdu(uri, cutout)
                hdu_list.writeto(filename)
                del hdu_list
            else:
                copy(uri, filename)
            return filename
        except Exception as e:
            logger.debug("vos sent back error: {} code: {}".format(str(e), getattr(e, 'errno', 0)))

    return None


def get_hdu(uri, cutout):
    """Get a at the given uri from VOSpace, possibly doing a cutout.

    If the cutout is flips the image then we also must flip the datasec keywords.  Also, we must offset the
    datasec to reflect the cutout area being used.

    @return: fits.HDU
    """

    logger.debug("Pulling: {} from VOSpace".format(uri))
    if cutout is not None:
        vos_ptr = vospace.open(uri, view='cutout', cutout=cutout)
    else:
        vos_ptr = vospace.open(uri, view='data')
    fpt = cStringIO.StringIO(vos_ptr.read())
    fpt.seek(0, 2)
    print fpt.tell()
    fpt.seek(0)
    logger.debug("Read from vospace completed. Building fits object.")
    hdu_list = fits.open(fpt)
    vos_ptr.close()
    logger.debug("Got image from vospace")

    if cutout is None:
        return hdu_list
    flip_datasec = '[-*' in cutout
    flop_datasec = '-*]' in cutout

    datasec = hdu_list[0].header.get('DATASEC', "[33:2080,1:4612]")
    if datasec is None or len(datasec) != 4:
        return hdu_list
    hdu_list[0].header['OLDSEC'] = datasec
    datasec = re.findall(r'(\d+)', datasec)
    for idx in range(len(datasec)):
        datasec[idx] = int(datasec)

    parts = re.findall(r'(\d+?)[:,\]]+', cutout)
    if parts is not None and len(parts) == 4:
        for idx in range(len(parts)):
            parts[idx] = int(parts[idx])
        flip_datasec = parts[0] < parts[1]
        flop_datasec = parts[2] < parts[3]
    else:
        parts = datasec

    naxis1 = hdu_list[0].header['NAXIS1']
    naxis2 = hdu_list[0].header['NAXIS2']

    x1 = min(parts[0:2])
    x2 = max(parts[0:2])
    y1 = min(parts[2:])
    y2 = max(parts[2:])
    datasec[0] = max(1, 1 + int(datasec[0]) - x1)
    datasec[1] = min(naxis1, naxis1 - (x2 - int(datasec[1])))
    datasec[2] = max(1, 1 + int(datasec[2]) - y1)
    datasec[3] = min(naxis2, naxis2 - (y2 - int(datasec[3])))

    if flip_datasec:
        x2 = naxis1 - int(datasec[0]) + 1
        x1 = naxis1 - int(datasec[1]) + 1
    if flop_datasec:
        y2 = naxis2 - int(datasec[2]) + 1
        y1 = naxis2 - int(datasec[3]) + 1

    datasec = "[{}:{},{}:{}]".format(x1, x2, y1, y2)
    hdu_list[0].header['DATASEC'] = datasec
    return hdu_list


def get_trans(expnum, ccd, prefix=None, version='p'):
    uri = get_uri(expnum, ccd, version, ext='trans.jmp', prefix=prefix)
    fobj = open_vos_or_local(uri)
    line = fobj.read()
    fobj.close()
    vs = line.split()
    trans = {'dx': float(vs[0]),
             'cd11': float(vs[1]),
             'cd12': float(vs[2]),
             'dy': float(vs[3]),
             'cd21': float(vs[4]),
             'cd22': float(vs[5])}
    return trans


def get_fwhm(expnum, ccd, prefix=None, version='p'):
    """Get the FWHM computed for the given expnum/ccd combo.

    @param expnum:
    @param ccd:
    @param prefix:
    @param version:
    @return:
    """

    uri = get_uri(expnum, ccd, version, ext='fwhm', prefix=prefix)
    print uri
    filename = os.path.basename(uri)

    if os.access(filename, os.F_OK):
        logger.debug("File already on disk: {}".format(filename))
        return float(open(filename, 'r').read())

    try:
        return float(open_vos_or_local(uri).read())
    except Exception as e:
        logger.warning(str(e))
        logger.warning("Using default FWHM of 4.0")
    return 4


def get_zeropoint(expnum, ccd, prefix=None, version='p'):
    """Get the zeropoint for this exposure.

    This command expects that there is a file called #######p##.zeropoint.used which contains the zeropoint.

    @param expnum: exposure to get zeropoint of
    @param ccd: which ccd (extension - 1) to get zp
    @param prefix: possible string prefixed to expsoure number.
    @param version:  one of [spo] as in #######p##
    @return:
    """
    uri = get_uri(expnum, ccd, version, ext='zeropoint.used', prefix=prefix)
    try:
        return float(open_vos_or_local(uri).read())
    except:
        url = uri.replace('vos:', 'https://www.canfar.phys.uvic.ca/data/pub/vospace/')
        return float(requests.get(url, cert=vospace.conn.vospace_certfile, verify=False).content)


def mkdir(dirname):
    """make directory tree in vospace.

    @param dirname: name of the directory to make
    """
    dir_list = []

    while not vospace.isdir(dirname):
        dir_list.append(dirname)
        dirname = os.path.dirname(dirname)
    while len(dir_list) > 0:
        logging.debug("Creating directory: %s" % (dir_list[-1]))
        vospace.mkdir(dir_list.pop())


def vofile(filename, **kwargs):
    """Open and return a handle on a VOSpace data connection"""
    kwargs['view'] = kwargs.get('view', 'data')
    return vospace.open(filename, **kwargs)


def open_vos_or_local(path, mode="rb"):
    """
    Opens a file which can either be in VOSpace or the local filesystem.
    """
    if path.startswith("vos:"):
        primary_mode = mode[0]
        if primary_mode == "r":
            vofile_mode = os.O_RDONLY
        elif primary_mode == "w":
            vofile_mode = os.O_WRONLY
        elif primary_mode == "a":
            vofile_mode = os.O_APPEND
        else:
            raise ValueError("Can't open with mode %s" % mode)

        return vofile(path, mode=vofile_mode)
    else:
        return open(path, mode)


def copy(source, dest):
    """use the vospace service to get a file. """

    logger.debug("deleting {} ".format(dest))
    try:
        vospace.delete(dest)
    except Exception as e:
        logger.debug(str(e))
        pass
    logger.debug("copying {} -> {}".format(source, dest))

    return vospace.copy(source, dest)


def vlink(s_expnum, s_ccd, s_version, s_ext,
          l_expnum, l_ccd, l_version, l_ext, s_prefix=None, l_prefix=None):
    """make a link between two version of a file.

    @param s_expnum:
    @param s_ccd:
    @param s_version:
    @param s_ext:
    @param l_expnum:
    @param l_ccd:
    @param l_version:
    @param l_ext:
    @param s_prefix:
    @param l_prefix:
    @return:
    """
    source_uri = get_uri(s_expnum, ccd=s_ccd, version=s_version, ext=s_ext, prefix=s_prefix)
    link_uri = get_uri(l_expnum, ccd=l_ccd, version=l_version, ext=l_ext, prefix=l_prefix)

    return vospace.link(source_uri, link_uri)


def remove(uri):
    try:
        vospace.delete(uri)
    except Exception as e:
        logger.debug(str(e))


def delete(expnum, ccd, version, ext, prefix=None):
    """delete a file, no error on does not exist."""
    uri = get_uri(expnum, ccd=ccd, version=version, ext=ext, prefix=prefix)
    remove(uri)


def my_glob(pattern):
    """get a listing matching pattern"""
    result = []
    if pattern[0:4] == 'vos:':
        dirname = os.path.dirname(pattern)
        flist = listdir(dirname)
        for fname in flist:
            fname = '/'.join([dirname, fname])
            if fnmatch.fnmatch(fname, pattern):
                result.append(fname)
    else:
        result = glob(pattern)
    return result


def listdir(directory, force=False):
    return vospace.listdir(directory, force=force)


def list_dbimages():
    return listdir(DBIMAGES)


def exists(uri, force=False):
    try:
        return vospace.getNode(uri, force=force) is not None
    except IOError as e:
        logger.error(str(e))
        # Sometimes the error code returned is the OS version, sometimes the HTTP version
        if e.errno in [404, os.errno.ENOENT]:
            return False
        raise e


def move(old_uri, new_uri):
    vospace.move(old_uri, new_uri)


def delete_uri(uri):
    vospace.delete(uri)


def has_property(node_uri, property_name, ossos_base=True):
    """
    Checks if a node in VOSpace has the specified property.
    """
    if get_property(node_uri, property_name, ossos_base) is None:
        return False
    else:
        return True


def get_property(node_uri, property_name, ossos_base=True):
    """
    Retrieves the value associated with a property on a node in VOSpace.
    """
    # Must use force or we could have a cached copy of the node from before
    # properties of interest were set/updated.
    node = vospace.getNode(node_uri, force=True)
    property_uri = tag_uri(property_name) if ossos_base else property_name

    if property_uri not in node.props:
        return None

    return node.props[property_uri]


def set_property(node_uri, property_name, property_value, ossos_base=True):
    """
    Sets the value of a property on a node in VOSpace.  If the property
    already has a value then it is first cleared and then set.
    """
    node = vospace.getNode(node_uri)
    property_uri = tag_uri(property_name) if ossos_base else property_name

    # If there is an existing value, clear it first
    if property_uri in node.props:
        node.props[property_uri] = None
        vospace.addProps(node)

    node.props[property_uri] = property_value
    vospace.addProps(node)


def build_counter_tag(epoch_field, dry_run=False):
    """
    Builds the tag for the counter of a given epoch/field,
    without the OSSOS base.
    """
    logger.info("Epoch Field: {}, OBJECT_COUNT {}".format(str(epoch_field), str(OBJECT_COUNT)))
    tag = epoch_field[1] + "-" + OBJECT_COUNT

    if dry_run:
        tag += "-DRYRUN"

    return tag


def read_object_counter(node_uri, epoch_field, dry_run=False):
    """
    Reads the object counter for the given epoch/field on the specified
    node.

    Returns:
      count: str
        The current object count.
    """
    return get_property(node_uri, build_counter_tag(epoch_field, dry_run),
                        ossos_base=True)


def increment_object_counter(node_uri, epoch_field, dry_run=False):
    """
    Increments the object counter for the given epoch/field on the specified
    node.

    Returns:
      new_count: str
        The object count AFTER incrementing.
    """
    current_count = read_object_counter(node_uri, epoch_field, dry_run=dry_run)

    if current_count is None:
        new_count = "01"
    else:
        new_count = coding.base36encode(coding.base36decode(current_count) + 1,
                                        pad_length=2)

    set_property(node_uri,
                 build_counter_tag(epoch_field, dry_run=dry_run),
                 new_count,
                 ossos_base=True)

    return new_count


def get_mopheader(expnum, ccd):
    """
    Retrieve the mopheader, either from cache or from vospace
    """
    mopheader_uri = dbimages_uri(expnum=expnum,
                                 ccd=ccd,
                                 version='p',
                                 ext='.mopheader')
    if mopheader_uri in mopheaders:
        return mopheaders[mopheader_uri]

    filename = os.path.basename(mopheader_uri)

    if os.access(filename, os.F_OK):
        logger.debug("File already on disk: {}".format(filename))
        mopheader_fpt = cStringIO.StringIO(open(filename, 'r').read())
    else:
        try:
            mopheader_fpt = cStringIO.StringIO(open_vos_or_local(mopheader_uri).read())
        except Exception as e:
            logger.error("Failed to open mopheader @ {}".format(mopheader_uri))
            logger.error(str(e))
            return None

    mopheader = fits.open(mopheader_fpt)

    ## add some values to the mopheader so it can be an astrom header too.
    header = mopheader[0].header
    try:
        header['FWHM'] = get_fwhm(expnum, ccd)
    except IOError:
        header['FWHM'] = 10
    header['SCALE'] = mopheader[0].header['PIXSCALE']
    header['NAX1'] = header['NAXIS1']
    header['NAX2'] = header['NAXIS2']
    header['MOPversion'] = header['MOP_VER']
    header['MJD_OBS_CENTER'] = str(Time(header['MJD-OBSC'],
                                        format='mjd',
                                        scale='utc', precision=5).replicate(format='mpc'))
    header['MAXCOUNT'] = MAXCOUNT
    mopheaders[mopheader_uri] = header
    mopheader.close()
    return mopheaders[mopheader_uri]


def _getheader(uri):
    """
    Internal method for accessing the storage system.
    Given the vospace URI for fits file get the header of the file.
    """
    #'https://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/vospace/OSSOS/dbimages/1616100/ccd00/1616100p00.psf.fits'

    data_url = DATA_WEB_SERVICE + "/vospace/"
    url = data_url + URLparse(uri).path
    payload = {'fhead': 'true'}
    logger.info("Requesting URL: {}".format(url))
    r = requests.get(url, params=payload, cert=CERTFILE, verify=False)
    if r.status_code != 200:
        logger.error("{}".format(r.status_code))
        logger.error(r.content)
        return None
    str_header = r.content.split('\n')
    headers = []
    fobj = cStringIO.StringIO()
    for line in str_header:
        if "END" not in line[0:4]:
            fobj.write(line + "\n")
            continue
        fobj.seek(0)
        header = fits.Header.fromfile(fobj,
                                      sep='\n',
                                      endcard=False,
                                      padding=False)
        headers.append(header)
        fobj.close()
        fobj = cStringIO.StringIO()

    # check if there are lines left in fobj and try to make a header from those.
    fobj.seek(0, 2)
    if fobj.tell() > 0:
        fobj.seek(0)
        headers.append(fits.Header.fromfile(fobj,
                                            sep='\n',
                                            endcard=False,
                                            padding=False))
    fobj.close()

    return headers


def get_header(uri):
    """
    Pull a FITS header from observation at the given URI
    """
    if uri not in astheaders:
        astheaders[uri] = _getheader(uri)
    return astheaders[uri]


def get_astheader(expnum, ccd, version='p', ext='.fits'):
    ast_uri = dbimages_uri(expnum, ccd, version=version, ext=ext)
    headers = get_header(ast_uri)
    if headers is None:
        ast_uri = dbimages_uri(expnum, version=version, ext=ext)
        header = get_header(ast_uri)[int(ccd) + 1]
    else:
        header = headers[0]
    return header
