"""
Given an LSST Pipepeline CORR file extact the PRIMARY header and IMAGE from extension 1 and store as SIF
"""
from astropy.io import fits
import argparse
import re, numpy


def extract_image(filename):
    with fits.open(filename) as hdulist:
        ccdno = int(hdulist[0].header['T_SDOID'])
        expid = hdulist[0].header['EXP-ID']
        beeid = int(hdulist[0].header['T_BEEID'])
        expnum = int(re.match('HSCE(\d+)', expid).group(1)) + beeid

        out_imagename = f"{expnum}p{ccdno}"
        new_hdu = hdulist[0]
        SKYLEVEL = hdulist[0].header.get('SKYLEVEL', 0)
        new_hdu.data = numpy.int16(hdulist[1].data+SKYLEVEL)
        for keyword in hdulist[1].header:
            if keyword in ['EXTEND', 'XTENSION'] :
                continue
            new_hdu.header[keyword] = hdulist[1].header[keyword]
        new_hdu.header['GAIN'] = hdulist[1].header.get('T_GAIN1', 4.5)
        new_hdu.writeto(f"{out_imagename}.fits", overwrite=True)
        out_weightname = f"{expnum}p{ccdno}_weight.fits"
        new_hdu = hdulist[0]
        new_hdu.data = hdulist[3].data
        for keyword in hdulist[3].header:
            if keyword in ['EXTEND', 'XTENSION'] :
                continue
            new_hdu.header[keyword] = hdulist[3].header[keyword]
        new_hdu.writeto(out_weightname, overwrite=True)
    return out_imagename


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('filename', help="Name of CORR file to extract IMAGE extension from")
    args = parser.parse_args()
    print(extract_image(args.filename))


if __name__=='__main__':
    main()

