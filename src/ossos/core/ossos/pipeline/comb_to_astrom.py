#!/usr/bin/env python
# use the xy2skypv code to generate the astrometric values
# that measure3 would normally produce.
import os
import argparse
import logging
from pathlib import Path
from ossos import storage
from astropy import wcs
from astropy.io import fits

SUCCESS_FILE = "measure3.OK"
FAILED_EXT = "measure3.FAILED"
CANDS_COMB_EXT = 'cands.comb'
CANDS_ASTROM_EXT = 'measure3.cands.astrom'


def main():
    parser = argparse.ArgumentParser(
        description="convert x/y in cands.comb to ra/dec in cands.astrom")
    parser.add_argument('base_image',
                        help="The base image referencing the .cands.comb file")

    args = parser.parse_args()
    base_image = args.base_image
    run(base_image)


def run(base_image):
    """
    convert from x/y to ra/dec
    """
    Path(f'{base_image}.{FAILED_EXT}').touch()

    cands_filename = "%s.%s" % (base_image, CANDS_COMB_EXT)
    if not os.access(cands_filename, os.R_OK):
        FileNotFoundError("Failed to open input candidate file %s\n" % cands_filename)
        return

    astrom_header = """##   X        Y        X_0     Y_0          R.A.          DEC                   \n"""

    astrom_filename = "%s.%s" % (base_image, CANDS_ASTROM_EXT)
    coords = []
    wcs_list = []
    line_counter = 0
    xy_files = {}
    xy_lines = {}
    get_image_names = True
    started = False
    with open(astrom_filename, 'w') as astrom_file:
        cands_lines = open(cands_filename).readlines()
        for cands_line in cands_lines:
            # read the names of the exposures to work from...
            if len(cands_line.strip()) == 0:
                # Skip EMPTY lines...
                continue
            if cands_line.lstrip()[0] == '#':
                # write out all the header lines except the one with column
                # names as its different for the astrom version 'X_0' is a
                # column name in the cands.comb files.
                if "X_0" not in cands_line:
                    astrom_file.write(cands_line)
                else:
                    astrom_file.write(astrom_header)
                if cands_line.lstrip()[1] == '#':
                    get_image_names = not started
                    continue
                logging.info("Read this line: {}".format(cands_line[2:].strip()))
                if get_image_names and cands_line.lstrip()[1] == ' ':
                    base_name = cands_line[2:].strip()
                    wcs_list.append(wcs.WCS(fits.open(f"{base_name}.fits")[0].header))
                    started = True
                    continue
                continue
            if line_counter % len(wcs_list) == 0:
                astrom_file.write("\n")
            v = cands_line.strip().split()
            coords.append(v)
            xy = [float(v[0]), float(v[1])]
            rd = wcs_list[line_counter % len(wcs_list)].all_pix2world(xy[0], xy[1], 1)
            astrom_file.write(" %8.2f %8.2f %8.2f %8.2f %12.7f %12.7f\n" % (float(v[0]),
                                                                            float(v[1]),
                                                                            float(v[2]),
                                                                            float(v[3]),
                                                                            rd[0],
                                                                            rd[1]))
            line_counter += 1

    Path(f'{base_image}.{SUCCESS_FILE}').touch()
    os.unlink(f'{base_image}.{FAILED_EXT}')


if __name__ == '__main__':
    main()
