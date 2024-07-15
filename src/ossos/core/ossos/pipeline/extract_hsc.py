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

        file_extensions = {1: '_image', 2: '_mask', 3: '_weight'}
        for extension in [2, 3, 1]:
            if file_extensions[extension] == '_image':
                SKYLEVEL = hdulist[0].header.get('SKYLEVEL', 0)
                hdulist[extension].data += SKYLEVEL
                hdulist[extension].header['GAIN'] = hdulist[extension].header.get('T_GAIN1', 4.5)
                hdulist[extension].header.extend(hdulist[0].header, unique=True, update=False, strip=True)
            out_image_name = f"{expnum}p{ccdno:02d}{file_extensions[extension]}.fits"
            fits.writeto(out_image_name,
                         data=hdulist[extension].data,
                         header=hdulist[extension].header,
                         overwrite=True)
    return f"{expnum}p{ccdno:02d}"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('filename', help="Name of CORR file to extract IMAGE extension from")
    args = parser.parse_args()
    print(extract_image(args.filename))


if __name__ == '__main__':
    main()

