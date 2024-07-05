"""
Given an LSST Pipepeline CORR file extact the PRIMARY header and IMAGE from extension 1 and store as SIF
"""
from astropy.io import fits
import argparse
import re


def extract_image(filename):
    with fits.open(filename) as hdulist:
        ccdno = int(hdulist[0].header['T_SDOID'])
        expid = hdulist[0].header['EXP-ID']
        beeid = int(hdulist[0].header['T_BEEID'])
        expnum = int(re.match('HSCE(\d+)', expid).group(1)) + beeid

        out_filename = f"{expnum}p{ccdno}.fits"
        new_hdu = hdulist[0]
        new_hdu.data = hdulist[1].data
        for keyword in hdulist[1].header:
            if keyword in ['EXTEND', 'XTENSION'] :
                continue
            new_hdu.header[keyword] = hdulist[1].header[keyword]
        new_hdu.writeto(out_filename)
        out_filename = f"{expnum}p{ccdno}_weight.fits"
        new_hdu = hdulist[0]
        new_hdu.data = hdulist[3].data
        for keyword in hdulist[3].header:
            if keyword in ['EXTEND', 'XTENSION'] :
                continue
            new_hdu.header[keyword] = hdulist[3].header[keyword]
        new_hdu.writeto(out_filename)


if __name__=='__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('filename', help="Name of CORR file to extract IMAGE extension from")
    args = parser.parse_args()
    extract_image(args.filename)

