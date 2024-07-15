import sys
from astropy.io import fits
import numpy
from astropy.table import Table
import argparse

MP_MASK=['MP_NO_DATA', 'MP_SAT', 'MP_BAD', 'MP_INTRP', 'MP_UNMASKEDNAN', 'MP_EDGE']

def read_obj_matt(filename):
    """
    load a table that is written to a file in .obj.matt form 

    Columns are: ##   X       Y        FLUX     SIZE MAX_INT  ELON 

    Strategy is to read the header and store as meta keyword
    """
    names = ['x', 'y', 'flux', 'size', 'max_int', 'elon']
    _t = Table(names=names)
    with open(filename, 'r') as fobj:
        header = ""
        rows = []
        for line in fobj.readlines():
            if line[0]=='#':
                header += line
                continue
            _t.add_row([ float(x) for x in line.strip('\n').split()])

    _t.meta['header']=header
    return _t


def write_obj_matt(table, filename):
    """
    write out a table in .obj.matt format, assumed the header is stored in .meta of the table

    """
    with open(filename, 'w') as fobj:
        fobj.write(table.meta['header'])
        table.write(fobj,format='ascii.fixed_width_no_header', delimiter="")


def main():
    parser = argparse.ArgumentParser(description='Mask objects from a .obj.matt file')
    parser.add_argument('obj_table', help='The .obj.matt file to mask')
    parser.add_argument('mask', help='The mask file to use')
    parser.add_argument('output', help='The output .obj.matt file')
    args = parser.parse_args()
    obj_table = read_obj_matt(args.obj_table)
    mask = fits.open(args.mask)
    bit_mask = 0
    for mp in MP_MASK:
        bit_mask += 2**mask[2].header[mp]

    m = mask[2].data[obj_table['y'].astype(numpy.int16), obj_table['x'].astype(numpy.int16)]
    obj_table=obj_table[(m & bit_mask) == 0]
    write_obj_matt(obj_table, args.output)


if __name__ == '__main__':
    main()
