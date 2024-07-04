__author__ = 'Michele Bannister   git:@mtbannister'
import argparse
from ossos.ephem_target import EphemTarget
from mp_ephem import BKOrbit
from astropy.time import Time
import logging
import datetime
from astropy import units

header="""<?xml version = "1.0"?>
<!DOCTYPE ASTRO SYSTEM "http://vizier.u-strasbg.fr/xml/astrores.dtd">
<ASTRO ID="v0.8" xmlns:ASTRO="http://vizier.u-strasbg.fr/doc/astrores.htx">
  <TABLE ID="Table">
    <NAME>Ephemeris</NAME>
    <TITLE>Ephemeris for CFHT QSO</TITLE>
    <!-- Definition of each field -->
    <FIELD name="DATE_UTC"  datatype="A" width="19" format="YYYY-MM-DD hh:mm:ss"> 
        <DESCRIPTION>UTC Date</DESCRIPTION>
    </FIELD>	
    <FIELD name="RA_J2000"  datatype="A" width="11" unit="h"   format="RAh:RAm:RAs">
        <DESCRIPTION>Right ascension of target</DESCRIPTION>
    </FIELD>
    <FIELD name="DEC_J2000" datatype="A" width="11" unit="deg" format="DEd:DEm:DEs">
        <DESCRIPTION>Declination of target</DESCRIPTION>
    </FIELD>
    <!-- Data table -->
<DATA><CSV headlines="4" colsep="|">
<![CDATA[
DATE_UTC           |RA_J2000   |DEC_J2000  |
YYYY-MM-DD hh:mm:ss|hh:mm:ss.ss|+dd:mm:ss.s|
1234567890123456789|12345678901|12345678901|
-------------------|-----------|-----------|
"""
footer="""
]]></CSV></DATA>
  </TABLE>
</ASTRO>
"""

if __name__ == '__main__':
     logger = logging.getLogger()
     parser = argparse.ArgumentParser()
     parser.add_argument('mpc_files', nargs='+', help='mpc_file to base table from.')
     parser.add_argument('--verbose', '-v', action='store_true', default=None, help='verbose feedback')
     parser.add_argument('--start', '-s', default=None, help='Date as YYYY/MM/DD, default is current date')
     parser.add_argument('--range', '-r', default=30, help='Length of table is days', type=int)
 
     opt = parser.parse_args()
 
if opt.verbose:
     logger.setLevel(logging.INFO)
 
start_date = (opt.start is not None and Time(opt.start, scale='utc')) or Time(datetime.datetime.utcnow().isoformat(), scale='utc')
 

## build orbit instance for object
i = 0
for mpc_file in opt.mpc_files:
    orbit = BKOrbit(None, mpc_file)
    with open(f'{orbit.name}_astres.xml', 'w') as f:
        f.write(header)
        for day in range(30):
            i+=1
            orbit.predict(start_date + 0.5*day*24*units.hour)
            sra, sdec = orbit.coordinate.to_string(style='hmsdms', sep=":").split()
            f.write("%-19s|%11s|%11s|\n" % (orbit.date[0:19].replace("T"," "), sra[0:11], sdec[0:11]))
        f.write(footer)

