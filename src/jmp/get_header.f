C -*-compile-command: "make_lib"; -*-

      subroutine get_header (head_name, lun_h, lun_o, thresh, echelle,
     $  maxcount)

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine gets the value of the header keywords and write them
c down into the object file.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c
c J-M. Petit  Observatoire de Besancon
c Version 1 : October 2003
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c INPUT
c     head_name: Name of header file.
c     lun_h    : Logical unit of header file
c     lun_o    : Logical unit of output file
c     thresh   : Value of threshold for detection
c     echelle  : Value of FWHM for detection
c     maxcount : Maximum allowable value of pixel
c OUTPUT
c     none.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      implicit none

      integer*4
     $     lun_o, naxis1, naxis2, lun_h, expnum, chipnum, istat,
     $     rwmode, status, naxis

      real*4
     $  thresh, echelle, maxcount, mopvers

      real*8
     $  crpix1, crval1, crpix2, crval2, exptime,
     $  mjd_obs, pixscale, phpadu, rdnoise

      character
     $  detect*20, line*80, head_name*80, comment*80

      if (lun_h .le. 0) stop

      write (6, *) 'Reading the header '//head_name
      rwmode = 0
      status = 0
c     Reads header keywords.
      call ftnopn(lun_h, head_name, rwmode, status)
      write (6,*) status
      call ftgkyj(lun_h, 'EXPNUM', expnum, comment, status)
      write (6,*) status
      call ftgkyj(lun_h, 'CHIPNUM', chipnum, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'MJD-OBSC', mjd_obs, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'EXPTIME', exptime, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'CRVAL1', crval1, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'CRVAL2', crval2, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'CRPIX1', crpix1, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'CRPIX2', crpix2, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'PIXSCALE', pixscale, comment, status)
      write (6,*) status
      call ftgkyj(lun_h, 'ONAXIS', naxis, comment, status)
      write (6,*) status
      call ftgkyj(lun_h, 'ONAXIS1', naxis1, comment, status)
      write (6,*) status
      call ftgkyj(lun_h, 'ONAXIS2', naxis2, comment, status)
      write (6,*) status
      call ftgkys(lun_h, 'DETECTOR', detect, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'PHPADU', phpadu, comment, status)
      write (6,*) status
      call ftgkyd(lun_h, 'RDNOISE', rdnoise, comment, status)
      write (6,*) status
      call ftgkye(lun_h, 'MOP_VER', mopvers, comment, status)
      write (6,*) status
      call ftclos(lun_h, status)

      call check_version (mopvers, head_name, 80, istat)

c Writes catalog file header

      write (lun_o, '(a)') '## MOPversion'
      write (lun_o, '(a2, f5.2)') '# ', mopvers
      write (lun_o, '(a)') '## MJD-OBS-CENTER  EXPTIME THRES FWHM  '
     $  //'MAXCOUNT CRVAL1     CRVAL2     EXPNUM'
      write (lun_o, 104) mjd_obs, exptime, thresh, echelle, maxcount,
     $  crval1, crval2, expnum
      write (lun_o, '(a)') '## SCALE CHIP CRPIX1    CRPIX2    '
     $  //'NAX1  NAX2   DETECTOR           PHADU RDNOIS'
      write (lun_o, 105) pixscale, chipnum, crpix1, crpix2, naxis1,
     $  naxis2, detect(1:19), phpadu, rdnoise
      write (lun_o, '(a)') '##   X       Y        FLUX     SIZE '
     $  //'MAX_INT  ELON   X^2  SHARP   SKY    N_PIX'

 104  format ('# ', f16.7, f8.2, 2f6.2, f9.1, 2f11.5, i9)
 105  format ('# ', f6.3, i4, 2f10.2, 2i6, ' ', a19, 2f6.2)

      return

 1000 continue
      write (6, '(a)') 'Couldn''t open file '//head_name
      stop 'Exiting.'

      end

      subroutine get_nth_key_s (head_name, lun_h, n_th, string, istat)

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine returns the value of the n'th keyword as a string.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c
c J-M. Petit  Observatoire de Besancon
c Version 1 : October 2003
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c INPUT
c     head_name: Name of header file.
c     lun_h    : Logical unit of header file
c     n_th     : Ordinal number of keyword to retrieve
c OUTPUT
c     string   : n'th keyword value (string)
c     istat    : Error code
c                   0 : nominal run
c                  10 : could not open file
c                  20 : could not read keyword
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      implicit none

      integer*4
     $  lun_h, n_th, istat

      character
     $  head_name*80, string*(*), line*80

      if (lun_h .le. 0) stop

      istat = 0
      open (unit=lun_h, file=head_name, status='old',
     $  form='unformatted', access='direct', recl=80, err=1000)

      read (lun_h, rec=n_th, err=1100) line
      string = line(12:30)
      close (lun_h)
      return

 1000 continue
      istat = 10
      close (lun_h)
      return

 1100 continue
      istat = 20
      close (lun_h)
      return

      end

      subroutine get_nth_key_d (head_name, lun_h, n_th, val, istat)

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine returns the value of the n'th keyword as a double.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c
c J-M. Petit  Observatoire de Besancon
c Version 1 : October 2003
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c INPUT
c     head_name: Name of header file.
c     lun_h    : Logical unit of header file
c     n_th     : Ordinal number of keyword to retrieve
c OUTPUT
c     val      : n'th keyword value (R8)
c     istat    : Error code
c                   0 : nominal run
c                  10 : could not open file
c                  20 : could not read keyword
c                  30 : could not read keyword value
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      implicit none

      integer*4
     $  lun_h, n_th, istat

      real*8
     $  val

      character
     $  head_name*80, line*80

      if (lun_h .le. 0) stop

      istat = 0
      open (unit=lun_h, file=head_name, status='old',
     $  form='unformatted', access='direct', recl=80, err=1000)

      read (lun_h, rec=n_th, err=1100) line
      read (line(11:30), *, err=1200) val
      close (lun_h)
      return

 1000 continue
      istat = 10
      close (lun_h)
      return

 1100 continue
      istat = 20
      close (lun_h)
      return

 1200 continue
      istat = 30
      close (lun_h)
      return

      return
      end

      subroutine get_nth_key_r (head_name, lun_h, n_th, val, istat)

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine returns the value of the n'th keyword as a real.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c
c J-M. Petit  Observatoire de Besancon
c Version 1 : October 2003
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c INPUT
c     head_name: Name of header file.
c     lun_h    : Logical unit of header file
c     n_th     : Ordinal number of keyword to retrieve
c OUTPUT
c     val      : n'th keyword value (R8)
c     istat    : Error code
c                   0 : nominal run
c                  10 : could not open file
c                  20 : could not read keyword
c                  30 : could not read keyword value
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      implicit none

      integer*4
     $  lun_h, n_th, istat

      real*4
     $  val

      character
     $  head_name*80, line*80

      if (lun_h .le. 0) stop

      istat = 0
      open (unit=lun_h, file=head_name, status='old',
     $  form='unformatted', access='direct', recl=80, err=1000)

      read (lun_h, rec=n_th, err=1100) line
      read (line(11:30), *, err=1200) val
      close (lun_h)
      return

 1000 continue
      istat = 10
      close (lun_h)
      return

 1100 continue
      istat = 20
      close (lun_h)
      return

 1200 continue
      istat = 30
      close (lun_h)
      return

      return
      end

      subroutine get_nth_key_j (head_name, lun_h, n_th, val, istat)

c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine returns the value of the n'th keyword as an integer.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c
c J-M. Petit  Observatoire de Besancon
c Version 1 : October 2003
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c INPUT
c     head_name: Name of header file.
c     lun_h    : Logical unit of header file
c     n_th     : Ordinal number of keyword to retrieve
c OUTPUT
c     val      : n'th keyword value (I4)
c     istat    : Error code
c                   0 : nominal run
c                  10 : could not open file
c                  20 : could not read keyword
c                  30 : could not read keyword value
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

      implicit none

      integer*4
     $  lun_h, n_th, istat

      integer*4
     $  val

      character
     $  head_name*80, line*80

      if (lun_h .le. 0) stop

      istat = 0
      open (unit=lun_h, file=head_name, status='old',
     $  form='unformatted', access='direct', recl=80, err=1000)

      read (lun_h, rec=n_th, err=1100) line
      read (line(11:30), *, err=1200) val
      close (lun_h)
      return

 1000 continue
      istat = 10
      close (lun_h)
      return

 1100 continue
      istat = 20
      close (lun_h)
      return

 1200 continue
      istat = 30
      close (lun_h)
      return

      return
      end
