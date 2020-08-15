C -*-compile-command: "make_lib"; -*-

      subroutine open_image (image_name, lun_i, naxis1, naxis2, ext)
c-------------------------------------------------------------------

      implicit none

      integer*4
     $  mode, naxis, naxes(2), status, blocksize, i,
     $  lun_i, naxis1, naxis2, hdutype, nhdu, ext

      character
     $  image_name*80, title*80, comment*80

c Reads FITS file.
      mode = 0
      status=0
      write (6,*) 'openning ', image_name
      call ftopen (lun_i, image_name, mode, blocksize, status)
      if (status .gt. 0) then
         call ftgerr (status, title)
         write (6, *) 'FITSIO Open Error Status = ', status, ': ', title
         call ftgmsg (title)
         do while (title .ne. ' ')
            write (6, *) title
            call ftgmsg (title)
         end do
         stop
      end if
      write (6,*) 'Getting NAXIS from EXT ', ext
      call ftmahd(lun_i, ext, hdutype, status)
      call ftgknj(lun_i, 'NAXIS', 1, 2, naxes, naxis, status)
      write (6, *) 'Got ', naxis, (naxes(i),i=1,naxis)
      if (status .gt. 0) then
         call ftgerr (status, title)
         write (6,*) 'FITSIO Header ERROR =', status, ': ', title
         call ftgmsg (title)
         do while (title .ne. ' ')
            write (6,*) title
            call ftgmsg (title)
         end do
         stop
      end if
      if (.not. naxis .gt. 0) then
         write (6, *) 'FTGKNJ failed to read the NAXIS keyword.'
         write (6, *) naxis, (naxes(i),i=1,naxis)
         STOP
      end if
      
 100  continue

      naxis1 = naxes(1)
      naxis2 = naxes(2)

      return
      end
