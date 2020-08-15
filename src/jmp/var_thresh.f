C -*-compile-command: "make_lib"; -*-

      subroutine var_thresh (coeff, mask, variance, thres1, thres2,
     $  two_thresh, image, sky, nx, ny, yoff)
c-----------------------------------------------------------------

      integer*4
     $     nx, ny, i, j, ns, yoff

      real*4
     $     coeff(nx,ny), mask(nx,ny), image(nx,ny), mean,
     $     variance(nx, ny),
     $     sky, thres1,
     $     thres2, t1, t2, lsky

      logical
     $     two_thresh
      t1 = thres1*thres1
      
      if (two_thresh) then
         t2 = thres2*thres2
      else
         t2 = t1
      end if
      
      ns = 0
      sky = 0.
      do i = 1, ny
         lsky = 0.
         do j = 1, nx
            if (coeff(j,i) .ge. t1*variance(j,i)) then
               mask(j,i) = -2.
            else if (coeff(j,i) .ge. t2*variance(j,i)) then
               mask(j,i) = -1.
            else
               mask(j,i) = 0.
               ns = ns + 1
               lsky = lsky + image(j,i)
            end if
         end do
         sky = sky + lsky
      end do
      if (ns .gt. 0) sky = sky/float(ns)

      return
      end
