      subroutine obj_output
      
      use time_module
      !use hydrograph_module
        
      integer :: ihd, iob, iunit
      
      do
        do itot = 1, mobj_out
          
                
          iob = ob_out(itot)%objno
          ihd = ob_out(itot)%hydno
          iunit = ob_out(itot)%unitno          
                
          write (iunit+itot,100) time%day, time%yrc,                            &
             ob_out(itot)%obtyp, ob_out(itot)%obtypno,                          &
             ob(iob)%hd(ihd)

        end do
        exit
      end do
      
      return
         
100   format (2i8,a8,i8, 1(1x, f17.3), 24(1x,e17.3))

      end subroutine obj_output