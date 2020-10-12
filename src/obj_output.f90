      subroutine obj_output
      
      use time_module
      use hydrograph_module
      use soil_module
      use hru_module, only : ihru
      
      implicit none
        
      integer :: ihd           !            |
      integer :: iob           !            | 
      integer :: iunit         !            |
      integer :: itot          !none        |counter
      integer :: nlyr          
      integer :: nly
      integer :: j

      j = ihru
      
      do
        do itot = 1, mobj_out
          iob = ob_out(itot)%objno
          ihd = ob_out(itot)%hydno
          iunit = ob_out(itot)%unitno          
                
          if (iob <= sp_ob%objs) then
            if (ihd > 5) then
              write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, (soil(j)%phys(nly)%st, nly = 1,soil(ihru)%nly)
            else
              write (iunit+itot,*) time%day, time%mo, time%day_mo, time%yrc, ob(iob)%name, ob(iob)%typ, ob(iob)%hd(ihd) 
            end if   
          end if

        end do
        exit
      end do
      
      return

      end subroutine obj_output
