      subroutine lcu_softcal_read
   
      use input_file_module
      use jrw_datalib_module
      use hydrograph_module
      use parm, only : hru, ihru  
      use hru_lte_module

      character (len=80) :: titldum, header
      integer :: eof
      
      imax = 0
      mcal = 0
	  
      inquire (file=in_chg%ls_regions_cal, exist=i_exist)
      if (i_exist == 0 .or. in_chg%ls_regions_cal == 'null') then
           allocate (lscal(0:0))		        
      else  
        do
          open (107,file=in_chg%ls_regions_cal)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mlscal
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          allocate (lscal(mlscal))

          do i = 1, mlscal

            !! read landscape soft calibration data for each land use
            read (107,*,iostat=eof) ilum_mx, nreg
            if (eof < 0) exit
            allocate (lscal(i)%lum(ilum_mx))
            allocate (lscal(i)%reg(nreg))
            allocate (lscal(i)%ireg(nreg))
            backspace (107)
            read (107,*,iostat=eof) lscal(i)%lum_num, lscal(i)%num_reg, lscal(i)%reg
            if (lscal(i)%lum_num > 0) then
              read (107,*,iostat=eof) header
              if (eof < 0) exit
              !! read soft calibration data for each land use within the region
              do ilum = 1, ilum_mx
                read (107,*,iostat=eof) lscal(i)%lum(ilum)%meas
                if (eof < 0) exit
              end do
            end if 
               
            !! xwalk regions with region()%name - save soft ls data for region
            do icalreg = 1, lscal(i)%num_reg
              do ireg = 1, db_mx%lsu_reg
                if (lscal(i)%reg(icalreg) == region(ireg)%name) then
                  region(ireg)%lscal = i
                  lscal(i)%ireg(icalreg) = i
                end if
              end do
            end do

            !! set landscape region for hru and hru-lte
            if (db_mx%lsu_reg > 0) then
              do iob = 1, region(i)%num_tot
                ihru = region(i)%num(iob)
                if (ihru <= sp_ob%hru) hru(ihru)%region = lscal(i)%name
                if (ihru <= sp_ob%hru_lte) hlt(ihru)%region = lscal(i)%name
              end do
            end if
            
!            do icalreg = 1, lscal(i)%num_reg
!              !! allocate land uses within the region for print out
!              ireg = lscal(i)%ireg(icalreg)
!              allocate (region(ireg)%lum_ha(ilum_mx))
!              allocate (rwb_d(ireg)%lum(ilum_mx)); allocate (rwb_m(ireg)%lum(ilum_mx))
!              allocate (rwb_y(ireg)%lum(ilum_mx))
!              allocate (rnb_d(ireg)%lum(ilum_mx)); allocate (rnb_m(ireg)%lum(ilum_mx))
!              allocate (rnb_y(ireg)%lum(ilum_mx))
!              allocate (rls_d(ireg)%lum(ilum_mx)); allocate (rls_m(ireg)%lum(ilum_mx))
!              allocate (rls_y(ireg)%lum(ilum_mx))
!              allocate (rpw_d(ireg)%lum(ilum_mx)); allocate (rpw_m(ireg)%lum(ilum_mx))
!              allocate (rpw_y(ireg)%lum(ilum_mx))
!            end do
          end do    !mlscal

          exit
        end do 
      end if	  
        
      db_mx%lscal_reg = mlscal
	  
      return
      end subroutine lcu_softcal_read