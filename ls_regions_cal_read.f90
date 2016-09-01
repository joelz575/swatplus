      subroutine ls_regions_cal_read
   
      use input_file_module
      use jrw_datalib_module
      use hydrograph_module

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
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        allocate (lscal(mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) lscal(i)%name, lscal(i)%lum_num, nspu
        
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) lscal(i)%name, lscal(i)%lum_num,  nspu, (elem_cnt(isp), isp = 1, nspu)

          !!save the object number of each defining unit
          ielem = 0
          do ii = 1, nspu
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
            else
              if (elem_cnt(ii+1) < 0) then
                ie2 = abs(elem_cnt(ii+1))
                do ie = ie1, ie2
                  ielem = ielem + 1
                end do
                if (ii+1 == nspu) exit
              else
                ielem = ielem + 1
              end if
            end if
            if (ii == nspu .and. elem_cnt(ii) < 0) exit
          end do
          allocate (lscal(i)%num(ielem))
          lscal(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              lscal(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                lscal(i)%num(ielem) = ie1
                ielem = ielem + 1
                lscal(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  lscal(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
        else
          !!all hrus are in region
          allocate (lscal(i)%num(sp_ob%hru))
          lscal(i)%num_tot = sp_ob%hru
          do ihru = 1, sp_ob%hru
            lscal(i)%num(ihru) = ihru
          end do      
        end if
        
        !! read landscape soft calibration data for each land use
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        if (lscal(i)%lum_num > 0) then
          ilum_mx = lscal(i)%lum_num
          allocate (lscal(i)%lum(ilum_mx))
          do ilum = 1, ilum_mx
            read (107,*,iostat=eof) lscal(i)%lum(ilum)%meas
            if (eof < 0) exit
              !!crosswalk lum database name with ls calibration lum name
              do ilumdb = 1, db_mx%landuse
                if (lum(ilumdb)%name == lscal(i)%lum(ilum)%meas%name) then
                  lscal(i)%lum(ilum)%lum_no = ilumdb
                  exit
                end if
              end do
          end do
        end if 

      end do    !mreg
      exit
         
      end do 
      end if	  
        
      db_mx%lscal_reg = mreg
	  
      return
      end subroutine ls_regions_cal_read