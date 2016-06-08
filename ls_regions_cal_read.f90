      subroutine ls_regions_cal_read
   
      use input_file_module
      use jrw_datalib_module
      use hydrograph_module

      integer, dimension (:), allocatable :: elem_cnt1
      character (len=80) :: titldum, header
      integer :: eof
      
      imax = 0
      mcal = 0
        
      do
        open (107,file = 'ls_regions.cal')
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
          backspace (107)
          allocate (elem_cnt1(nspu))
          read (107,*,iostat=eof) lscal(i)%name, lscal(i)%lum_num, nspu, (elem_cnt1(isp), isp = 1, nspu)
          if (eof < 0) exit
          
          !!set object elements when they are input (num_tot > 0)
          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt1(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
            else
              ie2 = elem_cnt1(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 2
              else
                ielem = ielem + (abs(ie2) - ie1) + 1
              end if
              ii = ii + 2
            end if
          end do
          allocate (lscal(i)%subs(ielem))
          lscal(i)%msubs = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt1(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              lscal(i)%subs(ielem) = ie1
            else
              ie2 = elem_cnt1(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                lscal(i)%subs(ielem) = ie1
                ielem = ielem + 1
                lscal(i)%subs(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  lscal(i)%subs(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt1)
        else
          lscal(i)%msubs = 0
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
        
      end do
      exit
         
      end do     
        
      db_mx%lscal_reg = mreg
      return
      end subroutine ls_regions_cal_read