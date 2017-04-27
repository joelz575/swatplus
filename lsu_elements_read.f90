      subroutine lsu_elements_read
   
      use input_file_module
      use jrw_datalib_module
      use hydrograph_module

      character (len=80) :: titldum, header
      integer :: eof
      
      imax = 0
      mcal = 0
            
    !! read landscape cataloging unit definitions for output (old subbasin output file)
    inquire (file=in_regs%def_lsu, exist=i_exist)
    if (i_exist /= 0 .or. in_regs%def_lsu /= 'null') then
      do
        open (107,file=in_regs%def_lsu)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        
        !allocate subbasin (landscape unit) inputs and outputs
        allocate (lsu_out(0:mreg))
        allocate (ruwb_d(0:mreg)); allocate (ruwb_m(0:mreg)); allocate (ruwb_y(0:mreg)); allocate (ruwb_a(0:mreg))
        allocate (runb_d(0:mreg)); allocate (runb_m(0:mreg)); allocate (runb_y(0:mreg)); allocate (runb_a(0:mreg))
        allocate (ruls_d(0:mreg)); allocate (ruls_m(0:mreg)); allocate (ruls_y(0:mreg)); allocate (ruls_a(0:mreg))
        allocate (rupw_d(0:mreg)); allocate (rupw_m(0:mreg)); allocate (rupw_y(0:mreg)); allocate (rupw_a(0:mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) k, lsu_out(i)%name, lsu_out(i)%area_ha, nspu
        
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, lsu_out(i)%name, lsu_out(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)

          !!save the object number of each defining unit
          if (nspu == 1) then
            allocate (lsu_out(i)%num(1))
            lsu_out(i)%num_tot = 1
            lsu_out(i)%num(1) = elem_cnt(1)
          else
          !! nspu > 1
          ielem = 0
          do ii = 2, nspu
            ie1 = elem_cnt(ii-1)
            if (elem_cnt(ii) > 0) then
              if (ii == nspu) then
                ielem = ielem + 1
              else
                if (elem_cnt(ii+1) > 0) then
                  ielem = ielem + 1
                end if
              end if
            else
              ielem = ielem + abs(elem_cnt(ii)) - elem_cnt(ii-1) + 1
            end if
          end do
          allocate (lsu_out(i)%num(ielem))
          lsu_out(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              lsu_out(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                lsu_out(i)%num(ielem) = ie1
                ielem = ielem + 1
                lsu_out(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  lsu_out(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
          end if   !nspu > 1
        else
          !!all hrus are in region 
          allocate (lsu_out(i)%num(sp_ob%hru))
          lsu_out(i)%num_tot = sp_ob%hru
          do ihru = 1, sp_ob%hru
            lsu_out(i)%num(ihru) = ihru
          end do      
        end if

      end do    ! i = 1, mreg

      db_mx%lsu_out = mreg
      
      exit
      end do 
      end if	  
        
    !! setting up regions for landscape soft cal and/or output by landuse
    inquire (file=in_regs%def_lsu_reg, exist=i_exist)
    if (i_exist /= 0 .or. in_regs%def_lsu_reg /= 'null') then
      do
        open (107,file=in_regs%def_lsu_reg)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header

      do i = 1, mreg

        read (107,*,iostat=eof) k, lsu_reg(i)%name, lsu_reg(i)%area_ha, nspu
        
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, lsu_reg(i)%name, lsu_reg(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)

          !!save the object number of each defining unit
          if (nspu == 1) then
            allocate (lsu_reg(i)%num(1))
            lsu_reg(i)%num_tot = 1
            lsu_reg(i)%num(1) = elem_cnt(1)
          else
          !! nspu > 1
          ielem = 0
          do ii = 2, nspu
            ie1 = elem_cnt(ii-1)
            if (elem_cnt(ii) > 0) then
              if (ii == nspu) then
                ielem = ielem + 1
              else
                if (elem_cnt(ii+1) > 0) then
                  ielem = ielem + 1
                end if
              end if
            else
              ielem = ielem + abs(elem_cnt(ii)) - elem_cnt(ii-1) + 1
            end if
          end do
          allocate (lsu_reg(i)%num(ielem))
          lsu_reg(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              lsu_reg(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                lsu_reg(i)%num(ielem) = ie1
                ielem = ielem + 1
                lsu_reg(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  lsu_reg(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
          end if   !nspu > 1
        else
          !!all hrus are in region 
          allocate (lsu_reg(i)%num(sp_ob%hru))
          lsu_reg(i)%num_tot = sp_ob%hru
          do ihru = 1, sp_ob%hru
            lsu_reg(i)%num(ihru) = ihru
          end do      
        end if

      end do    ! i = 1, mreg
      exit
      
      db_mx%lsu_reg = mreg
      end do 
      end if	  
      
      !! if no regions are input, don't need elements
      if (mreg > 0) then
      
      !! allocate regional output files
      allocate (region(0:mreg))
      allocate (rwb_d(mreg)); allocate (rwb_m(mreg)); allocate (rwb_y(mreg)); allocate (rwb_a(mreg))
      allocate (rnb_d(mreg)); allocate (rnb_m(mreg)); allocate (rnb_y(mreg)); allocate (rnb_a(mreg))
      allocate (rls_d(mreg)); allocate (rls_m(mreg)); allocate (rls_y(mreg)); allocate (rls_a(mreg))
      allocate (rpw_d(mreg)); allocate (rpw_m(mreg)); allocate (rpw_y(mreg)); allocate (rpw_a(mreg))

      !! allocate land use within each region for soft cal and output
      do ireg = 1, mreg      
        allocate (region(ireg)%lum_ha_tot(db_mx%landuse))
        allocate (region(ireg)%lum_num_tot(db_mx%landuse))
        region(ireg)%lum_ha_tot = 0.
        region(ireg)%lum_num_tot = 0
        region(ireg)%lum_ha_tot = 0.
        allocate (rwb_a(ireg)%lum(db_mx%landuse))
        allocate (rnb_a(ireg)%lum(db_mx%landuse))
        allocate (rls_a(ireg)%lum(db_mx%landuse))
        allocate (rpw_a(ireg)%lum(db_mx%landuse))
      end do
      end if    ! mreg > 0
      
      !!read data for each element in all landscape cataloging units
      inquire (file=in_regs%ele_lsu, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_regs%ele_lsu)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof == 0)
              read (107,*,iostat=eof) i
              if (eof < 0) exit
              imax = Max(i,imax)
          end do

        allocate (lsu_elem(imax))

        rewind (107)
        read (107,*) titldum
        read (107,*) header

        db_mx%lsu_elem = imax
        do isp = 1, imax
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, lsu_elem(i)%name, lsu_elem(i)%obtyp, lsu_elem(i)%obtypno,      &
                                    lsu_elem(i)%bsn_frac, lsu_elem(i)%sub_frac, lsu_elem(i)%reg_frac
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        do ihru = 1, region(ireg)%num_tot      !elements have to be hru or hru_lte
          ielem = region(ireg)%num(ihru)
          !switch %num from element number to hru number
          region(ireg)%num(ihru) = lsu_elem(ielem)%obtypno
          region(ireg)%hru_ha(ihru) = lsu_elem(ielem)%sub_frac * region(ireg)%area_ha
        end do
      end do
      
      close (107)

      return
      end subroutine lsu_elements_read