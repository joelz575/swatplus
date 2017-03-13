      subroutine rec_elements_read
   
      use input_file_module
      use jrw_datalib_module
      use hydrograph_module

      character (len=80) :: titldum, header
      integer :: eof
      
      imax = 0
      mcal = 0
            
    inquire (file=in_regs%def_psc, exist=i_exist)
    if (i_exist /= 0 .or. in_regs%def_psc /= 'null') then
      do
        open (107,file=in_regs%def_res)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        
        !! allocate recall outputsby region
        allocate (srec_d(0:mreg)); allocate (srec_m(0:mreg)); allocate (srec_y(0:mreg)); allocate (srec_a(0:mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) k, pcu_out(i)%name, pcu_out(i)%area_ha, nspu
        
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, pcu_out(i)%name, pcu_out(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)

          !!save the object number of each defining unit
          if (nspu == 1) then
            allocate (pcu_out(i)%num(1))
            pcu_out(i)%num_tot = 1
            pcu_out(i)%num(1) = elem_cnt(1)
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
          allocate (pcu_out(i)%num(ielem))
          pcu_out(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              pcu_out(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                pcu_out(i)%num(ielem) = ie1
                ielem = ielem + 1
                pcu_out(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  pcu_out(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
          end if   !nspu > 1
        else
          !!all hrus are in region 
          allocate (pcu_out(i)%num(sp_ob%hru))
          pcu_out(i)%num_tot = sp_ob%recall
          do irec = 1, sp_ob%recall
            pcu_out(i)%num(irec) = irec
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%rec_out = mreg
      end do 
      end if	  
        
    !! setting up regions for recall soft cal and/or output by type
    inquire (file=in_regs%def_psc_reg, exist=i_exist)
    if (i_exist /= 0 .or. in_regs%def_psc_reg /= 'null') then
      do
        open (107,file=in_regs%def_res)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header

      do i = 1, mreg

        read (107,*,iostat=eof) k, pcu_reg(i)%name, pcu_reg(i)%area_ha, nspu
        
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, pcu_reg(i)%name, pcu_reg(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)

          !!save the object number of each defining unit
          if (nspu == 1) then
            allocate (pcu_reg(i)%num(1))
            pcu_reg(i)%num_tot = 1
            pcu_reg(i)%num(1) = elem_cnt(1)
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
          allocate (pcu_reg(i)%num(ielem))
          pcu_reg(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              pcu_reg(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                pcu_reg(i)%num(ielem) = ie1
                ielem = ielem + 1
                pcu_reg(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  pcu_reg(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
          end if   !nspu > 1
        else
          !!all hrus are in region 
          allocate (pcu_reg(i)%num(sp_ob%hru))
          pcu_reg(i)%num_tot = sp_ob%hru
          do ihru = 1, sp_ob%hru
            pcu_reg(i)%num(ihru) = ihru
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%rec_reg = mreg
      end do 
      end if	  
      
      !! if no regions are input, don't need elements
      if (mreg > 0) then
        do ireg = 1, mreg
          pcu_cal(ireg)%lum_ha_tot = 0.
          pcu_cal(ireg)%lum_num_tot = 0
          pcu_cal(ireg)%lum_ha_tot = 0.
          !allocate (region(ireg)%lum_ha_tot(db_mx%landuse))
          !allocate (region(ireg)%lum_num_tot(db_mx%landuse))
          !allocate (rwb_a(ireg)%lum(db_mx%landuse))
          !allocate (rnb_a(ireg)%lum(db_mx%landuse))
          !allocate (rls_a(ireg)%lum(db_mx%landuse))
          !allocate (rpw_a(ireg)%lum(db_mx%landuse))
        end do
      end if    ! mreg > 0
      
      !!read data for each element in all landscape cataloging units
      inquire (file='in_regs%ele_psc', exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file='in_regs%ele_psc')
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof <= 0)
              read (107,*,iostat=eof) i
              if (eof < 0) exit
              imax = Max(i,imax)
          end do
       
        msub_elems = imax
        
        allocate (pcu_elem(imax))

        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        ielem_sub = 0
   
        do isp = 1, imax
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, pcu_elem(i)%name, pcu_elem(i)%obtyp, pcu_elem(i)%obtypno,      &
                                    pcu_elem(i)%bsn_frac, pcu_elem(i)%sub_frac, pcu_elem(i)%reg_frac
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        do ires = 1, pcu_reg(ireg)%num_tot      !elements have to be hru or hru_lte
          ielem = pcu_reg(ireg)%num(ires)
          !switch %num from element number to hru number
          pcu_cal(ireg)%num(ihru) = pcu_elem(ielem)%obtypno
          pcu_cal(ireg)%hru_ha(ihru) = pcu_elem(ielem)%sub_frac * pcu_cal(ireg)%area_ha
        end do
      end do
      
      close (107)

      return
      end subroutine rec_elements_read