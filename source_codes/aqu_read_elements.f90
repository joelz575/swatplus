      subroutine aqu_read_elements
   
      use input_file_module
      use calibration_data_module
      use hydrograph_module
      use aquifer_module
      use maximum_data_module
      
      implicit none

      character (len=80) :: titldum     !             |title of file
      character (len=80) :: header      !             |header of file
      integer :: eof                    !             |end of file
      integer :: imax                   !             |determine max number for array (imax) and total number in file
      integer :: mcal                   !             |
      logical :: i_exist                !none         |check to determine if file exists
      integer :: mreg                   !             |
      integer :: i                      !none         |counter
      integer :: k                      !             |
      integer :: nspu                   !             | 
      integer :: isp                    !             |
      integer :: ielem                  !none         |counter
      integer :: ii                     !none         |counter
      integer :: ie1                    !none         |counter
      integer :: ie2                    !none         |counter
      integer :: ie                     !none         |counter
      integer :: ihru                   !none         |counter
      integer :: iaqu                   !none         |counter
      integer :: ireg                   !none         |counter
                
      mreg = 0
      imax = 0
      mcal = 0
            
    inquire (file=in_regs%def_aqu, exist=i_exist)
    if (i_exist .or. in_regs%def_aqu /= "null") then
      do
        open (107,file=in_regs%def_aqu)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        allocate (acu_reg(0:mreg)); allocate (acu_out(0:mreg)); allocate (acu_cal(0:mreg))
        !! allocate aquifer outputs for writing
        allocate (saqu_d(0:mreg)); allocate (saqu_m(0:mreg)); allocate (saqu_y(0:mreg)); allocate (saqu_a(0:mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) k, acu_out(i)%name, acu_out(i)%area_ha, nspu        
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, acu_out(i)%name, acu_out(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
          !!save the object number of each defining unit
          if (nspu == 1) then
            allocate (acu_out(i)%num(1))
            acu_out(i)%num_tot = 1
            acu_out(i)%num(1) = elem_cnt(1)
            deallocate (elem_cnt)
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
          allocate (acu_out(i)%num(ielem))
          acu_out(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              acu_out(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                acu_out(i)%num(ielem) = ie1
                ielem = ielem + 1
                acu_out(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  acu_out(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
          end if   !nspu > 1
        else
          !!all hrus are in region 
          allocate (acu_out(i)%num(sp_ob%hru))
          acu_out(i)%num_tot = sp_ob%hru
          do ihru = 1, sp_ob%hru
            acu_out(i)%num(ihru) = ihru
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%aqu_out = mreg
      end do 
      end if	  
        
    !! setting up regions for aquifer soft cal and/or output by type
    inquire (file=in_regs%def_aqu_reg, exist=i_exist)
    if (i_exist .or. in_regs%def_aqu_reg /= "null") then
      do
        open (107,file=in_regs%def_aqu)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
      do i = 1, mreg

        read (107,*,iostat=eof) k, acu_reg(i)%name, acu_reg(i)%area_ha, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, acu_reg(i)%name, acu_reg(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
          !!save the object number of each defining unit
          if (nspu == 1) then
            allocate (acu_reg(i)%num(1))
            acu_reg(i)%num_tot = 1
            acu_reg(i)%num(1) = elem_cnt(1)
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
          allocate (acu_reg(i)%num(ielem))
          acu_reg(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              acu_reg(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                acu_reg(i)%num(ielem) = ie1
                ielem = ielem + 1
                acu_reg(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  acu_reg(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
          end if   !nspu > 1
        else
          !!all hrus are in region 
          allocate (acu_reg(i)%num(sp_ob%hru))
          acu_reg(i)%num_tot = sp_ob%hru
          do iaqu = 1, sp_ob%aqu
            acu_reg(i)%num(ihru) = iaqu
          end do      
        end if

      end do    ! i = 1, mreg
      exit
                 
      db_mx%aqu_reg = mreg
      
      end do 
      end if	  

      !! if no regions are input, don"t need elements
      if (mreg > 0) then
      
      do ireg = 1, mreg
        acu_cal(ireg)%lum_ha_tot = 0.
        acu_cal(ireg)%lum_num_tot = 0
        acu_cal(ireg)%lum_ha_tot = 0.
        !allocate (region(ireg)%lum_ha_tot(db_mx%landuse))
        !allocate (region(ireg)%lum_num_tot(db_mx%landuse))
        !allocate (rwb_a(ireg)%lum(db_mx%landuse))
        !allocate (rnb_a(ireg)%lum(db_mx%landuse))
        !allocate (rls_a(ireg)%lum(db_mx%landuse))
        !allocate (rpw_a(ireg)%lum(db_mx%landuse))
      end do
      end if    ! mreg > 0
      
      !!read data for each element in all landscape cataloging units
      inquire (file=in_regs%ele_aqu, exist=i_exist)
      if (i_exist .or. in_regs%ele_aqu /= "null") then
      do
        open (107,file=in_regs%ele_aqu)
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

        allocate (acu_elem(imax))

        rewind (107)
        read (107,*) titldum
        read (107,*) header

        db_mx%aqu_elem = imax
        do isp = 1, imax
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, acu_elem(i)%name, acu_elem(i)%obtyp, acu_elem(i)%obtypno,      &
                                    acu_elem(i)%bsn_frac, acu_elem(i)%ru_frac, acu_elem(i)%reg_frac
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        do iaqu = 1, acu_reg(ireg)%num_tot      !elements have to be hru or hru_lte
          ielem = acu_reg(ireg)%num(iaqu)
          !switch %num from element number to hru number
          acu_cal(ireg)%num(iaqu) = acu_elem(ielem)%obtypno
          acu_cal(ireg)%hru_ha(iaqu) = acu_elem(ielem)%ru_frac * acu_cal(ireg)%area_ha
        end do
      end do
      
      close (107)

      return
      end subroutine aqu_read_elements