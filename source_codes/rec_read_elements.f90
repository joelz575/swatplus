      subroutine rec_read_elements
   
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      implicit none

      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: imax                 !none       |end of loop
      integer :: isp                  !none       |counter
      integer :: mcal                 !           |           
      integer :: mreg                 !none       |end of loop
      integer :: i                    !none       |counter
      integer :: k                    !           |  
      integer :: nspu                 !none       |end of loop
      integer :: ielem                !           | 
      integer :: ii                   !none       |counter
      integer :: ie1                  !none       |beginning of loop
      integer :: max                  !           |
      integer :: ie                   !none       |counter
      integer :: ireg                 !none       |counter
      integer :: irec                 !none       |counter
      integer :: ie2
      
      imax = 0
      mcal = 0
      mreg = 0
            
    inquire (file=in_regs%def_psc, exist=i_exist)
    if (i_exist .or. in_regs%def_psc /= "null") then
      do
        open (107,file=in_regs%def_psc)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        
        !! allocate recall outputsby region
        allocate (srec_d(0:mreg)); allocate (srec_m(0:mreg)); allocate (srec_y(0:mreg)); allocate (srec_a(0:mreg))

      do i = 1, mreg

        read (107,*,iostat=eof) k, pcu_out(i)%name, pcu_out(i)%area_ha, nspu      
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, pcu_out(i)%name, pcu_out(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit

          !!save the object number of each defining unit
          if (nspu == 1) then
            allocate (pcu_out(i)%num(1))
            pcu_out(i)%num_tot = 1
            pcu_out(i)%num(1) = elem_cnt(1)
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
    if (i_exist .or. in_regs%def_psc_reg /= "null") then
      do
        open (107,file=in_regs%def_psc_reg)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      do i = 1, mreg

        read (107,*,iostat=eof) k, pcu_reg(i)%name, pcu_reg(i)%area_ha, nspu       
        if (eof < 0) exit
        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) k, pcu_reg(i)%name, pcu_reg(i)%area_ha, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
          
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
          allocate (pcu_reg(i)%num(sp_ob%recall))
          pcu_reg(i)%num_tot = sp_ob%recall
          do irec = 1, sp_ob%recall
            pcu_reg(i)%num(irec) = irec
          end do      
        end if

      end do    ! i = 1, mreg
      exit
         
      db_mx%rec_reg = mreg
      end do 
      end if	  
      
      !! if no regions are input, don"t need elements
      if (mreg > 0) then
        do ireg = 1, mreg
          pcu_cal(ireg)%lum_ha_tot = 0.
          pcu_cal(ireg)%lum_num_tot = 0
          pcu_cal(ireg)%lum_ha_tot = 0.
        end do
      end if    ! mreg > 0
      
      !!read data for each element in all landscape cataloging units
      inquire (file=in_regs%ele_psc, exist=i_exist)
      if (i_exist .or. in_regs%ele_psc /= "null") then
      do
        open (107,file=in_regs%ele_psc)
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

        allocate (pcu_elem(imax))

        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        ielem_ru = 0
   
        do isp = 1, imax
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          backspace (107)
          read (107,*,iostat=eof) k, pcu_elem(i)%name, pcu_elem(i)%obtyp, pcu_elem(i)%obtypno,      &
                                    pcu_elem(i)%bsn_frac, pcu_elem(i)%ru_frac, pcu_elem(i)%reg_frac
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        do irec = 1, pcu_reg(ireg)%num_tot
          ielem = pcu_reg(ireg)%num(irec)
          !switch %num from element number to hru number
          pcu_cal(ireg)%num(irec) = pcu_elem(ielem)%obtypno
          pcu_cal(ireg)%hru_ha(irec) = pcu_elem(ielem)%ru_frac * pcu_cal(ireg)%area_ha
        end do
      end do
      
      close (107)

      return
      end subroutine rec_read_elements