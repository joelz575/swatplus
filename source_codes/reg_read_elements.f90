      subroutine reg_read_elements
   
      use input_file_module
      use maximum_data_module
      use calibration_data_module
      use landuse_data_module
      use hydrograph_module
      use hru_module, only : hru, ihru 
      use output_landscape_module
      
      implicit none

      character (len=80) :: titldum     !             |title of file
      character (len=80) :: header      !             |header of file
      integer :: eof                    !             |end of file
      integer :: i_exist                !none         |check to determine if file exists
      integer :: imax                   !             |determine max number for array (imax) and total number in file
      integer :: mcal                   !             |
      integer :: mreg                   !             |
      integer :: mlug
      integer :: ireg
      integer :: i                      !none         |counter
      integer :: k                      !             |
      integer :: ilum
      integer :: nspu                   !             | 
      integer :: isp                    !             |
      integer :: ielem                  !none         |counter
      integer :: ii                     !none         |counter
      integer :: ie1                    !none         |counter
      integer :: ie2                    !none         |counter
      integer :: ie                     !none         |counter
      integer :: iihru                  !none         |counter
      integer :: ihru_tot               !             |
      integer :: ilsu                   !             |
      
      imax = 0
      mcal = 0
            
    !! setting up regions for landscape soft cal and/or output by landuse
    inquire (file=in_regs%def_reg, exist=i_exist)
    if (i_exist /= 0 .or. in_regs%def_reg /= "null") then
      do
        open (107,file=in_regs%def_reg)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mreg, mlug
        if (eof < 0) exit

        !! allocate regional output files
        allocate (lsu_reg(0:mreg))
        allocate (region(0:mreg))
        allocate (rwb_d(mreg)); allocate (rwb_m(mreg)); allocate (rwb_y(mreg)); allocate (rwb_a(mreg))
        allocate (rnb_d(mreg)); allocate (rnb_m(mreg)); allocate (rnb_y(mreg)); allocate (rnb_a(mreg))
        allocate (rls_d(mreg)); allocate (rls_m(mreg)); allocate (rls_y(mreg)); allocate (rls_a(mreg))
        allocate (rpw_d(mreg)); allocate (rpw_m(mreg)); allocate (rpw_y(mreg)); allocate (rpw_a(mreg))

        db_mx%landuse = mlug
      
        !read the land use groups within each region
        allocate (region(ireg)%lumc(mlug))
        allocate (lum_grp%name(mlug))
        if (mlug > 0) then
          backspace (107)
          read (107,*,iostat=eof) i, lum_grp%num, (lum_grp%name(ilum), ilum = 1, mlug)
          if (eof < 0) exit
        end if
        read (107,*,iostat=eof) header
 
      !! if no regions are input, don"t need elements
      if (mreg > 0) then

      !! allocate land use within each region for soft cal and output
      do ireg = 1, mreg
        allocate (region(ireg)%lum_ha_tot(mlug))
        allocate (region(ireg)%lum_num_tot(mlug))
        region(ireg)%lum_ha_tot = 0.
        region(ireg)%lum_num_tot = 0
        region(ireg)%lum_ha_tot = 0.
        allocate (rwb_a(ireg)%lum(mlug))
        allocate (rnb_a(ireg)%lum(mlug))
        allocate (rls_a(ireg)%lum(mlug))
        allocate (rpw_a(ireg)%lum(mlug))
      end do
      end if    ! mreg > 0

      db_mx%lsu_reg = mreg
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

      end do 
      end if	  

      !!read data for each element in all landscape cataloging units
      inquire (file=in_regs%ele_reg, exist=i_exist)
      if (i_exist /= 0 .or. in_regs%ele_reg /= "null") then
      do
        open (107,file=in_regs%ele_reg)
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

        allocate (reg_elem(imax))

        rewind (107)
        read (107,*) titldum
        read (107,*) header

        db_mx%reg_elem = imax
        do isp = 1, imax
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, reg_elem(i)%name, reg_elem(i)%ha, reg_elem(i)%obtyp, reg_elem(i)%obtypno
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        ihru_tot = 0
        do ielem = 1, db_mx%lsu_reg     !lsu_reg(ireg)%num_tot      !elements - lsu, hru or hru_lte
          select case (reg_elem(ielem)%obtyp)
          case ("hru")
            ihru_tot = ihru_tot + 1
          case ("lsu")
            ilsu = reg_elem(ielem)%obtypno
            ihru_tot = ihru_tot + lsu_out(ilsu)%num_tot
          end select
        end do
      end do
      
      ! set hru number from element number and set hru areas in the region
      do ireg = 1, mreg
        ihru = 0
        region(ireg)%num_tot = ihru_tot
        allocate (region(ireg)%num(ihru_tot))
        allocate (region(ireg)%hru_ha(ihru_tot))
        do ielem = 1, db_mx%lsu_reg     !lsu_reg(ireg)%num_tot      !elements - lsu, hru or hru_lte
          select case (reg_elem(ireg)%obtyp)
          case ("hru")
            ! xwalk lum groups
            ihru = ihru + 1
            region(ireg)%num(ihru) = reg_elem(ielem)%obtypno
            region(ireg)%hru_ha(ihru) = hru(ihru)%area_ha
          case ("lsu")
            ilsu = reg_elem(ielem)%obtypno
            do iihru = 1, lsu_out(ilsu)%num_tot
              ihru = ihru + 1
              region(ireg)%num(ihru) = lsu_elem(iihru)%obtypno
              region(ireg)%hru_ha(ihru) = lsu_elem(iihru)%ru_frac * lsu_out(ilsu)%area_ha
            end do
          end select
        end do
      end do

      close (107)

      return
      end subroutine reg_read_elements