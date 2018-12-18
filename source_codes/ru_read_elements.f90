      subroutine ru_read_elements
    
      use hydrograph_module
      use input_file_module
      
      implicit none
  
      character (len=3) :: iobtyp     !none       |object type   
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: nspu                 !           |
      integer :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: max                  !           | 
      integer :: isp                  !none       |counter
      integer :: k                    !           |
      integer :: iob                  !           |
      integer :: iob1                 !none       |beginning of loop
      integer :: iob2                 !none       |ending of loop
      integer :: iru                  !none       |counter
      integer :: numb                 !           |
      integer :: ielem                !none       |counter
      integer :: ii                   !none       |counter
      integer :: ie1                  !none       |beginning of loop
      integer :: ie2                  !none       |ending of loop  
      integer :: ie                   !none       |counter
      integer :: iru_tot              !           |
      
      eof = 0
      imax = 0
      
      !!read data for each element in all subbasins
      inquire (file=in_ru%ru_ele, exist=i_exist)
      if (i_exist /= 0 .or. in_ru%ru_ele /= "null") then
      do
        open (107,file=in_ru%ru_ele)
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

        allocate (ru_def(imax))
        allocate (ru_elem(imax))
        allocate (ielem_ru(imax))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        ielem_ru = 0
   
        do isp = 1, imax
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, ru_elem(i)%name, ru_elem(i)%obtyp, ru_elem(i)%obtypno,     &
                                ru_elem(i)%htyp, ru_elem(i)%frac, ru_elem(i)%idr
          if (eof < 0) exit
        end do
        exit
      end do
      close (107)
              
      !read all delivery ratio data for subbasin deliveries
      inquire (file=in_ru%ru_dr, exist=i_exist)
      if (i_exist /= 0 .or. in_ru%ru_dr /= "null") then
      do
        open (107,file=in_ru%ru_dr)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0) 
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
             
        allocate (ru_dr(imax))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        do i = 1, imax
          read (107,*,iostat=eof) ru_dr(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      end if
      
      !!read subbasin definition data -ie. hru"s in the subbasin
      inquire (file=in_ru%ru_def, exist=i_exist)
      if (i_exist /= 0 .or. in_ru%ru_def /= "null") then
      do
        open (107,file=in_ru%ru_def)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do

        rewind (107)
        read (107,*) titldum
        read (107,*) header

      iob1 = sp_ob1%ru
      iob2 = sp_ob1%ru + sp_ob%ru - 1
      do iru = 1, sp_ob%ru
        iob = sp_ob1%ru + iru - 1
        ob(iob)%ru_tot = 0
        read (107,*,iostat=eof) numb, namedum, nspu
        
        if (eof < 0) exit
        if (nspu > 0) then
          backspace (107)
          allocate (elem_cnt(nspu))
          read (107,*,iostat=eof) numb, ru_def(iru)%name, nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
          
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
            if (nspu == 1) ie2 = ie1
            if (ii == nspu .and. elem_cnt(ii) < 0) exit
          end do
          allocate (ru_def(iru)%num(ielem))
          ru_def(iru)%num_tot = ielem
          iob = sp_ob1%ru + iru - 1
          ob(iob)%dfn_tot = ru_def(iru)%num_tot
          
          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              ru_def(iru)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                ru_def(iru)%num(ielem) = ie1
                ielem = ielem + 1
                ru_def(iru)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  ru_def(iru)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do

          ! determine how many subbasins the object is in
          do ii = ie1, ie2
            iobtyp = ru_elem(ii)%obtyp       !object type in sub
            select case (iobtyp)
            case ("hru")   !hru
              ru_elem(ii)%obj = sp_ob1%hru + ru_elem(ii)%obtypno - 1
            case ("hlt")   !hru_lte
              ru_elem(ii)%obj = sp_ob1%hru_lte + ru_elem(ii)%obtypno - 1
            case ("ru")   !ru
              ru_elem(ii)%obj = sp_ob1%ru + ru_elem(ii)%obtypno - 1
            case ("cha")   !channel
              ru_elem(ii)%obj = sp_ob1%chan + ru_elem(ii)%obtypno - 1
            case ("exc")   !export coefficient
              ru_elem(ii)%obj = sp_ob1%exco + ru_elem(ii)%obtypno - 1
            case ("dr")   !delivery ratio
              ru_elem(ii)%obj = sp_ob1%dr + ru_elem(ii)%obtypno - 1
            case ("out")   !outlet
              ru_elem(ii)%obj = sp_ob1%outlet + ru_elem(ii)%obtypno - 1
            case ("sdc")   !swat-deg channel
              ru_elem(ii)%obj = sp_ob1%chandeg + ru_elem(ii)%obtypno - 1
            end select
            k = ru_elem(ii)%obj
            ob(k)%ru_tot = ob(k)%ru_tot + 1
          end do
          
          do ii = ie1, ie2
          select case (ru_elem(ii)%htyp)
            case ("tot")   !total flow
               ru_elem(ii)%htypno = 1
            case ("rhg")   !recharge
               ru_elem(ii)%htypno = 2              
            case ("sur")   !surface
               ru_elem(ii)%htypno = 3 
            case ("lat")   !lateral
               ru_elem(ii)%htypno = 4
            case ("til")   !tile
               ru_elem(ii)%htypno = 5  
            end select
          end do

          deallocate (elem_cnt)  
        end if
      end do    ! i = subbasin object numbers
        exit
      enddo
      endif
      
        ! set all subbasins that each element is in
        do iru = 1, sp_ob%ru
          do ielem = 1, ru_def(iru)%num_tot
            ie = ru_def(iru)%num(ielem)
            iob = ru_elem(ie)%obj
            iru_tot = ob(iob)%ru_tot
            allocate (ob(iob)%ru(1))
          end do

          do ielem = 1, ru_def(iru)%num_tot
            ie = ru_def(iru)%num(ielem)
            ielem_ru(ie) = ielem_ru(ie) + 1
            iob = ru_elem(ie)%obj
            ob(iob)%ru(ielem_ru(ie)) = iru
            ob(iob)%elem = ielem
          end do
        end do

      close (107)
      return

      end subroutine ru_read_elements