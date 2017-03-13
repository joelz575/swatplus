      subroutine sub_elements_read
    
      use hydrograph_module
      use input_file_module
 
      character (len=80) :: titldum, header
      character (len=3) :: iobtyp
      character (len=16) :: namedum
      integer :: eof, imax, nspu

      eof = 0
      imax = 0
      
      !!read data for each element in all subbasins
      inquire (file=in_sub%ele_sub, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sub%ele_sub)
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
        
        allocate (sub_elem(imax))
        allocate (ielem_sub(imax))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        ielem_sub = 0
   
        do isp = 1, imax
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, sub_elem(i)%name,               &
           sub_elem(i)%obtyp, sub_elem(i)%obtypno,                   &
           sub_elem(i)%htyp, sub_elem(i)%frac, sub_elem(i)%idr
          if (eof < 0) exit
        end do
        exit
      end do
      close (107)
              
      !read all delivery ratio data for subbasin deliveries
      inquire (file=in_sub%sub_del, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sub%sub_del)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0) 
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
             
        allocate (sub_dr(imax))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        do i = 1, imax
          read (107,*,iostat=eof) sub_dr(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      end if
      
      !!read subbasin definition data -ie. hru's in the subbasin
      inquire (file=in_sub%def_sub, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sub%def_sub)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do

        rewind (107)
        read (107,*) titldum
        read (107,*) header

      iob1 = sp_ob1%sub
      iob2 = sp_ob1%sub + sp_ob%sub - 1
      do i = iob1, iob2
        isub = ob(i)%props
        !ob(i)%typ = "sub"
        ob(i)%subs_tot = 0
        read (107,*,iostat=eof) numb, namedum, nspu
        allocate (elem_cnt(nspu))
        
        if (eof < 0) exit
        if (nspu > 0) then
          backspace (107)
          read (107,*,iostat=eof) numb,namedum, nspu,                     &
                                         (elem_cnt(isp), isp = 1, nspu)
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
            if (ii == nspu .and. elem_cnt(ii) < 0) exit
          end do
          allocate (sub_d(isub)%num(ielem))
          sub_d(isub)%num_tot = ielem
          ob(i)%dfn_tot = sub_d(isub)%num_tot
          
          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              sub_d(isub)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                sub_d(isub)%num(ielem) = ie1
                ielem = ielem + 1
                sub_d(isub)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  sub_d(isub)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
 !       end if
     
          ! determine how many subbasins the object is in
          do ii = ie1, ie2
            iobtyp = sub_elem(ii)%obtyp       !object type in sub
            select case (iobtyp)
            case ("hru")   !hru
              sub_elem(ii)%obj = sp_ob1%hru + sub_elem(ii)%obtypno - 1
            case ("hlt")   !hru_lte
              sub_elem(ii)%obj = sp_ob1%hru_lte + sub_elem(ii)%obtypno - 1
            case ("cha")   !channel
              sub_elem(ii)%obj = sp_ob1%chan + sub_elem(ii)%obtypno - 1
            case ("exc")   !export coefficient
              sub_elem(ii)%obj = sp_ob1%exco + sub_elem(ii)%obtypno - 1
            case ("del")   !delivery ratio
              sub_elem(ii)%obj = sp_ob1%dr + sub_elem(ii)%obtypno - 1
            case ("out")   !outlet
              sub_elem(ii)%obj = sp_ob1%outlet + sub_elem(ii)%obtypno - 1
            case ("sdc")   !swat-deg channel
              sub_elem(ii)%obj = sp_ob1%chandeg + sub_elem(ii)%obtypno - 1
            end select
            k = sub_elem(ii)%obj
            ob(k)%subs_tot = ob(k)%subs_tot + 1
          end do
          
          do ii = ie1, ie2
          select case (sub_elem(ii)%htyp)
            case ("tot")   !total flow
               sub_elem(ii)%htypno = 1
            case ("rhg")   !recharge
               sub_elem(ii)%htypno = 2              
            case ("sur")   !surface
               sub_elem(ii)%htypno = 3 
            case ("lat")   !lateral
               sub_elem(ii)%htypno = 4
            case ("til")   !tile
               sub_elem(ii)%htypno = 5  
            end select
          end do
          
        end if
        deallocate (elem_cnt)
      end do    ! i = subbasin object numbers
        exit
      enddo
      endif
      
        ! set all subbasins that each element is in
        do ielem = 1, msub_elems
          iob = sub_elem(ielem)%obj
          isub_tot = ob(iob)%subs_tot
          allocate (ob(iob)%sub(isub_tot))
        end do
        do isub = 1, sp_ob%sub
          do ielem = 1, sub_d(isub)%num_tot
            ie = sub_d(isub)%num(ielem)
            ielem_sub(ie) = ielem_sub(ie) + 1
            iob = sub_elem(ie)%obj
            ob(iob)%sub(ielem_sub(ie)) = isub
            ob(iob)%elem = ielem
          end do
        end do

      close (107)
      return

      end subroutine sub_elements_read