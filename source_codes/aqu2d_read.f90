      subroutine aqu2d_read
    
      use hydrograph_module
      use input_file_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: nspu                 !           |
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: isp                  !none       |counter
      integer :: numb                 !           |
      integer :: iaq                  !none       |counter
      integer :: iaq_db               !none       |counter
      integer :: ielem                !none       |counter
      integer :: ii                   !none       |counter
      integer :: ie1                  !none       |beginning of loop
      integer :: ie2                  !none       |ending of loop  
      integer :: ie                   !none       |counter
      integer :: aqu_tot              !           |counter for total aquifers

      eof = 0
      imax = 0
      aqu_tot = 0
      
    !!read data for aquifer elements for 2-D groundwater model
      inquire (file=in_link%aqu_cha, exist=i_exist)
      if (i_exist /= 0 .or. in_link%aqu_cha /= "null" ) then
      do
        open (107,file=in_link%aqu_cha)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
        do while (eof == 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
          aqu_tot = aqu_tot + 1
        end do
      end do

      allocate (aq_ch(sp_ob%aqu))
      rewind (107)
      read (107,*) titldum
      read (107,*) header

      do iaq_db = 1, aqu_tot

        read (107,*,iostat=eof) iaq, namedum, nspu
        if (eof < 0) exit
        
        if (nspu > 0) then
          backspace (107)
          allocate (elem_cnt(nspu))
          read (107,*,iostat=eof) numb, aq_ch(iaq)%name, nspu, (elem_cnt(isp), isp = 1, nspu)
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
          allocate (aq_ch(iaq)%num(ielem))
          aq_ch(iaq)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              aq_ch(iaq)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                aq_ch(iaq)%num(ielem) = ie1
                ielem = ielem + 1
                aq_ch(iaq)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  aq_ch(iaq)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
            deallocate (elem_cnt)
          end do

        end if
      end do
      end if

      close (107)
      
      return
      end subroutine aqu2d_read