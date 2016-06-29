      subroutine aqu2d_read
    
      use hydrograph_module
      use input_file_module
      
      character (len=80) :: titldum, header
      character (len=16) :: namedum
      integer :: eof, imax, nspu
      
      eof = 0
      imax = 0
      
    !!read data for aquifer elements for 2-D groundwater model
      inquire (file=in_link%chan_aqu, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_link%chan_aqu)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mcha_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
        do while (eof >= 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
          
        allocate (ch_aqu(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*,iostat=eof) mcha_sp
        read (107,*) header

        do ise = 1, mcha_sp
          read (107,*,iostat=eof) i, namedum, nspu
          allocate (ch_aqu(i)%aqu_no(nspu))

          if (eof < 0) exit
          if (nspu > 0) then
            backspace (107)
            read (107,*,iostat=eof) numb, ch_aqu(i)%name, ch_aqu(i)%num,       &
                                 (ch_aqu(i)%aqu_no(isp), isp = 1, nspu)
            if (eof < 0) exit
          end if
        end do
      end do
      close (107)
      end if
      return
      end subroutine aqu2d_read