      subroutine cli_wmeas
      
      use input_file_module
      use climate_module
      use jrw_datalib_module, only : db_mx
      use time_module
      
      character (len=80) :: header
      character (len=80) :: titldum
      integer :: eof, i, imax, iyr

       eof = 0
       imax = 0

      !! read all measured daily solar radiation data
      inquire (file=in_cli%wnd_cli, exist=i_exist)
      if (i_exist == 0 .or. in_cli%wnd_cli == 'null') then
         allocate (wnd(0:0))
         allocate (wnd_n(0))
      else
      do 
        open (107,file=in_cli%wnd_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
      allocate (wnd(0:imax))
      allocate (wnd_n(imax))
      
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      do i = 1, imax
        read (107,*,iostat=eof) wnd_n(i)
        if (eof < 0) exit
      end do
      
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      
      do i = 1, imax
        read (107,*,iostat = eof) wnd(i)%filename
        if (eof < 0) exit
        
!!!!!weather path code
       if (in_path_wnd%wnd == 'null') then
         open (108,file = wnd(i)%filename)
       else
        open (108,file = TRIM(ADJUSTL(in_path_wnd%wnd))//wnd(i)%filename)
       endif
!!!!!weather path code
        
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        read (108,*,iostat=eof) wnd(i)%nbyr, tmp(i)%tstep, wnd(i)%lat, wnd(i)%long,     &
                               wnd(i)%elev
        if (eof < 0) exit
       
        ! the precip time step has to be the same as time%step
        allocate (wnd(i)%ts(366,wnd(i)%nbyr))

        ! read and store entire year
       do 
         read (108,*,iostat=eof) iyr, istep
         if (eof < 0) exit
         if (iyr == time%yrc .and. istep == time%idaf) exit
       end do
       
       backspace (108)
       iyr_prev = iyr
       iyrs = 1
       
       do
         read (108,*,iostat=eof) iyr, istep, wnd(i)%ts(istep,iyrs)
         if (eof < 0) exit
         if (istep == 365 .or. istep == 366) then
           read (108,*,iostat=eof) iyr, istep
           if (eof < 0) exit
           backspace (108)
           if (iyr /= iyr_prev) then
             iyr_prev = iyr
             iyrs = iyrs + 1
           end if
         end if
       end do
       close (108)

      end do
      close (107)
      exit
      end do
      endif
             
       db_mx%wndfiles = imax
       
      return
      end subroutine cli_wmeas