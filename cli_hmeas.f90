      subroutine cli_hmeas

      use input_file_module
      
      character (len=80) :: header
      character (len=80) :: titldum
      integer :: eof, i, imax
       
       eof = 0
       imax = 0

      !! read all measured daily relative humidity data
      inquire (file=in_cli%hmd_cli, exist=i_exist)
      if (i_exist == 0 .or. in_cli%hmd_cli == 'null') then
         allocate (hmd(0:0))
      else
      do 
        open (107,file=in_cli%hmd_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = imax + 1
          end do
          
      allocate (hmd(0:imax))
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      
      do i = 1, imax
        read (107,*,iostat = eof) hmd(i)%filename
        if (eof < 0) exit
        open (108,file = hmd(i)%filename)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        read (108,*,iostat=eof) hmd(i)%nbyr, hmd(i)%lat, hmd(i)%long,     &
                                hmd(i)%elev
        if (eof < 0) exit
        

       ! the precip time step has to be the same as time%step
       allocate (hmd(i)%ts(366,hmd(i)%nbyr))
    
        ! read and store entire year
       do 
         read (108,*,iostat=eof) iyr, istep, relhum
         if (eof < 0) exit
         if (iyr == time%yrc) exit
       end do
       
       backspace (108)
       iyr_prev = iyr
       iyrs = 1
       
       do
         read (108,*,iostat=eof) iyr, istep, hmd(i)%ts(istep,iyrs)
         if (eof < 0) exit
         read (108,*,iostat=eof) iyr, istep
         if (eof < 0) exit
         backspace (108)
         if (iyr /= iyr_prev) then
           iyr_prev = iyr
           iyrs = iyrs + 1
         endif
       end do
       close (108)
       
      end do
      close (107)
      exit
      end do
      endif
      
      db_mx%rhfiles = imax
      
      return      
      end subroutine cli_hmeas