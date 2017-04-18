      subroutine cli_smeas
      
      use climate_parms
      use input_file_module
      
      character (len=80) :: header
      character (len=80) :: titldum
      integer :: eof, i, imax
       
       eof = 0
       imax = 0

      !! read all measured daily solar radiation data
      inquire (file=in_cli%slr_cli, exist=i_exist)
      if (i_exist == 0 .or. in_cli%slr_cli == 'null') then
        allocate (slr(0:0))
        allocate (slr_n(0))
      else
      do 
        open (107,file=in_cli%slr_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1 
          end do
          
      allocate (slr(0:imax))
      allocate (slr_n(imax))
      
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      do i = 1, imax
        read (107,*, iostat=eof) slr_n(i)
        if (eof < 0) exit
      end do
      
      rewind (107)
      read (107,*) titldum
      read (107,*) header      
      
      do i = 1, imax
        read (107,*,iostat = eof) slr(i)%filename
        if (eof < 0) exit
        open (108,file = slr(i)%filename)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        read (108,*,iostat=eof) slr(i)%nbyr, slr(i)%lat, slr(i)%long,     &
                                slr(i)%elev
        if (eof < 0) exit
       
        ! the precip time step has to be the same as time%step
        allocate (slr(i)%ts(366,slr(i)%nbyr))

        ! read and store entire year
       do 
         read (108,*,iostat=eof) iyr, istep, solrad
         if (eof < 0) exit
         if (iyr == time%yrc) exit
       end do
       
       backspace (108)
       iyr_prev = iyr
       iyrs = 1
       
       do
         read (108,*,iostat=eof) iyr, istep, slr(i)%ts(istep,iyrs)
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
      
      db_mx%slrfiles = imax
      
      return      
      end subroutine cli_smeas