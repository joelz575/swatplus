      subroutine cli_pmeas
      
      use climate_parms
      use basin_module
      use input_file_module
      use time_module
      
      character (len=80) :: header
      character (len=80) :: titldum
      character (len=5) :: hr_min
      integer :: eof, i, imax
       
       mpcp = 0
       eof = 0
       imax = 0

      !! read all measured daily precipitation data
      inquire (file=in_cli%pcp_cli, exist=i_exist)
      if (i_exist == 0 .or. in_cli%pcp_cli == 'null') then
        allocate (pcp(0:0))
      else
      do 
        open (107,file=in_cli%pcp_cli)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
      allocate (pcp(0:imax))
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      
      do i = 1, imax
        read (107,*,iostat = eof) pcp(i)%filename
        if (eof < 0) exit
        
        open (108,file = pcp(i)%filename)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
        read (108,*,iostat=eof) pcp(i)%nbyr, pcp(i)%tstep, pcp(i)%lat,    & 
                pcp(i)%long, pcp(i)%elev
        if (eof < 0) exit
        
        if (pcp(i)%tstep > 0) then
          ! the precip time step has to be the same as time%step
          allocate (pcp(i)%tss(time%step,366,pcp(i)%nbyr))
        else
          allocate (pcp(i)%ts(366,pcp(i)%nbyr))
        end if
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
         if (pcp(i)%tstep > 0.) then
           do iss = 1, time%step
             read (108,*,iostat=eof)iyr, istep, hr_min,               & 
                  pcp(i)%tss(iss,istep,iyrs)
             if (eof < 0) exit
           end do
         else    
           read (108,*,iostat=eof)iyr, istep, pcp(i)%ts(istep,iyrs)
           if (eof < 0) exit
         endif 
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
      
      db_mx%pcpfiles = imax

      return
      end subroutine cli_pmeas