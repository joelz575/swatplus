      subroutine overbank_read
    
      use hydrograph_module
      use input_file_module
      use jrw_datalib_module
      
      character (len=80) :: titldum, header
      character (len=16) :: namedum
      integer :: eof, imax, nspu
      
      eof = 0
      imax = 0
      
    !!read data for surface elements in the floodplain-for overbank flooding
      inquire (file=in_link%chan_surf, exist=i_exist)
      if (i_exist /= 0 .or. in_link%chan_surf /= 'null') then
      do
        open (107,file=in_link%chan_surf)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mcha_sp
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
        do while (eof == 0)
          read (107,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
          
        allocate (ch_sur(imax))
        rewind (107)
        read (107,*) titldum
        read (107,*,iostat=eof) mcha_sp
        read (107,*) header

        !db_mx%ch_surf
        do ise = 1, mcha_sp
          read (107,*,iostat=eof) i, namedum, nspu
          allocate (ch_sur(i)%obtyp(nspu))
          allocate (ch_sur(i)%obtypno(nspu))
          allocate (ch_sur(i)%wid(nspu))
          allocate (ch_sur(i)%dep(nspu))
          allocate (ch_sur(i)%flood_volmx(nspu))
          allocate (ch_sur(i)%hd(nspu))
        
          if (eof < 0) exit
          if (nspu > 0) then
            backspace (107)
            read (107,*,iostat=eof) numb, ch_sur(i)%name, ch_sur(i)%num,     &
           (ch_sur(i)%obtyp(isp), ch_sur(i)%obtypno(isp), isp = 1, nspu)
            if (eof < 0) exit
          end if

        end do
        exit
      end do
      close (107)
      end if
      return
      end subroutine overbank_read