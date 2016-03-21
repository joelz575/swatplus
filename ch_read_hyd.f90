      subroutine ch_read_hyd (imax)
      
      use basin_module
      use input_file_module

      character (len=80) :: titldum, header
      integer :: eof, mon, i, imax

      eof = 0
      imax = 0
      mch_i = 0
      
      inquire (file=in_cha%init, exist=i_exist)
      if (i_exist == 0 .or. in_cha%init == 'null') then
        allocate (ch_init(0:0))
      else   
      do
       open (105,file=in_cha%init)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof >= 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
          mch_i = mch_i + 1
        end do
      
      allocate (ch_init(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
      
       do ich = 1, mch_i
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, ch_init(ich)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif

      imax = 0
      mch_i = 0
      inquire (file=in_cha%dat, exist=i_exist)
      if (i_exist == 0 .or. in_cha%dat == 'null') then
        allocate (ch_dat(0:0))
      else   
      do
       open (105,file=in_cha%dat)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
       mch_i = 0
        do while (eof >= 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
          mch_i = mch_i + 1
        end do
      
      allocate (ch_dat(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
     
       do ich = 1, mch_i
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, ch_dat(ich)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif
        
      mch_i = 0
      imax = 0
      inquire (file=in_cha%hyd, exist=i_exist)
      if (i_exist == 0 .or. in_cha%hyd == 'null') then
        allocate (ch_hyd(0:0))
      else   
      do
       open (105,file=in_cha%hyd)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof >= 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
          mch_i = mch_i + 1
        end do
      
      allocate (ch_hyd(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
    
       do ich = 1, mch_i
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, ch_hyd(ich)
         if (eof < 0) exit
         
        ch_hyd(ich)%alpha_bnk = Exp(-ch_hyd(ich)%alpha_bnk)
        if (ch_hyd(ich)%s <= 0.) ch_hyd(ich)%s = .0001
        if (ch_hyd(ich)%n <= 0.01) ch_hyd(ich)%n = .01
        if (ch_hyd(ich)%n >= 0.70) ch_hyd(ich)%n = 0.70
        if (ch_hyd(ich)%l <= 0.) ch_hyd(ich)%l = .0010
        if (ch_hyd(ich)%wdr <= 0.) ch_hyd(ich)%wdr = 3.5
        if (ch_hyd(ich)%side <= 1.e-6) ch_hyd(ich)%side = 2.0
        
       end do
       close (105)
      exit
      enddo
      endif

      return    
      end subroutine ch_read_hyd