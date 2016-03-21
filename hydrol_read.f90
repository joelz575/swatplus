      subroutine hydrol_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mhydrol = 0
      eof = 0
      imax = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_hyd%hydrol_hyd, exist=i_exist)
      if (i_exist == 0.or. in_hyd%hydrol_hyd == 'null') then
        allocate (hyd_db(0:0))
        allocate (hyd_xw(0:0))
      else
        do
          open (107,file=in_hyd%hydrol_hyd)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
             imax = imax + 1
          end do
          
          allocate (hyd_db(0:imax))
          allocate (hyd_xw(0:imax))
          
          rewind (107)
          read (107,*) titldum
          read (107,*) header
                 
          do ithyd = 1, imax
             read (107,*,iostat=eof) hyd_db(ithyd)
             
             !! hydrology.hyd
             hyd_xw(ithyd) = hyd_db(ithyd)%name
             
             if (eof < 0) exit
          end do
          exit
        enddo
      endif
      close (107)
 
      db_mx%hyd = imax
      
      return
      end subroutine hydrol_read