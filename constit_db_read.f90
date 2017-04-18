      subroutine constit_db_read
      
      use basin_module
      use input_file_module
      use constituent_mass_module
      use jrw_datalib_module
         
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_const%cs_db, exist=i_exist)
      if (i_exist == 0 .or. in_const%cs_db == 'null') then
        allocate (cs_db(0:0))
      else
      do
        open (106,file=in_const%cs_db)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (106,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
          end do
          
        allocate (cs_db(0:imax))

        rewind (106)
        read (106,*) titldum
        read (106,*) header 
        
        do ip = 1, imax
          read (106,*) i
          backspace (106)
          read (106,*,iostat=eof) cs_db(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      
      db_mx%cs_db = imax

      close (106)
      return
      end subroutine constit_db_read