      subroutine snowdb_read
      
      use input_file_module
      use maximum_data_module
      use hydrology_data_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      msno = 0
      eof = 0
      imax = 0
      
      
      !! read snow database data from snow.sno
      inquire (file=in_parmdb%snow, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%snow == 'null') then
        allocate (snodb(0:0))
      else 
      do 
        open (107,file=in_parmdb%snow)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        allocate (snodb(0:imax))
    
        do isno = 1, imax
          read (107,*,iostat=eof) snodb(isno)         
          if (eof < 0) exit
        end do

      exit
      enddo
      
      endif
      close (107)
      
      db_mx%sno = imax
      
      return
      end subroutine snowdb_read