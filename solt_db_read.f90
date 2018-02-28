      subroutine solt_db_read
      
      use input_file_module
      use hru_module, only : isolt
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      msolt_db = 0
      eof = 0
      imax = 0
      
      !! read all soil test operations data from soiltest.dat
      inquire (file=in_sol%nut_sol,exist=i_exist)
      if (i_exist == 0 .or. in_sol%nut_sol == 'null') then
        allocate (solt_db(0:0))
      else
        do
         open (107,file=in_sol%nut_sol)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
              imax = imax + 1
          end do
                
         allocate (solt_db(0:imax))
         
         rewind (107)
         read (107,*) titldum
         read (107,*) header       
                
         do isolt = 1, imax
          read (107,*,iostat=eof) solt_db(isolt)       
          if (eof < 0) exit
         end do 
       exit
        enddo
      endif
      
      db_mx%soiltest = imax
      
      close(107)      
      return  
      end subroutine solt_db_read