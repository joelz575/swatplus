      subroutine toposub_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mtopo = 0
      eof = 0
      imax = 0
      
      !! read all subbasin data from toposub.top
      inquire (file=in_hyd%toposub_hyd, exist=i_exist)
      if (i_exist == 0 .or. in_hyd%toposub_hyd == 'null') then
        allocate (toposub_db(0:0))
      else
        do
          open (107,file=in_hyd%toposub_hyd)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          allocate (toposub_db(0:imax))
          
          rewind (107)
          read (107,*) titldum
          read (107,*) header
                   
          do ith = 1, imax 
             read (107,*,iostat=eof) k, toposub_db(ith)           
             if (eof < 0) exit
          end do
          exit
        enddo
      endif
      close (107)
      
         db_mx%toposub = imax  
         
      return  
      end subroutine toposub_read