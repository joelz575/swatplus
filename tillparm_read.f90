      subroutine tillparm_read
      
      use input_file_module

      integer :: it     
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: tilldbase
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mtl = 0
      
      inquire (file=in_parmdb%till_til, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%till_til == 'null') then
          allocate (tilldb(0:0))
      else
      do
        open (105,file=in_parmdb%till_til)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (tilldb(0:imax)) 
        
        rewind (105)
        read (105,*) titldum
        read (105,*) header     
        
          do itl = 1, imax
            read (105,*,iostat=eof) tilldb(itl)
            if (eof < 0) exit
          end do    
        exit
      enddo
      endif
      
      db_mx%tillparm = imax

      close (105)
      return
      end subroutine tillparm_read