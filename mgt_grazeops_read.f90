      subroutine mgt_grazeops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      mgrazops = 0
      eof = 0
      imax = 0
                                      
      !! read grazing operations
      inquire (file=in_ops%graze_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%graze_ops == 'null') then
         allocate (grazeop_db(0:0))
      else
      do
        open (107,file=in_ops%graze_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do while (eof >= 0)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
        allocate (grazeop_db(0:imax)) 
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header
              
        do igrazop = 1, imax 
          read (107,*,iostat=eof) grazeop_db(igrazop)
          if (eof < 0) exit
        end do
 
        exit
      enddo
      endif
      close(107)
 
      db_mx%grazeop_db = imax
      
      return  
      end subroutine mgt_grazeops_read