      subroutine mgt_pestops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mpestops = 0
      eof = 0
      imax = 0
       
      !! read pesticide operations
      inquire (file=in_ops%pest_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%pest_ops == 'null') then
        allocate (pestop_db(0:0))
        allocate (pestop_xw(0:0))
      else
      do 
        open (107,file=in_ops%pest_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do       
        
        allocate (pestop_db(0:imax))
        allocate (pestop_xw(0:imax))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header  
           
        do ipestop = 1, imax
          read (107,*,iostat=eof) pestop_db(ipestop)
          
          !! pest
          pest_xw(ipestop) = pestop_db(ipestop)%name
          if (eof < 0) exit
        end do

        exit
      enddo
      endif
      close(107)
      
      db_mx%pestdb = imax
      
      return     
      end subroutine mgt_pestops_read