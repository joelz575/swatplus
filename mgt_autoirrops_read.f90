      subroutine mgt_autoirrops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      mairrops = 0
      eof = 0
      imax = 0
          
          !! read autoirrigation operations
          inquire (file=in_ops%autoirr_ops, exist=i_exist)
          if (i_exist == 0 .or. in_ops%autoirr_ops == 'null') then
            allocate (airrop_db(0:0))
            allocate (irra_xw(0:0))
          else
          do
            open (107,file=in_ops%autoirr_ops)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*) header
            if (eof < 0) exit
             do while (eof >= 0)
               read (107,*,iostat=eof) titldum
               if (eof < 0) exit
               imax = imax + 1
             end do
            allocate (airrop_db(0:imax))
            allocate (irra_xw(0:imax))
            
            rewind (107)
            read (107,*) titldum
            read (107,*) header
            
            do iairrop = 1, imax
              read (107,*,iostat=eof) airrop_db(iairrop)
              
              !! irra
              irra_xw(iairrop) = airrop_db(iairrop)%name
              if (eof < 0) exit
            end do

            exit
          enddo
          endif
          
          db_mx%airrop_db = imax
                
          close(107) 
          
      return         
      end subroutine mgt_autoirrops_read