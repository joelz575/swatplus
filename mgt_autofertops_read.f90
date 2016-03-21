      subroutine mgt_autofertops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      mafertops = 0
      eof = 0
      imax = 0
      
          !! read autofertilizer operations
          inquire (file=in_ops%autofert_ops, exist=i_exist)
          if (i_exist == 0 .or. in_ops%autofert_ops == 'null') then
            allocate (autofertop_db(0:0))
            allocate (frta_xw(0:0))
          else
          do
            open (107,file=in_ops%autofert_ops)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit
             do while (eof >= 0)
               read (107,*,iostat=eof) titldum
               if (eof < 0) exit
               imax = imax + 1
             end do
             
            allocate (autofertop_db(0:imax))
            allocate (frta_xw(0:imax))
            rewind (107) 
            read (107,*) titldum
            read (107,*) header
            
            do iafertop = 1, imax
              read (107,*,iostat=eof) autofertop_db(iafertop)
              
              !! ferta
              frta_xw(iafertop) = autofertop_db(iafertop)%name
              if (eof < 0) exit
            end do

            exit
          end do
          endif
          close(107)
 
          db_mx%autofertop_db = imax
          
      return    
                              
      end subroutine mgt_autofertops_read