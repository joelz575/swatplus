       subroutine scen_bmpuser_read
      
       use input_file_module
       
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof, i, imax
       
       mbmpops = 0
       eof = 0
       imax = 0
      
       !! read user defined upland CP removal operations
       inquire (file=in_str%bmpuser_str, exist=i_exist)
       if (i_exist == 0 .or. in_str%bmpuser_str == 'null') then
         allocate (bmpuser_db(0:0))
         allocate (bmp_str_xw(0:0))
       else
       do
         open (107,file=in_str%bmpuser_str)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         do while (eof >= 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
         end do
         
         allocate (bmpuser_db(0:imax))
         allocate (bmp_str_xw(0:imax))
         rewind (107)
         read (107,*) titldum
         read (107,*) header
         
         do ibmpop = 1, imax
           read (107,*,iostat=eof) bmpuser_db(ibmpop)
           !! bmpuser.str 
           bmp_str_xw(ibmpop)= bmpuser_db(ibmpop)%name
           if (eof < 0) exit
         end do
         exit
       enddo
       endif
       db_mx%bmpuserop_db= imax
       close(107)
       return         
      end subroutine scen_bmpuser_read