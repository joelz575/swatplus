       subroutine scen_contour_read
      
       use input_file_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof, i, imax
       
       eof = 0
       imax = 0
      
       !! read contour operations
       inquire (file=in_str%contour_str, exist=i_exist)
       if (i_exist == 0 .or. in_str%contour_str == 'null') then
         allocate (contour_db(0:0))
         allocate (cont_str_xw(0:0))
       else
       do
         open (107,file=in_str%contour_str)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         do while (eof >= 0)
           read (107,*,iostat=eof) titldum
           if (eof < 0) exit
           imax = imax + 1
         end do
         
         allocate (contour_db(0:imax))
         allocate (cont_str_xw(0:imax))
         
         rewind (107)
         read (107,*) titldum
         read (107,*) header    
         
         do icontop = 1, imax
           read (107,*,iostat=eof) contour_db(icontop)
           
           !! contour.str
           cont_str_xw(icontop) = contour_db(icontop)%name
           if (eof < 0) exit
         end do
         exit
       enddo
       endif
       db_mx%contop_db = imax
       close(107)
       return      
      end subroutine scen_contour_read