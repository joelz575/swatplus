       subroutine basin_read_cc
      
       use input_file_module
       use basin_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof
       
       eof = 0
      
       !! read basin
       inquire (file=in_basin%codes_bas, exist=i_exist)
       if (i_exist /= 0 .or. in_basin%codes_bas /= 'null') then      
       do 
         open (107,file=in_basin%codes_bas)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         read (107,*,iostat=eof) bsn_cc
         if (eof < 0) exit
         exit
       enddo
       endif
          close(107)
       
       return
      end subroutine basin_read_cc