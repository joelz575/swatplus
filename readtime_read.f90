       subroutine readtime_read 
      
       use time_module
       use input_file_module
       
       character (len=500) :: header
       character (len=80) :: titldum
       integer :: eof
       
       eof = 0

       !! read weather codes
       inquire (file=in_sim%time, exist=i_exist)
       if (i_exist /= 0) then   
       do
         open (107,file=in_sim%time)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         read (107,*,iostat=eof) time%idaf, time%yrc_start, time%idal_in, time%yrc_end, time%step
         if (time%idaf <= 0) time%idaf = 1
         time%nbyr = time%yrc_end - time%yrc_start + 1
         if (eof < 0) exit
         exit
         close (107) 
       enddo
       endif
      
       time%yrc = time%yrc_start
       
       return
       end subroutine readtime_read            