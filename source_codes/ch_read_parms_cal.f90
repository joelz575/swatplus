       subroutine ch_read_parms_cal
      
       use calibration_data_module
       use input_file_module
      
      implicit none
       
      character (len=80) :: titldum    !             |title of file
      character (len=80) :: header     !             |header of file
      integer :: eof                   !             |end of file
      integer :: i_exist               !             |check to determine if file exists
      integer :: mchp                  !             |ending of loop
      integer :: i                     !none          |counter 
       
       eof = 0
       
      inquire (file=in_chg%ch_parms_cal, exist=i_exist)
      if (i_exist == 0 .or. in_chg%ch_parms_cal == "null") then
           allocate (ch_prms(0:0))         
      else    
       do 
         open (107,file=in_chg%ch_parms_cal)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) mchp
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         allocate (ch_prms(mchp))
         if (eof < 0) exit
         exit
       enddo
       endif
       
       do i = 1, mchp
         read (107,*,iostat=eof) ch_prms(i)%name, ch_prms(i)%chg_typ, ch_prms(i)%neg, ch_prms(i)%pos, ch_prms(i)%lo, ch_prms(i)%up
         if (eof < 0) exit 
       end do 
    
       close(107)
       return
      end subroutine ch_read_parms_cal