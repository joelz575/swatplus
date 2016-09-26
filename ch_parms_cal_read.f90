       subroutine ch_parms_cal_read
      
       use jrw_datalib_module
       use input_file_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof
       
       eof = 0
       
      inquire (file=in_chg%ch_parms_cal, exist=i_exist)
      if (i_exist == 0 .or. in_chg%ch_parms_cal == 'null') then
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
      end subroutine ch_parms_cal_read