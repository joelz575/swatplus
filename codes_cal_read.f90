       subroutine codes_cal_read
      
       use jrw_datalib_module
       use input_file_module
       use parm
      
       character (len=80) :: titldum, header
       integer :: eof
       
       eof = 0

       inquire (file=in_chg%codes_cal, exist=i_exist)
       if (i_exist == 0 .or. in_chg%codes_cal == 'null') then
 !       allocate (cal_codes(0:0))
       else		   
          
       do 
         open (107,file=in_chg%codes_cal)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         if (eof < 0) exit
         read (107,*,iostat=eof) cal_codes
         if (eof < 0) exit
         exit
       enddo
    
       if (cal_codes%ls == 'y') then
          allocate (hru_init(0:mhru))
          allocate (soil_init(0:mhru))
          allocate (pcom_init(0:mhru))
       end if
	   end if
       
       close(107)
       return
      end subroutine codes_cal_read