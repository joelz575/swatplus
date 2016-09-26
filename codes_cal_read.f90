       subroutine codes_cal_read
      
       use jrw_datalib_module
       use input_file_module
       use parm
       use hydrograph_module
       use sd_hru_module
       use sd_channel_module
      
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
    
         ical = 0
         if (cal_codes%hyd_hru == 'y' .or. cal_codes%hyd_hrul == 'y'.or.    &
             cal_codes%plt == 'y' .or. cal_codes%sed == 'y' .or.            &
             cal_codes%nut == 'y' .or. cal_codes%chsed == 'y' .or.          &
             cal_codes%chnut == 'y' .or. cal_codes%res == 'y') ical = 1
             
         if (ical == 1) then
           allocate (hru_init(0:sp_ob%hru))
           allocate (soil_init(0:sp_ob%hru))
           allocate (pcom_init(0:sp_ob%hru))
           allocate (sd_init(0:sp_ob%hru_lte))
           allocate (sdch_init(0:sp_ob%chandeg))
         end if
	   end if
       
       close(107)
       return
      end subroutine codes_cal_read