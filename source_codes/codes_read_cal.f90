       subroutine codes_read_cal
      
       use calibration_data_module
       use plant_data_module
       use input_file_module
       use hru_module, only : hru, hru_init
       use soil_module
       use plant_module
       use hydrograph_module
       use hru_lte_module
       use sd_channel_module
       use organic_mineral_mass_module
       
       implicit none
      
       character (len=80) :: titldum   !           |title of file
       character (len=80) :: header    !           |header of file
       integer :: eof                  !           |end of file
       integer :: icom                 !           | 
       integer :: i_exist              !none       |check to determine if file exists
       integer :: ical                 !           |
       integer :: j                    !none       |counter
       integer :: nplt                 !           |
       integer :: nly1                 !           |
       
       
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
           allocate (rsd1_init(0:sp_ob%hru))
           allocate (pcom_init(0:sp_ob%hru))
           do j = 1, sp_ob%hru
             icom = hru(j)%plant_cov
             nplt = pcomdb(icom)%plants_com
             allocate (pcom_init(j)%plg(nplt)) 
             allocate (pcom_init(j)%plm(nplt)) 
             allocate (pcom_init(j)%plstr(nplt)) 
             allocate (pcom_init(j)%plcur(nplt)) 
             allocate (rsd1_init(j)%tot(nplt))
             
             nly1 = soil(j)%nly + 1                                                                                                         
             allocate (soil_init(j)%ly(nly1))
             !allocate (soil_init(j)%ly(nly1)%rs(nplt))    !bac and pest not allocated
             allocate (soil_init(j)%phys(nly1))
           end do
           allocate (hlt_init(0:sp_ob%hru_lte))
           allocate (sdch_init(0:sp_ob%chandeg))
         end if
	   end if
       
       close(107)
       return
      end subroutine codes_read_cal