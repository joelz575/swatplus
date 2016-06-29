       subroutine ch_parms_cal_read
      
       use jrw_datalib_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof
       
       eof = 0
          
       do 
         open (107,file = 'chan_parms.cal')
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) mchp
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         allocate (ch_prms(mchp))
         if (eof < 0) exit
         exit
       enddo
       
       do i = 1, mlsp
         read (107,*,iostat=eof) ch_prms(i)%name, ch_prms(i)%chg_typ, ch_prms(i)%neg, ch_prms(i)%pos
         if (eof < 0) exit 
       end do 
    
       close(107)
       return
      end subroutine ch_parms_cal_read