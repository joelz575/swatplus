       subroutine ls_parms_cal_read
      
       use maximum_data_module
       use calibration_data_module
       use input_file_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof
       
       eof = 0

      inquire (file=in_chg%ls_parms_cal, exist=i_exist)
      if (i_exist == 0 .or. in_chg%ls_parms_cal == 'null') then
        allocate (ls_prms(0:0))	   	   
      else   
       do 
         open (107,file = in_chg%ls_parms_cal)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         read (107,*,iostat=eof) mlsp
         if (eof < 0) exit
         read (107,*,iostat=eof) header
         allocate (ls_prms(mlsp))
         if (eof < 0) exit
         exit
       enddo
       
       do i = 1, mlsp
         read (107,*,iostat=eof) ls_prms(i)%name, ls_prms(i)%chg_typ, ls_prms(i)%neg, ls_prms(i)%pos, ls_prms(i)%lo, ls_prms(i)%up
         if (eof < 0) exit 
       end do

      end if	   
    
      db_mx%lscal_prms = mlsp
      
      close(107)
      return
      end subroutine ls_parms_cal_read