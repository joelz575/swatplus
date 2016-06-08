       subroutine ls_parms_cal_read
      
       use jrw_datalib_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof
       
       eof = 0
          
       do 
         open (107,file = 'ls_parms.cal')
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
         read (107,*,iostat=eof) ls_prms(i)%name, ls_prms(i)%chg_typ, ls_prms(i)%neg, ls_prms(i)%pos
         if (eof < 0) exit 
       end do 
    
       close(107)
       return
      end subroutine ls_parms_cal_read