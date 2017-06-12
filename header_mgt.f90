     subroutine header_mgt
    
     use basin_module
     use parm
    
!!   open mgt.out file 
      if (pco%mgtout == 'y') then
        open (2612,file="mgt_out.mgt",recl=800)
        write (2612,*) mgt_hdr
        write (2612,*) mgt_hdr_unt1
        write (2612,*) mgt_hdr_unt2
        write (9000,*) 'MGT                 mgt_out.mgt'
      end if
          
      return
      end subroutine header_mgt  