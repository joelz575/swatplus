      subroutine basin_print_codes_read
      
      use input_file_module
       
      character (len=500) :: header
      character (len=80) :: titldum
      character (len=16) :: name
      integer :: eof
       
      eof = 0

      !! read time codes
      inquire (file=in_sim%prt, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sim%prt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%nyskip, pco%jd_start, pco%jd_end, pco%yr_start, pco%yr_end, pco%interval
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco%aa_numint
        if (pco%aa_numint > 0) then
          allocate (pco%aa_yrs(pco%aa_numint))
          backspace (107)
          read (107,*,iostat=eof) pco%aa_numint, (pco%aa_yrs(ii), ii = 1, pco%aa_numint)
          if (eof < 0) exit
        else
          allocate (pco%aa_yrs(1))
        end if

     !! read objects output
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%wb_bsn, pco%nb_bsn, pco%ls_bsn, pco%pw_bsn, pco%aqu_bsn,   & 
          pco%res_bsn, pco%chan_bsn, pco%recall_bsn
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%wb_reg, pco%nb_reg, pco%ls_reg, pco%pw_reg, pco%aqu_reg,   & 
          pco%res_reg, pco%chan_reg, pco%recall_reg
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%wb_sub, pco%nb_sub, pco%ls_sub, pco%pw_sub
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%wb_hru, pco%nb_hru, pco%ls_hru, pco%pw_hru
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%wb_sd, pco%nb_sd, pco%ls_sd, pco%pw_sd
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%chan 
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%aqu
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%res
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%hyd
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%hydcon
        if (eof < 0) exit
         read (107,*,iostat=eof) name, pco%solout
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%mgtout
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%csvout
        if (eof < 0) exit
        read (107,*,iostat=eof) name, pco%fdcout
        exit
      end do
      end if
      close (107)
      
      if (pco%jd_start == 0) pco%jd_start = 1
      if (pco%jd_end == 0) pco%jd_end = 366
      if (pco%yr_start == 0) pco%yr_start = time%yrc
      if (pco%yr_end == 0) pco%yr_end = time%yrc + time%nbyr
      int_print = pco%interval - 1  !incremented on first day of print period triggering print
            
      return
      end subroutine basin_print_codes_read           