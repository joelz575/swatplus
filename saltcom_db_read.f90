      subroutine saltcom_db_read
      
      use basin_module
      use input_file_module
      use constituent_mass_module
      use jrw_datalib_module
         
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_const%saltcom_db, exist=i_exist)
      if (i_exist == 0 .or. in_const%saltcom_db == 'null') then
        allocate (saltcom_db(0:0))
      else
      do
        open (106,file=in_const%saltcom_db)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (106,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (saltcom_db(0:imax))

        rewind (106)
        read (106,*) titldum
        read (106,*) header 
        
        do ip = 1, imax
         backspace (106)
          read (106,*,iostat=eof) saltcom_db(i)%name, saltcom_db(i)%init_df, saltcom_db(i)%recall_df,   &
              saltcom_db(i)%exco_df, saltcom_db(i)%dr_df, saltcom_db(i)%num
          backspace (106)
          num = saltcom_db(ip)%num
          allocate (saltcom_db(ip)%salts(num))
          read (106,*,iostat=eof) saltcom_db(i)%name, saltcom_db(i)%init_df, saltcom_db(i)%recall_df,   &
              saltcom_db(i)%exco_df, saltcom_db(i)%dr_df, saltcom_db(i)%num, saltcom_db(i)%salts

      
           !! pesticide.pst
           saltcom_xw(ip) = saltcom_db(ip)%name

        end do
        exit
      enddo
      endif
      
      db_mx%saltcom = imax

      close (106)
      return
      end subroutine saltcom_db_read