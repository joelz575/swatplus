      subroutine pathcom_db_read
      
      use basin_module
      use input_file_module
      use constituent_mass_module
      use jrw_datalib_module
         
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_const%pathcom_db, exist=i_exist)
      if (i_exist == 0 .or. in_const%pathcom_db == 'null') then
        allocate (pathcom_db(0:0))
      else
      do
        open (106,file=in_const%pathcom_db)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (106,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (pathcom_db(0:imax))

        rewind (106)
        read (106,*) titldum
        read (106,*) header 
        
        do ip = 1, imax
           read (106,*,iostat=eof) pathcom_db(ip)%name, pathcom_db(ip)%init_df, pathcom_db(ip)%recall_df,  &
             pathcom_db(ip)%exco_df, pathcom_db(ip)%dr_df, pathcom_db(ip)%num
           backspace (106)
           num = pathcom_db(ip)%num
           allocate (pathcom_db(ip)%paths(num))
           read (106,*,iostat=eof) pathcom_db(ip)%name, pathcom_db(ip)%init_df, pathcom_db(ip)%recall_df,  &
             pathcom_db(ip)%exco_df, pathcom_db(ip)%dr_df, pathcom_db(ip)%num, pathcom_db(ip)%paths 
        end do
        exit
      enddo
      endif
      
      db_mx%pathcom = imax

      close (106)
      return
      end subroutine pathcom_db_read