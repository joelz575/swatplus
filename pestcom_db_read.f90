      subroutine pestcom_db_read
      
      use basin_module
      use input_file_module
      use constituent_mass_module
      use jrw_datalib_module
         
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_const%pestcom_db, exist=i_exist)
      if (i_exist == 0 .or. in_const%pestcom_db == 'null') then
        allocate (pestcom_db(0:0))
      else
      do
        open (106,file=in_const%pestcom_db)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (106,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (pestcom_db(0:imax))

        rewind (106)
        read (106,*) titldum
        read (106,*) header 
        
        do i = 1, imax
          read (106,*,iostat=eof) pestcom_db(i)%name, pestcom_db(i)%init_df, pestcom_db(i)%recall_df,   &
              pestcom_db(i)%exco_df, pestcom_db(i)%dr_df, pestcom_db(i)%num
          backspace (106)
          num = pestcom_db(i)%num
          allocate (pestcom_db(i)%pests(num))
          allocate (pestcom_db(i)%num_db(num))
          read (106,*,iostat=eof) pestcom_db(i)%name, pestcom_db(i)%init_df, pestcom_db(i)%recall_df,   &
              pestcom_db(i)%exco_df, pestcom_db(i)%dr_df, pestcom_db(i)%num, pestcom_db(i)%pests

          !! crosswalk pesticide names with database
          do ipst = 1, db_mx%pestparm
            do ipcom = 1, pestcom_db(i)%num
              if (pestcom_db(i)%pests(ipcom) == pestdb(ipst)%pestnm) then
                pestcom_db(i)%num_db(ipcom) = ipst
                exit
              endif
            end do
          end do

        end do
        exit
      enddo
      endif
      
      db_mx%pestcom = imax

      close (106)
      return
      end subroutine pestcom_db_read