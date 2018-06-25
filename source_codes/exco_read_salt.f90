      subroutine exco_read_salt
    
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use maximum_data_module
      use exco_module
      use constituent_mass_module
 
      character (len=80) :: titldum, header
      integer :: eof, imax, ob1, ob2

      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_exco%salt, exist=i_exist)
      if (i_exist /= 0 .or. in_exco%salt /= "null") then
        do
          open (107,file=in_exco%salt)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          db_mx%exco_salt = imax
          
          allocate (exco_salt(imax))
          do iexco_salt = 1, imax
            allocate (exco_salt(iexco_salt)%salt(cs_db%num_salts))
          end do
          allocate (exco_salt_num(imax))
          allocate (exco_salt_name(imax))
          rewind (107)
          read (107,*) titldum
          read (107,*) header
      
          !read all export coefficient data
          do ii = 1, db_mx%exco_salt
            read (107,*,iostat=eof) titldum
            backspace (107)
            read (107,*,iostat=eof) exco_salt_name(ii), (exco_salt(ii)%salt(isalt)%sol, exco_salt(ii)%salt(isalt)%sor, isalt = 1, cs_db%num_salts)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
            
      ! xwalk with exco file to get sequential number
      do iexco = 1, db_mx%exco
        do iexco_salt = 1, db_mx%exco_salt
          if (exco_db(iexco)%salts_file == exco_salt_name(iexco_salt)) then
            exco_salt_num(iexco) = iexco_salt
            exit
          end if
        end do
      end do
            !set exco object hydrograph
      ob1 = sp_ob1%exco
      ob2 = sp_ob1%exco + sp_ob%exco - 1
      do iob = ob1, ob2
        iexco = ob(iob)%props
        iexco_salt = exco_salt_num(iexco)
        obcs(iob)%hd(1)%salt = exco_salt(iexco_salt)%salt
      end do
      
      return
      end subroutine exco_read_salt