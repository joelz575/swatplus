      subroutine exco_read_hmet
    
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use exco_module
      use maximum_data_module
 
      character (len=80) :: titldum, header
      integer :: eof, imax, ob1, ob2

      eof = 0
      imax = 0
      
      !read all export coefficient data
      inquire (file=in_exco%hmet, exist=i_exist)
      if (i_exist /= 0 .or. in_exco%hmet /= 'null') then
        do
          open (107,file=in_exco%hmet)
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
          
          db_mx%exco_hmet = imax
          
          allocate (exco_hmet(imax))
          do iexco_hmet = 1, imax
            allocate (exco_hmet(iexco_hmet)%hmet(cs_db%num_metals))
          end do
          allocate (exco_hmet_num(imax))
          allocate (exco_hmet_name(imax))
          rewind (107)
          read (107,*) titldum
          read (107,*) header
      
          !read all export coefficient data
          do ii = 1, db_mx%exco_hmet
            read (107,*,iostat=eof) titldum
            backspace (107)
            read (107,*,iostat=eof) exco_hmet_name(ii), (exco_hmet(ii)%hmet(ihmet)%sol, exco_hmet(ii)%hmet(ihmet)%sor, ihmet = 1, cs_db%num_metals)   
            if (eof < 0) exit
          end do
          close (107)
          exit
        end do
      end if
                  
      ! xwalk with exco file to get sequential number
      do iexco = 1, db_mx%exco
        do iexco_hmet = 1, db_mx%exco_hmet
          if (exco_db(iexco)%hmet_file == exco_hmet_name(iexco_hmet)) then
            exco_hmet_num(iexco) = iexco_hmet
            exit
          end if
        end do
      end do
      
      !set exco object hydrograph
      ob1 = sp_ob1%exco
      ob2 = sp_ob1%exco + sp_ob%exco - 1
      do iob = ob1, ob2
        iexco = ob(iob)%props
        iexco_hmet = exco_hmet_num(iexco)
        obcs(iob)%hd(1)%hmet = exco_hmet(iexco_hmet)%hmet
      end do
      
      return
      end subroutine exco_read_hmet