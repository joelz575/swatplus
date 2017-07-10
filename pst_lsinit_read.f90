      subroutine pst_lsinit_read
      
      use input_file_module
      use constituent_mass_module

      character (len=80) :: titldum, header
      integer :: ipestdb, eof
      integer :: i, imax
      
      eof = 0
      imax = 0
                             
      !! real all pesticide initialization data from pest_initial.dat
      inquire (file=in_init%initial_pst,exist=i_exist)
      if (i_exist == 0 .or. in_init%initial_pst == 'null') then
         allocate (pesti_db(0:0))
      else
        do
          open (107,file=in_init%initial_pst)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
           do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
           end do
           
          db_mx%pestdb = imax
           
           allocate (pesti_db(0:imax))
           rewind (107)
           read (107,*) titldum
           read (107,*) header
           
          do ipestdb = 1, db_mx%pestdb
            read (107,*,iostat=eof) titldum
            backspace (107)
            read (107,*,iostat=eof) pesti_db(i)%name, pesti_db(i)%num, pesti_db(i)%exco_df, pesti_db(i)%dr_df
            if (eof < 0) exit
            !allocate initial pest and exco and dr for pesticides
            allocate (pesti_db(ipestdb)%pesti(pesti_db(i)%num))
            allocate (exco_pest(ipestdb,pesti_db(i)%num))
            allocate (dr_pest(ipestdb,pesti_db(i)%num))
            
            !read initial pesticide concentrations on plant and soil
            do ipest = 1, pesti_db(i)%num
              read (107,*,iostat=eof) pesti_db(i)%pesti(ipest)%name, pesti_db(i)%pesti(ipest)%plt,      &
                  pesti_db(i)%pesti(ipest)%soil, pesti_db(i)%pesti(ipest)%enr
              if (eof < 0) exit
              !xwalk pest name with database name
              do idb = 1, db_mx%pestparm
                if (sched(isched)%mgt_ops(iop)%op_char == pestdb(ip)%pestnm) then
                  pesti_db(i)%pesti(ipest)%num_db = idb
                  exit
                endif
              end do
              !if (pesti_db(i)%pesti(ipest)%num_db == 0) write (9001,*) pesti_db(i)%pesti(ipest)%num_db, ' not found (pstinit)'
            end do
            
            !read export coefficients for pesticides
            inquire (file=pesti_db(i)%exco_df,exist=i_exist)
            if (pesti_db(i)%exco_df /= 'null' .and. i_exist /= 0) then
              open (108, file=pesti_db(i)%exco_df)
              read (107,*) titldum
              read (107,*) header
              do
                read (107,*,iostat=eof) (exco_pest(ipest_db,ii)%pest_sol,        &
                            exco_pest(ipest_db,ii)%pest_sor, ii = 1, pesti_db(i)%num)
                if (eof < 0) exit
              end do
            end if

            !read delivery ratios for pesticides
            inquire (file=pesti_db(i)%dr_df,exist=i_exist)
            if (pesti_db(i)%dr_df /= 'null' .and. i_exist /= 0) then
              open (108, file=pesti_db(i)%dr_df)
              read (107,*) titldum
              read (107,*) header
              do
                read (107,*,iostat=eof) (dr_pest(ipest_db,ii)%pest_sol,       &
                             dr_pest(ipest_db,ii)%pest_sor, ii = 1, pesti_db(i)%num)
                if (eof < 0) exit
              end do
            end if
            
          end do
          !db_mx%pestdb = mpesti_db
          exit
        end do
      end if
      
      close (107) 
      
      return
      end subroutine pst_lsinit_read