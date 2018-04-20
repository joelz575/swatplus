      subroutine constit_db_read
      
      use basin_module
      use input_file_module
      use constituent_mass_module
      use maximum_data_module
      
      implicit none
         
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: i_exist              !none       |check to determine if file exists
      integer :: eof                  !           |end of file
      integer :: i                    !           |
      integer :: imax                 !           |
      integer :: ip                   !none       |counter
       
      eof = 0
      imax = 0
      
      inquire (file=in_sim%cs_db, exist=i_exist)
      if (i_exist == 0 .or. in_sim%cs_db == 'null') then
        allocate (cs_db%pests(0:0))
        allocate (cs_db%paths(0:0))
        allocate (cs_db%metals(0:0))
        allocate (cs_db%salts(0:0))
      else
      do
        open (106,file=in_sim%cs_db)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) cs_db%num_pests
        if (eof < 0) exit
        allocate (cs_db%pests(0:cs_db%num_pests))
        allocate (cs_db%pest_num(0:cs_db%num_pests))
        read (106,*,iostat=eof) (cs_db%pests(i), i = 1, cs_db%num_pests)
        if (eof < 0) exit
        read (106,*,iostat=eof) cs_db%num_paths
        if (eof < 0) exit
        allocate (cs_db%paths(cs_db%num_paths))
        allocate (cs_db%path_num(0:cs_db%num_paths))
        read (106,*,iostat=eof) (cs_db%paths(i), i = 1, cs_db%num_paths)
        if (eof < 0) exit
        read (106,*,iostat=eof) cs_db%num_metals
        if (eof < 0) exit
        allocate (cs_db%metals(cs_db%num_metals))
        allocate (cs_db%metals_num(0:cs_db%num_metals))
        read (106,*,iostat=eof) (cs_db%metals(i), i = 1, cs_db%num_metals)
        if (eof < 0) exit
        read (106,*,iostat=eof) cs_db%num_salts
        if (eof < 0) exit
        allocate (cs_db%salts(cs_db%num_salts))
        allocate (cs_db%salts_num(0:cs_db%num_salts))
        read (106,*,iostat=eof) (cs_db%salts(i), i = 1, cs_db%num_salts)
        exit
      end do
      end if

      cs_db%num_tot_con = cs_db%num_pests + cs_db%num_paths + cs_db%num_metals + cs_db%num_salts
      
      close (106)
      return
      end subroutine constit_db_read