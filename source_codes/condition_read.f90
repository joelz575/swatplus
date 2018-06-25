      subroutine condition_read
      
      use maximum_data_module
      use reservoir_data_module
      use landuse_data_module
      use mgt_operations_module
      use tillage_data_module
      use fertilizer_data_module
      use input_file_module
      use conditional_module
      
      implicit none
                  
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=13) :: file
      integer :: eof                  !           |end of file
      integer :: i                    !none       |counter 
      integer :: mdtbl                !none       |ending of loop
      integer :: ic                   !none       |counter 
      integer :: ial                  !none       |counter 
      integer :: iac                  !none       !counter 
      integer :: i_exist              !none       |check to determine if file exists
      integer :: idb                  !none       |counter
      integer :: ilum                 !none       |counter
      integer :: iburn                !none       |counter
      
      
      mdtbl = 0
      eof = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_cond%cond_ctl, exist=i_exist)
      if (i_exist == 0 .or. in_cond%cond_ctl == "null") then
        allocate (d_tbl(0:0)) 
      else
        do
          open (107,file=in_cond%cond_ctl)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mdtbl
          if (eof < 0) exit
          read (107,*,iostat=eof)
          if (eof < 0) exit
          allocate (d_tbl(0:mdtbl))

          do i = 1, mdtbl
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) d_tbl(i)%name, d_tbl(i)%conds, d_tbl(i)%alts, d_tbl(i)%acts
            allocate (d_tbl(i)%cond(d_tbl(i)%conds))
            allocate (d_tbl(i)%alt(d_tbl(i)%conds,d_tbl(i)%alts))
            allocate (d_tbl(i)%act(d_tbl(i)%acts))
            allocate (d_tbl(i)%act_hit(d_tbl(i)%alts))
            allocate (d_tbl(i)%act_typ(d_tbl(i)%acts))
            allocate (d_tbl(i)%act_app(d_tbl(i)%acts))
            allocate (d_tbl(i)%act_outcomes(d_tbl(i)%acts,d_tbl(i)%alts))
            
            !read conditions and condition alternatives
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do ic = 1, d_tbl(i)%conds
              read (107,*,iostat=eof) d_tbl(i)%cond(ic), (d_tbl(i)%alt(ic,ial), ial = 1, d_tbl(i)%alts)
              if (eof < 0) exit
            end do
                        
            !read actions and action outcomes
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            do iac = 1, d_tbl(i)%acts
              read (107,*,iostat=eof) d_tbl(i)%act(iac), (d_tbl(i)%act_outcomes(iac,ial), ial = 1, d_tbl(i)%alts)
              if (eof < 0) exit
            end do
            read (107,*,iostat=eof)
            if (eof < 0) exit
            
            !cross walk characters to get array numbers
            do iac = 1, d_tbl(i)%acts
                select case (d_tbl(i)%act(iac)%typ)
                    
                case ("plant")
                  do idb = 1, db_mx%irrop_db
                    if (d_tbl(i)%act(iac)%file_pointer == irrop_db(idb)%name) then
                      d_tbl(i)%act_typ(iac) = idb
                      exit
                    end if
                  end do
                    
                  case ("harvest_kill")
                  do idb = 1, db_mx%harvop_db
                    if (d_tbl(i)%act(iac)%file_pointer == harvop_db(idb)%name) then
                      d_tbl(i)%act_typ(iac) = idb
                      exit
                    endif
                  end do
                
                  case ("till")
                  do idb = 1, db_mx%tillparm
                    if (d_tbl(i)%act(iac)%option == tilldb(idb)%tillnm) then
                      d_tbl(i)%act_typ(iac) = idb
                      exit
                    endif
                  end do
                
                case ("irrigate")
                  do idb = 1, db_mx%irrop_db
                    if (d_tbl(i)%act(iac)%option == irrop_db(idb)%name) then
                      d_tbl(i)%act_typ(iac) = idb
                      exit
                    end if
                  end do
                  
                case ("fertilize")
                  !xwalk fert name with fertilizer data base
                  do idb = 1, db_mx%fertparm
                    if (d_tbl(i)%act(iac)%option == fertdb(idb)%fertnm) then
                      d_tbl(i)%act_typ(iac) = idb
                      exit
                    endif
                  end do
                  !xwalk application type with chemical application data base
                  do idb = 1, db_mx%chemapp_db
                    if (d_tbl(i)%act(iac)%file_pointer == chemapp_db(idb)%name) then
                      d_tbl(i)%act_app(iac) = idb
                      exit
                    endif
                  end do
                  
                case ("release")
                  do idb = 1, db_mx%res_weir
                    if (d_tbl(i)%act(iac)%option == "weir") then
                    if (d_tbl(i)%act(iac)%file_pointer == res_weir(idb)%name) then
                      d_tbl(i)%act_typ(iac) = idb
                      exit
                    end if
                    end if
                  end do
                  
                case ("lu_change")
                  do ilum = 1, db_mx%landuse
                    if (d_tbl(i)%act(iac)%file_pointer == lum(ilum)%name) then
                      d_tbl(i)%act_typ(iac) = ilum
                      exit
                    end if
                  end do
                               
                case ("burn")
                  do iburn = 1, db_mx%fireop_db
                    if (d_tbl(i)%act(iac)%option == fire_db(iburn)%name) then
                      d_tbl(i)%act_typ(iac) = iburn
                      exit
                    end if
                  end do
                end select
                
            end do
            
          end do
          db_mx%d_tbl = mdtbl
          exit
        enddo
      endif
      close (107)
      
      return  
      end subroutine condition_read