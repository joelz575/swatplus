      subroutine condition_read
      
      use jrw_datalib_module
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, mdtbl, ic, ial, iac
      
      mdtbl = 0
      eof = 0
      
      !! read all data from hydrol.dat
      inquire (file=in_cond%cond_ctl, exist=i_exist)
      if (i_exist == 0 .or. in_cond%cond_ctl == 'null') then
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
            allocate (d_tbl(i)%act_hit(d_tbl(i)%acts))
            allocate (d_tbl(i)%act_ptr(d_tbl(i)%acts))
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
              if (d_tbl(i)%act(iac)%option == "file") then
                select case (d_tbl(i)%act(iac)%name)
                case ("irrigate")
                  do idb = 1, db_mx%irrop_db
                    if (d_tbl(i)%act(iac)%file_pointer == irrm_xw(idb)) then
                      d_tbl(i)%act_ptr = idb
                      exit
                    end if
                  end do
                end select
              end if
            end do
            
          end do
          db_mx%d_tbl = mdtbl
          exit
        enddo
      endif
      close (107)
      
      return  
      end subroutine condition_read