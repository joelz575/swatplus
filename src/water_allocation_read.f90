      subroutine water_allocation_read
      
      use input_file_module
      use water_allocation_module
      use mgt_operations_module
      use maximum_data_module
      
      implicit none 
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      logical :: i_exist              !none       |check to determine if file exists
      integer :: i                    !none       |counter
      integer :: k                    !none       |counter
      integer :: isrc                 !none       |counter
      integer :: iwro                 !none       |counter
      integer :: num_objs
      integer :: idmd
      integer :: idb
      
      eof = 0
      imax = 0
      
      !! read water allocation inputs
      inquire (file=in_watrts%water_rights, exist=i_exist)
      if (.not. i_exist .or. in_watrts%water_rights == "null") then
        allocate (wallo(0:0))
      else
      do 
        open (107,file=in_watrts%water_rights)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) imax
        if (eof < 0) exit
        
        allocate (wallo(imax))

        do iwro = 1, imax
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          read (107,*,iostat=eof) wallo(iwro)%name, wallo(iwro)%rule_typ, wallo(iwro)%res_lim,         &
            wallo(iwro)%comp, wallo(iwro)%dmd_obs
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          num_objs = wallo(iwro)%dmd_obs
          db_mx%wallo_db = num_objs
          allocate (wallo(iwro)%dmd(num_objs))
          allocate (wallod_out(num_objs))
          allocate (wallom_out(num_objs))
          allocate (walloy_out(num_objs))
          allocate (walloa_out(num_objs))
          
          
          do idmd = 1, num_objs
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            backspace (107)
            read (107,*,iostat=eof) k, wallo(iwro)%dmd(i)%ob_typ, wallo(iwro)%dmd(i)%obtyp_num,                 &
              wallo(iwro)%dmd(i)%dmd_typ, wallo(iwro)%dmd(i)%amount, (wallo(iwro)%dmd(i)%src(isrc), isrc = 1, 2)
            
            !! xwalk with irrigation database
            if (wallo(iwro)%dmd(i)%ob_typ == "hru") then
            do idb = 1, db_mx%irrop_db
              if (wallo(iwro)%dmd(i)%dmd_typ == irrop_db(idb)%name) then
                wallo(iwro)%dmd(i)%irr_typ = idb
                exit
              end if
            end do
            end if
            
            !! zero output variables for summing
            wallod_out(idmd)%src(1) = walloz
            wallod_out(idmd)%src(2) = walloz
            wallom_out(idmd)%src(1) = walloz
            wallom_out(idmd)%src(2) = walloz
            walloy_out(idmd)%src(1) = walloz
            walloy_out(idmd)%src(2) = walloz
            walloa_out(idmd)%src(1) = walloz
            walloa_out(idmd)%src(2) = walloz
            
          end do
        end do

        exit
      enddo
      endif
      close(107)

      return
      end subroutine water_allocation_read
