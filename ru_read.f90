      subroutine ru_read
      
      use basin_module
      use input_file_module
      use time_module
      use ru_module
      use hydrograph_module, only : ru_d, ru_m, ru_y, ru_a, sp_ob
      use jrw_datalib_module, only : db_mx, topo_db, field_db
      
      ! read subbasin parameters (ie drainage area and topographic inputs)
      character (len=80) :: titldum, header
      integer :: eof, imax
      
      msub_db = 0
      eof = 0
      imax = 0
      
      inquire (file=in_ru%ru, exist=i_exist)
      if (i_exist == 0 .or. in_ru%ru == 'null') then
          allocate (ru(0:0))
      else
      do
        open (107,file=in_ru%ru)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) i
            if (eof < 0 .or. i == 0) exit
            imax = Max(imax,i)
            msub_db = msub_db + 1
          end do
          
        allocate (ru(0:sp_ob%sub))
        allocate (ru_d(sp_ob%sub))
        allocate (ru_m(sp_ob%sub))
        allocate (ru_y(sp_ob%sub))
        allocate (ru_a(sp_ob%sub))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header

      !! read subbasin parameters
        do isub = 1, msub_db
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, ru(i)%name, ru(i)%dbsc
          if (eof < 0) exit
        end do
     
      !! read spatial input to each subbasin (ie hru fractions)

      allocate (ru_tc(0:sp_ob%sub))
      allocate (ru_n(0:sp_ob%sub))
      allocate (uhs(0:sp_ob%sub,time%step+1))
      allocate (hyd_flo(time%step+1))
      allocate (itsb(sp_ob%sub))

      hyd_flo = 0.
      uhs = 0

      !! need a hyd_output name for sub_dr to xwalk
!      do ith = 1, db_mx%topo
!        if (ru(i)%dbsc%elem_dr == topo_db(ith)%name) then
!             ru(i)%dbs%elem_dr = ith
!        exit
!        end if
!      end do
            
      do ith = 1, db_mx%topo
        if (ru(i)%dbsc%toposub_db == topo_db(ith)%name) then
             ru(i)%dbs%toposub_db = ith
        exit
        end if
        !if (ru(i)%dbs%toposub_db == 0) write (9001,*) ru(i)%dbsc%toposub_db, ' not found (ru-toposub)' 
      end do
      
      do ith = 1, db_mx%field
        if (ru(i)%dbsc%field_db == field_db(ith)%name) then
             ru(i)%dbs%field_db = ith
        exit
        end if
        !if (ru(i)%dbs%field_db == 0) write (9001,*) ru(i)%dbsc%field_db, ' not found (ru-field_db)'
      end do

      
      close(107)
        exit
      enddo
      endif      

      return
      end subroutine ru_read