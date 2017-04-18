      subroutine ru_read
      
      use basin_module
      use input_file_module
      use time_module
      
      ! read subbasin parameters (ie drainage area and topographic inputs)
      character (len=80) :: titldum, header
      integer :: eof, i, imax
      
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
          
        allocate (ru(0:imax))
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
      allocate (ru_def(0:imax))

      allocate (ru_tc(0:imax))
      allocate (ru_n(0:imax))
      allocate (uhs(0:imax,time%step+1))
      allocate (hyd_flo(time%step+1))
      allocate (itsb(sp_ob%sub))

      hyd_flo = 0.
      uhs = 0
            
      do ith = 1, sp_ob%sub
        if (ru(i)%dbsc%elem_def == ru_def(i)%name) then
             ru(i)%dbs%elem_def = ith
        exit
        end if
      end do
         
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
      end do
      
      do ith = 1, db_mx%field
        if (ru(i)%dbsc%field_db == field_db(ith)%name) then
             ru(i)%dbs%field_db = ith
        exit
        end if
      end do
      
      close(107)
        exit
      enddo
      endif      

      return
      end subroutine ru_read