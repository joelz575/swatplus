      subroutine sub_read
      
      use basin_module
      use input_file_module
      use time_module
      
      ! read subbasin parameters (ie drainage area and topographic inputs)
      character (len=80) :: titldum, header
      integer :: eof, i, imax
      
      msub_db = 0
      eof = 0
      imax = 0
      
      inquire (file=in_sub%sub, exist=i_exist)
      if (i_exist == 0 .or. in_sub%sub == 'null') then
          allocate (sub(0:0))
      else
      do
        open (107,file=in_sub%sub)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0 .or. i == 0) exit
            imax = Max(imax,i)
            msub_db = msub_db + 1
          end do
          
        allocate (sub(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header

      !! read subbasin parameters
        do isub = 1, msub_db
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, sub(i)%name, sub(i)%dbs
          if (eof < 0) exit
        end do
     
      !! read spatial input to each subbasin (ie hru fractions)
      allocate (sub_d(0:imax))

      allocate (sub_tc(0:imax))
      allocate (sub_n(0:imax))
      allocate (uhs(0:imax,time%step+1))
      allocate (hyd_flo(time%step+1))
      allocate (itsb(sp_ob%sub))

      hyd_flo = 0.
      uhs = 0
      
  !      do ith = 1, db_mx%toposub
  !        if (sub(i)%dbsc%toposub == toposub_db(ith)%name) then
  !             sub(i)%dbs%toposub = ith
  !        exit
  !        end if
  !      end do
      
      close(107)
        exit
      enddo
      endif      

      return
      end subroutine sub_read