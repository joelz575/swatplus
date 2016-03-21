      subroutine mgt_harvops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      mharvops = 0
      eof = 0
      imax = 0
      
      !! read harvest only operations
      inquire (file=in_ops%harv_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%harv_ops == 'null') then
        allocate (harvop_db(0:0))
        allocate (harv_xw(0:0))
      else
      do 
        open (107,file=in_ops%harv_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do while (eof >= 0)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
        allocate (harvop_db(0:imax))
        allocate (harv_xw(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        do iharvop = 1, imax
            read (107,*,iostat=eof) harvop_db(iharvop)
            !! harv
            harv_xw(iharvop) = harvop_db(iharvop)%name
            if (eof < 0) exit
        end do

        exit
      enddo
      endif
      close(107)
      
      db_mx%harvop_db = imax
      
      return
      end subroutine mgt_harvops_read