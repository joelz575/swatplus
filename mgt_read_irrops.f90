       subroutine mgt_read_irrops
      
       use input_file_module
       use maximum_data_module
       use mgt_operations_module
      
       character (len=80) :: titldum
       character (len=80) :: header
       integer :: eof, i, imax
       
       mirrops = 0
       eof = 0
       imax = 0
       
      !! read irrigation operations
      inquire (file=in_ops%irr_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%irr_ops == 'null') then
        allocate (irrop_db(0:0))
      else
      do
        open (107,file=in_ops%irr_ops)
        read (107,*) titldum
        if (eof < 0) exit
        read (107,*) header
        if (eof < 0) exit
        do while (eof == 0)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do

        allocate (irrop_db(0:imax))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        do irr_op = 1, imax
          read (107,*,iostat=eof) irrop_db(irr_op)
          if (eof < 0) exit
        end do

        exit
      enddo
      endif
      
      db_mx%irrop_db = imax
      
      close(107)
      
      return
      end subroutine mgt_read_irrops