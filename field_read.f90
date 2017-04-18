      subroutine field_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mfield = 0
      eof = 0
      imax = 0
        
      !! read all data from topo.dat
      inquire (file=in_hyd%field_fld, exist=i_exist)
      if (i_exist == 0 .or. in_hyd%field_fld == 'null') then
        allocate (field_db(0:0))
      else
        do
          open (107,file=in_hyd%field_fld)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
            mfield = mfield + 1
          end do
          
          allocate (field_db(0:imax))
          rewind (107)
          read (107,*) titldum
          read (107,*) header
          
          do ith = 1, mfield
             read (107,*,iostat=eof) i
             backspace (107) 
             read (107,*,iostat=eof) k, field_db(i)
             if (eof < 0) exit
          end do
          exit
        enddo
      endif
      
      db_mx%field = mfield
      close (107)
         
      return  
      end subroutine field_read