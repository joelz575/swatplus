      subroutine overland_n_read
      
      use input_file_module
      use maximum_data_module
      use landuse_data_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_lum%ovn_lum, exist=i_exist)
      if (i_exist == 0 .or. in_lum%ovn_lum == 'null') then
          allocate (overland_n(0:0))
      else
      do
        open (108,file=in_lum%ovn_lum)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (108,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (overland_n(0:imax)) 
        
        rewind (108)
        read (108,*) titldum
        read (108,*) header
            
         do il = 1, imax
           read (108,*,iostat=eof) overland_n(il)
           if (eof < 0) exit
         end do
       exit
      enddo
      endif

      db_mx%ovn = imax
      
      close (108)
      return
      end subroutine overland_n_read