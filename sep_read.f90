      subroutine sep_read
      
      use input_file_module
      use hru_module, only : isep
      use maximum_data_module
      use septic_data_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_str%septic_str,exist=i_exist)                  
      if (i_exist == 0 .or. in_str%septic_str == 'null') then 
        allocate (sep(0:0)) 
      else
        do 
          open (172,file=in_str%septic_str)
          read (172,*,iostat=eof) titldum
          if (eof < 0) exit
          read (172,*,iostat=eof) header
          if (eof < 0) exit
          do while (eof == 0)
            read (172,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          allocate (sep(0:imax))
          rewind (172)
          read (172,*) titldum
          read (172,*) header   
                
          do isep = 1, imax
            read(172,*,iostat=eof) sep(isep)        
            if (eof < 0) exit
          end do    
          exit
        enddo
        end if
 
      close(172)
      
      db_mx%septic = imax
      
      return  
      end subroutine sep_read