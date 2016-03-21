      subroutine fertparm_read
      
      use input_file_module
   
      integer :: it
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: fertdbase
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mfrt = 0
      
      inquire (file=in_parmdb%fert_frt, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%fert_frt == 'null') then
         allocate (fertdb(0:0))
         allocate (fert_xw(0:0))
      else
      do  
        open (107,file=in_parmdb%fert_frt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
           do while (eof >= 0) 
             read (107,*,iostat=eof) titldum
             if (eof < 0) exit
             imax = imax + 1
           end do
           
        allocate (fertdb(0:imax))
        allocate (fert_xw(0:imax))
        
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        do it = 1, imax
          read (107,*,iostat=eof) fertdb(it)
          
          !! fert
          fert_xw(it) = fertdb(it)%fertnm
          if (eof < 0) exit
        end do
       exit
      enddo
      endif
      
      db_mx%fertparm  = imax 
      
      close (107)
      return
      end subroutine fertparm_read