      subroutine data_files_read

      character (len=500) :: header
      character (len=80) :: titldum
      integer :: eof
      
      eof = 0

      !! read database file names
      inquire (file="datafiles.dat", exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file="datafiles.dat")
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) dbase_files
        if (eof < 0) exit
        exit
      enddo 
      endif
        close (107) 
            
      return
      end subroutine data_files_read