       subroutine aqu_read 
      
       use input_file_module
      
       character (len=500) :: header
       character (len=80) :: titldum
       integer :: eof, i, imax
       
       msh_aqp = 0
       eof = 0
       imax = 0

       !! read shallow aquifer property data from aquifer.aqu
       inquire (file=in_aqu%aqu, exist=i_exist)
       if (i_exist == 0 .or. in_aqu%aqu == 'null') then
            allocate (aqudb(0:0))
            allocate (aqu_prm(0:0))
          else
       do
          open (107,file=in_aqu%aqu)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
            do while (eof == 0)
              read (107,*,iostat=eof) i
              if (eof < 0) exit
              imax = Max(imax,i)
              msh_aqp = msh_aqp + 1
            end do 
                       
          allocate (aqudb(0:imax))
          allocate (aqu_prm(0:imax))
          rewind (107)
          read (107,*) titldum
          read (107,*) header
          
          do ish_aqp = 1, msh_aqp
            read (107,*,iostat=eof) i
            backspace (107)
            !! read from the aquifer database file named aquifer.aqu
            read (107,*,iostat=eof) k, aqudb(i)
            if (eof < 0) exit
          end do

          close (107)
          exit
       enddo
       endif
          
       return
       end subroutine aqu_read         