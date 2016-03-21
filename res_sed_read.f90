      subroutine res_sed_read
      
      use input_file_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.          

      integer :: eof, i, imax
      character (len=80) :: titldum
      character (len=80) :: header
      real :: orgpi, solpi, orgni, no3i, nh3i, no2i
      real :: lkarea
      
      mres_q = 0
      eof = 0
      imax = 0

      inquire (file=in_res%sed_res,exist=i_exist)
      if (i_exist == 0 .or. in_res%sed_res == 'null') then
        allocate (res_sed(0:0))
      else
      do
        open (105,file=in_res%sed_res)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (105,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mres_q = mres_q + 1
          end do        
        
        allocate (res_sed(0:imax))
        rewind (105)
        read (105,*) titldum
        read (105,*) header
          
        do ires = 1, mres_q
          read (105,*,iostat=eof) i
          backspace (105)
          read (105,*) k, res_sed(i)
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      close(105)

      return
      end subroutine res_sed_read