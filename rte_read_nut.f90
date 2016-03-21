      subroutine rte_read_nut

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.          

      integer :: eof, i, imax
      character (len=80) :: titldum
      character (len=80) :: header

      eof = 0
      imax = 0

      inquire (file='nutrients.rte',exist=i_exist)
      if (i_exist == 0) then
        allocate (rte_nut(0:0))
      else
      do
        open (105,file='nutrients.rte')
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (105,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mrch = mch + 1
          end do        
        
        allocate (rte_nut(0:imax))
        rewind (105)
        read (105,*) titldum
        read (105,*) header
          
        do ich = 1, imax
          read (105,*,iostat=eof) i
          backspace (105)
          read (105,*) k, rte_nut(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      close(105)

      return
      end subroutine rte_read_nut