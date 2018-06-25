      subroutine ch_read_pst
      
      use input_file_module
      use maximum_data_module
      use channel_data_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the 
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.     

      implicit none

      integer :: eof                   !end of file
      integer :: i                     !units     |description
      integer :: imax                  !          !determine max number for array (imax) and total number in file
      character (len=80) :: titldum    !title of file
      character (len=80) :: header     !header of file
      integer :: i_exist               !          |check to determine if file exists
      integer :: ich                   !none      |counter

      eof = 0
      imax = 0

      inquire (file=in_cha%pest,exist=i_exist)
      if (i_exist == 0 .or. in_cha%pest == "null") then
        allocate (ch_pst(0:0))
      else
      do
        open (105,file=in_cha%pest)
        read (105,*,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (105,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do  
          
        db_mx%ch_pst = imax
        
        allocate (ch_pst(0:imax))
        rewind (105)
        read (105,*) titldum
        read (105,*) header
          
        do ich = 1, db_mx%ch_pst
          read (105,*,iostat=eof) titldum
          backspace (105)
          read (105,*)  ch_pst(ich)
          if (eof < 0) exit
          
          if (ch_pst(ich)%pst_vol <= 1.e-6) ch_pst(ich)%pst_vol = 0.01
          if (ch_pst(ich)%pst_koc <= 1.e-6) ch_pst(ich)%pst_koc = 0.
          if (ch_pst(ich)%pst_stl <= 1.e-6) ch_pst(ich)%pst_stl = 1.
          if (ch_pst(ich)%pst_rsp <= 1.e-6) ch_pst(ich)%pst_rsp = 0.002
          if (ch_pst(ich)%pst_mix <= 1.e-6) ch_pst(ich)%pst_mix = 0.001
          if (ch_pst(ich)%sedpst_conc <= 1.e-6)ch_pst(ich)%sedpst_conc=0.
          if (ch_pst(ich)%sedpst_rea <= 1.e-6) ch_pst(ich)%sedpst_rea=0.05
          if (ch_pst(ich)%sedpst_bry <= 1.e-6)ch_pst(ich)%sedpst_bry=0.002
          if (ch_pst(ich)%sedpst_act <= 1.e-6)ch_pst(ich)%sedpst_act=0.030

        end do
        exit
      enddo
      endif
      close(105)

      return
      end subroutine ch_read_pst