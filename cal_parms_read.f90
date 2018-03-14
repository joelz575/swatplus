      subroutine cal_parms_read
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function computes new paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chg_par     |variable      |new parameter value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use input_file_module
      use maximum_data_module
      use calibration_data_module

      integer, dimension (:), allocatable :: elem_cnt
      character (len=80) :: titldum, header
      integer :: eof
      
      imax = 0
      mchg_par = 0
        
      !!read parameter change values for calibration
      inquire (file=in_chg%cal_parms, exist=i_exist)
      if (i_exist == 0 .or. in_chg%cal_parms == 'null') then
        allocate (cal_parms(0:0))
      else
        do
          open (107,file=in_chg%cal_parms)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mchg_par
          allocate (cal_parms(mchg_par))
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          do i = 1, mchg_par
            read (107,*,iostat=eof) cal_parms(i)
            if (eof < 0) exit
          end do
          exit
        end do
      end if         
     
      db_mx%cal_parms = mchg_par
      return
      end subroutine cal_parms_read