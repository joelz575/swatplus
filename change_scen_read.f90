      subroutine change_scen_read
      
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

      integer, dimension (:), allocatable :: elem_cnt
      character (len=80) :: titldum, header
      integer :: eof
      
      imax = 0
      mchg_scen = 0
        
      !!read parameter change values for calibration
      inquire (file=in_chg%scen_chg, exist=i_exist)
      if (i_exist == 0 .or. in_chg%scen_chg == 'null') then
        allocate (chg_scen(0:0))
      else
      do
        open (107,file=in_chg%scen_chg)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mchg_scen
        allocate (chg_scen(0:mchg_scen))
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      do jj = 1, mchg_scen
        i = jj - 1
        read (107,*,iostat=eof) chg_scen(i)%name, chg_scen(i)%day,       &
              chg_scen(i)%year, chg_scen(i)%typ, nspu
        allocate (elem_cnt(nspu))
        
        if (eof < 0) exit
        if (nspu > 0) then
          backspace (107)
          read (107,*,iostat=eof) chg_scen(i)%name, chg_scen(i)%day,    &
              chg_scen(i)%year, chg_scen(i)%typ,                        &
                              nspu, (elem_cnt(isp), isp = 1, nspu)
          if (eof < 0) exit
          
          !!save the object number of each defining unit
          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
              else
                ielem = ielem + (abs(ie2) - ie1) + 1
              end if
              ii = ii + 2
            end if
          end do
          allocate (chg_scen(i)%num(ielem))
          chg_scen(i)%num_tot = ielem

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              chg_scen(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                chg_scen(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  chg_scen(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
        end if
      end do
      exit
      end do
      end if      

      return
      end subroutine change_scen_read