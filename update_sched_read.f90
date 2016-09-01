      subroutine update_sched_read
      
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
      use jrw_datalib_module
      use conditional_module

      integer, dimension (:), allocatable :: elem_cnt
      character (len=80) :: titldum, header
      integer :: eof
      
      imax = 0
      mchg_sched = 0
        
      !!read parameter change values for calibration
      inquire (file=in_sch%structural_sch, exist=i_exist)
      if (i_exist == 0 .or. in_sch%structural_sch == 'null') then
        allocate (upd_sched(0:0))
      else
      do
        open (107,file=in_sch%structural_sch)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mchg_sched
        allocate (upd_sched(0:mchg_sched))
        db_mx%sched_up = mchg_sched
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit

      do i = 1, mchg_sched
        read (107,*,iostat=eof) upd_sched(i)%typ, upd_sched(i)%num, upd_sched(i)%name, upd_sched(i)%day,        &
            upd_sched(i)%year, upd_sched(i)%lum, nspu
        if (eof < 0) exit

        if (nspu > 0) then
          allocate (elem_cnt(nspu))
          backspace (107)
          read (107,*,iostat=eof) upd_sched(i)%typ, upd_sched(i)%num, upd_sched(i)%name, upd_sched(i)%day,        &
            upd_sched(i)%year, upd_sched(i)%new_lu, upd_sched(i)%num_tot, (elem_cnt(isp), isp = 1, nspu)
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
          allocate (upd_sched(i)%num(ielem))

          ielem = 0
          ii = 1
          do while (ii <= nspu)
            ie1 = elem_cnt(ii)
            if (ii == nspu) then
              ielem = ielem + 1
              ii = ii + 1
              upd_sched(i)%num(ielem) = ie1
            else
              ie2 = elem_cnt(ii+1)
              if (ie2 > 0) then
                ielem = ielem + 1
                upd_sched(i)%num(ielem) = ie2
              else
                ie2 = abs(ie2)
                do ie = ie1, ie2
                  ielem = ielem + 1
                  upd_sched(i)%num(ielem) = ie
                end do
              end if
              ii = ii + 2
            end if
          end do
          deallocate (elem_cnt)
        end if          

        if (upd_sched(i)%typ == 'land_use') then
          !! crosswalk parameters with calibration parameter db
          do ilu = 1, db_mx%d_tbl
            if (upd_sched(i)%name == d_tbl(ilu)%name) then
              upd_sched(i)%new_lu = icond
              exit
            end if
          end do
        end if
          
          if (upd_sched(i)%typ == 'structure') then
            !! crosswalk structural objects
            select case(upd_sched(i)%name)

            case ("terrace")
              do iterr = 1, db_mx%terrop_db
                if (terrace_db(iterr)%name == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = iterr
                end if
              end do

            case ("tile")
              do isdr = 1, db_mx%sdr
                if (sdr(iterr)%name == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = isdr
                end if
              end do
            
            case ("contour")
              do icont = 1, db_mx%contop_db
                if (contour_db(iterr)%name == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = icont
                end if
              end do

            case ("filter")
              do ifilt = 1, db_mx%filtop_db
                if (filtstrip_db(iterr)%name == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = ifilt
                end if
              end do

            case ("stripcrop")
              do istrp = 1, db_mx%stripop_db
                if (stripcrop_db(iterr)%name == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = istrp
                end if
              end do

            case ("grassww")
              do igras = 1, db_mx%grassop_db
                if (grwaterway_db(iterr)%name == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = igras
                end if
              end do

            case ("user_def")                 !user defined Upland CP removal MJW
              do iuser = 1, db_mx%bmpuserop_db
                if (bmpuser_db(iterr)%name == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = iuser
                end if
              end do
      
            case ("septic")
              do isept = 1, db_mx%septic
                if (sepdb(iterr)%sepnm == upd_sched(i)%name) then
                  upd_sched(i)%str_lu = isept
                end if
              end do

            end select
          end if

      end do
      exit
      end do
      end if      

      return
      end subroutine update_sched_read