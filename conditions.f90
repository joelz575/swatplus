      subroutine conditions (id, ob_cur)
      !current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
      ! year_rot, year_cal, year_seq, prob, land_use   
      !target variables include: w_stress -> wp, fc, ul; vol -> pvol, evol
    
      use jrw_datalib_module
      use conditional_module
      use climate_parms
      use time_module
      use parm
      use reservoir_module
      use sd_channel_module
      
      integer, intent (in)  :: id, ob_cur
      integer :: ob_num
      integer :: nbz=748932582
      integer, dimension(1) :: seed = (/3/)

      d_tbl(id)%act_hit = "y"
      do ic = 1, d_tbl(id)%conds
        select case (d_tbl(id)%cond(ic)%var)
        !water stress
        case ("w_stress")
          ob_num = d_tbl(id)%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          ipl = Max (d_tbl(id)%cond(ic)%ob_num, 1)
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (pcom(ob_num)%plstr(ipl)%strsw > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (pcom(ob_num)%plstr(ipl)%strsw < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !nitrogen stress
        case ("n_stress")
          ob_num = d_tbl(id)%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          ipl = Max (d_tbl(id)%cond(ic)%ob_num, 1)
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (pcom(ob_num)%plstr(ipl)%strsn > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (pcom(ob_num)%plstr(ipl)%strsn < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !potential heat units - plant based
        case ("phu_plant")
          ob_num = d_tbl(id)%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          ipl = Max (d_tbl(id)%cond(ic)%ob_num, 1)
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (pcom(ob_num)%plcur(ipl)%phuacc > d_tbl(id)%cond(ic)%lim_const *        &
                                                      pcom(ob_num)%plg(ipl)%phumat) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (pcom(ob_num)%plcur(ipl)%phuacc < d_tbl(id)%cond(ic)%lim_const *       &
                                                      pcom(ob_num)%plg(ipl)%phumat) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !potential heat units - base zero
        case ("phu_base0")
          ob_num = d_tbl(id)%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          if (d_tbl(id)%cond(ic)%ob == 'hru') then
            iob = sp_ob1%hru + ob_num - 1
          end if
          if (d_tbl(id)%cond(ic)%ob == 'hlt') then
            iob = sp_ob1%hru_lte + ob_num - 1
          end if
          iwst = ob(iob)%wst
          
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (wst(iwst)%weat%phubase0 > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (wst(iwst)%weat%phubase0 < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !soil water
        case ("soil_water")
          !determine target variable
          ob_num = d_tbl(id)%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          select case (d_tbl(id)%cond(ic)%lim_var)
          case ("wp")   !wilting point
            targ_val = 0.
          case ("fc")   !field capacity
            targ_val = soil(ob_num)%sumfc
          case ("ul")   !upper limit (porosity)
            targ_val = soil(ob_num)%sumul
          end select
          
          !perform operation on target variable to get target
          select case ((d_tbl(id)%cond(ic)%lim_op))
          case ("*")
            targ = targ_val * d_tbl(id)%cond(ic)%lim_const
          case ("+")
            targ = targ_val + d_tbl(id)%cond(ic)%lim_const
          case ("-")
            targ = targ_val - d_tbl(id)%cond(ic)%lim_const
          case ("/")
            targ = targ_val / d_tbl(id)%cond(ic)%lim_const
          end select
          
          !determine if condition is met
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (soil(ob_num)%sw > targ) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (soil(ob_num)%sw < targ) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !julian day
        case ("jday")
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then
              if (time%day > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (time%day < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == "=") then
              if (time%day /= d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
        
        !month
        case ("month")
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then
              if (time%mo > int(d_tbl(id)%cond(ic)%lim_const)) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (time%mo < int(d_tbl(id)%cond(ic)%lim_const)) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == "=") then
              if (time%mo /= int(d_tbl(id)%cond(ic)%lim_const)) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !rotation year
        case ("year_rot")
            
        !calendar year
        case ("year_cal")
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then
              if (time%yrc > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (time%yrc < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == "=") then
              if (time%yrc /= d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !sequential year of simulation
        case ("year_seq")
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then
              if (time%yrs > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (time%yrs < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == "=") then
              if (time%yrs /= d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
                      
        !probability
        case ("prob")
          !call RANDOM_SEED ()
          !call RANDOM_NUMBER (ran_num)
          !ran_num = ran1(1)
          ran_num = Aunif(nbz)
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then
              if (ran_num > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (ran_num < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
                    
        !land use and management
        case ("land_use")
          ob_num = d_tbl(id)%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "=") then
              if (hru(ob_num)%dbsc%land_use_mgt /= d_tbl(id)%cond(ic)%lim_var) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            !else
            !  d_tbl(id)%act_hit(ialt) = "n"
            end if
          end do
                                 
        !channel management
        case ("ch_use")
          ob_num = d_tbl(id)%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "=") then
              if (sd_ch(ob_num)%order /= d_tbl(id)%cond(ic)%lim_var) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            !else
            !  d_tbl(id)%act_hit(ialt) = "n"
            end if
          end do
             
        !reservoir volume
        case ("vol")
          !determine target variable
          ires = d_tbl(id)%cond(ic)%ob_num
          if (ires == 0) ires = ob_cur
          
          select case (d_tbl(id)%cond(ic)%lim_var)
          case ("pvol")   !prinicpal storage volume
            targ_val = res_ob(ires)%pvol
          case ("evol")   !emergency storage volume
            targ_val = res_ob(ires)%evol
          end select
                      
          !perform operation on target variable to get target
          select case ((d_tbl(id)%cond(ic)%lim_op))
          case ("*")
            targ = targ_val * d_tbl(id)%cond(ic)%lim_const
          case ("+")
            targ = targ_val + d_tbl(id)%cond(ic)%lim_const
          case ("-")
            targ = targ_val - d_tbl(id)%cond(ic)%lim_const
          case ("/")
            targ = targ_val / d_tbl(id)%cond(ic)%lim_const
          end select

          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then
              if (res(ires)%flo > targ) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (res(ires)%flo < targ) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        end select
      end do
 
      return
      end subroutine conditions