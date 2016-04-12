      subroutine conditions (id, ob_num)
      !current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
      ! year_rot, year_cal, year_seq    
      !target variables include: w_stress -> wp, fc, ul; vol -> pvol, evol
    
      use jrw_datalib_module
      use conditional_module
      use climate_parms
      use time_module
      use parm
      use reservoir_module
      
      integer :: id, ob_num

      d_tbl(id)%act_hit = "y"
      do ic = 1, d_tbl(id)%conds
        select case (d_tbl(id)%cond(ic)%var)
        !water stress
        case ("w_stress")
          ihru = ob_num
          ipl = amax1 (d_tbl(id)%cond(ic)%ob_num, 1)
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (pcom(ihru)%plstr(ipl)%strsw > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (pcom(ihru)%plstr(ipl)%strsw < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !nitrogen stress
        case ("n_stress")
          ihru = ob_num
          ipl = amax1 (d_tbl(id)%cond(ic)%ob_num, 1)
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (pcom(ihru)%plstr(ipl)%strsn > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (pcom(ihru)%plstr(ipl)%strsn < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !potential heat units - plant based
        case ("phu_plant")
          ihru = ob_num
          ipl = amax1 (d_tbl(id)%cond(ic)%ob_num, 1)
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (pcom(j)%plcur(ipl)%phuacc > d_tbl(id)%cond(ic)%lim_const *        &
                                                      pcom(j)%plg(ipl)%phumat) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (pcom(j)%plcur(ipl)%phuacc < d_tbl(id)%cond(ic)%lim_const *       &
                                                      pcom(j)%plg(ipl)%phumat) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !potentail heat units - base zero
        case ("phu_base0")
          ihru = ob_num
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (phubase(ihru) > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (phubase(ihru) < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !soil water
        case ("soil_water")
          !determine target variable
          isol = ob_num   !same as hru number
          select case (d_tbl(id)%cond(ic)%lim_var)
          case ("wp")   !wilting point
            targ_val = 0.
          case ("fc")   !field capacity
            targ_val = sol(isol)%s%sumfc
          case ("ul")   !upper limit (porosity)
            targ_val = sol(isol)%s%sumul
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
              if (phubase(ihru) > targ) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (phubase(ihru) < targ) then
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
              if (time%day == d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
        
        !month
        case ("month")
          do ialt = 1, d_tbl(id)%alts
            if (d_tbl(id)%alt(ic,ialt) == "<") then
              if (time%mo > d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (time%mo < d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == "=") then
              if (time%mo == d_tbl(id)%cond(ic)%lim_const) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !rotation year
        case ("year_rot")
            
        !calendar year
        case ("year_cal")
            
        !sequential year of simulation
        case ("year_seq")
            
        !reservoir volume
        case ("vol")
          !determine target variable
          ires = ob_num   !reservoir number
          select case (d_tbl(id)%cond(ic)%lim_var)
          case ("pvol")   !prinicpal storage volume
            targ_val = res_hyd(ires)%pvol
          case ("evol")   !emergency storage volume
            targ_val = res_hyd(ires)%evol
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
              if (phubase(ihru) > targ) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl(id)%alt(ic,ialt) == ">") then
              if (phubase(ihru) < targ) then
                d_tbl(id)%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        end select
      end do
 
      return
      end subroutine conditions