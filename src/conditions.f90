      subroutine conditions (ob_cur, idtbl)
      !current conditions include: w_stress, n_stress, phu_plant, phu_base0, soil_water, jday, month, vol
      ! year_rot, year_cal, year_seq, prob, land_use   
      !target variables include: w_stress -> wp, fc, ul; vol -> pvol, evol
    
      use conditional_module
      use climate_module
      use time_module
      use hru_module, only : hru
      use soil_module
      use plant_module
      use reservoir_module
      use reservoir_data_module
      use sd_channel_module
      use hydrograph_module
      use output_landscape_module
      use aquifer_module
      use organic_mineral_mass_module

      implicit none

      integer, intent (in)  :: ob_cur         !          |
      integer, intent (in)  :: idtbl          !none      |
      integer :: ob_num                       !          |object number   
      integer :: nbz = 748932582              !          |
      integer, dimension(1) :: seed = (/3/)   !          |
      integer :: ic                           !none      |counter
      integer :: ialt                         !none      |counter
      integer :: iac                          !none      |counter
      integer :: iob                          !          |
      real :: targ_val                        !          |
      real :: ran_num                         !          |
      real :: aunif                           !          |
      integer :: ires                         !          |
      integer :: ipl                          !          |
      integer :: iipl                         !          |
      real :: targ                            !          |
      integer :: pl_sum                       !none      |number of plants growing
      integer :: days_tot                     !none      |
      real :: strs_sum                        !none      |sum of stress (water or n) of all growing plants
      real :: prob_cum                        !          |
      real :: prob_apply                      !          |
      real :: hru_exp_left                    !          |number of hru's expected to still be applied (uniform or normal distr)
      real :: hru_act_left                    !          |number of hru's actually still to be applied
      
      d_tbl%act_hit = "y"
      do ic = 1, d_tbl%conds
        select case (d_tbl%cond(ic)%var)
            
        !water stress
        case ("w_stress")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !! find average water stress of all growing plants
          pl_sum = 0
          strs_sum = 0.
          do ipl = 1, pcom(ob_num)%npl
            if (pcom(ob_num)%plcur(ipl)%gro == "y") then
              pl_sum = pl_sum + 1
              strs_sum = strs_sum + pcom(ob_num)%plstr(ipl)%strsw
            end if
          end do
          if (pl_sum > 0) then
            strs_sum = strs_sum / pl_sum
          else
            strs_sum = 1.
          end if

          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then    !to trigger irrigation
              if (strs_sum > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (strs_sum < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !nitrogen stress
        case ("n_stress")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !! find average water stress of all growing plants
          pl_sum = 0
          strs_sum = 0.
          do ipl = 1, pcom(ob_num)%npl
            if (pcom(ob_num)%plcur(ipl)%gro == "y") then
              pl_sum = pl_sum + 1
              strs_sum = strs_sum + pcom(ob_num)%plstr(ipl)%strsn
            end if
          end do
          if (pl_sum > 0) strs_sum = strs_sum / pl_sum

          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then    !to trigger fertilizer application
              if (strs_sum > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (strs_sum < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !potential heat units - plant based
        case ("phu_plant")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          !find the plant that is ready for harvest
          ipl = 1
          do iipl = 1, pcom(ob_num)%npl
            if (pcom(ob_num)%plcur(iipl)%phuacc > 1.e-6) then
              ipl = iipl
              exit
            end if
          end do
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (pcom(ob_num)%plcur(ipl)%phuacc > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (pcom(ob_num)%plcur(ipl)%phuacc < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !potential heat units - base zero
        case ("phu_base0")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          if (d_tbl%cond(ic)%ob == "hru") then
            iob = sp_ob1%hru + ob_num - 1
          end if
          if (d_tbl%cond(ic)%ob == "hlt") then
            iob = sp_ob1%hru_lte + ob_num - 1
          end if
          iwst = ob(iob)%wst
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (wst(iwst)%weat%phubase0 > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (wst(iwst)%weat%phubase0 < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                         
        !precip on current day
        case ("precip_cur")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          if (d_tbl%cond(ic)%ob == "hru") then
            iob = sp_ob1%hru + ob_num - 1
          end if
          if (d_tbl%cond(ic)%ob == "hlt") then
            iob = sp_ob1%hru_lte + ob_num - 1
          end if
          iwst = ob(iob)%wst
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (wst(iwst)%weat%precip > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (wst(iwst)%weat%precip < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                      
        !precip on next day day
        case ("precip_next")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          if (d_tbl%cond(ic)%ob == "hru") then
            iob = sp_ob1%hru + ob_num - 1
          end if
          if (d_tbl%cond(ic)%ob == "hlt") then
            iob = sp_ob1%hru_lte + ob_num - 1
          end if
          iwst = ob(iob)%wst
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (wst(iwst)%weat%precip_next > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (wst(iwst)%weat%precip_next < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                      
        !plant growing
        case ("plant_gro")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          ipl = Max (Int(d_tbl%cond(ic)%lim_const), 1)
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then    !determine if growing (y) or not (n)
              if (pcom(ob_num)%plcur(ipl)%gro /= d_tbl%cond(ic)%lim_var) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                    
        !specific plant growing
        case ("plant_name_gro")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          do iipl = 1, pcom(ob_num)%npl
            if (d_tbl%cond(ic)%lim_var == pcom(ob_num)%name) then
              ipl = iipl
            end if
          end do
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then    !determine if growing (y) or not (n)
              if (pcom(ob_num)%plcur(ipl)%gro == "n") then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                         
        !days since last plant
        case ("days_plant")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (pcom(ob_num)%days_plant /= Int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                   
        !days since last harvest
        case ("days_harv")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (pcom(ob_num)%days_harv /= Int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                                    
        !days since last action
        case ("days_act")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          iac = d_tbl%con_act(ic)
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (pcom(ob_num)%dtbl(idtbl)%days_act(iac) /= Int(d_tbl%cond(ic)%lim_const)+2) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (pcom(ob_num)%dtbl(idtbl)%days_act(iac) < Int(d_tbl%cond(ic)%lim_const+2)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                                      
        !days since first simulation day of year
        case ("day_start")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (time%day_start /= int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !soil water
        case ("soil_water")
          !determine target variable
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          targ_val = 0.
          select case (d_tbl%cond(ic)%lim_var)
          case ("wp")   !wilting point
            targ_val = 0.
          case ("fc")   !field capacity
            targ_val = soil(ob_num)%sumfc
          case ("ul")   !upper limit (porosity)
            targ_val = soil(ob_num)%sumul
          end select
          
          !perform operation on target variable to get target
          select case ((d_tbl%cond(ic)%lim_op))
          case ("*")
            targ = targ_val * d_tbl%cond(ic)%lim_const
          case ("+")
            targ = targ_val + d_tbl%cond(ic)%lim_const
          case ("-")
            targ = targ_val - d_tbl%cond(ic)%lim_const
          case ("/")
            targ = targ_val / d_tbl%cond(ic)%lim_const
          end select
          
          !determine if condition is met
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (soil(ob_num)%sw > targ) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (soil(ob_num)%sw < targ) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !julian day
        case ("jday")
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (time%day > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (time%day < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == "=") then
              if (time%day /= d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
        
        !month
        case ("month")
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (time%mo > int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (time%mo < int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == "=") then
              if (time%mo /= int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !rotation year
        case ("year_rot")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (pcom(ob_num)%rot_yr >= Int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (pcom(ob_num)%rot_yr <= Int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == "=") then
              if (pcom(ob_num)%rot_yr /= Int(d_tbl%cond(ic)%lim_const)) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
           
        !growth year of perennials
        case ("year_gro")
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (time%yrc /= d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !calendar year
        case ("year_cal")
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (time%yrc > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (time%yrc < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == "=") then
              if (time%yrc /= d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !sequential year of simulation
        case ("year_seq")
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (time%yrs > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (time%yrs < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == "=") then
              if (time%yrs /= d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                          
        !current years of maturity for perennial plants
        case ("cur_yrs_mat")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (pcom(ob_num)%plcur(1)%curyr_mat > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (pcom(ob_num)%plcur(1)%curyr_mat < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                       
        !above ground biomass
        case ("biomass")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (pl_mass(ob_num)%ab_gr_com%m > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (pl_mass(ob_num)%ab_gr_com%m < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                                               
        !leaf area index
        case ("leaf_area")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (pcom(ob_num)%lai_sum > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (pcom(ob_num)%lai_sum < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                                                         
        !total ground cover - above ground biomass + surface residue
        case ("ground_cov")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (pl_mass(ob_num)%ab_gr_com%m + rsd1(ob_num)%tot_com%m > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (pl_mass(ob_num)%ab_gr_com%m + rsd1(ob_num)%tot_com%m < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                   
        !probability
        case ("prob")
          !call RANDOM_SEED ()
          !call RANDOM_NUMBER (ran_num)
          !ran_num = ran1(1)
          ran_num = Aunif(rndseed_cond)
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (ran_num > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (ran_num < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                    
        !probability of event within a defined period assuming uniform distribution
        case ("prob_unif")
          prob_cum = 0.
          !check if period falls over a calendar year - ob_num is first and lim_const is last day
          if (d_tbl%cond(ic)%lim_const > d_tbl%cond(ic)%ob_num) then
            if (time%day <= d_tbl%cond(ic)%lim_const .and. time%day >= d_tbl%cond(ic)%ob_num) then
              !cumulative prob of uniform distribution on current day of the window
              days_tot = d_tbl%cond(ic)%lim_const - d_tbl%cond(ic)%ob_num + 1
              prob_cum = 1. / days_tot
            end if
          else
            if (time%day >= d_tbl%cond(ic)%lim_const .or. time%day <= d_tbl%cond(ic)%ob_num) then
              days_tot = time%day_end_yr - d_tbl%cond(ic)%lim_const + d_tbl%cond(ic)%ob_num + 1
              prob_cum = 1. / days_tot
            end if
          end if
          
          ran_num = Aunif(rndseed_cond)
          if (ran_num > prob_cum) then
            do ialt = 1, d_tbl%alts
              if (d_tbl%alt(ic,ialt) == "=") then
                d_tbl%act_hit(ialt) = "n"
              end if
            end do
          end if
                       
        !probability of event within a defined period assuming uniform distribution across a land use
        case ("prob_unif_lu")
          prob_cum = 0.
          !check if period falls over a calendar year - ob_num is first and lim_const is last day
          if (d_tbl%cond(ic)%lim_const > d_tbl%cond(ic)%ob_num) then
            if (time%day <= d_tbl%cond(ic)%lim_const .and. time%day >= d_tbl%cond(ic)%ob_num) then
              !cumulative prob of uniform distribution on current day of the window
              prob_cum = (time%day - d_tbl%cond(ic)%ob_num + 1) / (d_tbl%cond(ic)%lim_const - d_tbl%cond(ic)%ob_num)
            end if
          else
            if (time%day >= d_tbl%cond(ic)%lim_const .or. time%day <= d_tbl%cond(ic)%ob_num) then
              days_tot = time%day_end_yr - d_tbl%cond(ic)%lim_const + d_tbl%cond(ic)%ob_num
              if (time%day >= d_tbl%cond(ic)%lim_const) then
                !cumulative prob of uniform distribution on current day of the window
                prob_cum = (time%day - d_tbl%cond(ic)%lim_const) / days_tot
              else
                !cumulative prob of uniform distribution on current day of the window
                prob_cum = (time%day_end_yr - d_tbl%cond(ic)%lim_const + time%day + 1) / days_tot
              end if
            end if
          end if
          if (prob_cum > 0.) then
            ran_num = Aunif(rndseed_cond)
            hru_exp_left = d_tbl%hru_lu - (prob_cum * d_tbl%hru_lu)
            hru_act_left = d_tbl%hru_lu - d_tbl%hru_lu_cur
            prob_apply = (hru_act_left - hru_exp_left) / (hru_act_left + 1)
            !prob_apply = (prob_cum * d_tbl%hru_lu - d_tbl%hru_lu_cur + 1) / d_tbl%hru_lu
            if (ran_num > prob_apply) then
              do ialt = 1, d_tbl%alts
                if (d_tbl%alt(ic,ialt) == "=") then
                  d_tbl%act_hit(ialt) = "n"
                end if
              end do
            else
              !if (pcom(ob_cur)%dtbl(idtbl)%num_actions(iac) <= Int(d_tbl%act(iac)%const2)) then
              !  d_tbl%hru_lu_cur = d_tbl%hru_lu_cur + 1
              !  d_tbl%hru_ha_cur = d_tbl%hru_ha_cur + hru(ob_cur)%area_ha
              !end if
              ipl = 1
            end if
          else
            do ialt = 1, d_tbl%alts
              if (d_tbl%alt(ic,ialt) == "=") then
                d_tbl%act_hit(ialt) = "n"
              end if
            end do
          end if
          
          if (time%day > d_tbl%cond(ic)%lim_const) then
            d_tbl%hru_lu_cur = 0
            d_tbl%hru_ha_cur = 0.
          end if

        !tile flow
        case ("tile_flo")
          ob_num = ob_cur   !the dtbl ob_num is the sequential hyd number in the con file
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (hwb_d(ob_num)%qtile > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (hwb_d(ob_num)%qtile < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                            
        !irrigation demand
        case ("irr_demand")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !determine if condition is met
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (irrig(ob_num)%demand > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (irrig(ob_num)%demand < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                        
        !irrigation demand
        case ("irr_demand_wro")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          !determine if condition is met
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (wro(ob_num)%demand > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then    !may use for grazing or fire
              if (wro(ob_num)%demand <= d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        !aquifer depth below surface
        case ("aqu_dep")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (aqu_d(ob_num)%dep_wt > d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (aqu_d(ob_num)%dep_wt < d_tbl%cond(ic)%lim_const) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
          
        !land use and management
        case ("land_use")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (hru(ob_num)%dbsc%land_use_mgt /= d_tbl%cond(ic)%lim_var) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                                 
        !channel management
        case ("ch_use")
          ob_num = d_tbl%cond(ic)%ob_num
          if (ob_num == 0) ob_num = ob_cur
          
          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "=") then
              if (sd_ch(ob_num)%order /= d_tbl%cond(ic)%lim_var) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
             
        !reservoir volume
        case ("vol")
          !determine target variable
          ires = d_tbl%cond(ic)%ob_num
          if (ires == 0) ires = ob_cur
          
          select case (d_tbl%cond(ic)%lim_var)
          case ("e-pv")   !emergency minus prinicpal storage volume
            targ_val = res_hyd(ires)%evol - res_hyd(ires)%pvol
          case ("pvol")   !prinicpal storage volume
            targ_val = res_ob(ires)%pvol
          case ("evol")   !emergency storage volume
            targ_val = res_ob(ires)%evol
          end select
                      
          !perform operation on target variable to get target
          select case ((d_tbl%cond(ic)%lim_op))
          case ("*")
            targ = targ_val * d_tbl%cond(ic)%lim_const
          case ("+")
            targ = targ_val + d_tbl%cond(ic)%lim_const
          case ("-")
            targ = targ_val - d_tbl%cond(ic)%lim_const
          case ("/")
            targ = targ_val / d_tbl%cond(ic)%lim_const
          end select

          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (res(ires)%flo > targ) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (res(ires)%flo < targ) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
                         
        !wetland volume - stored on an hru
        case ("vol_wet")
          !determine target variable
          ires = d_tbl%cond(ic)%ob_num
          if (ires == 0) ires = ob_cur
          
          select case (d_tbl%cond(ic)%lim_var)
          case ("pvol")   !prinicpal storage volume
            targ_val = wet_ob(ires)%pvol
          case ("evol")   !emergency storage volume
            targ_val = wet_ob(ires)%evol
          end select
                      
          !perform operation on target variable to get target
          select case ((d_tbl%cond(ic)%lim_op))
          case ("*")
            targ = targ_val * d_tbl%cond(ic)%lim_const
          case ("+")
            targ = targ_val + d_tbl%cond(ic)%lim_const
          case ("-")
            targ = targ_val - d_tbl%cond(ic)%lim_const
          case ("/")
            targ = targ_val / d_tbl%cond(ic)%lim_const
          end select

          do ialt = 1, d_tbl%alts
            if (d_tbl%alt(ic,ialt) == "<") then
              if (wet(ires)%flo > targ) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
            if (d_tbl%alt(ic,ialt) == ">") then
              if (wet(ires)%flo < targ) then
                d_tbl%act_hit(ialt) = "n"
              end if
            end if
          end do
            
        end select
      end do
 
      return
      end subroutine conditions
