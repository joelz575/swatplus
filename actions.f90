      subroutine actions (id, ob_num)
      !actions include: irrigate, fertilize, release, till, plant, drainage, herd, water_rights
    
      use jrw_datalib_module
      use conditional_module
      use climate_parms
      use time_module
      use parm
      use reservoir_module
      use sd_channel_module
      use sd_hru_module
      
      integer :: id, ob_num, ir
      character(len=1) :: action

      do iac = 1, d_tbl(id)%acts
        action = "n"
        do ial = 1, d_tbl(id)%alts
          if (d_tbl(id)%act_hit(ial) == "y" .and. d_tbl(id)%act_outcomes(iac,ial) == "y") then
            action = "y"
            exit
          end if
        end do
      
        if (action == "y") then
          select case (d_tbl(id)%act(iac)%name)
          
          !irrigate
          case ("irrigate")     
            select case (d_tbl(id)%act(iac)%option)
            case ("amt_mm")        !to trigger irrigation
              aird(ob_num) = d_tbl(id)%act(iac)%const
              qird(ob_num) = 0.
            case ("file")      !may use for grazing or fire
              !need to crosswalk the file_pointer
              ir = d_tbl(id)%act_ptr(iac)
              aird(ob_num) = irrop_db(ir)%amt_mm * (1. - irrop_db(ir)%surq)
              qird(ob_num) = irrop_db(ir)%amt_mm * irrop_db(ir)%surq
            end select
            
          !reservoir release - res_hydro does release and water balance
          case ("release")
            idat = res_ob(ob_num)%props
            ihyd = res_dat(idat)%hyd
            ised = res_dat(idat)%sed
            call res_hydro (ob_num, id, iac, ihyd, ised)
            call res_sediment (ob_num, ihyd, ised)
            
          !fertilize
          case ("fertilize")
            !fertop = fertop_db(pointer to fert.dat)
            !ipl = 1
            !call pl_fert
            
          !tillage
          case ("till")
            !idtill = pointer to till.dat
            !ipl = 1
            !call mgt_newtillmix(ob_num, 0.)
            
          !plant
          case ("plant")
            !icom = pcom(ob_num)%pcomdb
            !do ipl = 1, pcom(ob_num)%npl
            !  idp = pcomdb(icom)%pl(ipl)%db_num
            !  if (mgt%op2 == 0 .or. mgt%op2 == ipl) then
            !    pcom(j)%plcur(ipl)%gro = 1
            !    pcom(j)%plcur(ipl)%idorm = 0
            !  end if
            !end do
            
          !harvest
          case ("harvest")
                      
          !initiate growing season for hru_lte
          case ("grow_init")
            ihru = ob_num
            sd(ihru)%igro = 1
            sd(ihru)%g = 0.
            sd(ihru)%alai = 0.
            sd(ihru)%dm = 0.
            sd(ihru)%hufh = 0.
            
          !end growing season for hru_lte
          case ("grow_end")
            !calculate yield - print lai, biomass and yield - add stress to yield?
            yield = sd(ihru)%dm * pldb(sd(ihru)%iplant)%hvsti  ! * sd(isd)%stress
            sd(ihru)%yield = yield / 1000.
            sd(ihru)%npp = sd(ihru)%dm / 1000.
            sd(ihru)%lai_mx = sd(ihru)%alai
            !compute annual net primary productivity (npp) for perennial non-harvested?
            !use output.mgt print code
            !write() isd, time%day, time%yrc, pldb(iplt)%plantnm, sd(isd)%alai, sd(isd)%dm, yield
            sd(ihru)%igro = 0
            sd(ihru)%g = 0.
            sd(ihru)%alai = 0.
            sd(ihru)%dm = 0.     !adjust for non-harvested perennials?
            sd(ihru)%hufh = 0.
              
          !drainage water management
          case ("drainage")
              
          !land use change
          case ("lu_change")
            !ihru, ilu and isol are in modparm
            ihru = ob_num
            ilu = d_tbl(id)%act_ptr(iac)
            hru(ob_num)%dbs%land_use_mgt = ilu
            hru(ob_num)%land_use_mgt_c = d_tbl(id)%act(iac)%file_pointer
            isol = hru(ob_num)%dbs%soil  
            call pcom_set_parms (1)
                     
          !channel change
          case ("chan_change")
            ich = ob_num
            !set new cover and name for calibration
            sd_ch(ich)%cov = d_tbl(i)%act(iac)%const
            sd_ch(ich)%order = d_tbl(i)%act(iac)%file_pointer
        
          !herd management - move the herd
          case ("herd")
            
          !water rights decision to move water
          case ("water_rights")
            
          end select
        end if
      end do

      return
      end subroutine actions