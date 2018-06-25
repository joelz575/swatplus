      subroutine actions (id, ob_cur)
   
      use conditional_module
      use climate_module
      use time_module
      use hru_module, only : bio_eat, bio_trmp, fertno3, fertnh3, fertorgn, fertorgp, fertsolp, frt_kg,  &
        grz_days, ihru, igrz, ipl, irr_no, irr_sc, irramt, isol, manure_id, manure_kg, ndeat,       &
        phubase, sol_sumno3, sol_sumsolp, hru, yield 
      use soil_module
      use plant_module, only : pcom
      use plant_data_module
      use mgt_operations_module  
      use tillage_data_module
      use reservoir_module
      use sd_channel_module
      use hru_lte_module
      use basin_module
      use organic_mineral_mass_module
      
      implicit none
      
      integer, intent (in)  :: id          !         |
      integer, intent (in)  :: ob_cur      !         |
      integer :: ob_num                    !         |object number 
      integer :: icom                      !         |
      integer :: iac                       !none     |counter
      integer :: ial                       !none     |counter
      integer :: jj                        !none     |counter
      integer :: i                         !none     |counter
      integer :: iburn                     !none     |burn type from fire data base
      integer :: ich                       !none     |object number 
      integer :: idtill                    !none     |tillage type
      integer :: ifertop                   !         |surface application fraction from chem app data base
      integer :: ifrt                      !         |fertilizer type from fert data base
      integer :: iharvop                   !         |harvest operation type
      integer :: iihru                     !         |
      integer :: ilu                       !         |landuse type 
      integer :: irrop                     !         |irrigation operations
      integer :: j                         !none     |counter
      real :: hiad1                        !         |
      real :: amt_mm                       !         |
      real :: biomass                      !         |
      real :: wur                          !         |
      real :: idp                          !         |
      character(len=1) :: action           !         |

      do iac = 1, d_tbl(id)%acts
        action = "n"
        do ial = 1, d_tbl(id)%alts
          if (d_tbl(id)%act_hit(ial) == "y" .and. d_tbl(id)%act_outcomes(iac,ial) == "y") then
            action = "y"
            exit
          end if
        end do
      
        if (action == "y") then
          select case (d_tbl(id)%act(iac)%typ)
          
          !irrigate - hru action
          case ("irrigate")
            ipl = 1
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            irrop = d_tbl(id)%act_typ(iac)  !from irrop_db - ie: drip, sprinkler, etc - condition_read
            amt_mm = d_tbl(id)%act(iac)%const
            call pl_irrigate (ob_num, amt_mm, irrop)
            
            if (pco%mgtout == "year") then
              write (2612, *) ob_num, time%yrc, time%mo, time%day, "        ", "IRRIGATE", phubase(j),  &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass, rsd1(j)%tot(ipl)%m,       &
                  sol_sumno3(j), sol_sumsolp(j),irramt(j), irr_sc(j), irr_no(j)
            end if

          !reservoir release - res_hydro does release and water balance
          case ("release")
            !idat = res_ob(ob_num)%props
            !ihyd = res_dat(idat)%hyd
            !ised = res_dat(idat)%sed
            !call res_hydro (ob_num, id,ihyd, ised)
            !call res_sediment (ob_num, ihyd, ised)
            
          !fertilize
          case ("fertilize")
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            ipl = 1
            ifrt = d_tbl(id)%act_typ(iac)               !fertilizer type from fert data base
            frt_kg = d_tbl(id)%act(iac)%const           !amount applied in kg/ha
            ifertop = d_tbl(id)%act_app(iac)            !surface application fraction from chem app data base
            call pl_fert (ob_num, ifrt, frt_kg, ifertop)

            if (pco%mgtout == "year") then
              write (2612, *) ob_num, time%yrc, time%mo, time%day, chemapp_db(mgt%op4)%name, "    FERT", &
                phubase(j),pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pcom(j)%plm(ipl)%mass,                 &
                rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,              &
                fertorgn, fertsolp, fertorgp
            endif
 
          !tillage
          case ("till")
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            idtill = d_tbl(id)%act_app(iac)
            ipl = 1
            call mgt_newtillmix(ob_num, 0., idtill)
            
            if (pco%mgtout == "year") then
              write (2612, *) ob_num, time%yrc, time%mo, time%day, tilldb(idtill)%tillnm, "TILLAGE",    &
                  phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pcom(j)%plm(ipl)%mass,             &
                  rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), tilldb(idtill)%effmix
            end if

          !plant
          case ("plant")
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            icom = pcom(ob_num)%pcomdb
            do ipl = 1, pcom(ob_num)%npl
              idp = pcomdb(icom)%pl(ipl)%db_num
              if (d_tbl(id)%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm) then
                pcom(ob_num)%plcur(ipl)%gro = "y"
                pcom(ob_num)%plcur(ipl)%idorm = "n"
              end if
              if (pco%mgtout ==  "year") then
                write (2612, *) ob_num, time%yrc, time%mo, time%day, pldb(idp)%plantnm, pcomdb(icom)%name,  &
                    phubase(ob_num), pcom(ob_num)%plcur(ipl)%phuacc,  soil(ihru)%sw,                        &
                    pcom(ob_num)%plm(ipl)%mass, soil(ob_num)%ly(1)%rsd, sol_sumno3(ob_num),                 &
                    sol_sumsolp(ob_num), pcom(ob_num)%plg(ipl)%lai, pcom(ob_num)%plcur(ipl)%laimx_pop
              end if
            end do
            
          !harvest only
          case ("harvest")
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            iharvop = d_tbl(i)%act_typ(iac)
            
            do ipl = 1, pcom(j)%npl
              biomass = pcom(j)%plm(ipl)%mass
              if (d_tbl(id)%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm .or. d_tbl(id)%act(iac)%option == "all") then
                          
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ("biomass")    
                  call mgt_harvestop (ob_num, ipl, iharvop)
                case ("grain")
                  call mgt_harvgrainop (ob_num, ipl, iharvop)
                case ("residue")
                case ("tree")
                case ("tuber")
                end select

                j = ob_num
                !! sum yield and num. of harvest to calc ave yields
                pcom(j)%plcur(ipl)%yield = pcom(j)%plcur(ipl)%yield + yield
                pcom(j)%plcur(ipl)%harv_num=pcom(j)%plcur(ipl)%harv_num+1
            
                idp = pcom(j)%plcur(ipl)%idplt
                if (pco%mgtout == "year") then
                  write (2612, *) ob_num, time%yrc, time%mo, time%day,  pldb(idp)%plantnm, "HARVEST",     &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,      &
                      sol_sumno3(j), sol_sumsolp(j), yield, pcom(j)%plstr(ipl)%sum_n,                     &
                      pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,     &
                      pcom(j)%plstr(ipl)%sum_a
                end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
            end do
    
          !kill plant
          case ("kill")
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur

            do ipl = 1, pcom(j)%npl
              biomass = pcom(j)%plm(ipl)%mass
              if (d_tbl(id)%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm .or. d_tbl(id)%act(iac)%option == "all") then

                call mgt_killop (ob_num, ipl)

                j = ob_num
                idp = pcom(j)%plcur(ipl)%idplt
                if (pco%mgtout == "year") then
                  write (2612, *) ob_num, time%yrc, time%mo, time%day,  pldb(idp)%plantnm, "HARV/KILL",     &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,        &
                      sol_sumno3(j), sol_sumsolp(j), yield, pcom(j)%plstr(ipl)%sum_n,                       &
                      pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,       &
                      pcom(j)%plstr(ipl)%sum_a
                end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
              phubase(j) = 0.
            end do
  
          !harvest and kill
          case ("harvest_kill")
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            iharvop = d_tbl(i)%act_typ(iac)
            
            do ipl = 1, pcom(j)%npl
              biomass = pcom(j)%plm(ipl)%mass
              if (d_tbl(id)%act(iac)%option == pcomdb(icom)%pl(ipl)%cpnm .or. d_tbl(id)%act(iac)%option == "all") then
                          
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ("biomass")    
                  call mgt_harvestop (ob_num, ipl, iharvop)
                case ("grain")
                  call mgt_harvgrainop (ob_num, ipl, iharvop)
                case ("residue")
                case ("tree")
                case ("tuber")
                end select
            
                call mgt_killop (ob_num, ipl)

                j = ob_num
                !! sum yield and num. of harvest to calc ave yields
                pcom(j)%plcur(ipl)%yield = pcom(j)%plcur(ipl)%yield + yield
                pcom(j)%plcur(ipl)%harv_num=pcom(j)%plcur(ipl)%harv_num+1
            
                idp = pcom(j)%plcur(ipl)%idplt
                if (pco%mgtout == "year") then
                  write (2612, *) ob_num, time%yrc, time%mo, time%day,  pldb(idp)%plantnm, "HARV/KILL",     &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, rsd1(j)%tot(ipl)%m,        &
                      sol_sumno3(j), sol_sumsolp(j), yield, pcom(j)%plstr(ipl)%sum_n,                       &
                      pcom(j)%plstr(ipl)%sum_p, pcom(j)%plstr(ipl)%sum_tmp, pcom(j)%plstr(ipl)%sum_w,       &
                      pcom(j)%plstr(ipl)%sum_a
                end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
              phubase(j) = 0.
            end do
  
          !initiate growing season for hru_lte
          case ("grow_init")
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            ihru = ob_num
            hlt(ihru)%gro = "y"
            hlt(ihru)%g = 0.
            hlt(ihru)%alai = 0.
            hlt(ihru)%dm = 0.
            hlt(ihru)%hufh = 0.
            
          !end growing season for hru_lte
          case ("grow_end")
            !calculate yield - print lai, biomass and yield
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            ihru = ob_num
            idp = hlt(ihru)%iplant
            if (hlt(ihru)%pet < 10.) then
              wur = 100.
            else
              wur = 100. * hlt(ihru)%aet / hlt(ihru)%pet
            endif
            hiad1 = (pldb(idp)%hvsti - pldb(idp)%wsyf) *                            &   
                        (wur / (wur + Exp(6.13 - .0883 * wur))) + pldb(idp)%wsyf
            hiad1 = amin1 (hiad1, pldb(idp)%hvsti)
            yield = 0.8 * hlt(ihru)%dm * hiad1  ! * hlt(isd)%stress
            hlt(ihru)%yield = yield / 1000.
            hlt(ihru)%npp = hlt(ihru)%dm / 1000.
            hlt(ihru)%lai_mx = hlt(ihru)%alai
            !compute annual net primary productivity (npp) for perennial non-harvested?
            !use output.mgt print code
            !write() isd, time%day, time%yrc, pldb(iplt)%plantnm, hlt(isd)%alai, hlt(isd)%dm, yield
            hlt(ihru)%gro = "n"
            hlt(ihru)%g = 0.
            hlt(ihru)%alai = 0.
            hlt(ihru)%dm = 0.     !adjust for non-harvested perennials?
            hlt(ihru)%hufh = 0.
            hlt(ihru)%aet = 0.
            hlt(ihru)%pet = 0.
              
          !drainage water management
          case ("drainage") !! set drain depth for drainage water management
            ob_num = d_tbl(id)%act(iac)%ob_num
            if (ob_num == 0) ob_num = ob_cur
            iihru = ob_num
            hru(iihru)%lumv%sdr_dep = d_tbl(id)%act(iac)%const
            if (hru(iihru)%lumv%sdr_dep > 0) then
              do jj = 1, soil(iihru)%nly
                if (hru(iihru)%lumv%sdr_dep < soil(iihru)%phys(jj)%d) hru(iihru)%lumv%ldrain = jj
                if (hru(iihru)%lumv%sdr_dep < soil(iihru)%phys(jj)%d) exit
              end do
            else
                hru(iihru)%lumv%ldrain = 0
            endif 
              
          !land use change
          case ("lu_change")
            !ihru, ilu and isol are in modparm
            ihru = ob_num
            ilu = d_tbl(id)%act_typ(iac)
            hru(ob_num)%dbs%land_use_mgt = ilu
            hru(ob_num)%land_use_mgt_c = d_tbl(id)%act(iac)%file_pointer
            isol = hru(ob_num)%dbs%soil  
            call plant_init (1)
                     
          !channel change
          case ("chan_change")
            ich = ob_num
            !set new cover and name for calibration
            sd_ch(ich)%cov = d_tbl(i)%act(iac)%const
            sd_ch(ich)%order = d_tbl(i)%act(iac)%file_pointer
        
          ! burning
          case ("burn")
            iburn = d_tbl(i)%act_typ(iac)           !burn type from fire data base
            do ipl = 1, pcom(j)%npl
              call pl_burnop (j, ipl, iburn)
            end do
                        
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day, "        ", "    BURN", phubase(j),    &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass, rsd1(j)%tot(ipl)%m,   &
                  sol_sumno3(j), sol_sumsolp(j)
            end if

          
          case ("graze")    !! grazing operation
            ndeat(j) = 0
            igrz(j) = 1
            ipl = 1
            manure_id(j) = d_tbl(id)%act_typ(iac)
            grz_days(j) = d_tbl(i)%act(iac)%const
            bio_eat(j) = grazeop_db(mgt%op1)%eat
            bio_trmp(j) = grazeop_db(mgt%op1)%tramp           
            if (grazeop_db(mgt%op1)%manure <= 0.) then 
              grazeop_db(mgt%op1)%manure = 0.95 * grazeop_db(mgt%op1)%eat
            end if
            manure_kg(j) = grazeop_db(mgt%op1)%manure
            
            if (pco%mgtout == "y") then
              write (2612, *) j, time%yrc, time%mo, time%day, "         ", "    GRAZE",          &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass,        &
                rsd1(j)%tot(ipl)%m, sol_sumno3(j), sol_sumsolp(j), manure_kg(j)
            endif

          !herd management - move the herd
          case ("herd")
            
          !water rights decision to move water
          case ("water_rights")
            
          end select
        end if
      end do

      return
      end subroutine actions