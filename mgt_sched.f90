      subroutine mgt_sched (isched)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                |none          | 

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module, only : mgt, pcomdb, pldb, harvop_db, tilldb, chemapp_db, grazeop_db,  &
         sweepop_db, sweepop, sched
      use basin_module
      use hydrograph_module
      use parm, only : pcom, soil, hru, ihru, imp_trig, phubase, ndeat, igrz, manure_id, grz_days, bio_trmp, bio_min,   &
        manure_kg, yr_skip, nop, bio_eat, sol_sumno3, sol_sumsolp, strsn_sum, strsp_sum, strstmp_sum, strsw_sum,        &
        strsa_sum, irramt, irr_sc, irr_no, fertnh3, fertno3, fertorgn, fertorgp, fertsolp, idp, ipl, sweepeff,          &
        yr_skip, yield
      use time_module
      use constituent_mass_module
      
      j = ihru
      
      ! determine which plant in community (%op2)
      if (mgt%op /= 'fert      ') then
      mgt%op2 = 0
      icom = pcom(j)%pcomdb
      if (icom > 0) then
        if (pcom(j)%npl > 1) then
          do ipl = 1, pcom(j)%npl
            if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm) then
                mgt%op2 = ipl
                exit
            end if
          end do
        end if
      end if
      end if
         
      select case (mgt%op)

          case ("plnt")    !! plant one plant or entire community
            icom = pcom(j)%pcomdb
            do ipl = 1, pcom(j)%npl
              idp = pcomdb(icom)%pl(ipl)%db_num
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm) then
                pcom(j)%plcur(ipl)%gro = 1
                pcom(j)%plcur(ipl)%idorm = 0
                if (pco%mgtout ==  'y') then
                  write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,  "PLANT ",             &
                      phubase(ihru), pcom(ihru)%plcur(ipl)%phuacc,  soil(ihru)%sw,                          &
                      pcom(ihru)%plm(ipl)%mass, soil(ihru)%ly(1)%rsd, sol_sumno3(ihru),                     &
                      sol_sumsolp(ihru),pcom(ihru)%plg(ipl)%lai, pcom(ihru)%plcur(ipl)%laimx_pop
                end if
              end if
            end do
            
          case ("mons")  !! begin and end monsoon initiation period
            !!begin monsoon initiation period
            if (int(mgt%op3) == 1) then
              pcom(j)%mseas = 1
              do ipl = 1, pcom(j)%npl
                pcom(j)%plcur(ipl)%monsoon_init = 1
              end do
            else
              pcom(j)%mseas = 0
             do ipl = 1, pcom(j)%npl
               if (pcom(j)%plcur(ipl)%monsoon_init == 1) then             
                  pcom(j)%plcur(ipl)%gro = 1
                  pcom(j)%plcur(ipl)%phuacc = 0. 
                  pcom(j)%plcur(ipl)%idorm = 0
                  pcom(j)%plcur(ipl)%monsoon_init = 0
               endif
            end do
            end if
            
          case ("harv")  !! harvest only operation
            iharvop = mgt%op1

            do ipl = 1, pcom(j)%npl
              biomass = pcom(j)%plm(ipl)%mass
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm .or. mgt%op_char == 'all') then
                          
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ('biomass')    
                  call mgt_harvestop (j, ipl, iharvop)
                case ('grain')
                  call mgt_harvgrainop (j, ipl, iharvop)
                case ('residue')
                case ('tree')
                case ('tuber')
                  call mgt_harvgrainop (j, ipl, iharvop)
                case ('peanuts')
                  call mgt_harvgrainop (j, ipl, iharvop)
                case ('stripper')
                  call mgt_harvgrainop (j, ipl, iharvop)
                case ('picker')
                  call mgt_harvgrainop (j, ipl, iharvop)
                end select

                !! sum yield and number of harvest to calc ave yields
                pcom(j)%plcur(ipl)%yield = pcom(j)%plcur(ipl)%yield + yield
                pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num + 1
            
                idp = pcom(j)%plcur(ipl)%idplt
                if (pco%mgtout == 'y') then
                  write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "HARVEST ",         &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, soil(j)%ly(1)%rsd,    &
                      sol_sumno3(j), sol_sumsolp(j), yield, strsn_sum(j), strsp_sum(j), strstmp_sum(j), &
                      strsw_sum(j), strsa_sum(j)
                end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
            end do
          
            case ("kill")   !! kill operation
              do ipl = 1, pcom(j)%npl
                biomass = pcom(j)%plm(ipl)%mass
                if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm .or. mgt%op_char == 'all') then
                  call mgt_killop (j, ipl)
  
                  idp = pcom(j)%plcur(ipl)%idplt
                  if (pco%mgtout == 'y') then
                    write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "HARVEST ",       &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, soil(j)%ly(1)%rsd,    &
                      sol_sumno3(j), sol_sumsolp(j), yield, strsn_sum(j), strsp_sum(j), strstmp_sum(j), &
                      strsw_sum(j), strsa_sum(j)
                  end if 
                end if
                pcom(j)%plcur(ipl)%phuacc = 0.
              end do
       
          case ("hvkl")   !! harvest and kill operation
            iharvop = mgt%op1

            do ipl = 1, pcom(j)%npl
              biomass = pcom(j)%plm(ipl)%mass
              if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm .or. mgt%op_char == 'all') then
                          
                !harvest specific type
                select case (harvop_db(iharvop)%typ)
                case ('biomass')    
                  call mgt_harvestop (j, ipl, iharvop)
                case ('grain')
                  call mgt_harvgrainop (j, ipl, iharvop)
                case ('residue')
                case ('tree')
                case ('tuber')
                end select
            
                call mgt_killop (j, ipl)

                !! sum yield and num. of harvest to calc ave yields
                pcom(j)%plcur(ipl)%yield = pcom(j)%plcur(ipl)%yield + yield
                pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num + 1
            
                idp = pcom(j)%plcur(ipl)%idplt
                if (pco%mgtout == 'y') then
                  write (2612, *) j, time%yrc, time%mo, time%day_mo,  pldb(idp)%plantnm, "HARV/KILL ",      &
                      phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, biomass, soil(j)%ly(1)%rsd,    &
                      sol_sumno3(j), sol_sumsolp(j), yield, strsn_sum(j), strsp_sum(j), strstmp_sum(j), &
                      strsw_sum(j), strsa_sum(j)
                end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
              phubase(j) = 0.
            end do
          
          case ("till")   !! tillage operation
            idtill = mgt%op1
            ipl = Max(1, mgt%op2)
            call mgt_newtillmix(j, 0., idtill)
            
            if (pco%mgtout == 'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, tilldb(idtill)%tillnm, "TILLAGE ",     &
                  phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pcom(j)%plm(ipl)%mass,         &
                  soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j), tilldb(idtill)%effmix
            end if

          case ("irrm")  !! irrigation operation
            ipl = 1
            amt_mm = mgt%op3
            irrop = mgt%op1     !! from irrop_db - ie: drip, sprinkler, etc - xwalk in read_mgtops
            call pl_irrigate (j, amt_mm, irrop)
            
            if (pco%mgtout == 'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "IRRIGATE ", phubase(j),   &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,   &
                  sol_sumno3(j), sol_sumsolp(j),irramt(j), irr_sc(j), irr_no(j)
            end if

          case ("rel")    !! release/impound water in rice fields
            imp_trig(j) = mgt%op1
            ipl = Max(1, mgt%op2)
          
            if (pco%mgtout ==  'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "         ","RELEASE/IMPOUND ", phubase(j),    &
                pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass,                                &
                soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j)
            end if
          
          case ("fert")   !! fertilizer operation
            ipl = 1
            ifrt = mgt%op1                          !fertilizer type from fert data base
            frt_kg = mgt%op3                        !amount applied in kg/ha
            ifertop = mgt%op4                       !surface application fraction from chem app data base
            call pl_fert (j, ifrt, frt_kg, ifertop)

            if (pco%mgtout == 'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, chemapp_db(mgt%op4)%name, "    FERT ", &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw, pcom(j)%plm(ipl)%mass,           &
                soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,         &
                fertorgn, fertsolp, fertorgp
            endif
 
          case ("pest")   !! pesticide operation
            !xwalk application in the mgt file with the pest community
            iob = sp_ob%hru + ihru - 1
            do ipestcom = 1, obcs(iob)%num_pests
              if (obcs(iob)%pests(ipestcom) == mgt%op_char) then
                mgt%op1 = ipestcom
              end if
            end do

            ipl = 1
            ipest = mgt%op1             !sequential pesticide type from pest community
            pest_kg = mgt%op3           !amount applied in kg/ha
            ipestop = mgt%op4            !surface application fraction from chem app data base
            call pl_apply (j, ipest, pest_kg, ipestop)
            
            if (pco%mgtout == 'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, chemapp_db(mgt%op4)%name, "    FERT ", &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass,            &
                soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j), pst_kg
            endif

          case ("graz")    !! grazing operation
            ndeat(j) = 0
            igrz(j) = 1
            ipl = Max(1, mgt%op2)
            manure_id(j) = mgt%op1
            grz_days(j) = mgt%op3
            bio_eat(j) = grazeop_db(mgt%op1)%eat
            bio_trmp(j) = grazeop_db(mgt%op1)%tramp
            bio_min(j) = grazeop_db(mgt%op1)%biomin
            !if (grazeop_db(mgt%op1)%manure <= 0.) then 
            !  grazeop_db(mgt%op1)%manure = 0.95 * grazeop_db(mgt%op1)%eat
            !end if
            manure_kg(j) = grazeop_db(mgt%op1)%manure
            
            if (pco%mgtout == 'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "         ", "    GRAZE ",         &
                phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass,        &
                soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j), manure_kg(j)
            endif

          case ("burn")   !! burning
            iburn = mgt%op1                 !burn type from fire data base
            do ipl = 1, pcom(j)%npl
              call pl_burnop (j, ipl, iburn)
            end do
                        
            if (pco%mgtout == 'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "    BURN ", phubase(j),   &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,   &
                  sol_sumno3(j), sol_sumsolp(j)
            end if

          case ("swep")   !! street sweeping (only if iurban=2)
            ipl = Max(1, mgt%op2)
            sweepop = sweepop_db(mgt%op1)
            sweepeff = sweepop%eff
            fr_curb = sweepop%fr_curb
                  
            if (pco%mgtout == 'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "STREET SWEEP ", phubase(j),    &
                  pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,       &
                  sol_sumno3(j), sol_sumsolp(j)
            end if

          case ("prtp")    !! print plant community status to output.mgt
            do ipl = 1, pcomdb(icom)%plants_com
              idp = pcom(j)%plcur(ipl)%idplt
              write (2612, *) j, time%yrc, time%mo, time%day_mo, "        ", "STREET SWEEP ", phubase(j),        &
                  pldb(idp)%plantnm, phubase(j), pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,  &
                  pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j),                  &
                  pcom(j)%plg(ipl)%lai, pcom(j)%plcur(ipl)%laimx_pop
            end do
            
          case ("dwm")    !! set drain depth for drainage water management
            hru(j)%lumv%sdr_dep = mgt%op3
            if (hru(j)%lumv%sdr_dep > 0) then
              do jj = 1, soil(ihru)%nly
                if (hru(j)%lumv%sdr_dep < soil(ihru)%phys(jj)%d) hru(ihru)%lumv%ldrain = jj
                if (hru(j)%lumv%sdr_dep < soil(ihru)%phys(jj)%d) exit
              end do
            else
                hru(ihru)%lumv%ldrain = 0
            endif 
            !! added below changed plcur(ipl) to plcur(j) and plm(ipl) to plm(j) gsm 1/30/2018
            if (pco%mgtout ==  'y') then
              write (2612, *) j, time%yrc, time%mo, time%day_mo, pldb(idp)%plantnm,  "DRAINAGE_MGT ",          &
                   phubase(ihru), pcom(ihru)%plcur(j)%phuacc,  soil(ihru)%sw,                          &
                   pcom(ihru)%plm(j)%mass, soil(ihru)%ly(1)%rsd, sol_sumno3(ihru),                     &
                   sol_sumsolp(ihru),hru(j)%lumv%sdr_dep
            endif

          case ("skip")    !! skip a year
            yr_skip(j) = 1

      end select

      if (mgt%op /= "skip") nop(j) = nop(j) + 1  !don't icrement if skip year
      if (nop(j) > sched(isched)%num_ops) then
        nop(j) = 1
      end if
      
      mgt = sched(isched)%mgt_ops(nop(j))
   
      return

      end subroutine mgt_sched