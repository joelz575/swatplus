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

      use jrw_datalib_module
      use basin_module
      
      j = ihru
      
      ! determine which plant in community (%op2)
      if (mgt%op /= 'fert      ') then
      mgt%op2 = 0
      icom = pcom(j)%pcomdb
      if (icom > 0) then
        if (npl(j) > 1) then
          do ipl = 1, npl(j)
            if (mgt%op_plant == pcom_xw(icom)%pl_name(ipl)) then
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
            do ipl = 1, npl(j)
              idp = pcomdb(icom)%pl(ipl)%db_num
              if (mgt%op_plant == pcomdb(icom)%pl(ipl)%cpnm) then
                pcom(j)%plcur(ipl)%gro = 1
                pcom(j)%plcur(ipl)%idorm = 0
              end if
              if (pco%mgtout ==  1) then
                write (143, 1000) j, time%yrc,i_mo,iida,                 &
                pldb(idp)%plantnm, pcomdb(icom)%name, phubase(j),        &
                pcom(j)%plcur(ipl)%phuacc,                               &
                hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd, &
                sol_sumno3(j),                                           &
                sol_sumsolp(j),pcom(j)%plg(ipl)%lai,                     &
                pcom(j)%plg(ipl)%laimx_pop
              end if
            end do
            
          case ("mons")  !! begin and end monsoon initiation period
            !!begin monsoon initiation period
            if (int(mgt%op3) == 1) then
              pcom(j)%mseas = 1
              do ipl = 1, npl(j)
                pcom(j)%plcur(ipl)%monsoon_init = 1
              end do
            else
              pcom(j)%mseas = 0
             do ipl = 1, npl(j)
               if (pcom(j)%plcur(ipl)%monsoon_init == 1) then             
                  pcom(j)%plcur(ipl)%gro = 1
                  pcom(j)%plcur(ipl)%phuacc = 0. 
                  pcom(j)%plcur(ipl)%idorm = 0
                  pcom(j)%plcur(ipl)%monsoon_init = 0
               endif
            end do
            end if
            
          case ("harv")  !! harvest only operation
            ihk = mgt%op1
            ipl = amax1(1, mgt%op2)
            harvop = harvop_db(mgt%op1)
            hi_ovr = harvop%hi_ovr
            harveff = harvop%eff
            if (harveff <= 0.) harveff = 1.0 

            do ipl = 1, npl(j)
            if (mgt%op_plant == pcomdb(icom)%pl(ipl)%cpnm) then
            
            !harvest specific type
            select case (harvop%typ)
            case ('biomass')    
              call mgt_harvestop
            case ('grain')
              call mgt_harvgrainop
            case ('residue')
            case ('tree')
            case ('tuber')
            end select
            
            !! sum yield and num. of harvest to calc ave yields
            pcom(j)%plg(ipl)%yield = pcom(j)%plg(ipl)%yield + yield
            pcom(j)%plcur(ipl)%harv_num = pcom(j)%plcur(ipl)%harv_num+1
            
            idp = pcom(j)%plcur(ipl)%idplt
            if (pco%mgtout ==  1) then
              write (143, 1001) j, time%yrc, i_mo, iida,                 &
              pldb(idp)%plantnm,                                         &
              "HARVEST ONLY",phubase(j),pcom(j)%plcur(ipl)%phuacc,       &
              hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,                      &
              soil(j)%ly(1)%rsd, yield, strsn_sum(j), strsp_sum(j),      &
              strstmp_sum(j), strsw_sum(j), strsa_sum(j)
            end if
            end if
            end do
          
            case ("kill")   !! kill operation
              ihk = mgt%op1
              
              do ipl = 1, npl(j)
                if (mgt%op_char == pcomdb(icom)%pl(ipl)%cpnm) then
                call mgt_killop
  
              if (pco%mgtout ==  1) then 
                write (143, 1000) j, time%yrc,i_mo, iida,                &
                "         ",                                             &
           "    KILL", phubase(j), pcom(j)%plcur(ipl)%phuacc,            &
                hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,                    &
                 soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j)
              end if
            
              phubase(j) = 0.
              pcom(j)%plcur(ipl)%phuacc = 0.
              end if
              end do
       
          case ("hvkl")   !! harvest and kill operation
            if (mgt%op1 <= 0) mgt%op1 = 1
            harvop = harvop_db(mgt%op1)
            hi_ovr = harvop%hi_ovr
            harveff = harvop%eff
            do ipl = 1, npl(j)
              biomass = pcom(j)%plm(ipl)%mass
              if (mgt%op_plant == pcomdb(icom)%pl(ipl)%cpnm) then
                          
              !harvest specific type
              select case (harvop%typ)
              case ('biomass')    
                call mgt_harvestop
              case ('grain')
                !hru(j)%pl_tot(ipl)%mass = 2000.
                call mgt_harvgrainop
              case ('residue')
              case ('tree')
              case ('tuber')
              end select
            
              call mgt_killop
              
              !call mgt_harvkillop
              !! sum yield and num. of harvest to calc ave yields
              pcom(j)%plg(ipl)%yield = pcom(j)%plg(ipl)%yield + yield
              pcom(j)%plcur(ipl)%harv_num=pcom(j)%plcur(ipl)%harv_num+1
            
              idp = pcom(j)%plcur(ipl)%idplt
              if (pco%mgtout == 1) then
                write (143, 1001) j, time%yrc, i_mo,iida,                &
          pldb(idp)%plantnm, "HARV/KILL", phubase(j),                    & 
          pcom(j)%plcur(ipl)%phuacc,hru(j)%sol%sw,biomass,               &
          soil(j)%ly(1)%rsd, sol_sumno3(j),sol_sumsolp(j),yield,         &
          strsn_sum(j), strsp_sum(j), strstmp_sum(j), strsw_sum(j),      &
          strsa_sum(j)
1001  format (4i6,2a15,8f10.2,30x,5f10.2) 
              end if 
              end if
              pcom(j)%plcur(ipl)%phuacc = 0.
            end do
            phubase(j) = 0.
            
          case ("till")   !! tillage operation
            idtill = mgt%op1
            ipl = amax1(1, mgt%op2)
            call mgt_newtillmix(j,0.)
            
            if (pco%mgtout == 1) then
              write (143, 1003) j, time%yrc, i_mo, iida,                 &
              tilldb(idtill)%tillnm,                                     &
              "TILLAGE",phubase(j),pcom(j)%plcur(ipl)%phuacc,            &
              hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,   &
              sol_sumno3(j), sol_sumsolp(j), tilldb(idtill)%effmix
1003  format (4i6,2a15,7f10.2,30x,f10.2)
            end if

          case ("irrm")  !! irrigation operation
            irrop = irrop_db(mgt%op1)
            if (mgt%op3 > 1.e-6) irrop%amt_mm = mgt%op3
            if (irrop%eff < 1.e-6) irrop%eff = 1.0
            call pl_irrigate (j, irrop%amt_mm)
            
            if (pco%mgtout == 1) then
              write (143, 1002) j, time%yrc, i_mo,                      & 
              iida, "        ", "IRRIGATE", phubase(j),                 &
              pcom(j)%plcur(ipl)%phuacc,                                &
              hru(j)%sol%sw,pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,   &
              sol_sumno3(j),                                            &
              sol_sumsolp(j),irramt(j), irr_sc(j), irr_no(j)
1002        format (4i6,2a15,7f10.2,10x,f10.2,70x,2i7)
            end if
          
          case ("rel")    !! release/impound water in rice fields
            imp_trig(j) = mgt%op1
            ipl = amax1(1, mgt%op2)
          
            if (pco%mgtout ==  1) then
              write (143, 1000) j, time%yrc, i_mo,iida,                  & 
       "         ","RELEASE/IMPOUND", phubase(j),                        &
       pcom(j)%plcur(ipl)%phuacc, hru(j)%sol%sw,pcom(j)%plm(ipl)%mass,   &
       soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j)
            end if
          
          case ("fert")   !! fertilizer operation
            ipl = 1
            fertop = fertop_db(mgt%op1)
            if (mgt%op3 > 1.e-6) fertop%amt_kgh = mgt%op3
            if (fertop%surface <= 1.e-6) fertop%surface = 0.2
            call pl_fert
            !call bac_apply_hrucon    THIS SHOULD BE CALLED FROM AN HRU CONTROL MODULE 
            !hru(j)%ly(1)%bacsol(ibac) = sol_bacsol
            !hru(j)%ly(1)%bacsor(ibac) = sol_bacsor
            
            if (pco%mgtout == 1) then
              write (143, 1004) j, time%yrc,i_mo,iida,                  & 
              fertop_db(mgt%op1)%fertnm,                                &
              "    FERT", phubase(j),pcom(j)%plcur(ipl)%phuacc,         &
              hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,  &
              sol_sumno3(j), sol_sumsolp(j), frt_kg, fertno3, fertnh3,  &
              fertorgn, fertsolp, fertorgp
1004  format (4i6,2a15,7f10.2,20x,f10.2,10x,5f10.2)
            endif
 
          case ("pest")   !! pesticide operation
            pestop = pestop_db(mgt%op1)
            if (mgt%op3 > 1.e-6) pestop%amt_kgh = mgt%op3
            
            call pl_apply
            
            if (pco%mgtout == 1) then
              write (143, 1004) j, time%yrc,i_mo,iida,                  & 
              pestdb(ipest)%pestnm, "   PEST", phubase(j),              &
              pcom(j)%plcur(ipl)%phuacc,                                &
              hru(j)%sol%sw,pcom(j)%plm(ipl)%mass,                      &
              soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j),pst_kg
            endif

          case ("graz")    !! grazing operation
            ndeat(j) = 0
            igrz(j) = 1
            ipl = amax1(1, mgt%op2)
            grazeop = grazeop_db(mgt%op1)
            manure_id(j) = mgt%op1
            grz_days(j) = grazeop%days
            bio_eat(j) = grazeop%eat
            bio_trmp(j) = grazeop%tramp           
            if (grazeop%manure <= 0.) then 
              grazeop%manure = 0.95 * grazeop%eat
            end if
            manure_kg(j) = grazeop%manure
          
            if (pco%mgtout ==  1) then
              write (143, 1005) j, time%yrc,i_mo,iida,                   &
              "         ",                                               &
           "   GRAZE", phubase(j), pcom(j)%plcur(ipl)%phuacc,            &
           hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,                         &
           soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j),manure_kg(j)
1005  format (4i6,2a15,7f10.2,20x,f10.2)
            end if

          case ("burn")   !! burning
            burn_frlb = mgt%op3
            ipl = amax1(1, mgt%op2)
            call pl_burnop
            if (pco%mgtout ==  1) then
              write (143, 1000) j, time%yrc, i_mo,iida,                 & 
              "         ",                                              &
           "      BURN", phubase(j),pcom(j)%plcur(ipl)%phuacc,          &
              hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,                     &
              soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j)
            end if

          case ("swep")   !! street sweeping (only if iurban=2)
            ipl = amax1(1, mgt%op2)
            sweepop = sweepop_db(mgt%op1)
            sweepeff = sweepop%eff
            fr_curb = sweepop%fr_curb
            
            if (pco%mgtout ==  1) then
              write (143, 1000) j, time%yrc, i_mo,iida,                 & 
              "         ",                                              &
       "STREET SWEEP",phubase(j),pcom(j)%plcur(ipl)%phuacc,             &
              hru(j)%sol%sw, pcom(j)%plm(ipl)%mass,                     &
              soil(j)%ly(1)%rsd, sol_sumno3(j), sol_sumsolp(j)        
            end if
               
          case ("prtp")    !! print plant community status to output.mgt
            do ipl = 1, pcomdb(icom)%plants_com
              idp = pcom(j)%plcur(ipl)%idplt
              write (143, 1000) j, time%yrc, i_mo,iida,                 & 
              pldb(idp)%plantnm, pcomdb(icom)%name, phubase(j),         &
              pcom(j)%plcur(ipl)%phuacc,                                &
              hru(j)%sol%sw, pcom(j)%plm(ipl)%mass, soil(j)%ly(1)%rsd,  &
              sol_sumno3(j),                                            &
              sol_sumsolp(j), pcom(j)%plg(ipl)%lai,                     &
              pcom(j)%plg(ipl)%laimx_pop
            end do
            
          case ("dwm")    !! set drain depth for drainage water management
            hru(j)%sdr_dep = mgt%op3
            if (hru(j)%sdr_dep > 0) then
              do jj = 1, hru(ihru)%sol%nly
                if (hru(j)%sdr_dep < soil(ihru)%phys(jj)%d) ldrain(ihru) = jj
                if (hru(j)%sdr_dep < soil(ihru)%phys(jj)%d) exit
              end do
            else
               ldrain(ihru) = 0
            endif 

          case ("skip")    !! skip a year
            yr_skip(j) = 1

      end select

      if (mgt%op /= "skip") nop(j) = nop(j) + 1  !don't icrement if skip year
      if (nop(j) > sched(isched)%num_ops) then
        nop(j) = 1
      end if
      
      mgt = sched(isched)%mgt_ops(nop(j))
      
1000  format (4i6,2a15,9f10.2)    
      return

      end subroutine mgt_sched