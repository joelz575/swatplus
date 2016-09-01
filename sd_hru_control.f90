      subroutine sd_hru_control (isd)
      
      use jrw_datalib_module
      use basin_module
      
      integer :: isd
      real :: timeint(1000)
      !real :: ws, ts

      isd_db = ob(icmd)%props
      iwst = ob(icmd)%wst
      iwgn = wst(iwst)%wco%wgn
      iplt = sd(isd)%iplant
      precip = wst(iwst)%weat%precip
      tmax = wst(iwst)%weat%tmax
      tmin = wst(iwst)%weat%tmin
      raobs = wst(iwst)%weat%solrad
      rmx = wst(iwst)%weat%solradmx
      
      tave  = (tmax + tmin) / 2. 
      !calculate base 0 heat units
      if (time%day == 1) phubase0 = 0.
      if (tave > 0.) phubase0 = phubase0 + tave / wgn_pms(iwgn)%phutot
      
      yield = 0.
      ws = 0.
      strsair = 0.
      tstress = 0.
      snowfall = 0.
      snowmelt = 0.
          IF (tave .lt.0.) THEN 
            ! IF ave temp < 0  compute snowfall    
            snowfall = precip 
            sd(isd)%snow = sd(isd)%snow + precip 
            runoff = 0. 
          ELSE
            snowfall = 0.     
            ! IF ave temp > 0  compute runoff                             
            snowmelt = 4.57 * tave  
            IF (snowmelt > sd(isd)%snow) THEN 
              snowmelt = sd(isd)%snow 
              sd(isd)%snow = 0.
            ELSE
              sd(isd)%snow = sd(isd)%snow - snowmelt
            END IF 
            
            xx = sd(isd)%wrt1 - sd(isd)%wrt2 * sd(isd)%sw
            if (xx < -20.) xx = -20.
            if (xx > 20.) xx = 20.
            if ((sd(isd)%sw + Exp(xx)) > 0.001) then
              r2 = sd(isd)%smx * (1. - sd(isd)%sw / (sd(isd)%sw + Exp(xx)))
            end if
            r2 = amax1(3.,r2)
            cn_sd = 25400. / (r2 + 254.)
            precipeff = precip + snowmelt
            xx = precipeff - a1 * r2 
            IF (xx.gt.0.) THEN 
              runoff = xx ** 2 / (precipeff + a2 * r2) 
            ELSE 
              runoff = 0. 
            END IF 
            sd(isd)%sw = sd(isd)%sw + (precipeff - runoff) 
          END IF 
                                                                        
          xxi = 30. * time%mo - 15. 
          xsd = .4102 * SIN((xxi-80.25)/58.13) 
          ch = -sd(isd)%yls * tan(xsd) / sd(isd)%ylc 
          IF (ch.lt.1.) THEN 
            IF (ch.le.-1.) THEN 
              h = 3.1415 
            ELSE 
              h = acos(ch) 
            END IF 
          ELSE 
            h = 0. 
          END IF 
          
          IF (sd_db(isd_db)%ipet .eq. 0) THEN
            ! compute potential et with Hargrove Method
            ramm = rmx / (2.5 - .0022 * tave )
            pet = .0032 * ramm * (tave +17.8) * (tmax - tmin) ** .6
          ELSE
            ! compute potential et with Preistley-Taylor Method
            tk = tave  + 273.
            alb = .23
            d = EXP(21.255-5304./tk) * 5304. / tk ** 2
            gma = d / (d +.68)
            ho = 23.9 * raobs * (1.-alb) / 58.3
            aph = 1.28
            pet = aph * ho * gma
          END IF
          pet = sd(isd)%etco * pet
!
!         compute actual et
!
          xx = 1. - sd(isd)%sw / sd(isd)%awc
          IF (xx.lt.0.0001) xx = 0.0001 
          aet = pet * EXP(-xx) 
                                                                        
!                                                                       
!         compute plant growth - boimass and leaf area                  
!         b1=et adjustment factor b1=1 during growing season b1=.6 IF no
!               
!         begin growth for tropical plants - Strauch, Volk, et al.
          if (sd_db(isd_db)%tropical == 1) then
            if (time%day == sd_db(isd_db)%igrow1) sd(isd)%igro = 0
            if (time%day >= sd_db(isd_db)%igrow1 .and. time%day <= sd_db(isd_db)%igrow2) then
              if (sd(isd)%igro == 0 .and. sd(isd)%sw > pldb(iplt)%frsw_gro * sd(isd)%awc) then
                sd(isd)%igro = 1
                sd(isd)%g = 0.
                sd(isd)%alai = 0.
                sd(isd)%dm = 0.
                sd(isd)%hufh = 0.
              end if
              if (sd(isd)%igro == 0 .and. time%day == sd_db(isd_db)%igrow2) then
                ! calc yield, print max lai, dm and yield
                if (pco%mgtout == 1) then
                  yield = sd(isd)%dm * pldb(iplt)%hvsti
                  write (4700,*) isd, time%day, time%yrc, pldb(iplt)%plantnm,    &
                     sd(isd)%alai, sd(isd)%dm, yield
                  if (pco%csvout == 1 .and. pco%mgtout == 1) then
                    write (4701,'(*(G0.3,:","))') isd, time%day, time%yrc, pldb(iplt)%plantnm,    &
                      sd(isd)%alai, sd(isd)%dm, yield 
                  end if
                end if

                sd(isd)%igro = 1
                sd(isd)%g = 0.
                sd(isd)%alai = 0.
                sd(isd)%dm = 0.
                sd(isd)%hufh = 0.
              end if
            end if
          ELSE
                
!         begin growth for non-tropical plants
          IF (time%day == sd_db(isd_db)%igrow1) then
            sd(isd)%igro = 1
            sd(isd)%g = 0.
            sd(isd)%alai = 0.
            sd(isd)%dm = 0.
            sd(isd)%hufh = 0.
          END IF
          
!         end growth

          IF (time%day == sd_db(isd_db)%igrow2) then
            !calculate yield - print lai, biomass and yield - add stress to yield?
            yield = sd(isd)%dm * pldb(iplt)%hvsti  ! * sd_db(isd_db)%stress
            
            !compute annual net primary productivity (npp) for perennial non-harvested?
            !use output.mgt print code
            !write() isd, time%day, time%yrc, pldb(iplt)%plantnm, sd(isd)%alai, sd(isd)%dm, yield
            sd(isd)%igro = 0
            sd(isd)%g = 0.
            sd(isd)%alai = 0.
            sd(isd)%dm = 0.     !adjust for non-harvested perennials?
            sd(isd)%hufh = 0.
          END IF
                    
         ! calc yield, print max lai, dm and yield
          if (pco%mgtout == 1) then
            write (4700,*) isd, time%day, time%yrc, pldb(iplt)%plantnm, sd(isd)%alai, sd(isd)%dm, yield
            if (pco%csvout == 1 .and. pco%mgtout == 1) then
              write (4701,*) isd, time%day, time%yrc, pldb(iplt)%plantnm, sd(isd)%alai, sd(isd)%dm, yield
            end if
          end if
          
        END IF
           
          b1 = sd(isd)%etco - .4        !evap coef ranges from .4-.8 when etco ranges from .8-1.2
          IF (sd(isd)%igro == 1) THEN
            b1 = sd(isd)%etco
            delg=(tave - pldb(iplt)%t_base) / sd(isd)%phu 
            IF (delg.lt.0.) THEN 
              delg = 0. 
            END IF 
            sd(isd)%g = sd(isd)%g + delg 
            parad = .5 * raobs * (1.-EXP(-.65 * (sd(isd)%alai + .05))) 
            drymat = parad * pldb(iplt)%bio_e * sd_db(isd_db)%stress
            biomass = biomass + drymat
            ws = aet / pet
            
            !compute aeration stress
            if (sd(isd)%sw .gt. sd(isd)%awc) THEN 
              satco = (sd(isd)%sw - sd(isd)%awc) / (sd(isd)%por - sd(isd)%awc) 
              pl_aerfac = .85
              scparm = 100. * (satco - pl_aerfac) / (1.0001 - pl_aerfac)
              if (scparm > 0.) then
                strsair = 1. - (scparm / (scparm + Exp(2.9014 - .03867 * scparm)))
              else
                strsair = 1.
              end if
            end if
                                                                        
            !irrigate IF water stress is < 0.7                             
                                                                        
            IF (sd_db(isd_db)%irr.gt.0) THEN 
              IF (ws.lt.0.7) THEN 
                air = sd(isd)%awc - sd(isd)%sw 
                IF (sd_db(isd_db)%irrsrc.eq.1) THEN 
                  sd(isd)%gw = sd(isd)%gw - air 
                  IF (sd(isd)%gw.lt.0.) THEN 
                    air = air + sd(isd)%gw 
                    sd(isd)%gw = 0. 
                  END IF 
                ELSE 
                  sd(isd)%gwdeep = sd(isd)%gwdeep - air 
                  IF (sd(isd)%gwdeep.lt.0.) THEN 
                    air = air + sd(isd)%gwdeep 
                    sd(isd)%gwdeep = 0. 
                  END IF 
                END IF 
              END IF 
            END IF                                  
                                                                  
            if (tave .gt.pldb(iplt)%t_base) THEN
              tgx = 2. * pldb(iplt)%t_opt - pldb(iplt)%t_base - tave
              rto = ((pldb(iplt)%t_opt - tave ) /(tgx + 1.e-6))**2
              IF (rto.le.200.) THEN 
                tstress = EXP(-0.1054*rto) 
              ELSE 
                tstress = 0. 
              END IF
            ELSE
              tstress = 0. 
            END IF 
                                                        
            reg = amin1(ws,tstress,strsair) 
            sd(isd)%dm = sd(isd)%dm + reg * drymat 
            f = sd(isd)%g / (sd(isd)%g + EXP(plcp(iplt)%leaf1 - plcp(iplt)%leaf2 * sd(isd)%g))
            ff = f - sd(isd)%hufh 
            sd(isd)%hufh = f 
            deltalai = ff * pldb(iplt)%blai * (1.0 - EXP(5.0 *(sd(isd)%alai - pldb(iplt)%blai))) * sqrt(reg)
            sd(isd)%alai = sd(isd)%alai + deltalai 
          END IF
                                                                  
!         adjust actual et for growing season                           
          aet = b1 * aet 
          
          !compute lateral soil flow
          sw_excess = sd(isd)%sw - sd(isd)%awc
          if (sw_excess > 0.) then
            swf = (sd(isd)%sw - sd(isd)%awc) / (sd(isd)%por - sd(isd)%awc) 
            flowlat = .024 * swf * sd(isd)%sc * sd_db(isd_db)%slope / sd_db(isd_db)%slopelen
            flowlat = amin1(sd(isd)%sw, flowlat)
            sd(isd)%sw = sd(isd)%sw - flowlat
          else
            flowlat = 0.
          end if
        
          !compute tile flow
          sw_excess = sd(isd)%sw - sd(isd)%awc
          if (sw_excess > 0. .and. sd(isd)%tdrain > 0.) then
            flow_tile = sw_excess * (1. - Exp(-24. / sd(isd)%tdrain))
            flow_tile = amin1(flow_tile, 10.)     !assume a drainage coefficient of 12.5 mm
          else
            flow_tile = 0.
          end if
          flow_tile = amin1(sd(isd)%sw, flow_tile)
          sd(isd)%sw = sd(isd)%sw - flow_tile
          
          !compute percolation from bottom of soil profile
          sw_excess = sd(isd)%sw - sd(isd)%awc * sd(isd)%perco
          if (sw_excess > 0.) then
            perc = sw_excess * (1. - Exp(-24. / sd(isd)%hk))
          else
            perc = 0.
          end if
          if (perc.lt.0.) perc = 0.
          perc = amin1(sd(isd)%sw, perc)
          sd(isd)%sw = sd(isd)%sw - perc
          !limit perc for depth to impermeable layer
          !xx = (sd(isd)%dep_imp - sd_db(isd_db)%soildep) / 1000.
          !if (xx < 1.e-4) then
          !  perc = 0.
          !else
          !  perc = perc * xx / (xx + Exp(8.833 - 2.598 * xx))
          !end if
                                                                        
          aet = amin1(sd(isd)%sw, aet)
          sd(isd)%sw = sd(isd)%sw - aet 
                                                                        
          sd(isd)%gw = sd(isd)%gw + perc 
          revap = aet * sd_db(isd_db)%revapc 
          percdeep = perc * sd_db(isd_db)%percc
          sd(isd)%gwflow = sd(isd)%gwflow * sd_db(isd_db)%abf + perc * (1. - sd_db(isd_db)%abf)
          sd(isd)%gwflow = amin1(sd(isd)%gwflow, sd(isd)%gw)
          sd(isd)%gw = sd(isd)%gw - sd(isd)%gwflow
                                                                        
          revap = amin1(revap, sd(isd)%gw)
          sd(isd)%gw = sd(isd)%gw - revap
                                                                        
          percdeep = amin1(percdeep, sd(isd)%gw)
          sd(isd)%gw = sd(isd)%gw - percdeep
                                                                        
          sd(isd)%gwdeep = sd(isd)%gwdeep + percdeep
                                                                        
          chflow = runoff + flowlat + flow_tile + sd(isd)%gwflow

!!        compute channel peak rate using SCS triangular unit hydrograph
          chflow_m3 = 1000. * chflow * sd_db(isd_db)%dakm2
	    runoff_m3 = 1000. * runoff * sd_db(isd_db)%dakm2
	    bf_m3 = 1000. * (flowlat + sd(isd)%gwflow)*sd_db(isd_db)%dakm2
          peakr = 2. * runoff_m3 / (1.5 * sd_db(isd_db)%tc)
	    peakrbf = bf_m3 / 86400.
          peakr = (peakr + peakrbf)     !* prf     
          
!!        compute sediment yield with MUSLE
          sedin = (runoff * peakr * 1000. * sd_db(isd_db)%dakm2) ** .56 * sd(isd)%uslefac 
	    !! add subsurf sediment - t=ppm*mm*km2/1000.
	    qssubconc = 500.
	    qssub = qssubconc * (flowlat + sd(isd)%gwflow) * sd_db(isd_db)%dakm2 / 1000.
	    sedin = sedin + qssub

          cnv = sd_db(isd_db)%dakm2 * 1000.
          
         !! set values for outflow hydrograph
         !! storage locations set to zero are not currently used
         ob(icmd)%peakrate = peakr
         ob(icmd)%hd(1)%temp = 5. + .75 * tave        !!wtmp
         ob(icmd)%hd(1)%flo = chflow * cnv            !!qdr m3/d
         ob(icmd)%hd(1)%sed = sedin                   !!sedyld
         ob(icmd)%hd(1)%orgn = 0.
         ob(icmd)%hd(1)%sedp = 0.
         ob(icmd)%hd(1)%no3 = 0.
         ob(icmd)%hd(1)%solp = 0.
         ob(icmd)%hd(1)%chla = 0.
         ob(icmd)%hd(1)%nh3 = 0.                         !! NH3
         ob(icmd)%hd(1)%no2 = 0.                         !! NO2
         ob(icmd)%hd(1)%cbod = 0.
         ob(icmd)%hd(1)%dox = 0.
         if (ob(icmd)%hd(1)%flo > .1) then
          ob(icmd)%hd(1)%bacp = 0.
          ob(icmd)%hd(1)%baclp = 0.
         end if
         ob(icmd)%hd(1)%met1 = 0.                            !! cmetal #1
         ob(icmd)%hd(1)%met2 = 0.                            !! cmetal #2
         ob(icmd)%hd(1)%met3 = 0.                            !! cmetal #3
         ob(icmd)%hd(1)%san = 0.                             !! det sand
         ob(icmd)%hd(1)%sil = 0.                             !! det silt
         ob(icmd)%hd(1)%cla = 0.                             !! det clay
         ob(icmd)%hd(1)%sag = 0.                             !! det sml ag
         ob(icmd)%hd(1)%lag = 0.                             !! det lrg ag
         
         !! set values for recharge hydrograph
         !ob(icmd)%hd(2)%flo = perc
         
         if (time%yrs > pco%nyskip) then
           call sd_hru_output (isd, isd_db)
         end if
         
       return
 
      end subroutine sd_hru_control