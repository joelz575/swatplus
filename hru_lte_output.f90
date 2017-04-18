      subroutine hru_lte_output (isd, ihlt_db)

      use time_module
      use basin_module
      
      integer :: isd, ihlt_db
      
!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily
               
!   output_waterbal - SWAT-DEG0140
        hltwb_d(isd)%precip = precip             
        hltwb_d(isd)%snofall = snowfall     
        hltwb_d(isd)%snomlt = snowmelt           
        hltwb_d(isd)%surq_gen = runoff   
        hltwb_d(isd)%latq = flowlat + hlt(isd)%gwflow
        hltwb_d(isd)%wateryld = chflow
        hltwb_d(isd)%perc = perc                
        hltwb_d(isd)%et = aet                   
        hltwb_d(isd)%tloss = 0.                  
        hltwb_d(isd)%eplant = 0.                
        hltwb_d(isd)%esoil = 0.                
        hltwb_d(isd)%surq_cont = 0.
        hltwb_d(isd)%cn = cn_sd
        hltwb_d(isd)%sw = hlt(isd)%sw
        hltwb_d(isd)%snopack = hlt(isd)%snow       
        hltwb_d(isd)%pet = pet             
        hltwb_d(isd)%qtile = flow_tile
        hltwb_d(isd)%irr = air
        
!    output_nutbal - no nutrients currently in SWAT-DEG
!        hltnb_d(isd)%cfertn = 0.   !! cfertn
!        hltnb_d(isd)%cfertp = 0.   !! cfertp
!        hltnb_d(isd)%grazn =  0.   !! grazn
!        hltnb_d(isd)%grazp =  0.   !! grazp
!        hltnb_d(isd)%auton =  0.   !! auton
!        hltnb_d(isd)%autop =  0.   !! autop
!        hltnb_d(isd)%rmp1tl = 0.   !! rmp1tl
!        hltnb_d(isd)%roctl =  0.   !! roctl
!        hltnb_d(isd)%fertn =  0.   !! fertn
!        hltnb_d(isd)%fertp =  0.   !! fertp
!        hltnb_d(isd)%fixn =  0.    !! fixn
!        hltnb_d(isd)%wdntl = 0.    !! wdntl
!        hltnb_d(isd)%hmntl = 0.    !! hmntl
!        hltnb_d(isd)%rwntl = 0.    !! rwntl
!        hltnb_d(isd)%hmptl = 0.    !! hmptl
!        hltnb_d(isd)%rmn2tl = 0.   !! rmn2tl
!        hltnb_d(isd)%rmptl = 0.    !! rmptl
!        hltnb_d(isd)%no3pcp = 0.   !! no3pcp

!    output_losses - SWAT-DEG
        hltls_d(isd)%sedyld = sedin / (100. * hlt_db(ihlt_db)%dakm2) !! sedyld(isd) / hru_ha(isd)
        hltls_d(isd)%sedorgn = 0.   !! sedorgn(isd)
        hltls_d(isd)%sedorgp = 0.   !! sedorgp(isd)
        hltls_d(isd)%surqno3 = 0.   !! surqno3(isd)
        hltls_d(isd)%latno3 = 0.    !! latno3(isd)
        hltls_d(isd)%surqsolp = 0.  !! surqsolp(isd)
        hltls_d(isd)%usle = 0.      !! usle
        hltls_d(isd)%bactp = 0.     !! bactrop + bactsedp
        hltls_d(isd)%bactlp = 0.    !! bactrolp + bactsedlp
        hltls_d(isd)%sedmin = 0.    !! sedminpa(isd) + sedminps(isd)
        hltls_d(isd)%tileno3 = 0.   !! tileno3(isd)
        
!    output_plantweather - SWAT-DEG
        hltpw_d(isd)%lai =  hlt(isd)%alai     !! lai
        hltpw_d(isd)%bioms =  hlt(isd)%dm     !! total biomass
        hltpw_d(isd)%yield =  yield          !! crop yield
        hltpw_d(isd)%residue =  0.           !! residue
        hltpw_d(isd)%sol_tmp =  0.           !! soil(isd)%phys(2))%tmp
        hltpw_d(isd)%strsw = 1. - ws         !! (1.-strsw_av(isd))
        hltpw_d(isd)%strsa = 1. - strsair    !! (1.-strsw_av(isd))
        hltpw_d(isd)%strstmp = 1. - tstress  !! (1.-strstmp_av)
        hltpw_d(isd)%strsn = 0.              !! (1.-strsn_av)        
        hltpw_d(isd)%strsp = 0.              !! (1.-strsp_av)
        hltpw_d(isd)%nplnt = 0.              !! nplnt(isd)
        hltpw_d(isd)%percn = 0.              !! percn(isd)
        hltpw_d(isd)%pplnt = 0.              !! pplnt(isd)
        hltpw_d(isd)%tmx = tmax              !! tmx(isd)
        hltpw_d(isd)%tmn = tmin              !! tmn(isd)
        hltpw_d(isd)%tmpav = tave            !! tmpav(isd)
        hltpw_d(isd)%solrad = raobs          !! hru_ra(isd)
        hltpw_d(isd)%wndspd = wndspd         !! windspeed(isd)
        hltpw_d(isd)%rhum = rhum             !! relative humidity(isd)
        hltpw_d(isd)%phubase0 = wst(iwst)%weat%phubase0     !! base zero potential heat units
        
        hltwb_m(isd) = hltwb_m(isd) + hltwb_d(isd)
        hltnb_m(isd) = hltnb_m(isd) + hltnb_d(isd)
        hltls_m(isd) = hltls_m(isd) + hltls_d(isd) 
        hltpw_m(isd) = hltpw_m(isd) + hltpw_d(isd)

      ! summing hru output for the basin only if it is routed somewhere
      ! or if it is not routed and not in a subbasin
      if (ob(icmd)%src_tot > 0 .or. ob(icmd)%src_tot + ob(icmd)%subs_tot == 0) then
        const = bsn%area_ha / ob(icmd)%area_ha       !only have / operator set up (could * frac_dfn directly)
        bwb_d = bwb_d + hltwb_d(isd) / const
        bnb_d = bnb_d + hltnb_d(isd) / const
        bls_d = bls_d + hltls_d(isd) / const
        bpw_d = bpw_d + hltpw_d(isd) / const
      end if

!!!!! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                        .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%wb_sd%d == 'y') then
            write (2300,100) time%day, time%yrc, isd, hltwb_d(isd)  !! waterbal
              if (pco%csvout == 'y') then 
                write (2304,'(*(G0.3,:","))') time%day, time%yrc, isd, hltwb_d(isd)  !! waterbal
              end if 
          end if
!          if (pco%nb_sd%d == 'y') then
!            write (2420,100) time%day, time%yrc, isd, hltnb_d(isd)  !! nutrient bal
!             if (pco%csvout == 'y') then 
!               write (2424,'(*(G0.3,:","))') time%day, time%yrc, isd, hltnb_d(isd)  !! nutrient bal
!             end if 
!          end if
          if (pco%ls_sd%d == 'y') then
            write (2440,102) time%day, time%yrc, isd, hltls_d(isd)  !! losses
              if (pco%csvout == 'y') then 
                write (2444,'(*(G0.3,:","))') time%day, time%yrc, isd, hltls_d(isd)  !! losses
              endif 
          end if
          if (pco%pw_sd%d == 'y') then
            write (2460,101) time%day, time%yrc, isd, hltpw_d(isd)  !! plant weather 
              if (pco%csvout == 'y') then 
                write (2464,'(*(G0.3,:","))') time%day, time%yrc, isd, hltpw_d(isd)  !! plant weather 
              end if 
          end if
        end if
                                                    
        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hltpw_m(isd) = hltpw_m(isd) // const
          hltwb_m(isd)%cn = hltwb_m(isd)%cn / const 
          hltwb_m(isd)%sw = hltwb_m(isd)%sw / const
          hltwb_y(isd) = hltwb_y(isd) + hltwb_m(isd)
          hltnb_y(isd) = hltnb_y(isd) + hltnb_m(isd)
          hltls_y(isd) = hltls_y(isd) + hltls_m(isd)
          hltpw_y(isd) = hltpw_y(isd) + hltpw_m(isd)
          
          !! monthly print
           if (pco%wb_sd%m == 'y') then
             write (2301,100) time%mo, time%yrc, isd, hltwb_m(isd)
               if (pco%csvout == 'y') then 
                 write (2305,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltwb_m(isd)
               end if 
           end if
!           if (pco%nb_sd%m == 'y') then
!             write (2421,100) time%mo, time%yrc, isd, hltnb_m(isd)
!             if (pco%csvout == 'y') then 
!               write (2425,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltnb_m(isd)
!             end if 
!           end if
           if (pco%ls_sd%m == 'y') then
             write (2441,102) time%mo, time%yrc, isd, hltls_m(isd)
               if (pco%csvout == 'y') then 
                 write (2445,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltls_m(isd)
               end if 
           end if
           if (pco%pw_sd%m == 'y') then
             write (2461,101) time%mo, time%yrc, isd, hltpw_m(isd)
               if (pco%csvout == 'y') then 
                 write (2465,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltpw_m(isd)
               end if 
           end if
          
          hltwb_m(isd) = hwbz
          hltnb_m(isd) = hnbz
          hltpw_m(isd) = hpwz
          hltls_m(isd) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hltpw_y(isd) = hltpw_y(isd) // 12.
          hltwb_y(isd)%cn = hltwb_y(isd)%cn / 12. 
          hltwb_y(isd)%sw = hltwb_y(isd)%sw / 12.
          hltwb_a(isd) = hltwb_a(isd) + hltwb_y(isd)
          hltnb_a(isd) = hltnb_a(isd) + hltnb_y(isd)
          hltls_a(isd) = hltls_a(isd) + hltls_y(isd)
          hltpw_a(isd) = hltpw_a(isd) + hltpw_y(isd)
          
          !! yearly print
           if (time%end_yr == 1 .and. pco%wb_sd%y == 'y') then
             write (2302,100) time%end_yr, time%yrc, isd, hltwb_y(isd)
                if (pco%csvout == 'y') then 
                  write (2306,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltwb_y(isd)
                end if 
           end if
!           if (time%end_yr == 1 .and. pco%nb_sd%y == 'y') then
!             write (2422,100) time%end_yr, time%yrc, isd, hltnb_y(isd)
!             if (pco%csvout == 'y') then 
!               write (2426,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltnb_y(isd)
!             end if 
!           end if
           if (time%end_yr == 1 .and. pco%ls_sd%y == 'y') then
             write (2442,102) time%end_yr, time%yrc, isd, hltls_y(isd)
               if (pco%csvout == 'y') then 
                 write (2446,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltls_y(isd)
               end if 
           end if
           if (time%end_yr == 1 .and. pco%pw_sd%y == 'y') then
             write (2462,101) time%end_yr, time%yrc, isd, hltpw_y(isd)
              if (pco%csvout == 'y') then 
                write (2466,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltpw_y(isd)
              end if 
           end if

        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%wb_sd%a == 'y') then
           hltwb_a(isd) = hltwb_a(isd) / time%yrs_prt
           write (2303,100) time%end_yr, time%yrs, isd, hltwb_a(isd)
           if (pco%csvout == 'y') then 
             write (2307,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltwb_a(isd)
           end if
           hltwb_a(isd) = hwbz
         end if
        
!         if (time%end_sim == 1 .and. pco%nb_sd%a == 'y') then 
!           hltnb_a(isd) = hltnb_a(isd) / time%yrs_prt
!           write (2423,100) time%end_yr, time%yrs, isd, hltnb_a(isd)
!         if (pco%csvout == 'y') then 
!             write (2427,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltnb_a(isd)
!           end if
!         end if
!         hltnb_a(isd) = hnbz       
         
         if (time%end_sim == 1 .and. pco%ls_sd%a == 'y') then
           hltls_a(isd) = hltls_a(isd) / time%yrs_prt  
           write (2443,101) time%end_yr, time%yrs, isd, hltls_a(isd)
           if (pco%csvout == 'y') then 
             write (2447,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltls_a(isd)
           end if
         end if
         hltls_a(isd) = hlsz
        
         if (time%end_sim == 1 .and. pco%pw_sd%a == 'y') then   
           hltpw_a(isd) = hltpw_a(isd) / time%yrs_prt      
           write (2463,102) time%end_yr, time%yrs, isd, hltpw_a(isd)
           if (pco%csvout == 'y') then 
             write (2467,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltpw_a(isd)
           end if
           hltpw_a(isd) = hpwz
         end if
         
      return
     
100   format (2i6,i8,21f12.3)
101   format (2i6,i8,21f12.3)
102   format (2i6,i8,21f12.3)
103   format (2i6,i8,4x,a,5x,f12.3)
 
      end subroutine hru_lte_output