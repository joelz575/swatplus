      subroutine sd_hru_output (isd, isd_db)

      use time_module
      use basin_module
      
      integer :: isd, isd_db
      
!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily
               
!   output_waterbal - SWAT-DEG0140
        sdwb_d(isd)%precip = precip             
        sdwb_d(isd)%snofall = snowfall     
        sdwb_d(isd)%snomlt = snowmelt           
        sdwb_d(isd)%surq_gen = runoff   
        sdwb_d(isd)%latq = flowlat + sd(isd)%gwflow
        sdwb_d(isd)%wateryld = chflow
        sdwb_d(isd)%perc = perc                
        sdwb_d(isd)%et = aet                   
        sdwb_d(isd)%tloss = 0.                  
        sdwb_d(isd)%eplant = 0.                
        sdwb_d(isd)%esoil = 0.                
        sdwb_d(isd)%surq_cont = 0.
        sdwb_d(isd)%cn = cn_sd
        sdwb_d(isd)%sw = sd(isd)%sw             
        sdwb_d(isd)%pet = pet             
        sdwb_d(isd)%qtile = 0.
        sdwb_d(isd)%irr = air
        
!    output_nutbal - no nutrients currently in SWAT-DEG
!        sdnb_d(isd)%cfertn = 0.   !! cfertn
!        sdnb_d(isd)%cfertp = 0.   !! cfertp
!        sdnb_d(isd)%grazn =  0.   !! grazn
!        sdnb_d(isd)%grazp =  0.   !! grazp
!        sdnb_d(isd)%auton =  0.   !! auton
!        sdnb_d(isd)%autop =  0.   !! autop
!        sdnb_d(isd)%rmp1tl = 0.   !! rmp1tl
!        sdnb_d(isd)%roctl =  0.   !! roctl
!        sdnb_d(isd)%fertn =  0.   !! fertn
!        sdnb_d(isd)%fertp =  0.   !! fertp
!        sdnb_d(isd)%fixn =  0.    !! fixn
!        sdnb_d(isd)%wdntl = 0.    !! wdntl
!        sdnb_d(isd)%hmntl = 0.    !! hmntl
!        sdnb_d(isd)%rwntl = 0.    !! rwntl
!        sdnb_d(isd)%hmptl = 0.    !! hmptl
!        sdnb_d(isd)%rmn2tl = 0.   !! rmn2tl
!        sdnb_d(isd)%rmptl = 0.    !! rmptl
!        sdnb_d(isd)%no3pcp = 0.   !! no3pcp

!    output_losses - SWAT-DEG
        sdls_d(isd)%sedyld = sedin /                                    & 
               (100. * sd_db(isd_db)%dakm2) !! sedyld(isd) / hru_ha(isd)
        sdls_d(isd)%sedorgn = 0.   !! sedorgn(isd)
        sdls_d(isd)%sedorgp = 0.   !! sedorgp(isd)
        sdls_d(isd)%surqno3 = 0.   !! surqno3(isd)
        sdls_d(isd)%latno3 = 0.    !! latno3(isd)
        sdls_d(isd)%surqsolp = 0.  !! surqsolp(isd)
        sdls_d(isd)%usle = 0.      !! usle
        sdls_d(isd)%bactp = 0.     !! bactrop + bactsedp
        sdls_d(isd)%bactlp = 0.    !! bactrolp + bactsedlp
        sdls_d(isd)%sedmin = 0.    !! sedminpa(isd) + sedminps(isd)
        sdls_d(isd)%tileno3 = 0.   !! tileno3(isd)
        
!    output_plantweather - SWAT-DEG
        sdpw_d(isd)%lai =  sd(isd)%alai   !! lai
        sdpw_d(isd)%bioms =  sd(isd)%dm   !! total biomass
        sdpw_d(isd)%yield =  0.           !! crop uield
        sdpw_d(isd)%residue =  0.         !! residue
        sdpw_d(isd)%sol_tmp =  0.         !! soil(isd)%phys(2))%tmp
        sdpw_d(isd)%strsw = ws            !! (1.-strsw_av(isd))
        sdpw_d(isd)%strstmp = tstress     !! (1.-strstmp_av)
        sdpw_d(isd)%strsn = 0.            !! (1.-strsn_av)        
        sdpw_d(isd)%strsp = 0.            !! (1.-strsp_av)
        sdpw_d(isd)%nplnt = 0.            !! nplnt(isd)
        sdpw_d(isd)%percn = 0.            !! percn(isd)
        sdpw_d(isd)%pplnt = 0.            !! pplnt(isd)
        sdpw_d(isd)%tmx = tmax            !! tmx(isd)
        sdpw_d(isd)%tmn = tmin            !! tmn(isd)
        sdpw_d(isd)%tmpav = tave          !! tmpav(isd)
        sdpw_d(isd)%solrad = raobs        !! hru_ra(isd)
        sdpw_d(isd)%phubase0 = phubase0   !! base zero potential heat units
        
        sdwb_m(isd) = sdwb_m(isd) + sdwb_d(isd)
        sdnb_m(isd) = sdnb_m(isd) + sdnb_d(isd)
        sdls_m(isd) = sdls_m(isd) + sdls_d(isd) 
        sdpw_m(isd) = sdpw_m(isd) + sdpw_d(isd)

      ! summing hru output for the basin only if it is routed somewhere
      ! or if it is not routed and not in a subbasin
      if (ob(icmd)%src_tot > 0 .or. ob(icmd)%src_tot + ob(icmd)%subs_tot == 0) then
        const = bsn%area_ha / ob(icmd)%area_ha       !only have / operator set up (could * frac_dfn directly)
        bwb_d = bwb_d + sdwb_d(isd) / const
        bnb_d = bnb_d + sdnb_d(isd) / const
        bls_d = bls_d + sdls_d(isd) / const
        bpw_d = bpw_d + sdpw_d(isd) / const
      end if

!!!!! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
          if (pco%wb_sd == 3) then
            write (4100,100) time%day, time%yrc, isd, sdwb_d(isd)  !! waterbal
              if (pco%csvout == 1 .and. pco%wb_sd == 3) then 
                write (4023,'(*(G0.3,:","))') time%day, time%yrc, isd, sdwb_d(isd)  !! waterbal
              end if 
          end if
!          if (pco%nb_sd == 3) then
!            write (4101,100) time%day, time%yrc, isd, sdnb_d(isd)  !! nutrient bal
!             if (pco%csvout == 1 .and. pco%nb_sd == 3) then 
!               write (4025,'(*(G0.3,:","))') time%day, time%yrc, isd, sdnb_d(isd)  !! nutrient bal
!             end if 
!          end if
          if (pco%ls_sd == 3) then
            write (4102,102) time%day, time%yrc, isd, sdls_d(isd)  !! losses
              if (pco%csvout == 1 .and. pco%ls_sd == 3) then 
                write (4027,'(*(G0.3,:","))') time%day, time%yrc, isd, sdls_d(isd)  !! losses
              endif 
          end if
          if (pco%pw_sd == 3) then
            write (4103,101) time%day, time%yrc, isd, sdpw_d(isd)  !! plant weather 
              if (pco%csvout == 1 .and. pco%pw_sd == 3) then 
                write (4029,'(*(G0.3,:","))') time%day, time%yrc, isd, sdpw_d(isd)  !! plant weather 
              end if 
          end if
        end if
                                                    
        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          sdpw_m(isd) = sdpw_m(isd) // const
          sdwb_m(isd)%cn = sdwb_m(isd)%cn / const 
          sdwb_m(isd)%sw = sdwb_m(isd)%sw / const
          sdwb_y(isd) = sdwb_y(isd) + sdwb_m(isd)
          sdnb_y(isd) = sdnb_y(isd) + sdnb_m(isd)
          sdls_y(isd) = sdls_y(isd) + sdls_m(isd)
          sdpw_y(isd) = sdpw_y(isd) + sdpw_m(isd)
          
          !! monthly print
           if (pco%wb_sd == 2) then
             write (4100,100) time%mo, time%yrc, isd, sdwb_m(isd)
               if (pco%csvout == 1 .and. pco%wb_sd == 2) then 
                 write (4023,'(*(G0.3,:","))') time%mo, time%yrc, isd, sdwb_m(isd)
               end if 
           end if
!           if (pco%nb_sd == 2) then
!             write (4101,100) time%mo, time%yrc, isd, sdnb_m(isd)
!             if (pco%csvout == 1 .and. pco%nb_sd == 2) then 
!               write (4025,'(*(G0.3,:","))') time%mo, time%yrc, isd, sdnb_m(isd)
!             end if 
!           end if
           if (pco%ls_sd == 2) then
             write (4102,102) time%mo, time%yrc, isd, sdls_m(isd)
               if (pco%csvout == 1 .and. pco%ls_sd == 2) then 
                 write (4027,'(*(G0.3,:","))') time%mo, time%yrc, isd, sdls_m(isd)
               end if 
           end if
           if (pco%pw_sd == 2) then
             write (4103,101) time%mo, time%yrc, isd, sdpw_m(isd)
               if (pco%csvout == 1 .and. pco%pw_sd == 2) then 
                 write (4029,'(*(G0.3,:","))') time%mo, time%yrc, isd, sdpw_m(isd)
               end if 
           end if
          
          sdwb_m(isd) = hwbz
          sdnb_m(isd) = hnbz
          sdpw_m(isd) = hpwz
          sdls_m(isd) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          sdpw_y(isd) = sdpw_y(isd) // 12.
          sdwb_y(isd)%cn = sdwb_y(isd)%cn / 12. 
          sdwb_y(isd)%sw = sdwb_y(isd)%sw / 12.
          sdwb_a(isd) = sdwb_a(isd) + sdwb_y(isd)
          sdnb_a(isd) = sdnb_a(isd) + sdnb_y(isd)
          sdls_a(isd) = sdls_a(isd) + sdls_y(isd)
          sdpw_a(isd) = sdpw_a(isd) + sdpw_y(isd)
          
          !! yearly print
           if (pco%wb_sd == 1) then
             write (4100,100) time%end_yr, time%yrc, isd, sdwb_y(isd)
                if (pco%csvout == 1 .and. pco%wb_sd == 1) then 
                  write (4023,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, sdwb_y(isd)
                end if 
           end if
!           if (pco%nb_sd == 1) then
!             write (4101,100) time%end_yr, time%yrc, isd, sdnb_y(isd)
!             if (pco%csvout == 1 .and. pco%nb_sd == 1) then 
!               write (4025,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, sdnb_y(isd)
!             end if 
!           end if
           if (pco%ls_sd == 1) then
             write (4102,102) time%end_yr, time%yrc, isd, sdls_y(isd)
               if (pco%csvout == 1 .and. pco%ls_sd == 1) then 
                 write (4027,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, sdls_y(isd)
               end if 
           end if
           if (pco%pw_sd == 1) then
             write (4103,101) time%end_yr, time%yrc, isd, sdpw_y(isd)
              if (pco%csvout == 1 .and. pco%pw_sd == 1) then 
                write (4029,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, sdpw_y(isd)
              end if 
           end if
          
          sdwb_y(isd) = hwbz
          sdnb_y(isd) = hnbz
          sdpw_y(isd) = hpwz
          sdls_y(isd) = hlsz
        end if
        
!!!!! average annual print
         if (time%end_sim == 1) then
           sdwb_a(isd) = sdwb_a(isd) / time%yrs_prt
           write (4104,100) time%end_yr, time%yrs, isd, sdwb_a(isd)
             if (pco%csvout == 1) then 
               write (4124,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, sdwb_a(isd)
             end if 
         end if
        
!         if (time%end_sim == 1) then 
!           sdnb_a(isd) = sdnb_a(isd) / time%yrs_prt
!           write (4105,100) time%end_yr, time%yrs, isd, sdnb_a(isd)
!         end if
        
         if (time%end_sim == 1) then
           sdls_a(isd) = sdls_a(isd) / time%yrs_prt  
           write (4106,101) time%end_yr, time%yrs, isd, sdls_a(isd)
         end if
        
         if (time%end_sim == 1) then   
           sdpw_a(isd) = sdpw_a(isd) / time%yrs_prt      
           write (4107,102) time%end_yr, time%yrs, isd, sdpw_a(isd)
           if (pco%csvout == 1) then 
             write (4030,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, sdpw_a(isd)
           end if  
         end if
         
      return
     
100   format (2i6,i8,18f12.3)
101   format (2i6,i8,18f12.3)
102   format (2i6,i8,18f12.3)
103   format (2i6,i8,4x,a,5x,f12.3)
 
      end subroutine sd_hru_output