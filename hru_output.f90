      subroutine hru_output
      
      use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps
!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily

      j = ihru
        
        !hwb_d(j) = hwbz
        !hnb_d(j) = hnbz
        !hpw_d(j) = hpwz
        !hls_d(j) = hlsz
        
!   output_waterbal
        hwb_d(j)%precip = wst(iwst)%weat%precip
        hwb_d(j)%snofall = snofall
        hwb_d(j)%snomlt = snomlt
        hwb_d(j)%surq_gen = qday
        hwb_d(j)%latq = latq(j)
        !hwb_d(j)%rchrg =  rchrg(j)
        hwb_d(j)%wateryld = qdr(j)
        hwb_d(j)%perc = sepbtm(j)
        hwb_d(j)%et = etday
        hwb_d(j)%tloss = tloss
        hwb_d(j)%eplant = ep_day
        hwb_d(j)%esoil = es_day
        hwb_d(j)%surq_cont = surfq(j)
        hwb_d(j)%cn = cnday(j)
        hwb_d(j)%sw = soil(j)%sw
        hwb_d(j)%pet = pet_day
        hwb_d(j)%qtile = qtile
        hwb_d(j)%irr = aird(j)

!    output_nutbal
        hnb_d(j)%cfertn = cfertn
        hnb_d(j)%cfertp =  cfertp
        hnb_d(j)%grazn = grazn
        hnb_d(j)%grazp = grazp
        hnb_d(j)%auton = auton
        hnb_d(j)%autop = autop
        hnb_d(j)%rmp1tl = rmp1tl
        hnb_d(j)%roctl = roctl
        hnb_d(j)%fertn = fertn
        hnb_d(j)%fertp = fertp
        hnb_d(j)%fixn = fixn
        hnb_d(j)%wdntl = wdntl
        hnb_d(j)%hmntl = hmntl
        hnb_d(j)%rwntl = rwntl
        hnb_d(j)%hmptl = hmptl
        hnb_d(j)%rmn2tl = rmn2tl
        hnb_d(j)%rmptl = rmptl
        hnb_d(j)%no3pcp = no3pcp

!    output_plantweather
        hpw_d(j)%lai = sumlai
        hpw_d(j)%bioms = sumbm
        hpw_d(j)%residue = hru(j)%rsd%mass
        hpw_d(j)%yield = yield
        hpw_d(j)%sol_tmp =  soil(j)%phys(2)%tmp
        hpw_d(j)%strsw = (1.-strsw_av(j))
        hpw_d(j)%strsw = (1.-strsa_av)
        hpw_d(j)%strsn = (1.-strsn_av)        
        hpw_d(j)%strsp = (1.-strsp_av)
        hpw_d(j)%nplnt = nplnt(j)
        hpw_d(j)%percn = percn(j)
        hpw_d(j)%pplnt = pplnt(j)
        hpw_d(j)%tmx = tmx(j)
        hpw_d(j)%tmn = tmn(j)
        hpw_d(j)%tmpav = tmpav(j)
        hpw_d(j)%solrad = hru_ra(j)
        hpw_d(j)%phubase0 = phubase(j)

!    output_losses
        hls_d(j)%sedyld = sedyld(j) / hru(j)%area_ha
        hls_d(j)%sedorgn = sedorgn(j)
        hls_d(j)%sedorgp = sedorgp(j)
        hls_d(j)%surqno3 = surqno3(j)
        hls_d(j)%latno3 = latno3(j)
        hls_d(j)%surqsolp = surqsolp(j)
        hls_d(j)%usle = usle
        hls_d(j)%bactp = bactrop + bactsedp
        hls_d(j)%bactlp = bactrolp + bactsedlp
        hls_d(j)%sedmin = sedminpa(j) + sedminps(j)
        hls_d(j)%tileno3 = tileno3(j)

        hwb_m(j) = hwb_m(j) + hwb_d(j)
        hnb_m(j) = hnb_m(j) + hnb_d(j)
        hls_m(j) = hls_m(j) + hls_d(j) 
        hpw_m(j) = hpw_m(j) + hpw_d(j)

      ! summing hru output for the basin only if it is routed somewhere
      ! or if it is not routed and not in a subbasin
      if (ob(icmd)%src_tot > 0 .or. ob(icmd)%src_tot + ob(icmd)%subs_tot == 0) then
        const = bsn%area_ha / ob(icmd)%area_ha       !only have / operator set up (could * frac_dfn directly)
        bwb_d = bwb_d + hwb_d(j) / const
        bnb_d = bnb_d + hnb_d(j) / const
        bls_d = bls_d + hls_d(j) / const
        bpw_d = bpw_d + hpw_d(j) / const
      end if

!!!!! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
          if (pco%wb_hru == 3) then
            write (4000,100) time%day, time%yrc, j, hwb_d(j)  !! waterbal
             if (pco%csvout == 1 .and. pco%wb_hru == 3) then
               write (4015,'(*(G0.3,:","))') time%day, time%yrc, j, hwb_d(j)  !! waterbal
             end if
          end if
          if (pco%nb_hru == 3) then
            write (4001,100) time%day, time%yrc, j, hnb_d(j)  !! nutrient bal
              if (pco%csvout == 1 .and. pco%nb_hru == 3) then
                write (4017,'(*(G0.3,:","))') time%day, time%yrc, j, hnb_d(j)  !! nutrient bal
              end if
          end if
          if (pco%ls_hru == 3) then
            write (4002,102) time%day, time%yrc, j, hls_d(j)  !! losses
              if (pco%csvout == 1 .and. pco%ls_hru == 3) then
                write (4019,'(*(G0.3,:","))') time%day, time%yrc, j, hls_d(j)  !! losses
              end if
          end if
          if (pco%pw_hru == 3) then
            write (4003,101) time%day, time%yrc, j, hpw_d(j)  !! plant weather 
              if (pco%csvout == 1 .and. pco%pw_hru == 3) then 
                write (4021,'(*(G0.3,:","))') time%day, time%yrc, j, hpw_d(j)  !! plant weather
              end if 
          end if
        end if

        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpw_m(j) = hpw_m(j) // const
          !hwb_m(j) = hwb_m(j) // const
          hwb_m(j)%cn = hwb_m(j)%cn / const 
          hwb_m(j)%sw = hwb_m(j)%sw / const
          hwb_y(j) = hwb_y(j) + hwb_m(j)
          hnb_y(j) = hnb_y(j) + hnb_m(j)
          hls_y(j) = hls_y(j) + hls_m(j)
          hpw_y(j) = hpw_y(j) + hpw_m(j)
          
          !! monthly print
           if (pco%wb_hru == 2) then
             write (4000,100) time%mo, time%yrc, j, hwb_m(j)
               if (pco%csvout == 1 .and. pco%wb_hru == 2) then
                 write (4015,'(*(G0.3,:","))') time%mo, time%yrc, j, hwb_m(j)
               end if
           end if
           if (pco%nb_hru == 2) then
             write (4001,100) time%mo, time%yrc, j, hnb_m(j)
               if (pco%csvout == 1 .and. pco%nb_hru == 2) then
                 write (4017,'(*(G0.3,:","))') time%mo, time%yrc, j, hnb_m(j)
               end if
           end if
           if (pco%ls_hru == 2) then
             write (4002,102) time%mo, time%yrc, j, hls_m(j)
               if (pco%csvout == 1 .and. pco%ls_hru == 2) then 
                 write (4019,'(*(G0.3,:","))') time%mo, time%yrc, j, hls_m(j)
               end if
           end if
           if (pco%pw_hru == 2) then
             write (4003,101) time%mo, time%yrc, j, hpw_m(j)
               if (pco%csvout == 1 .and. pco%pw_hru == 2) then 
                 write (4021,'(*(G0.3,:","))') time%mo, time%yrc, j, hpw_m(j)
               end if 
           end if
          
          hwb_m(j) = hwbz
          hnb_m(j) = hnbz
          hpw_m(j) = hpwz
          hls_m(j) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hpw_y(j) = hpw_y(j) // 12.
          !hwb_y(j) = hwb_y(j) // 12.
          hwb_y(j)%cn = hwb_y(j)%cn / 12. 
          hwb_y(j)%sw = hwb_y(j)%sw / 12.
          hwb_a(j) = hwb_a(j) + hwb_y(j)
          hnb_a(j) = hnb_a(j) + hnb_y(j)
          hls_a(j) = hls_a(j) + hls_y(j)
          hpw_a(j) = hpw_a(j) + hpw_y(j)
          
          !! yearly print
           if (pco%wb_hru == 1) then
             write (4000,100) time%end_yr, time%yrc, j, hwb_y(j)
               if (pco%csvout == 1 .and. pco%wb_hru == 1) then
                 write (4015,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hwb_y(j)
               end if
           end if
           if (pco%nb_hru == 1) then
             write (4001,100) time%end_yr, time%yrc, j, hnb_y(j)
               if (pco%csvout == 1 .and. pco%nb_hru == 1) then
                 write (4017,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hnb_y(j)
               end if
           end if
           if (pco%ls_hru == 1) then
             write (4002,102) time%end_yr, time%yrc, j, hls_y(j)
               if (pco%csvout == 1 .and. pco%ls_hru == 1) then
                 write (4019,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hls_y(j)
               end if
           end if
           if (pco%pw_hru == 1) then
             write (4003,101) time%end_yr, time%yrc, j, hpw_y(j)
               if (pco%csvout == 1 .and. pco%pw_hru == 1) then 
                 write (4021,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hpw_y(j)
               end if 
           end if
          
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%wb_hru > 0) then
           hwb_a(j) = hwb_a(j) / time%yrs_prt
           write (4004,100) time%end_yr, time%yrs, j, hwb_a(j)
           if (pco%csvout == 1) then
             write (4016,100) time%end_yr, time%yrs, j, hwb_a(j)
           end if
           hwb_a(j) = hwbz
         end if
        
         if (time%end_sim == 1 .and. pco%nb_hru > 0) then 
           hnb_a(j) = hnb_a(j) / time%yrs_prt
           write (4005,100) time%end_yr, time%yrs, j, hnb_a(j)
             if (pco%csvout == 1) then 
               write (4018,'(*(G0.3,:","))') time%end_yr, time%yrs, j, hnb_a(j)
             end if
             hnb_a(j) = hnbz
         end if
        
         if (time%end_sim == 1 .and. pco%ls_hru > 0) then
           hls_a(j) = hls_a(j) / time%yrs_prt 
           write (4006,101) time%end_yr, time%yrs, j, hls_a(j)
             if (pco%csvout == 1) then 
               write (4020,'(*(G0.3,:","))') time%end_yr, time%yrs, j, hls_a(j)
             end if
             hls_a(j) = hlsz
         end if
        
         if (time%end_sim == 1 .and. pco%pw_hru > 0) then     
           hpw_a(j) = hpw_a(j) / time%yrs_prt      
           write (4007,102) time%end_yr, time%yrs, j, hpw_a(j)
             if (pco%csvout == 1) then 
               write (4022,'(*(G0.3,:","))') time%end_yr, time%yrs, j, hpw_a(j)
             end if
             hpw_a(j) = hpwz
         end if

         if (time%end_sim == 1) then
           do ipl = 1, npl(j)
             idp = pcom(j)%plcur(ipl)%idplt
             if (pcom(j)%plcur(ipl)%harv_num > 0) then 
               pcom(j)%plg(ipl)%yield = pcom(j)%plg(ipl)%yield /           &
                                         pcom(j)%plcur(ipl)%harv_num
             endif
            write (4008,103) time%end_yr, time%yrs, j,pldb(idp)%plantnm,   &
                                                 pcom(j)%plg(ipl)%yield
            if (pco%csvout == 1) then
              write (4009,'(*(G0.3,:","))') time%end_yr, time%yrs, j,pldb(idp)%plantnm,   &
                                                 pcom(j)%plg(ipl)%yield 
            end if
           end do
         end if
      return
      
100   format (2i6,i8,18f12.3)
101   format (2i6,i8,18f12.3)
102   format (2i6,i8,18f12.3)
103   format (2i6,i8,4x,a,5x,f12.3)
       
      end subroutine hru_output