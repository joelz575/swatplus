      subroutine sd_channel_control
      
      ich = isdch
      isd_db = ob(icmd)%props
      
      !! set incoming flow and sediment
      chflow_m3 = ob(icmd)%hin%flo
      if (chflow_m3 < 1.e-6) then
        ob(icmd)%hd(1) = hz
      
      else
      sedin = ob(icmd)%hin%sed
      hyd_rad = 0.
      timeint = 0.
      
      !! compute headcut
      !! adjust peak rate for headcut advance -also adjusts CEAP gully from
      !! edge-of-field to trib (assuming rectangular shape and constant tc)
      pr_ratio = (sd_ch(ich)%chl - sd_ch(ich)%hc_len / 1000.) /           &
                                                        sd_ch(ich)%chl
      pr_ratio = amax1(pr_ratio, 0.)
      !assume triangular hydrograph
      peakrate = 2. * chflow_m3 / (1.5 * sd_chd(ich)%tc)
      peakrate = peakrate / 60.   !convert min to sec
      attack = peakrate * sd_chd(ich)%hc_hgt
      hc_adv = sd_ch(ich)%hc_co * (attack - sd_ch(ich)%attack0)
      hc_adv = amax1(hc_adv, 0.)
      hc_adv = amin1(hc_adv, sd_ch(ich)%chl*1000.)
      sd_ch(ich)%hc_len = sd_ch(ich)%hc_len + hc_adv / 1000.
      !! compute sediment yield from headcut- assume bd = 1.2 t/m3
      !! assume channel dimensions are same as data file
      hc_sed = hc_adv * sd_chd(ich)%chw * sd_chd(ich)%chd * 1.2
      
         !! compute changes in channel dimensions
          chside = sd_chd(ich)%chss
          b = sd_ch(ich)%chw
          sd_ch(ich)%phi(6) = b
          sd_ch(ich)%phi(7) = sd_ch(ich)%chd

          !! compute flow and travel time at bankfull depth
          p = b + 2. * sd_ch(ich)%chd * Sqrt(chside * chside + 1.)
          a = b * sd_ch(ich)%chd + chside * sd_ch(ich)%chd *            &           
              sd_ch(ich)%chd
          rh = a / p
          sd_ch(ich)%phi(1) = a
          sd_ch(ich)%phi(5) = Qman(a,rh,sd_chd(ich)%chn,sd_ch(ich)%chs)
  
          IF (peakrate > sd_ch(ich)%phi(5)) THEN
            !! OVERBANK FLOOD
            sd_ch(ich)%overbank = "ob"
            rcharea = sd_ch(ich)%phi(1)
            rchdep = sd_ch(ich)%chd
            !calculate hydraulic radius at hydrograph time increments for degredation
            sdti = 0.
            rchdep = 0.
            valint = 1. / float(maxint)
            ivalint = 1
            tbase = 1.5 * sd_chd(ich)%tc * 60.   !hydrograph base time in s
            tb_pr = tbase
            DO WHILE (sdti < peakrate)
              rchdep = rchdep + 0.01
              rcharea = (sd_ch(ich)%phi(6) + chside * rchdep) * rchdep
              p=sd_ch(ich)%phi(6)+2. * rchdep*Sqrt(1.+chside *chside)
              rh = rcharea / p
              sdti = Qman(rcharea, rh, sd_chd(ich)%chn, sd_ch(ich)%chs)
              !need to save hydraulic radius and time for each flow interval for downcutting and widening
              if (sdti > valint * peakrate) then
                hyd_rad(ivalint) = rh
                tb = (peakrate - sdti) * tbase / peakrate
                timeint(ivalint) = (tb_pr - tb) / 3600.   !sec to hr
                tb_pr = tb
                ivalint = ivalint + 1
                valint = float (ivalint) / float(maxint)
              end if
            END DO
            
            !! estimate overbank flow - assume a triangular hyd
            tbase = 1.5 * sd_chd(ich)%tc * 60.  !seconds
            vol_ovb = 0.5 * (peakrate - sd_ch(ich)%phi(5)) *              &
                                    sd_ch(ich)%phi(5) / peakrate * tbase
            vol_ovb = amin1(vol_ovb, chflow_m3)
            vol_ovb = peakrate - sd_ch(ich)%phi(5)
            const = vol_ovb / peakrate
            ob(icmd)%hd(3) = const * ob(icmd)%hin
            !find current total flood volume (ht1)
            ht1 = hz
            ics = ob(icmd)%props2
            if (ics > 0) then   ! flood elements are specified - link to surface elements
            ob_const = const
            do ii = 1, ch_sur(ics)%num
              ht1 = ht1 + ch_sur(ics)%hd(ii)
            end do
            !add current and new flood volumes
            ht1 = ht1 + ob(icmd)%hd(3)
           if (ht1%flo > ch_sur(ics)%flood_volmx(0)) then
            !calc flood depth above channel bottom (flodd_dep)
            sum_vol = 0.
            do ii = 1, ch_sur(ics)%num
              if (ht1%flo < ch_sur(ics)%flood_volmx(ii)) then
                !solve quadrative for depth above base of current element
                a = sd_ch(ich)%chl * 1000. / sd_ch(ich)%chs
                b = ch_sur(ics)%wid(ii-1) * sd_ch(ich)%chl * 1000.
                c =  ch_sur(ics)%flood_volmx(ii-1) - ht1%flo
                xx = b ** 2 - 4. * a * c
                dep = (-b + sqrt(xx)) / (2. * a)
                dep = amax1(0., dep)
                ic = ii
                exit
              end if
              if (ii == ch_sur(ics)%num) then
                !flood is over the  max storage - assume linear upward wall
                ic = ch_sur(ics)%num
                vol_overmx = ht1%flo - ch_sur(ics)%flood_volmx(ii)
                dep = ch_sur(ics)%dep(ic) + (vol_overmx / (sd_ch(ich)%chl *  &
                               (sd_ch(ich)%chw + 2. * ch_sur(ics)%wid(ic))))
              end if
            end do
            !calc flood depth above channel bottom
            flood_dep = dep + ch_sur(ics)%dep(ic-1)
            !calc new flood volume for each element
            do ii = 1, ch_sur(ics)%num
              !calc depth in the element
              
              if (flood_dep < ch_sur(ics)%dep(ii-1)) then
                !no flooding on element
                ch_sur(ics)%hd(ii)%flo = 0.
              else if (flood_dep < ch_sur(ics)%dep(ii)) then
                !flood level within the element
                dep_e = flood_dep - ch_sur(ics)%dep(ii-1)
                ch_sur(ics)%hd(ii)%flo = dep_e ** 2 / sd_ch(ich)%chs       &
                                                        * sd_ch(ich)%chl
              else
                !flood level over max element depth
                ch_sur(ics)%hd(ii)%flo = 2. * sd_ch(ich)%chw *             & 
                  (flood_dep - ch_sur(ics)%dep(ii)) + sd_ch(ich)%chw *     &
                  (ch_sur(ics)%dep(ii) - ch_sur(ics)%dep(ii-1))
              end if
              ch_sur(ics)%hd(ii)%flo = amin1 (                             &
                 ch_sur(ics)%hd(ii)%flo, ch_sur(ics)%flood_volmx(ii))
              sum_vol = sum_vol + ch_sur(ics)%hd(ii)%flo
            end do
            !determine fraction of total flood volume and set volume for each element
            rto = sum_vol / ht1%flo  !ensure water balance is maintained
            do ii = 1, ch_sur(ics)%num
              const = rto * ch_sur(ics)%hd(ii)%flo / ht1%flo
              ch_sur(ics)%hd(ii) = const * ht1
            end do
           end if
           end if
          ELSE
            !! CHANNELIZED FLOW
            !! find the crossectional area and depth for volrt
            !! by iteration method at 1cm interval depth
            !! find the depth until the discharge rate is equal to volrt
            !zero overbank flow
            sd_ch(ich)%overbank = "ub"
            ob(icmd)%hd(3) = hz
            ob_const = 1.
            sdti = 0.
            rchdep = 0.
            sumtime = 0.
            valint = 1. / float(maxint)
            ivalint = 1
            tbase = 1.5 * sd_chd(ich)%tc * 60.   !hydrograph base time in s
            tb_pr = tbase
            DO WHILE (sdti < peakrate)
              rchdep = rchdep + 0.01
              rcharea = (sd_ch(ich)%phi(6) + chside * rchdep) * rchdep
              p=sd_ch(ich)%phi(6)+2. * rchdep*Sqrt(1.+chside *chside)
              rh = rcharea / p
              sdti = Qman(rcharea, rh, sd_chd(ich)%chn, sd_ch(ich)%chs)
              !need to save hydraulic radius and time for each flow interval for downcutting and widening
              if (sdti > valint * peakrate) then
                hyd_rad(ivalint) = rh
                tb = (peakrate - sdti) * tbase / peakrate
                timeint(ivalint) = (tb_pr - tb) / 3600.   !sec to hr
                sumtime = sumtime + timeint(ivalint)
                tb_pr = tb
                ivalint = ivalint + 1
                valint = float (ivalint) / float(maxint)
              end if
            END DO
            timeint = timeint / sumtime
          END IF

         !!     calculate flow velocity
          vc = 0.001
          if (rcharea > 1.e-4) then
            vc = peakrate / rcharea
            if (vc > sd_ch(ich)%phi(9)) vc = sd_ch(ich)%phi(9)
          end if

        if (sd_ch(ich)%chs > sd_chd(ich)%chseq) then
          erode_btm = 0.
          erode_bank = 0.
          deg_btm = 0.
          deg_bank = 0.
          !break hydrograph into maxint segments and compute deg at each flow increment
          do ihval = 1, maxint
          !! downcutting bottom
          ! bedlaod capacity - simplified Meyers-Peter-Mueler from Chap 4 on Sediment Transport
          if (sd_chd(ich)%clay >= 10.) then
            chns = .0156
          else
            chns = (sd_chd(ich)%d50 / 25.4) ** .16666 / 39.
          end if
          shear_btm_cr = sd_chd(ich)%d50
          shear_btm = 9800. * hyd_rad(ihval) * sd_ch(ich)%chs   !! Pa = N/m^2 * m * m/m
          if (shear_btm > shear_btm_cr) then
            bedld_cap = 0.253 * (shear_btm - shear_btm_cr) ** 1.5
            shear_btm_adj = shear_btm * (1. - bedld / bedld_cap) !!* (chns / sd_chd(ich)%chn) ** 2
          else
            shear_btm_adj = 0.
          end if
	      if (shear_btm_adj > shear_btm_cr) then
            e_btm = timeint(ihval) *  sd_chd(ich)%cherod * (shear_btm_adj - shear_btm_cr)    !! cm = hr * cm/hr/Pa * Pa
            erode_btm = erode_btm + e_btm
            !! calculate mass of sediment eroded
            ! t = cm * m/100cm * width (m) * length (km) * 1000 m/km * bd (t/m3)
            !! degradation of the bottom (downcutting)
            perim_bed = sd_ch(ich)%chw
            deg_btm = deg_btm + 10. * e_btm * perim_bed * sd_ch(ich)%chl * sd_chd(ich)%bd
          end if
          
          !! widening sides
          perim_bank = 2. * ((sd_chd(ich)%chd ** 2) * (1. + sd_chd(ich)%chss ** 2)) ** 0.5
          tw = perim_bed + 2. * sd_chd(ich)%chss * rchdep
          s_bank = 1.77 * (perim_bed / perim_bank + 1.5) ** - 1.4
          shear_bank = shear_btm * .75                              !s_bank * (tw * perim_bed) / (2. * perim_bank)
          shear_bank_adj = shear_bank * (1. - sd_chd(ich)%cov)      !* (chns / sd_chd(ich)%chn) ** 2
          shear_bank_cr = 0.493 * 10. ** (.0182 * sd_chd(ich)%clay)
          if (shear_bank_adj > shear_bank_cr) then
            e_bank = timeint(ihval) * sd_chd(ich)%cherod * (shear_bank_adj - shear_bank_cr)    !! cm = hr * cm/hr/Pa * Pa
            erode_bank = erode_bank + e_bank
            !! calculate mass of sediment eroded
            ! t = cm * m/100cm * width (m) * length (km) * 1000 m/km * bd (t/m3)
            !! degradation of the bank (widening)
            deg_bank = deg_bank + 10. * e_bank * perim_bank * sd_ch(ich)%chl * sd_chd(ich)%bd
          end if

          end do
          
          erode_btm = MAX(0., erode_btm)
          erode_bank = MAX(0., erode_bank)
          sd_ch(ich)%chd = sd_ch(ich)%chd + erode_btm / 100.
          sd_ch(ich)%chw = sd_ch(ich)%chw + 2. * erode_bank / 100.
          sd_ch(ich)%chs = sd_ch(ich)%chs - (erode_btm / 100.) / (sd_ch(ich)%chl * 1000.)
          sd_ch(ich)%chs = MAX(sd_chd(ich)%chseq, sd_ch(ich)%chs)

        end if
        
        !! compute nutrient losses using 2-stage ditch model
        !! calculate nutrient concentrations
        ht1 = hz
        ht1 = ob(icmd)%hin
        !convert mass to concentration
        call hyd_convert_conc (ht1)
        call sd_channel_nutrients (ht1)
        
      END IF
          
      !! compute sediment leaving the channel
	  bedld = sd_chd(ich)%bedldcoef * sedin
	  washld = (1. - sd_chd(ich)%bedldcoef) * sedin
	  dep =  bedld - bedld_cap
      dep = amax1 (0., dep)
	  sedout = (bedld - dep) + washld + hc_sed + deg_btm + deg_bank
      
      !! output_channel
      chsd_d(ich)%flo = ob_const * ob(icmd)%hin%flo  / 86400.  !adjust if overbank flooding is moved to landscape
      chsd_d(ich)%peakr = peakrate 
      chsd_d(ich)%sed_in = ob(icmd)%hin%sed
      chsd_d(ich)%sed_out = sedout
      chsd_d(ich)%washld = washld
      chsd_d(ich)%bedld = bedld - dep
      chsd_d(ich)%dep = dep
      chsd_d(ich)%deg_btm = deg_btm
      chsd_d(ich)%deg_bank = deg_bank
      chsd_d(ich)%hc_sed = hc_sed
      
      !! set values for outflow hydrograph
      !! storage locations set to zero are not currently used
      ob(icmd)%hd(1)%temp = 5. + .75 * tave        !!wtmp
      ob(icmd)%hd(1)%flo = chflow_m3               !!qdr m3/d
      ob(icmd)%hd(1)%sed = amax1 (0., ob(icmd)%hin%sed - sed_reduc_t)  !!sedyld
      ob(icmd)%hd(1)%orgn = amax1 (0., (ht1%orgn - tp_reduc) * ob(icmd)%hin%flo / 1000.) 
      ob(icmd)%hd(1)%sedp = amax1 (0., (ht1%sedp - tp_reduc) * ob(icmd)%hin%flo / 1000.)
      ob(icmd)%hd(1)%no3 = amax1 (0., ob(icmd)%hin%no3 - no3_reduc_t) 
      ob(icmd)%hd(1)%solp = amax1 (0., ob(icmd)%hin%solp - srp_reduc_t) 
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
      ob(icmd)%hd(2)%flo = perc  

      if (time%yrs > pco%nyskip) then
        call sd_channel_output
      end if
      
      return
      
      end subroutine sd_channel_control