      subroutine time_conc_init 
    
      use hydrograph_module
      use subbasin_module
          
     ! compute weighted Mannings n for each subbasin
      do isub = 1, sp_ob%sub
        sub_n(isub) = 0.
        do ii = 1, sub_d(isub)%num_tot
          ielem = sub_d(isub)%num(ii)
          if (sub_elem(ielem)%obtyp == "hru") then
            ihru = sub_elem(ielem)%obtypno 
            sub_n(isub) = sub_n(isub) + hru(ihru)%luse%ovn* hru(ihru)%km
          end if
        end do
      end do
            
      do isub = 1, sp_ob%sub
        iob = sp_ob1%sub + isub - 1
        sub(isub)%da_km2 = ob(iob)%area_ha / 100.
        bsn%area_ha = bsn%area_ha + ob(iob)%area_ha
        sub_n(isub) = sub_n(isub) / sub(isub)%da_km2
        ith = sub(isub)%dbs%toposub_db
        ifld = sub(isub)%dbs%field_db
        !if (ith > 0 .and. ichan > 0) then                  
        ! compute tc for the subbasin
          tov = .0556 * (toposub_db(ith)%slope_len * sub_n(isub)) ** .6 /     &
                                              (toposub_db(ith)%slope + .0001) ** .3
          ch_slope = .5 * (toposub_db(ith)%slope + 0001)
          ch_n = .05
          ch_l = field_db(ifld)%length / 1000.
          t_ch = .62 * ch_l * ch_n**.75 / (sub(isub)%da_km2**.125 * ch_slope**.375)
          sub_tc(isub) = tov + t_ch
        !end if                                             
      end do
      
      !!compute time of concentration (sum of overland and channel times)
      do ihru = 1, mhru
        ith = hru_db(ihru)%dbs%topo
        ifld = hru_db(ihru)%dbs%field
        t_ov(ihru) = .0556 * (hru(ihru)%topo%slope_len *                    &
           hru(ihru)%luse%ovn) ** .6 / (hru(ihru)%topo%slope + .00001) ** .3
        ch_slope = .5 * topo_db(ith)%slope
        ch_n = .05
        ch_l = field_db(ifld)%length / 1000.
        t_ch = .62 * ch_l * ch_n**.75 / (hru(ihru)%km**.125 * (ch_slope + .00001)**.375)
        tconc(ihru) = t_ov(ihru) + t_ch
        !! compute fraction of surface runoff that is reaching the main channel
        if (time%step > 0) then
          brt(ihru) = 1.-Exp(-bsn_prm%surlag / (tconc(ihru) /               &
              (time%dtm / 60.)))	!! urban modeling by J.Jeong
        else
          brt(ihru) = 1. - Exp(-bsn_prm%surlag / tconc(ihru))
        endif
        sumn = sumn + hru(ihru)%luse%ovn * hru(ihru)%km
      end do
      
     return
     end subroutine time_conc_init