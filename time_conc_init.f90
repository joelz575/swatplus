      subroutine time_conc_init 
    
      use ru_module
      use hru_module, only : brt, hru, hru_db, ihru, t_ov, tconc
      use hydrograph_module, only : sp_ob, ru_def, ru_elem, sp_ob1, ob
      use jrw_datalib_module, only : topo_db, field_db
      use time_module
      use basin_module
          
     ! compute weighted Mannings n for each subbasin
      do isub = 1, sp_ob%sub
        ru_n(isub) = 0.
        do ii = 1, ru_def(isub)%num_tot
          ielem = ru_def(isub)%num(ii)
          if (ru_elem(ielem)%obtyp == "hru") then
            ihru = ru_elem(ielem)%obtypno 
            ru_n(isub) = ru_n(isub) + hru(ihru)%luse%ovn * hru(ihru)%km
          else
            ru_n(isub) = 0.1
          end if
        end do
      end do
            
      do isub = 1, sp_ob%sub
        iob = sp_ob1%sub + isub - 1
        ru(isub)%da_km2 = ob(iob)%area_ha / 100.
        ru_n(isub) = ru_n(isub) / ru(isub)%da_km2
        ith = ru(isub)%dbs%toposub_db
        ifld = ru(isub)%dbs%field_db
        !if (ith > 0 .and. ichan > 0) then                  
        ! compute tc for the subbasin
          tov = .0556 * (topo_db(ith)%slope_len * ru_n(isub)) ** .6 /     &
                                              (topo_db(ith)%slope + .0001) ** .3
          ch_slope = .5 * (topo_db(ith)%slope + 0001)
          ch_n = ru_n(isub)
          ch_l = field_db(ifld)%length / 1000.
          t_ch = .62 * ch_l * ch_n**.75 / (ru(isub)%da_km2**.125 * ch_slope**.375)
          ru_tc(isub) = tov + t_ch
        !end if                                             
      end do
      
      !!compute time of concentration (sum of overland and channel times)
      do ihru = 1, sp_ob%hru
        ith = hru_db(ihru)%dbs%topo
        ifld = hru_db(ihru)%dbs%field
        t_ov(ihru) = .0556 * (hru(ihru)%topo%slope_len *                    &
           hru(ihru)%luse%ovn) ** .6 / (hru(ihru)%topo%slope + .00001) ** .3
        ch_slope = .5 * topo_db(ith)%slope
        ch_n = hru(ihru)%luse%ovn
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