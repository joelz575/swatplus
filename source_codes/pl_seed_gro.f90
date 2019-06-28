      subroutine pl_seed_gro
      
      use plant_data_module
      use basin_module
      use hru_module, only : hru, uapd, uno3d, lai_yrmx, par, bioday, ep_day, es_day,              &
         ihru, ipl, pet_day, rto_no3, rto_solp, sum_no3, sum_solp, uapd_tot, uno3d_tot, vpd
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      
      implicit none 
      
      integer :: j              !none               |HRU number
      integer :: idp            !                   |
    
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
      !! calculate plant ET values
      if (pcom(j)%plcur(ipl)%phuacc > 0.5 .and. pcom(j)%plcur(ipl)%phuacc < pldb(idp)%dlai) then 
        pcom(j)%plg(ipl)%plet = pcom(j)%plg(ipl)%plet + ep_day + es_day
        pcom(j)%plg(ipl)%plpet = pcom(j)%plg(ipl)%plpet + pet_day
      end if
     
      pcom(j)%plg(ipl)%hvstiadj = pldb(idp)%hvsti * 100. * pcom(j)%plcur(ipl)%phuacc /          &
                (100. * pcom(j)%plcur(ipl)%phuacc + Exp(11.1 - 10. * pcom(j)%plcur(ipl)%phuacc))
       
      !! adjust harvest index for temperature stress
      !x2 = 100 *hui
      !ajhi = hi * x2 / (x2 + Exp (11.1 - 0.1 * x2)
      !dhi = ajhi - - ajh0
      !x3 = t_opt - tx
      !if (x3 < 0. .and. hui > 0.7) then
      !  xx = Exp (8. * x3 / t_opt)
      !  dhi = dhi * xx
      !end if
      !ajhi = ajhi + dhi

      return
      end subroutine pl_seed_gro