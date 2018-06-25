      subroutine pl_leaf_mortality
      
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
      real :: rto               !none               |ratio of current years of growth:years to maturity of perennial
      real :: biomxyr
               
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
      !keep biomass below maximum - excess to residue (need to include c, n and p adjustments)
      biomxyr = rto * pldb(idp)%bmx_peren * 1000.  !t/ha -> kg/ha
      if (biomxyr > 1.e-6 .and. pcom(j)%plm(ipl)%mass > biomxyr) then
        rsd1(j)%tot(ipl)%m = pcom(j)%plm(ipl)%mass - biomxyr
        pcom(j)%plm(ipl)%mass = biomxyr
      end if

      return
      end subroutine pl_leaf_mortality