      subroutine soiltest_init (isol, isolt)
    
      use parm, only : soil  
      use jrw_datalib_module, only : solt_db
      use organic_mineral_mass_module

      do ly = 1, soil(isol)%nly
          dep_frac=Exp(-solt_db(isolt)%exp_co * soil(isol)%phys(ly)%d)
          soil1(isol)%mn(ly)%no3 = solt_db(isolt)%inorgn * dep_frac
          soil1(isol)%mp(ly)%lab = solt_db(isolt)%inorgp * dep_frac
          !soil1(isol)%hp(ly)%n = solt_db(isolt)%orgn * dep_frac
          !soil1(isol)%hp(ly)%p = solt_db(isolt)%orgp * dep_frac
          !soil(j)%ly(ly)%watersol_p = solt_db(isolt)%watersol_p* dep_frac
          !soil(j)%ly(ly)%h3a_p = solt_db(isolt)%h3a_p * dep_frac
          !soil(j)%ly(ly)%mehlich_p = solt_db(isolt)%mehlich_p * dep_frac
          !soil(j)%ly(ly)%bray_strong_p = solt_db(isolt)%bray_strong_p    
          !   &                                                      * dep_frac
      end do
      
      return
      end subroutine soiltest_init