      subroutine hru_soiltest_update(isol, isolt)

      do ly = 1, hru(isol)%sol%nly
          dep_frac=Exp(-solt_db(isolt)%exp_co * soil(isol)%phys(ly)%d)
          soil(isol)%nut(ly)%no3 = solt_db(isolt)%inorgn * dep_frac
          soil(isol)%nut(ly)%orgn = solt_db(isolt)%orgn * dep_frac
          soil(isol)%nut(ly)%solp = solt_db(isolt)%inorgp * dep_frac
          soil(isol)%nut(ly)%orgp = solt_db(isolt)%orgp * dep_frac
 !         hru(j)%ly(ly)%watersol_p = solt_db(isolt)%watersol_p* dep_frac
 !         hru(j)%ly(ly)%h3a_p = solt_db(isolt)%h3a_p * dep_frac
 !         hru(j)%ly(ly)%mehlich_p = solt_db(isolt)%mehlich_p * dep_frac
 !         hru(j)%ly(ly)%bray_strong_p = solt_db(isolt)%bray_strong_p    
 !   &                                                      * dep_frac
      end do
      
      return
      end subroutine hru_soiltest_update