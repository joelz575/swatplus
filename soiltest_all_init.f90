      subroutine soiltest_all_init
      
      use hru_module, only : sol, hru, soil, wfsh, pcom, pcom_init, ihru
      use maximum_data_module
      use soil_data_module
      use hydrograph_module, only : sp_ob

      do ihru = 1, sp_ob%hru
        isolt = hru(ihru)%dbs%soil_nutr_init
        if (isolt > 0) then
          call soiltest_init (ihru, isolt)
        end if
      end do
      
      return
      end subroutine soiltest_all_init