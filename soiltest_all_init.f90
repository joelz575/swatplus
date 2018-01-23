      subroutine soiltest_all_init
      
      use parm, only : sol, hru, soil, wfsh, pcom, pcom_init, ihru
      use jrw_datalib_module, only : db_mx, soildb, solt_db
      use hydrograph_module, only : sp_ob

      do ihru = 1, sp_ob%hru
        isolt = hru(ihru)%dbs%soil_nutr_init
        if (isolt > 0) then
          call soiltest_init (ihru, isolt)
        end if
      end do
      
      return
      end subroutine soiltest_all_init