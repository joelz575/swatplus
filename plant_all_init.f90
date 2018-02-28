      subroutine plant_all_init
    
      use hru_module, only : hru, ihru, isol, ilu
      use hydrograph_module, only : sp_ob

      !!assign land use pointers for the hru
      !!allocate and initialize land use and management
      do ihru = 1, sp_ob%hru
        !!ihru, ilu and isol are in modparm
        ilu = hru(ihru)%dbs%land_use_mgt
        isol = hru(ihru)%dbs%soil
        !send 0 value in when initializing- 1 for updating to deallocate
        call plant_init (0)
      end do

      end subroutine plant_all_init