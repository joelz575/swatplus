      subroutine soiltest_all_init
      
      use hru_module, only : hru, wfsh, ihru
      use soil_module
      use plant_module
      use maximum_data_module
      use soil_data_module
      use hydrograph_module, only : sp_ob
      
      implicit none 
      
      integer :: isolt            !           | 

      do ihru = 1, sp_ob%hru
        isolt = hru(ihru)%dbs%soil_nutr_init
        if (isolt > 0) then
          call soiltest_init (ihru, isolt)
        end if
      end do
      
      return
      end subroutine soiltest_all_init