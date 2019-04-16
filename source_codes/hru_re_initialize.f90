      subroutine hru_re_initialize (iihru)
    
      use hru_module, only : hru, hru_init, sno_hru, sno_init, bss
      use soil_module
      use plant_module
      use organic_mineral_mass_module
      use mgt_operations_module
      use hydrograph_module
      
      implicit none

      integer, intent (in) :: iihru             !           |

      hru(iihru) = hru_init(iihru)
      soil(iihru) = soil_init(iihru)
      soil1(iihru) = soil1_init(iihru)
      rsd1(iihru) = rsd1_init(iihru)
      pcom(iihru) = pcom_init(iihru)
      wet(iihru) = wet_om_init(iihru)
      sno_hru(iihru) = sno_init(iihru)
      bss(:,iihru) = 0.

      return
      end subroutine hru_re_initialize