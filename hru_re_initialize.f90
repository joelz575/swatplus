      subroutine hru_re_initialize (iihru)
    
      use parm, only : hru, hru_init, soil, soil_init, pcom, pcom_init, sno_hru, sno_init, nop, bss

      use organic_mineral_mass_module

      integer, intent (in) :: iihru
      
      hru(iihru) = hru_init(iihru)
      soil(iihru) = soil_init(iihru)
      soil1(iihru) = soil1_init(iihru)
      rsd1(iihru) = rsd1_init(iihru)
      pcom(iihru) = pcom_init(iihru)
      sno_hru(iihru) = sno_init(iihru)
      bss(:,iihru) = 0.
      nop(iihru) = 1
      
      return
      end subroutine hru_re_initialize