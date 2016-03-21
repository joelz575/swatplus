      subroutine wind_ero_veg

      !sc_fac = pldb(idp)%wind_stl * hru(j)%stl + pldb(idp)%wind_std * hru(j)%std + pldb(idp)%wind_flat * soil(j)%ly(1)%rsd
      
      ! wind_factors%veg = 1. - sc_fac / (sc_fac + exp(-0.331 - 1.055 * sc_fac)
      
      return
      end subroutine wind_ero_veg