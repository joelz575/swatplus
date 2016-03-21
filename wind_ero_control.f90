      subroutine wind_ero_control
 
      call wind_ero_bare
      
      call wind_ero_erod
      
      call wind_ero_veg
      
      call wind_ero_rough
      
      call wind_ero_unshelt
      
      return
      end subroutine wind_ero_control