      subroutine proc_allo
    
      use hydrograph_module
      
      implicit none

      call ru_allo
            
      !! allocate and initialize reservoir variables
      if (sp_ob%res > 0) then
        call res_allo
        call res_objects
        call res_initial
      end if

      call aqu_read
      call aqu_initial
      
      call pest_soil_init
      call path_soil_init
      call hmet_soil_init
      call salt_soil_init

	  return
      
      end subroutine proc_allo