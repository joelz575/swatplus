      subroutine calhard_control
    
      use aquifer_module
      use maximum_data_module 
      use hydrograph_module
      
      implicit none
      
      integer :: ihru        !none      |counter

      !! re-initialize all hru's
      do ihru = 1, sp_ob%hru
        call hru_re_initialize (ihru)
      end do
      !! re-initialize channel, reservoir, aquifer arrays
      ch_stor = ch_om_water_init
      res = res_om_init
      aqu_om_init = aqu_om_init
      
      !! adjust parameters
      
      !! rerun model
      cal_sim = " hard calibration simulation "
      call time_control

      return
      end subroutine calhard_control