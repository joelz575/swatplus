      subroutine proc_cal
   
      implicit none
      
      !integer :: date_time(8)           !              | 
      !character*10 b(3)                 !              |
      !integer :: irch                   !              |
      !integer :: idat                   !              |
      !integer :: isched                 !              | 
      !integer :: iauto                  !none          |counter
      !integer :: ictl                   !none          |counter
      !integer :: i                      !none          |counter
   
      !read calibration data (if included)
      call cal_read_parms
      call update_read_parm

      call update_init
            
      !! read update data
      !call update_sched_read
      call update_read_cond
            
      !! read soft calibration parameters
      call codes_read_cal
      call lsu_read_elements        !defining landscape units by hru
      !call reg_read_elements        !defining regions by lsu and/or hru
      call lcu_read_softcal         !soft data for landscape calibration (needs to be renamed)***
      call ls_read_lsparms_cal
      call pl_read_regions_cal      !soft data for hru_lte calibration
      call pl_read_parms_cal
      call aqu_read_elements        !defining regions by aquifer
      call ch_read_elements        !defining regions by channel
      call res_read_elements        !defining regions by reservoir
      call rec_read_elements        !defining regions by recall object (point source, gage data, model output, etc)
      call ch_read_orders_cal
      call ch_read_parms_cal
      
      call cal_init

	  return
      
      end subroutine proc_cal