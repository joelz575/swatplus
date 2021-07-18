      module gwflow_module
    
      implicit none

      
      !variables for grid cells
      integer :: grid_nrow,grid_ncol,num_active
      real    :: cell_size
      real, dimension (:,:), allocatable :: head_new,head_begin,gw_avail,gw_cell_satthick
      
      !time step
      real    :: gw_time_step
      
      !variables for linking HRUs to grid cells
      real, dimension (:,:), allocatable :: gw_cell_x,gw_cell_y,cell_hru_dist
      integer, dimension (:), allocatable :: num_cells_per_hru,hru_num_cells
      integer, dimension (:,:), allocatable :: cell_connected
      integer, dimension (:,:,:), allocatable :: hru_cells
      real, dimension (:), allocatable :: utmx_hru,utmy_hru,hru_cell_dist
      real, dimension (:,:), allocatable :: utmx_cell,utmy_cell
      
      !variables for river cells (cells connected to SWAT+ channels)
      integer :: num_rivcells
      integer, dimension (:), allocatable :: gw_riv_id,gw_riv_row,gw_riv_col,gw_riv_chan,gw_riv_gis
      real, dimension (:), allocatable :: gw_riv_len,gw_riv_elev,gw_riv_K,gw_riv_thick

      !variables for aquifer properties and system-response
      real, dimension (:,:), allocatable :: gw_cell_top,gw_cell_bot,gw_cell_inithead,gw_cell_head
      real, dimension (:,:), allocatable :: gw_cell_K,gw_cell_Sy
      integer, dimension (:,:), allocatable :: gw_cell_id,gw_cell_status
      
      !variables for typical groundwater sources and sinks
      integer :: gw_et_flag
      real, dimension (:), allocatable :: gwflow_perc,etremain,etactual
      real, dimension (:,:), allocatable :: gwflow_rech_sum,gwflow_et_sum,gwflow_gwsw_sum,gwflow_lateral_sum,gwflow_etact_sum
      real, dimension (:,:), allocatable :: gw_cell_ss_rech,gw_cell_ss_et,gw_cell_ss_gwsw
      real, dimension (:,:), allocatable :: gw_cell_ss_swgw,gw_cell_satex,gw_cell_ss_etact
      real, dimension (:,:), allocatable :: gw_cell_ss_pump,gw_cell_ss_other,gw_cell_ss,gw_cell_Q,gw_cell_error
      real, dimension (:,:), allocatable :: gw_volume_before_cell,gw_volume_after_cell
      real, dimension (:), allocatable :: chan_Q
      real, dimension (:), allocatable :: gw_delay,gw_rech
      real, dimension (:,:), allocatable :: gw_cell_exdp,gw_cell_et
      real, dimension (:,:), allocatable ::ss_rech_cell_total,ss_et_cell_total,ss_gwsw_cell_total,ss_satex_cell_total
      real, dimension (:,:), allocatable :: ss_pump_cell_total,ss_etact_cell_total
      
      !variables for tile drain flow
      integer :: gw_tile_flag
      integer :: gw_tile_ncells,gw_tile_ndrains
      real    :: gw_tile_depth,gw_tile_cond
      integer, dimension (:), allocatable :: gw_tile_rivcell
      integer, dimension (:), allocatable :: gw_tile_row,gw_tile_col,gw_tilecell_rivcell
      real, dimension (:,:), allocatable :: gwflow_tile_sum,gw_cell_ss_tile,ss_tile_cell_total
      
      !variables for saturation excess groundwater flow
      integer :: gw_satexcess_flag
      real, dimension (:,:), allocatable :: gw_cell_rivcell
      real, dimension (:,:), allocatable :: gwflow_satex_sum
      
      !variables for writing to files
      integer :: out_gwobs,out_gwconnect,out_gwheads,out_gwsw_chan,out_gw_chan,out_gw_rech,out_gw_et,out_gw_grid
      integer :: out_rivconnect,out_gw_satex,out_gwsw,out_lateral,out_gw_etact,out_gw_tile
      integer :: out_gwbal,out_gwbal_yr,out_gwbal_aa
      integer :: gw_num_output,gw_output_index
      integer, dimension (:), allocatable :: gw_output_yr,gw_output_day
      
      !observation well variables
      integer :: gw_num_obs_wells
      integer, dimension (:), allocatable :: gw_obs_cells_row,gw_obs_cells_col
      real, dimension (:), allocatable :: gw_obs_head
      
      !streamflow output
      real, dimension (:), allocatable :: channel_flow
      
      !water balance calculations
      real :: watershed_area
      real :: vol_change_yr,ss_rech_yr,ss_et_yr,ss_gw_yr,ss_sw_yr,ss_satex_yr,ss_Q_yr,ss_pump_yr,ss_tile_yr
      real :: vol_change_total,ss_rech_total,ss_et_total,ss_gw_total,ss_sw_total,ss_satex_total,ss_Q_total
      real :: ss_pump_total,ss_tile_total
            
      end module gwflow_module
