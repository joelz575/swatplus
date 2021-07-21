
      !This subroutine performs the following operations:
      !  1. Read in grid cell information
      !  2. Prepare the grid cells for the groundwater model
      !  3. Connect SWAT+ objects to grid cells
  
      !  Prepared by: Ryan Bailey, Colorado State University
      !  January-April 2020
  
      subroutine gwflow_read
      
      use gwflow_module
      use hydrograph_module
      use sd_channel_module
      
      implicit none
      
      character*10 b(3) 
      integer  date_time(8)  
      integer  in_gw,in_hru_cell,in_mapping
      integer  i,j,k,l,row,col,ob_num,num_hru,hru_id,dum,dum1,dum2,wt_init_type
      integer  nzones_aquK,nzones_aquSy,nzones_aqun,nzones_strK,nzones_strbed,K_zone,Sy_zone,n_zone,bed_zone
      integer  hru_nearest,hru_read,hru_cell_row,hru_cell_col,cell_count,obs_cell_row,obs_cell_col
      integer  first_found,first_cell,last_cell,found
      integer  already_written,counter,riv_num,count
      real     lat_deg,long_deg,utmx,utmy,usgs_id
      real     coord_x_hru,coord_y_hru
      real     dist_x,dist_y,min_dist,distance,hru_avg_area,hru_avg_side
      real     wt_start,delay,thickness,bed_change
      real     fraction_target,completed_fraction
      real     ru_area
      real, dimension (:), allocatable :: zones_aquK,zones_aquSy,zones_aqun,zones_strK,zones_strbed
      integer, dimension (:), allocatable :: chan_num_array,chan_gis_array
      

      !write message to screen
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,111) "reading from groundwater file      ", date_time(5), date_time(6), date_time(7)
      
      !integers for input and output files
      in_gw = 1230
      in_hru_cell = 1231
      in_mapping = 1232
      out_gwobs = 1240
      out_gwconnect = 1241
      out_gwheads = 1242
      out_gwbal = 1243
      out_gwsw_chan = 1245
      out_gw_chan = 1246
      out_gw_rech = 1247
      out_gw_et = 1248
      out_gw_grid = 1249
      out_rivconnect = 1250
      out_gw_satex = 1251
      out_gwsw = 1252
      out_lateral = 1253
      out_gw_etact = 1254
      out_gw_tile = 1255
      out_gwbal_yr = 1256
      out_gwbal_aa = 1257
      
      !number of HRUs in the simulation
      num_hru = sp_ob%hru
      
      !calculate watershed domain area (m2) (used for connecting HRUs to grid cells, and for water balance calculations)
      ob_num = sp_ob1%ru  !object number of first routing unit
      watershed_area = 0.
      do i=1,sp_ob%ru
        ru_area = ob(ob_num)%area_ha * 10000. !ha --> m2
        watershed_area = watershed_area + ru_area
        ob_num = ob_num + 1
      enddo
      
      
      !read in gwflow module information from gwflow.input ------------------------------------------------------------------------------------------
      open(in_gw,file='gwflow.input')
      read(in_gw,*)
      read(in_gw,*)
      
      !basic information
      read(in_gw,*) cell_size
      read(in_gw,*) grid_nrow,grid_ncol
      read(in_gw,*) wt_init_type !water table initiation flag
      read(in_gw,*) wt_start !water table initiation
      read(in_gw,*) gw_satexcess_flag !flag to simulate saturation excess routing
      read(in_gw,*) gw_et_flag
      read(in_gw,*) gw_tile_flag !flag to simulate tile drain flow
      allocate(gw_delay(num_hru)) !groundwater delay
      allocate(gw_rech(num_hru))
      read(in_gw,*) hru_read
      if(hru_read.eq.0) then
        read(in_gw,*) delay
        gw_delay = Exp(-1./(delay + 1.e-6))
      else
        do k=1,num_hru
          read(in_gw,*) delay
          gw_delay(k) = Exp(-1./(delay + 1.e-6))
        enddo
      endif
      gw_rech = 0.
      read(in_gw,*) gw_time_step !user-specified time step
      
      !Aquifer and streambed parameter zones
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*) nzones_aquK
      allocate(zones_aquK(nzones_aquK))
      do i=1,nzones_aquK
        read(in_gw,*) dum,zones_aquK(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_aquSy
      allocate(zones_aquSy(nzones_aquSy))
      do i=1,nzones_aquSy
        read(in_gw,*) dum,zones_aquSy(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_aqun
      allocate(zones_aqun(nzones_aqun))
      do i=1,nzones_aqun
        read(in_gw,*) dum,zones_aqun(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_strK
      allocate(zones_strK(nzones_strK))
      do i=1,nzones_strK
        read(in_gw,*) dum,zones_strK(i)
      enddo
      read(in_gw,*)
      read(in_gw,*) nzones_strbed
      allocate(zones_strbed(nzones_strbed))
      do i=1,nzones_strbed
        read(in_gw,*) dum,zones_strbed(i)
      enddo

      !grid cell information
      allocate(gw_cell_id(grid_nrow,grid_ncol))
      allocate(gw_cell_top(grid_nrow,grid_ncol))
      allocate(gw_cell_bot(grid_nrow,grid_ncol))
      allocate(gw_cell_K(grid_nrow,grid_ncol))
      allocate(gw_cell_Sy(grid_nrow,grid_ncol))
      allocate(gw_cell_inithead(grid_nrow,grid_ncol))
      allocate(gw_cell_status(grid_nrow,grid_ncol))
      allocate(gw_cell_x(grid_nrow,grid_ncol))
      allocate(gw_cell_y(grid_nrow,grid_ncol))
      allocate(gw_cell_exdp(grid_nrow,grid_ncol))
      allocate(gw_cell_et(grid_nrow,grid_ncol))
      gw_cell_inithead = 0.
      gw_cell_K = 0.
      gw_cell_Sy = 0.
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*)
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(wt_init_type.eq.3) then
            read(in_gw,*) gw_cell_id(i,j),dum1,dum2,gw_cell_status(i,j),gw_cell_x(i,j),gw_cell_y(i,j),gw_cell_top(i,j), &
		thickness,K_zone,Sy_zone,n_zone,gw_cell_exdp(i,j),gw_cell_et(i,j),gw_cell_inithead(i,j)
          else
            read(in_gw,*) gw_cell_id(i,j),dum1,dum2,gw_cell_status(i,j),gw_cell_x(i,j),gw_cell_y(i,j),gw_cell_top(i,j),thickness, &
		K_zone,Sy_zone,n_zone,gw_cell_exdp(i,j),gw_cell_et(i,j)
          endif
          gw_cell_bot(i,j) = gw_cell_top(i,j) - thickness
          if(wt_init_type.lt.3) then
          if(gw_cell_status(i,j).gt.0) then
            if(wt_init_type.eq.1) then  
              gw_cell_inithead(i,j) = gw_cell_top(i,j) - wt_start
            elseif(wt_init_type.eq.2) then
              gw_cell_inithead(i,j) = gw_cell_bot(i,j) + ((gw_cell_top(i,j) - gw_cell_bot(i,j)) * wt_start)
            endif
            if(gw_cell_inithead(i,j).lt.gw_cell_bot(i,j)) then
              gw_cell_inithead(i,j) = gw_cell_bot(i,j)
            endif
          endif
          endif
          if(gw_cell_status(i,j).gt.0) then
            gw_cell_K(i,j) = zones_aquK(K_zone)
            gw_cell_Sy(i,j) = zones_aquSy(Sy_zone)
          endif
        enddo
      enddo
      open(out_gw_grid,file='gwflow_grid_arrays')
      write(out_gw_grid,*) 'Cell Status: 0=inactive; 1=active; 2=boundary'
      do i=1,grid_nrow
        write(out_gw_grid,102) (gw_cell_status(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Ground surface elevation (m)'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_top(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Bedrock elevation (m)'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_bot(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Hydraulic conductivity (m/day)'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_K(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      write(out_gw_grid,*) 'Specific yield'
      do i=1,grid_nrow
        write(out_gw_grid,101) (gw_cell_Sy(i,j),j=1,grid_ncol)
      enddo
      write(out_gw_grid,*)
      
      !count the number of active cells
      num_active = 0
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            num_active = num_active + 1
          endif
        enddo
      enddo
      
      !groundwater output times (times at which groundwater head for each cell will be output)
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*) gw_num_output
      allocate(gw_output_yr(gw_num_output))
      allocate(gw_output_day(gw_num_output))
      do i=1,gw_num_output
        read(in_gw,*) gw_output_yr(i),gw_output_day(i)
      enddo
      
      !read in row and columns of cells for daily groundwater head output (i.e. observation wells)
      read(in_gw,*)
      read(in_gw,*) gw_num_obs_wells
      allocate(gw_obs_cells_row(gw_num_obs_wells))
      allocate(gw_obs_cells_col(gw_num_obs_wells))
      do k=1,gw_num_obs_wells
        read(in_gw,*) gw_obs_cells_row(k),gw_obs_cells_col(k)
      enddo
      allocate(gw_obs_head(gw_num_obs_wells))
      gw_obs_head = 0.
      open(out_gwobs,file='gwflow_obs')
      write(out_gwobs,*) 'Daily head values for observation wells'
      write(out_gwobs,*) 'Row and Column for each observation well'
      write(out_gwobs,*)
      
      !river cell information (cells that are connected to SWAT+ chandeg channels)
      num_rivcells = sp_ob%gwflow
      allocate(gw_riv_id(num_rivcells))
      allocate(gw_riv_row(num_rivcells))
      allocate(gw_riv_col(num_rivcells))
      allocate(gw_riv_chan(num_rivcells))
      allocate(gw_riv_gis(num_rivcells))
      allocate(gw_riv_len(num_rivcells))
      allocate(gw_riv_elev(num_rivcells))
      allocate(gw_riv_K(num_rivcells))
      allocate(gw_riv_thick(num_rivcells))
      read(in_gw,*)
      read(in_gw,*)
      read(in_gw,*) bed_change !vertical distance correction for streambed elevation
      read(in_gw,*)
      do i=1,num_rivcells
        read(in_gw,*) gw_riv_id(i),gw_riv_row(i),gw_riv_col(i),gw_riv_chan(i),gw_riv_gis(i),gw_riv_len(i),gw_riv_elev(i),K_zone, &
	bed_zone
        gw_riv_elev(i) = gw_riv_elev(i) - bed_change
        gw_riv_K(i) = zones_strK(K_zone)
        gw_riv_thick(i) = zones_strbed(bed_zone)
      enddo
      
      !tile drain cell information
      if(gw_tile_flag.eq.1) then
        read(in_gw,*)
        read(in_gw,*)
        read(in_gw,*) gw_tile_depth
        read(in_gw,*) gw_tile_cond
        read(in_gw,*) gw_tile_ndrains
        read(in_gw,*) gw_tile_ncells
        allocate(gw_tilecell_rivcell(gw_tile_ncells))
        allocate(gw_tile_row(gw_tile_ncells))
        allocate(gw_tile_col(gw_tile_ncells))
        do i=1,gw_tile_ncells
          read(in_gw,*) gw_tile_row(i),gw_tile_col(i)
          if(gw_cell_status(gw_tile_row(i),gw_tile_col(i)).gt.0) then
            !find the closest river cell
            min_dist = 1000000.
            do k=1,num_rivcells
              dist_x = (gw_riv_row(k) - gw_tile_row(i))
              dist_y = (gw_riv_col(k) - gw_tile_col(i))
              distance = sqrt((dist_x)**2 + (dist_y)**2)
              if(distance.lt.min_dist) then
                min_dist = distance
                riv_num = k
              endif
            enddo
            gw_tilecell_rivcell(i) = riv_num
          endif
        enddo
        open(out_gw_tile,file='gwflow_tile')
        write(out_gw_tile,*) 'Annual groundwater outflow to tile drains (m3/day)'
      endif
      allocate(gwflow_tile_sum(grid_nrow,grid_ncol))
      allocate(ss_tile_cell_total(grid_nrow,grid_ncol))
      gwflow_tile_sum = 0.
      ss_tile_cell_total = 0.
      
      
      
      
      !allocate other arrays for gwflow module
      !arrays for groundwater head and saturated thickness
      allocate(gw_cell_head(grid_nrow,grid_ncol))
      allocate(head_new(grid_nrow,grid_ncol))
      allocate(head_begin(grid_nrow,grid_ncol))
      allocate(gw_avail(grid_nrow,grid_ncol))
      allocate(gw_cell_satthick(grid_nrow,grid_ncol))
      !arrays for groundwater sources and sinks
      allocate(gw_cell_ss_rech(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_et(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_etact(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_gwsw(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_swgw(grid_nrow,grid_ncol))
      allocate(gw_cell_satex(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_pump(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_tile(grid_nrow,grid_ncol))
      allocate(gw_cell_ss_other(grid_nrow,grid_ncol))
      allocate(gw_cell_ss(grid_nrow,grid_ncol))
      allocate(gw_cell_Q(grid_nrow,grid_ncol))
      allocate(gw_cell_error(grid_nrow,grid_ncol))
      allocate(gw_volume_before_cell(grid_nrow,grid_ncol))
      allocate(gw_volume_after_cell(grid_nrow,grid_ncol))
      allocate(gwflow_perc(num_hru))
      allocate(etremain(num_hru))
      allocate(etactual(num_hru))
      gw_cell_ss_rech = 0.
      gw_cell_ss_et = 0.
      gw_cell_ss_etact = 0.
      gw_cell_ss_gwsw = 0.
      gw_cell_ss_swgw = 0.
      gw_cell_satex = 0.
      gw_cell_ss_pump = 0.
      gw_cell_ss_tile = 0.
      gw_cell_ss_other = 0.
      gw_cell_ss = 0.
      gw_cell_Q = 0.
      gw_cell_error = 0.
      gwflow_perc = 0.
      etremain = 0.
      etactual = 0.
      gw_volume_before_cell = 0.
      gw_volume_after_cell = 0.
      gw_cell_satthick = 0.
      gw_output_index = 1
           
      
      
      
      !connect grid cells to HRUs (each grid cell will be connected to 1 HRU) -----------------------------------------------------------------------
      
      !convert each HRU lat/long coordinate to UTM northing and easting
      ob_num = sp_ob1%hru  !object number of first HRU
      allocate(utmx_hru(num_hru))
      allocate(utmy_hru(num_hru))
      do i=1,num_hru 
        call ll2utm(ob(ob_num)%lat,ob(ob_num)%long,utmx,utmy)
        utmx_hru(i) = utmx
        utmy_hru(i) = utmy
        ob_num = ob_num + 1
      enddo
      
      !convert each cell lat/long coordinate to UTM northing and easting
      allocate(utmx_cell(grid_nrow,grid_ncol))
      allocate(utmy_cell(grid_nrow,grid_ncol))
      do i=1,grid_nrow 
        do j=1,grid_ncol
          call ll2utm(gw_cell_y(i,j),gw_cell_x(i,j),utmx,utmy)
          utmx_cell(i,j) = utmx
          utmy_cell(i,j) = utmy
        enddo
      enddo

      !match each grid cell to an HRU (within a certain distance)
      hru_avg_area = watershed_area / num_hru
      hru_avg_side = sqrt(hru_avg_area)
      allocate(hru_cells(num_hru,50000,2))
      allocate(hru_num_cells(num_hru))
      hru_cells = 0
      hru_num_cells = 0
      already_written = 0
      fraction_target = 0.10
      do k=1,num_hru
        !loop through the grid cells - record the cells that are within the designated distance from the HRU centroid
        count = 0
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).eq.1) then
              dist_x = abs(utmx_hru(k) - utmx_cell(i,j))
              dist_y = abs(utmy_hru(k) - utmy_cell(i,j))
              distance = sqrt((dist_x)**2 + (dist_y)**2) !distance between HRU and grid cell
              if(distance.lt.cell_size) then
                count = count + 1
                hru_cells(k,count,1) = i
                hru_cells(k,count,2) = j
              endif
            endif
          enddo
        enddo
        hru_num_cells(k) = count
        !if no cells were within the designated distance, then connect the HRU to the nearest cell
        if(count.eq.0) then
          min_dist = 1000000.
          do i=1,grid_nrow
            do j=1,grid_ncol
              if(gw_cell_status(i,j).eq.1) then
                dist_x = abs(utmx_hru(k) - utmx_cell(i,j))
                dist_y = abs(utmy_hru(k) - utmy_cell(i,j))
                distance = sqrt((dist_x)**2 + (dist_y)**2) !distance between HRU and grid cell
                if(distance.lt.min_dist) then
                  min_dist = distance
                  hru_cell_row = i
                  hru_cell_col = j
                endif
              endif
            enddo
          enddo
          hru_num_cells(k) = 1
          hru_cells(k,1,1) = hru_cell_row
          hru_cells(k,1,2) = hru_cell_col
        endif
        completed_fraction = real(k)/real(num_hru) !write out progress to screen
        if(completed_fraction.gt.fraction_target) then
          !print *, '   HRU fraction complete:',fraction_target
          fraction_target = fraction_target + 0.10
        endif
      enddo
      
      !determine which cells are now connected to HRUs
      allocate(cell_connected(grid_nrow,grid_ncol))
      cell_connected = 0
      do k=1,num_hru
        do i=1,hru_num_cells(k)
          cell_connected(hru_cells(k,i,1),hru_cells(k,i,2)) = 1
        enddo
      enddo
      
      !now: find cells that are not yet connected to an HRU, and connect them to the nearest HRU
      fraction_target = 0.10
      cell_count = 1
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
          if(cell_connected(i,j).eq.0) then !only proceed for cells not yet connected to HRUs
            !loop through the HRUs
            min_dist = 1000000.
            do k=1,num_hru
              dist_x = abs(utmx_hru(k) - utmx_cell(i,j))
              dist_y = abs(utmy_hru(k) - utmy_cell(i,j))
              distance = sqrt((dist_x)**2 + (dist_y)**2) !distance between grid cell and HRU
              if(distance.lt.min_dist) then
                min_dist = distance
                hru_nearest = k
              endif
            enddo
            hru_num_cells(hru_nearest) = hru_num_cells(hru_nearest) + 1
            hru_cells(hru_nearest,hru_num_cells(hru_nearest),1) = i !add to the list of cells
            hru_cells(hru_nearest,hru_num_cells(hru_nearest),2) = j
            !write out progress to screen
            completed_fraction = real(cell_count)/real(num_active)
            if(completed_fraction.gt.fraction_target) then
              !print *, '   Grid cell fraction complete:',fraction_target
              fraction_target = fraction_target + 0.10
            endif
          endif
          cell_count = cell_count + 1
          endif
        enddo
      enddo
      
      !write out the list of cells connected to each HRU
      open(out_gwconnect,file='gwflow_connect_hru_cells')
      write(out_gwconnect,*) 'HRU, number of connected grid cells'
      do k=1,num_hru
        write(out_gwconnect,*) k,hru_num_cells(k)  
        do i=1,hru_num_cells(k)
          write(out_gwconnect,*) hru_cells(k,i,1),hru_cells(k,i,2)
        enddo
      enddo
      
      

      
      !initialize groundwater balance ---------------------------------------------------------------------------------------------------------------
      
      !open file to track daily groundwater water balance
      open(out_gwbal,file='gwflow_balance_day')
      write(out_gwbal,*) 'watershed area (m2):',watershed_area
      write(out_gwbal,*) 'timestep,volume_before,volume_after,rech,et,gw-->sw,sw-->gw,satex,boundary,pump,tile,error, &
	fraction sat,wt_depth_avg'

      !open file to track yearly groundwater water balance
      open(out_gwbal_yr,file='gwflow_balance_yr')
      write(out_gwbal_yr,*) 'storage_change,rech,et,gw-->sw,sw-->gw,satex,boundary,pump'
      vol_change_yr = 0.
      ss_rech_yr = 0.
      ss_et_yr = 0.
      ss_gw_yr = 0.
      ss_sw_yr = 0.
      ss_satex_yr = 0.
      ss_Q_yr = 0.
      ss_pump_yr = 0.
      
      !open file to write out average annual groundwater water balance
      open(out_gwbal_aa,file='gwflow_balance_aa')
      write(out_gwbal_aa,*) 'storage_change,rech,et,gw-->sw,sw-->gw,satex,boundary,pump'
      vol_change_total = 0.
      ss_rech_total = 0.
      ss_et_total = 0.
      ss_gw_total = 0.
      ss_sw_total = 0.
      ss_satex_total = 0.
      ss_Q_total = 0.
      ss_pump_total = 0.
      ss_tile_total = 0.
      
      
      
      
      !prepare files for writing recharge, groundwater ET, and GW-SW exchange -----------------------------------------------------------------------
      open(out_gw_rech,file='gwflow_recharge')
      write(out_gw_rech,*) 'Annual groundwater recharge rates (m3/day)'
      allocate(gwflow_rech_sum(grid_nrow,grid_ncol))
      allocate(ss_rech_cell_total(grid_nrow,grid_ncol))
      gwflow_rech_sum = 0.
      ss_rech_cell_total = 0.
      if(gw_et_flag.eq.1) then
        open(out_gw_et,file='gwflow_gwet')
        write(out_gw_et,*) 'Annual groundwater ET rates (m3/day)'
        allocate(gwflow_et_sum(grid_nrow,grid_ncol))
        allocate(ss_et_cell_total(grid_nrow,grid_ncol))
        gwflow_et_sum = 0.
        ss_et_cell_total = 0.
      endif
      open(out_gwsw,file='gwflow_gwsw')
      write(out_gwsw,*) 'Annual Groundwater-Surface Water Exchange rates (m3/day)'
      allocate(gwflow_gwsw_sum(grid_nrow,grid_ncol))
      allocate(ss_gwsw_cell_total(grid_nrow,grid_ncol))
      gwflow_gwsw_sum = 0.
      ss_gwsw_cell_total = 0.
      open(out_lateral,file='gwflow_lateral')
      write(out_lateral,*) 'Annual Lateral Flow rates (m3/day)'
      allocate(gwflow_lateral_sum(grid_nrow,grid_ncol))
      gwflow_lateral_sum = 0.
      open(out_gw_etact,file='gwflow_et')
      write(out_gw_etact,*) 'Annual ET rates excluding groundater ET (m3/day)'
      allocate(gwflow_etact_sum(grid_nrow,grid_ncol))
      allocate(ss_etact_cell_total(grid_nrow,grid_ncol))
      gwflow_etact_sum = 0.
      ss_etact_cell_total = 0.

      
      

      !prepare files for writing groundwater-surface water interaction ------------------------------------------------------------------------------ 
      
      !open files to store gw-sw exchange rates (for each channel)      
      open(out_gwsw_chan,file='gwflow_gwsw_chanQ')
      write(out_gwsw_chan,*) 'Information for gw/sw flow rate exchange: each chandeg channel'
      write(out_gwsw_chan,*) 'Positive value = stream seepage to groundwater'
      write(out_gwsw_chan,*) 'Negative value = groundwater discharge to stream'
      allocate(chan_num_array(sp_ob%chandeg))
      allocate(chan_gis_array(sp_ob%chandeg))
      allocate(chan_Q(sp_ob%chandeg))
      ob_num = sp_ob1%chandeg  !object number of first chandeg channel
      do i=1,sp_ob%chandeg
        chan_num_array(i) = ob(ob_num)%num
        chan_gis_array(i) = ob(ob_num)%gis_id
        ob_num = ob_num + 1
      enddo
      write(out_gwsw_chan,103) (chan_num_array(i),i=1,sp_ob%chandeg)
      write(out_gwsw_chan,103) (chan_gis_array(i),i=1,sp_ob%chandeg)
      
      !open file to write out channel flow rates (m3/s)
      open(out_gw_chan,file='gwflow_chanQ')
      write(out_gw_chan,103) (chan_num_array(i),i=1,sp_ob%chandeg)
      write(out_gw_chan,103) (chan_gis_array(i),i=1,sp_ob%chandeg)
      allocate(channel_flow(sp_ob%chandeg))
      
      
      
      
      !prepare saturation excess routing ------------------------------------------------------------------------------------------------------------
      
      !for each grid cell: find the nearest River Cell (when saturation excess flow occurs, the water is routed to the River Cell; 
      !the water is then added to the stream channel that is connected to the River Cell)
      if(gw_satexcess_flag.eq.1) then
      allocate(gw_cell_rivcell(grid_nrow,grid_ncol))
      open(out_rivconnect,file='gwflow_connect_riv_cells')
      write(out_rivconnect,*) 'Grid cells connection to River Cells'
      write(out_rivconnect,*) 'Row Column Distance RiverCellID'
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
          !loop through the River Cells
          min_dist = 1000000.
          do k=1,num_rivcells
            dist_x = (gw_riv_row(k) - i)
            dist_y = (gw_riv_col(k) - j)
            distance = sqrt((dist_x)**2 + (dist_y)**2)
            if(distance.lt.min_dist) then
              min_dist = distance
              riv_num = k
            endif
          enddo
          gw_cell_rivcell(i,j) = riv_num
          write(out_rivconnect,*) i,j,min_dist,riv_num
          endif
        enddo
      enddo
      open(out_gw_satex,file='gwflow_satex')
      write(out_gw_satex,*) 'Annual groundwater saturation excess flows (m3/day)'
      allocate(gwflow_satex_sum(grid_nrow,grid_ncol))
      allocate(ss_satex_cell_total(grid_nrow,grid_ncol))
      gwflow_satex_sum = 0.
      ss_satex_cell_total = 0.
      endif
      
      
      
      
      !to start the simulation, set gw head values to the initial head values -----------------------------------------------------------------------
      gw_cell_head = gw_cell_inithead
      
      !write out initial heads
      open(out_gwheads,file='gwflow_heads')
      write(out_gwheads,*) 'Initial head values'
      do i=1,grid_nrow
        write(out_gwheads,101) (gw_cell_head(i,j),j=1,grid_ncol)
      enddo
      write(out_gwheads,*)

      
      return
      
100   format(i6,i6,10(f10.2))
101   format(1000(f12.4))
102   format(1000(i4))
103   format(10000(i8))
111   format (1x,a, 5x,"Time",2x,i2,":",i2,":",i2)

      end subroutine gwflow_read
      
      
      
      
      
      !this subroutine returns UTM coordinates (northing, easting) given LAT/LONG coordinates -------------------------------------------------------
      subroutine ll2utm(lat_deg,long_deg,utmx,utmy)
      
      implicit none
      real     pi,sin1,lat_deg,long_deg,utmx,utmy
      real     a,b,f,f_inv,rm,k0,e,e2,n,A0,B0,C0,D0,E0
      real     long_zone,long_zone_cm,delta_long,lat_rad,long_rad,r_curve_1,r_curve_2,mer_arc,K1,K2,K3,K4,K5,A6,northing_raw
      
      !calculation constants
      pi = 3.14159265359
      sin1 = 0.0000048481368
      
      !datum constants
      a = 6378135
      b = 6356751
      f = 0.003353
      f_inv = 298.2597
      rm = 6367434
      k0 = 0.9996
      e = sqrt(1 - (b / a)**2)
      e2 = (e * e) / (1 - (e * e))
      n = (a - b) / (a + b)

      !meridional arc constants
      A0 = a * (1 - n + (5 * n * (n / 4)) * (1 - n) + (81 * ((n)**4) / 64) * (1 - n))
      B0 = (3 * a * (n / 2)) * (1 - n - (7 * n * (n / 8)) * (1 - n) + 55 * (n)**4 / 64)
      C0 = (15 * a * n * n / 16) * (1 - n + (3 * n * n / 4) * (1 - n))
      D0 = (35 * a * (n)**3 / 48) * (1 - n + 11 * n * n / 16)
      E0 = (315 * a * (n)**4 / 51) * (1 - n)
     
      !convert lat/long to UTM coordinates
      long_zone = 31 + int(long_deg/6) - 1
      long_zone_cm = 6 * long_zone - 183
      delta_long = (long_deg - long_zone_cm) * 3600. / 10000.
      lat_rad = lat_deg * (pi / 180.)
      long_rad = long_deg * (pi / 180.)
      r_curve_1 = a * (1 - (e * e)) / ((1 - (e * Sin(lat_rad))**2)**(1.5))
      r_curve_2 = a / ((1 - (e * Sin(lat_rad))**2)**(0.5))
      mer_arc = A0 * lat_rad - B0 * Sin(2 * lat_rad) + C0 * Sin(4 * lat_rad) - D0 * Sin(6 * lat_rad) + E0 * Sin(8 * lat_rad)
      K1 = mer_arc * k0
      K2 = r_curve_2 * Sin(lat_rad) * Cos(lat_rad) * (sin1)**2 * k0 * (100000000) / 2
      K3 = (((sin1)**4 * r_curve_2 * Sin(lat_rad) * (Cos(lat_rad))**3) / 24) * (5 - (Tan(lat_rad))**2 + 9 * e2 * &
	(Cos(lat_rad))**2 + 4 * (e2)**2 * (Cos(lat_rad))**4) * k0 * (1E+16)
      K4 = r_curve_2 * Cos(lat_rad) * sin1 * k0 * 10000
      K5 = (sin1 * Cos(lat_rad))**3 * (r_curve_2 / 6) * (1 - Tan(lat_rad)**2 + e2 * Cos(lat_rad)**2) * k0 * (1000000000000.)
      A6 = ((delta_long * sin1)**6 * r_curve_2 * Sin(lat_rad) * Cos(lat_rad)**5 / 720) * (61 - 58 * Tan(lat_rad)**2 + &
	Tan(lat_rad)**4 + 270 * e2 * Cos(lat_rad)**2 - 330 * e2 * Sin(lat_rad)**2) * k0 * (1E+24)
      northing_raw = (K1 + K2 * delta_long * delta_long + K3 * (delta_long)**4)
      
      !northing
      if(northing_raw.lt.0) then
        utmy = 10000000 + northing_raw
      else
        utmy = northing_raw
      endif
        
      !easting
      utmx = 500000 + (K4 * delta_long + K5 * (delta_long)**3)
      
      return
      end
      
      
