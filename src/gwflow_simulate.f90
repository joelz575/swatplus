      !This subroutine performs the following operations:
      !  1. Convert SWAT+ variables to grid-based variables
      !  2. Calculate new groundwater head for each grid cell     
      !  3. Transfer groundwater-surface water exchange rates to SWAT+
      !  4. Write out results (heads, groundwater balance, gw-sw exchange rates)
  
      !  Prepared by: Ryan Bailey, Colorado State University
      !  January-April 2020
  
      subroutine gwflow_simulate
      
      use gwflow_module
      use hydrograph_module
      use hru_module
      use sd_channel_module
      use time_module
      
      implicit none
      
      integer  i,j,k,n,ob_num,ob_num_aqu,num_hru,row,col,chan_num,chan_gis,num_ts,dum,chan_ob_num
      integer  ts_decrease
      integer  riv_num,riv_ob_num,count,cell_row,cell_col
      real     max_ts,thick
      real     recharge
      real     max_gwet,et_surface,et_bottom,gw_head,gwet,et_volume,et_depth
      real     chan_depth,chan_width,chan_length,chan_stage,flow_area,head_diff,Q
      real     sat_thick,ts_stable
      real     Q_west,Q_east,Q_north,Q_south,face_K,face_sat,gradient,head_change,sat_thick1,sat_thick2
      real     storage_change,ss_rech,ss_et,ss_gw,ss_sw,ss_satex,ss_pump,ss_Q,ss_tile,mass_error,sum_ss
      real     gw_cell_Q_total,gw_cell_ss_rech_total,gw_cell_ss_et_total,gw_cell_ss_gwsw_total,gw_cell_ss_swgw_total,gw_cell_ss_satex_total,gw_cell_ss_pump_total,gw_cell_ss_tile_total
      real     tile_elev,ksat
      real     gwet_volume,rech_volume,cell_rech_volume,sum
      real     satex_depth,satex_volume,frac_sat,depth_wt_avg
      double precision gw_volume_before,gw_volume_after
      

      !number of HRUs
      num_hru = sp_ob%hru

      !calculate the available volume of groundwater in each cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
            gw_avail(i,j) = ((gw_cell_head(i,j)-gw_cell_bot(i,j)) * (cell_size * cell_size)) * gw_cell_Sy(i,j) !m3 of groundwater
          endif
        enddo
      enddo
      
      
      ! 1) groundwater sources and sinks ----------------------------------------------------------------------------------------
      
      !recharge from HRUs
      
      !calculate recharge to the water table, based on the percolation from the soil profile
      do k=1,num_hru
        recharge = gw_rech(k)
        gw_rech(k) = 0.
        gw_rech(k) = ((1.-gw_delay(k))*gwflow_perc(k)) + (gw_delay(k)*recharge)
        if (gw_rech(k) < 1.e-6) gw_rech(k) = 0.
      enddo
      
      !map the recharge from the HRUs to the grid cells
      ob_num = sp_ob1%hru  !object number of first HRU
      do k=1,num_hru
        rech_volume = (gw_rech(k)/1000.) * (ob(ob_num)%area_ha * 10000.) !m * m2 = m3 
        cell_rech_volume = rech_volume / hru_num_cells(k)
        do i=1,hru_num_cells(k)
          cell_row = hru_cells(k,i,1)
          cell_col = hru_cells(k,i,2)
          gw_cell_ss_rech(cell_row,cell_col) = gw_cell_ss_rech(cell_row,cell_col) + cell_rech_volume
          gwflow_rech_sum(cell_row,cell_col) = gwflow_rech_sum(cell_row,cell_col) + cell_rech_volume
          gw_avail(cell_row,cell_col) = gw_avail(cell_row,cell_col) + cell_rech_volume !update available groundwater in the cell
        enddo
        ob_num = ob_num + 1
      enddo

      
      !actual ET from HRUs (just for writing out --> to analyze ET results)
      ob_num = sp_ob1%hru  !object number of first HRU
      do k=1,num_hru
        et_depth = etactual(k) / hru_num_cells(k)
        et_volume = (et_depth/1000.) * (ob(ob_num)%area_ha * 10000.) !m3 of water
        do i=1,hru_num_cells(k)
          cell_row = hru_cells(k,i,1)
          cell_col = hru_cells(k,i,2)
          gw_cell_ss_etact(cell_row,cell_col) = gw_cell_ss_etact(cell_row,cell_col) + et_volume
          gwflow_etact_sum(cell_row,cell_col) = gwflow_etact_sum(cell_row,cell_col) + et_volume
        enddo
        ob_num = ob_num + 1
      enddo
      
      
      !remaining ET from HRUs
      ob_num = sp_ob1%hru  !object number of first HRU
      if(gw_et_flag.eq.1) then
      do k=1,num_hru
        max_gwet = etremain(k) !maximum ET rate from the water table
        max_gwet = max_gwet / hru_num_cells(k)
        do i=1,hru_num_cells(k)
          cell_row = hru_cells(k,i,1)
          cell_col = hru_cells(k,i,2)
          if(gw_cell_et(cell_row,cell_col).gt.0) then
            gwet = gw_cell_et(cell_row,cell_col)
          else
            et_surface = gw_cell_top(cell_row,cell_col) !ground surface
            et_bottom = et_surface - gw_cell_exdp(cell_row,cell_col) !lower elevation bound for ET to occur
            gw_head = gw_cell_head(cell_row,cell_col)
            gwet = 0.
            if(gw_head.le.et_bottom) then
              gwet = 0. !below the extinction depth
            elseif(gw_head.ge.et_surface) then
              gwet = max_gwet
            else
              if(gw_cell_exdp(cell_row,cell_col).ne.0) then
                gwet = max_gwet * (gw_head - et_bottom) / (et_surface - et_bottom) !vary ET linearly
              else
                gwet = 0.
              endif
            endif
          endif
          gwet_volume = (gwet/1000.) * (ob(ob_num)%area_ha * 10000.) !m3 of groundwater
          !check for available groundwater in the cell - can only remove what is there
          if(gw_cell_head(cell_row,cell_col).gt.gw_cell_bot(cell_row,cell_col)) then
            if(gwet_volume.ge.gw_avail(cell_row,cell_col)) then
              gwet_volume =  gw_avail(cell_row,cell_col)
            endif
          else
            gwet_volume = 0.
          endif
          gw_cell_ss_et(cell_row,cell_col) = gw_cell_ss_et(cell_row,cell_col) + (gwet_volume*(-1)) !(negative --> leaving the aquifer)
          gwflow_et_sum(cell_row,cell_col) = gwflow_et_sum(cell_row,cell_col) + (gwet_volume*(-1))
          gw_avail(cell_row,cell_col) = gw_avail(cell_row,cell_col) - gwet_volume          
        enddo
        ob_num = ob_num + 1
      enddo
      endif
      

      !gw/sw exchange between aquifer and streams; loop through the river cells
      sum = 0.
      chan_Q = 0.
      ob_num_aqu = sp_ob1%gwflow  !object number of first river cell
      do k=1,sp_ob%gwflow
        
        !connected SWAT+ channel
        chan_ob_num = ob(ob_num_aqu)%obj_out(1)
        chan_num = ob(chan_ob_num)%num
        chan_gis = ob(chan_ob_num)%gis_id
        chan_depth = sd_ch(chan_num)%chd !depth (m) of water in channel
        chan_width = sd_ch(chan_num)%chw !width (m) of channel
        
        !characteristics of stream channel in the river cell
        chan_length = gw_riv_len(k) !length (m) of channel
        chan_stage = gw_riv_elev(k) + chan_depth !stage (m) of water in channel
        
        !gw-sw exchange flow area
        flow_area = chan_width * chan_length
        
        !grid row and column of the river cell
        row = gw_riv_row(k)
        col = gw_riv_col(k)
        
        !only proceed if the cell is active and not a boundary; otherwise, it will be included here but not in the head calculations below
        if(gw_cell_status(row,col).eq.1) then 
        
          !calculate flow exchange rate (m3/day)
          !head difference --> head gradient --> flow rate
          gw_head = gw_cell_head(row,col)
          Q = 0.
          if(gw_head.lt.gw_riv_elev(k)) then
            head_diff = chan_depth
            Q =  gw_riv_K(k) * (head_diff / gw_riv_thick(k)) * flow_area !stream leakage (positive Q: entering aquifer)
          elseif (gw_head.gt.chan_stage) then
            head_diff = gw_head - chan_stage
            Q = gw_riv_K(k) * (head_diff / gw_riv_thick(k)) * flow_area * (-1) !gw discharge (negative Q: leaving aquifer)
          elseif (gw_head.gt.gw_riv_elev(k) .and. gw_head.lt.chan_stage) then
            head_diff = chan_stage - gw_head 
            Q = gw_riv_K(k) * (head_diff / gw_riv_thick(k)) * flow_area !stream leakage (positive Q: entering aquifer)
          endif
          
          !if groundwater is leaving the cell - can only remove what is there
          if(Q.lt.0) then
            if((Q*-1).ge.gw_avail(row,col)) then
              Q = gw_avail(row,col) * (-1)
            endif
          endif
          gw_avail(row,col) = gw_avail(row,col) + Q !update available groundwater in the cell

          !track totals and store in source/sink array
          if(Q.gt.0) then
            gw_cell_ss_swgw(row,col) = Q
          else
            gw_cell_ss_gwsw(row,col) = Q
          endif
          chan_Q(chan_num) = chan_Q(chan_num) + Q !store in array for writing out total channel exchange rate
          gwflow_gwsw_sum(row,col) = gwflow_gwsw_sum(row,col) + Q
          
          !store in river cell object (this will be added to the channel object in the "command" subroutine)
          Q = Q * (-1) !positive value now is discharge to stream (adding flow to stream), for SWAT+ channel routing
          ob(ob_num_aqu)%hd(1)%flo = 0
          if(ob(ob_num_aqu)%typ == "gwflow") then
            ob(ob_num_aqu)%hd(1)%flo = Q
          endif
        
        endif !cell status
        ob_num_aqu = ob_num_aqu + 1
      enddo
      write(out_gwsw_chan,104) (chan_Q(i),i=1,sp_ob%chandeg)


      !include groundwater pumping
      ss_pump = 0. !nothing for now...
      
      
      !groundwater discharge to tile drains; loop through the tile cells
      if(gw_tile_flag.eq.1) then
        ob_num_aqu = sp_ob1%gwflow  !object number of first river cell
        do k=1,gw_tile_ncells
          
          !row and column of tile cell
          row = gw_tile_row(k)
          col = gw_tile_col(k)
        
          !only proceed if cell is active
          if(gw_cell_status(row,col).eq.1) then

          !get elevation of the subsurface drain
          tile_elev = gw_cell_top(row,col) - gw_tile_depth
          
          !only proceed if groundwater head is higher than the drain
          if(gw_cell_head(row,col).gt.tile_elev) then 
            
            !calculate flow rate using Darcy's Law
            ksat = gw_cell_K(row,col)
            head_diff = gw_cell_head(row,col) - tile_elev
            Q = gw_tile_cond * ksat * head_diff !m3/day
            
            !check for available groundwater in the cell - can only remove what is there
            if(Q.ge.gw_avail(row,col)) then
              Q = gw_avail(row,col)
            endif
            gw_avail(row,col) = gw_avail(row,col) - Q !update available groundwater in the cell
            
            !add to source/sink arrays
            gw_cell_ss_tile(row,col) = Q * (-1) !leaving aquifer
            gwflow_tile_sum(row,col) = gwflow_tile_sum(row,col) + (Q*(-1)) !leaving aquifer
            
            !add flow to river cell (that will eventually go to the SWAT+ channel)
            riv_num = gw_tilecell_rivcell(k)
            riv_ob_num = ob_num_aqu + riv_num - 1
            ob(riv_ob_num)%hd(1)%flo = ob(riv_ob_num)%hd(1)%flo + Q

          endif
          endif
        enddo
      endif

      
      !calculate total volume of groundwater sources/sinks for each grid cell
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then  
            gw_cell_ss(i,j) = gw_cell_ss_rech(i,j) + gw_cell_ss_et(i,j) + gw_cell_ss_gwsw(i,j) + gw_cell_ss_swgw(i,j) + gw_cell_ss_pump(i,j) + gw_cell_ss_tile(i,j)
          endif
        enddo
      enddo     
      
      
      
      ! 2) calculate new groundwater head for each grid cell ----------------------------------------------------------------------------------------
      
      !compute total volumes at the beginning of the day
      gw_volume_before = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
            gw_volume_before_cell(i,j) = ((gw_cell_head(i,j)-gw_cell_bot(i,j)) * (cell_size * cell_size)) * gw_cell_Sy(i,j)
            gw_volume_before = gw_volume_before + gw_volume_before_cell(i,j)
          endif
        enddo
      enddo
      

      !determine number of time steps
      num_ts = int(1./gw_time_step)
      
      !calculate new head value for each cell
      count = 0
      head_new = 0.
      ss_Q = 0.
      gw_cell_Q = 0.
      do n=1,num_ts
        do i=1,grid_nrow
          do j=1,grid_ncol
            if(gw_cell_status(i,j).gt.0) then !only proceed if the cell is active
              if(gw_cell_status(i,j).eq.1) then !interior cell

                !flow across west face (flow from west --> east)
                if(j.eq.1) then
                  Q_west = 0.
                elseif(gw_cell_status(i,j-1).eq.0) then
                  Q_west = 0
                else
                  face_K = (gw_cell_K(i,j-1) + gw_cell_K(i,j)) / 2. !K at the interface
                  sat_thick1 = gw_cell_head(i,j-1) - gw_cell_bot(i,j-1)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i,j-1) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_west = face_K * gradient * flow_area !Darcy's Law
                  if(Q_west.lt.0) then !check against available groundwater in the cell
                    if((Q_west*-1).ge.gw_avail(i,j)) then
                      Q_west = gw_avail(i,j) * (-1)
                      count = count + 1
                      thick = gw_cell_head(i,j) - gw_cell_bot(i,j)
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_west.ge.gw_avail(i,j-1)) then
                      Q_west = gw_avail(i,j-1)
                      count = count + 1
                      thick = gw_cell_head(i,j-1) - gw_cell_bot(i,j-1)
                    endif
                  endif
                endif
                
                !flow across east face
                if(j.eq.grid_ncol) then
                  Q_east = 0.
                elseif(gw_cell_status(i,j+1).eq.0) then
                  Q_east = 0.
                else
                  face_K = (gw_cell_K(i,j+1) + gw_cell_K(i,j)) / 2. !K at the interface
                  sat_thick1 = gw_cell_head(i,j+1) - gw_cell_bot(i,j+1)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i,j+1) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_east = face_K * gradient * flow_area !Darcy's Law
                  if(Q_east.lt.0) then !check against available groundwater in the cell
                    if((Q_east*-1).ge.gw_avail(i,j)) then
                      Q_east = gw_avail(i,j) * (-1)
                      count = count + 1
                      thick = gw_cell_head(i,j) - gw_cell_bot(i,j)
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_east.ge.gw_avail(i,j+1)) then
                      Q_east = gw_avail(i,j+1)
                      count = count + 1
                      thick = gw_cell_head(i,j+1) - gw_cell_bot(i,j+1)
                    endif
                  endif
                endif
                
                !flow across north face
                if(i.eq.1) then
                  Q_north = 0.
                elseif(gw_cell_status(i-1,j).eq.0) then
                  Q_north = 0.
                else
                  face_K = (gw_cell_K(i-1,j) + gw_cell_K(i,j)) / 2. !K at the interface
                  sat_thick1 = gw_cell_head(i-1,j) - gw_cell_bot(i-1,j)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i-1,j) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_north = face_K * gradient * flow_area !Darcy's Law
                  if(Q_north.lt.0) then !check against available groundwater in the cell
                    if((Q_north*-1).ge.gw_avail(i,j)) then
                      Q_north = gw_avail(i,j) * (-1)
                      count = count + 1
                      thick = gw_cell_head(i,j) - gw_cell_bot(i,j)
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_north.ge.gw_avail(i-1,j)) then
                      Q_north = gw_avail(i-1,j)
                      count = count + 1
                      thick = gw_cell_head(i-1,j) - gw_cell_bot(i-1,j)
                    endif
                  endif
                endif

                !flow across south face
                if(i.eq.grid_nrow) then
                  Q_south = 0.
                elseif(gw_cell_status(i+1,j).eq.0) then
                  Q_south = 0.
                else
                  face_K = (gw_cell_K(i+1,j) + gw_cell_K(i,j)) / 2. !K at the interface
                  sat_thick1 = gw_cell_head(i+1,j) - gw_cell_bot(i+1,j)
                  sat_thick2 = gw_cell_head(i,j) - gw_cell_bot(i,j)
                  face_sat = (sat_thick1 + sat_thick2) / 2.   !saturated thickness at the interface
                  gradient = (gw_cell_head(i+1,j) - gw_cell_head(i,j)) / cell_size
                  flow_area = face_sat * cell_size
                  Q_south = face_K * gradient * flow_area !Darcy's Law
                  if(Q_south.lt.0) then !check against available groundwater in the cell
                    if((Q_south*-1).ge.gw_avail(i,j)) then
                      Q_south = gw_avail(i,j) * (-1)
                      count = count + 1
                      thick = gw_cell_head(i,j) - gw_cell_bot(i,j)
                    endif
                  else !check against available groundwater in adjacent cell that provides the flow
                    if(Q_south.ge.gw_avail(i+1,j)) then
                      Q_south = gw_avail(i+1,j)
                      count = count + 1
                      thick = gw_cell_head(i+1,j) - gw_cell_bot(i+1,j)
                    endif
                  endif
                endif                
                
                !calculate change in head
                head_change = (Q_west + Q_east + Q_north + Q_south + gw_cell_ss(i,j)) * (gw_time_step/(gw_cell_Sy(i,j) * cell_size * cell_size))
                
                !calculate new head value
                head_new(i,j) = gw_cell_head(i,j) + head_change
                
                !store inflow/outflow for each cell
                gw_cell_Q(i,j) = gw_cell_Q(i,j) + ((Q_west + Q_east + Q_north + Q_south)*gw_time_step)
                ss_Q = ss_Q + ((Q_west + Q_east + Q_north + Q_south)*gw_time_step)
                gwflow_lateral_sum(i,j) = gwflow_lateral_sum(i,j) + ((Q_west + Q_east + Q_north + Q_south)*gw_time_step)
                
              elseif(gw_cell_status(i,j).eq.2) then !constant head cell
                head_new(i,j) = gw_cell_inithead(i,j)
              endif
            endif
          enddo !next column
        enddo !next row
        
        !store new head values into regular head array
        gw_cell_head = head_new
        
      enddo !next time step
      

      !simulate groundwater saturation excess flow (routing to nearby streams)
      if(gw_satexcess_flag.eq.1) then
      ob_num_aqu = sp_ob1%gwflow  !object number of first river cell
      count = 0
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            if(gw_cell_head(i,j).gt.gw_cell_top(i,j)) then
            
              !calculate volume of saturation excess flow that is routed to streams
              satex_depth = gw_cell_head(i,j) - gw_cell_top(i,j)
              satex_volume = (cell_size * cell_size * satex_depth) * gw_cell_Sy(i,j) !m3 of groundwater
              count = count + 1
              
              !add water to the River Cell closest to the current cell
              riv_num = gw_cell_rivcell(i,j)
              riv_ob_num = ob_num_aqu + riv_num - 1
              ob(riv_ob_num)%hd(1)%flo = ob(riv_ob_num)%hd(1)%flo + satex_volume

              !sum up for total watershed
              gw_cell_satex(i,j) = satex_volume
              gwflow_satex_sum(i,j) = gwflow_satex_sum(i,j) + satex_volume
              
              !set groundwater head to the ground surface
              gw_cell_head(i,j) = gw_cell_top(i,j)
              
            endif
          endif
        enddo
      enddo
      endif
      frac_sat = real(count) / real(num_active) !calculate the fraction of active grid cells that are fully saturated
      

      !calculate the average depth to water table
      sum = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            sum = sum + (gw_cell_top(i,j) - gw_cell_head(i,j))
          endif
        enddo
      enddo
      depth_wt_avg = sum / num_active
      
      !print out new head values, if requested
      if(gw_output_yr(gw_output_index).eq.time%yrc .and. gw_output_day(gw_output_index).eq.time%day) then
        write(out_gwheads,*) 'Groundwater Head for:',time%yrc,time%day
        do i=1,grid_nrow
          write(out_gwheads,100) (gw_cell_head(i,j),j=1,grid_ncol)
        enddo
        write(out_gwheads,*)
        gw_output_index = gw_output_index + 1
      endif
      
      !print out head values for observation cells (each time step)
      do k=1,gw_num_obs_wells
        gw_obs_head(k) = gw_cell_head(gw_obs_cells_row(k),gw_obs_cells_col(k))
      enddo
      write(out_gwobs,102) time%yrc,time%day,(gw_obs_head(k),k=1,gw_num_obs_wells)
      
      !compute total volumes at the end of the day
      gw_volume_after = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).gt.0) then
            gw_volume_after_cell(i,j) = ((gw_cell_head(i,j)-gw_cell_bot(i,j)) * (cell_size * cell_size)) * gw_cell_Sy(i,j)  
            gw_volume_after = gw_volume_after + gw_volume_after_cell(i,j)
          endif
        enddo
      enddo
      

      !compute change in groundwater storage and mass balance error (as %)
      gw_volume_before = 0.
      gw_volume_after = 0.
      gw_cell_ss_rech_total = 0.
      gw_cell_ss_et_total = 0.
      gw_cell_ss_gwsw_total = 0.
      gw_cell_ss_swgw_total = 0.
      gw_cell_ss_satex_total = 0.
      gw_cell_Q_total = 0.
      gw_cell_ss_pump_total = 0.
      gw_cell_ss_tile_total = 0.
      do i=1,grid_nrow
        do j=1,grid_ncol
          if(gw_cell_status(i,j).eq.1) then
            gw_volume_before = gw_volume_before +  gw_volume_before_cell(i,j)
            gw_volume_after = gw_volume_after +  gw_volume_after_cell(i,j)
            gw_cell_ss_rech_total = gw_cell_ss_rech_total + gw_cell_ss_rech(i,j)
            gw_cell_ss_et_total = gw_cell_ss_et_total + gw_cell_ss_et(i,j)
            gw_cell_ss_gwsw_total = gw_cell_ss_gwsw_total + gw_cell_ss_gwsw(i,j)
            gw_cell_ss_swgw_total = gw_cell_ss_swgw_total + gw_cell_ss_swgw(i,j)
            gw_cell_ss_satex_total = gw_cell_ss_satex_total + gw_cell_satex(i,j)
            gw_cell_Q_total = gw_cell_Q_total + gw_cell_Q(i,j)
            gw_cell_ss_pump_total = gw_cell_ss_pump_total + gw_cell_ss_pump(i,j)
            gw_cell_ss_tile_total = gw_cell_ss_tile_total + gw_cell_ss_tile(i,j)
          endif
        enddo
      enddo
      mass_error = (1-((gw_volume_before + gw_cell_ss_rech_total + gw_cell_ss_et_total + gw_cell_ss_gwsw_total + gw_cell_ss_swgw_total - gw_cell_ss_satex_total + gw_cell_Q_total + gw_cell_ss_pump_total + gw_cell_ss_tile_total)/gw_volume_after)) * 100 
           
      !print out daily information (time step, water balance) in mm (normalized to watershed area)
      gw_volume_before = (gw_volume_before / watershed_area) * 1000. !m3 --> mm of water
      gw_volume_after = (gw_volume_after / watershed_area) * 1000.
      ss_rech = (gw_cell_ss_rech_total / watershed_area) * 1000.
      ss_et = (gw_cell_ss_et_total / watershed_area) * 1000.
      ss_gw = (gw_cell_ss_gwsw_total / watershed_area) * 1000.
      ss_sw = (gw_cell_ss_swgw_total / watershed_area) * 1000.
      ss_satex = (gw_cell_ss_satex_total / watershed_area) * 1000. * (-1) !leaving the aquifer
      ss_Q = (gw_cell_Q_total / watershed_area) * 1000.
      ss_pump = (gw_cell_ss_pump_total / watershed_area) * 1000.
      ss_tile = (gw_cell_ss_tile_total / watershed_area) * 1000.
      write(out_gwbal,100) gw_time_step,gw_volume_before,gw_volume_after,ss_rech,ss_et,ss_gw,ss_sw,ss_satex,ss_Q,ss_pump,ss_tile,mass_error,frac_sat,depth_wt_avg
      
      !add daily water balance volumes to yearly and total values
      vol_change_yr = vol_change_yr + (gw_volume_after-gw_volume_before)
      ss_rech_yr = ss_rech_yr + ss_rech
      ss_et_yr = ss_et_yr + ss_et
      ss_gw_yr = ss_gw_yr + ss_gw
      ss_sw_yr = ss_sw_yr + ss_sw
      ss_satex_yr = ss_satex_yr + ss_satex
      ss_Q_yr = ss_Q_yr + ss_Q
      ss_pump_yr = ss_pump_yr + ss_pump
      ss_tile_yr = ss_tile_yr + ss_tile
      vol_change_total = vol_change_total + (gw_volume_after-gw_volume_before)
      ss_rech_total = ss_rech_total + ss_rech
      ss_et_total = ss_et_total + ss_et
      ss_gw_total = ss_gw_total + ss_gw
      ss_sw_total = ss_sw_total + ss_sw
      ss_satex_total = ss_satex_total + ss_satex
      ss_Q_total = ss_Q_total + ss_Q
      ss_pump_total = ss_pump_total + ss_pump
      ss_tile_total = ss_tile_total + ss_tile
      
      !write out channel flow rates (new output file, to more easily view hydrographs)
      do i=1,sp_ob%chandeg
        channel_flow(i) = ch_out_d(i)%flo        
      enddo
      write(out_gw_chan,104) (channel_flow(i),i=1,sp_ob%chandeg)
      

      !write out recharge, groundwater ET, gwsw rates, and saturation excess flow values if the end of the year is reached
      if(time%day == 365) then
        !recharge
        gwflow_rech_sum = gwflow_rech_sum / 365.
        write(out_gw_rech,*) 'Recharge for year:',time%yrc
        do i=1,grid_nrow
          write(out_gw_rech,101) (gwflow_rech_sum(i,j),j=1,grid_ncol)
        enddo
        gwflow_rech_sum = 0.
        write(out_gw_rech,*)
        !groundwater ET
        if(gw_et_flag.eq.1) then
        gwflow_et_sum = gwflow_et_sum / 365.
        write(out_gw_et,*) 'Groundwater ET for year:',time%yrc
        do i=1,grid_nrow
          write(out_gw_et,101) (gwflow_et_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_et_sum = 0
        write(out_gw_et,*)
        endif
        !actual ET
        gwflow_etact_sum = gwflow_etact_sum / 365.
        write(out_gw_etact,*) 'ET (excluding groundwater ET) for year:',time%yrc
        do i=1,grid_nrow
          write(out_gw_etact,101) (gwflow_etact_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_etact_sum = 0
        write(out_gw_etact,*)
        !gw-sw exchange rates
        gwflow_gwsw_sum = gwflow_gwsw_sum / 365.
        write(out_gwsw,*) 'GW-SW Exchange Rates for year:',time%yrc
        do i=1,grid_nrow
          write(out_gwsw,101) (gwflow_gwsw_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_gwsw_sum = 0
        write(out_gwsw,*)
        !saturation excess flow
        if(gw_satexcess_flag.eq.1) then
        gwflow_satex_sum = gwflow_satex_sum / 365.
        write(out_gw_satex,*) 'Saturation Excess Volumes for:',time%yrc
        do i=1,grid_nrow
          write(out_gw_satex,101) (gwflow_satex_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_satex_sum = 0
        write(out_gw_satex,*)
        endif
        !lateral flow
        gwflow_lateral_sum = gwflow_lateral_sum / 365.
        write(out_lateral,*) 'Lateral flow for year:',time%yrc
        do i=1,grid_nrow
          write(out_lateral,101) (gwflow_lateral_sum(i,j),j=1,grid_ncol)
        enddo
        gwflow_lateral_sum = 0.
        write(out_lateral,*)
        !tile drain flow
        if(gw_tile_flag.eq.1) then
        gwflow_tile_sum = gwflow_tile_sum / 365.
        write(out_gw_tile,*) 'Tile Drain Outflow Volumes for:',time%yrc
        do i=1,grid_nrow
          write(out_gw_tile,101) (gwflow_tile_sum(i,j),j=1,grid_ncol)
        enddo  
        gwflow_tile_sum = 0
        write(out_gw_tile,*)
        endif
        !yearly water balance
        write(out_gwbal_yr,105) time%yrc,vol_change_yr,ss_rech_yr,ss_et_yr,ss_gw_yr,ss_sw_yr,ss_satex_yr,ss_Q_yr,ss_pump_yr,ss_tile_yr
        vol_change_yr = 0.
        ss_rech_yr = 0.
        ss_et_yr = 0.
        ss_gw_yr = 0.
        ss_sw_yr = 0.
        ss_satex_yr = 0.
        ss_Q_yr = 0.
        ss_pump_yr = 0.
        ss_tile_yr = 0.
        
      endif
      
      
      !add to the total source/sinks arrays - print out if last day of the simulation is reached
      ss_rech_cell_total = ss_rech_cell_total + gw_cell_ss_rech
      ss_et_cell_total = ss_et_cell_total + gw_cell_ss_et
      ss_gwsw_cell_total = ss_gwsw_cell_total + gw_cell_ss_gwsw
      ss_satex_cell_total = ss_satex_cell_total + gw_cell_satex
      ss_etact_cell_total = ss_etact_cell_total + gw_cell_ss_etact
      ss_tile_cell_total = ss_tile_cell_total + gw_cell_ss_tile
      if(time%yrc == time%yrc_end .and. time%day == time%day_end) then
        write(out_gw_rech,*) 'Total for entire Simulation'
        write(out_gw_et,*) 'Total for entire Simulation'
        write(out_gwsw,*) 'Total for entire Simulation'
        write(out_gw_satex,*) 'Total for entire Simulation'
        write(out_gw_etact,*) 'Total for entire Simulation'
        write(out_gw_tile,*) 'Total for entire Simulation'
        do i=1,grid_nrow
          write(out_gw_rech,101) (ss_rech_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_et,101) (ss_et_cell_total(i,j),j=1,grid_ncol)
          write(out_gwsw,101) (ss_gwsw_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_satex,101) (ss_satex_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_etact,101) (ss_etact_cell_total(i,j),j=1,grid_ncol)
          write(out_gw_tile,101) (ss_tile_cell_total(i,j),j=1,grid_ncol)
        enddo 
        
        !average annual water balance
        vol_change_total = vol_change_total + (gw_volume_after-gw_volume_before)
        ss_rech_total = ss_rech_total / time%nbyr
        ss_et_total = ss_et_total / time%nbyr
        ss_gw_total = ss_gw_total / time%nbyr
        ss_sw_total = ss_sw_total / time%nbyr
        ss_satex_total = ss_satex_total / time%nbyr
        ss_Q_total = ss_Q_total / time%nbyr
        ss_pump_total = ss_pump_total / time%nbyr
        ss_tile_total = ss_tile_total / time%nbyr
        write(out_gwbal_aa,105) time%yrc,vol_change_total,ss_rech_total,ss_et_total,ss_gw_total,ss_sw_total,ss_satex_total,ss_Q_total,ss_pump_total,ss_tile_total
        
      endif
      
      
      !zero out arrays for next day
      gw_cell_ss_rech = 0.
      gw_cell_ss_et = 0.
      gw_cell_ss_gwsw = 0.
      gw_cell_ss_swgw = 0.
      gw_cell_satex = 0.
      gw_cell_ss_pump = 0.
      gw_cell_ss_tile = 0.
      gw_cell_ss = 0.
      gw_cell_Q = 0.

      
100   format(1000(f12.4))
101   format(1000(e12.4))
102   format(i8,i8,1000(f12.4))
103   format(i8,i8,i8,i8,i8,i8,i8,50(f15.3))
104   format(10000(f12.2))
105   format(i8,50(f12.4))


      return
      end subroutine gwflow_simulate
      
      
      
      
