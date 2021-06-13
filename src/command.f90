      subroutine command
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    for every day of simulation, this subroutine steps through the command
!!    lines in the watershed configuration (.fig) file. Depending on the 
!!    command code on the .fig file line, a command loop is accessed
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: subbasin, route, routres, transfer, recmon
!!    SWAT: recepic, save, recday, recyear

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use hydrograph_module
      use ru_module
      use channel_module
      use hru_lte_module
      use aquifer_module
      use sd_channel_module
      use reservoir_module
      use organic_mineral_mass_module
      use constituent_mass_module
      use hru_module, only : ihru, hru
      use basin_module
      use maximum_data_module
      use output_landscape_module, only : hnb_d
      use gwflow_module
      
      implicit none

      real, dimension(time%step) :: hyd_flo     !flow hydrograph
      integer :: in                   !              |
      integer :: ielem                !              |  
      integer :: iob                  !              |
      integer :: kk                   !none          |counter
      integer :: iday                 !              |
      integer :: isd                  !none          |counter
      integer :: ires                 !none          |reservoir number
      integer :: irec                 !              |
      integer :: iout                 !none          |counter
      integer :: ihtyp                !              |
      integer :: iaq                  !none          |counter
      integer :: j                    !none          |counter
      integer :: ihyd                 !              |
      integer :: idr                  !              |
      integer :: ifirst               !              |
      integer :: iwro                 !              |
      integer :: ob_num               !              |
      real :: conv                    !              |
      real :: frac_in                 !              |
      integer dum
      integer :: i_mfl !rtb gwflow; counter
      real :: sum
            
      sum = 0.

      icmd = sp_ob1%objs
      do while (icmd /= 0)
        !subdaily - set current day of hydrograph
        if (time%step > 0) then
          !update current day of hydrograph for the object
          ob(icmd)%day_cur = ob(icmd)%day_cur + 1
          if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
        end if
        
        !sum all receiving hydrographs
        !if (ob(icmd)%rcv_tot > 0) then
          ob(icmd)%hin = hz
          ob(icmd)%hin_sur = hz
          ob(icmd)%hin_lat = hz
          ob(icmd)%hin_til = hz
          ht1 = hz
          obcs(icmd)%hin = hin_csz
          obcs(icmd)%hin_sur = hin_csz
          obcs(icmd)%hin_lat = hin_csz
          obcs(icmd)%hin_til = hin_csz
          hcs1 = hin_csz
          hcs2 = hin_csz
          if (time%step > 0) ob(icmd)%tsin = 0.
          ob(icmd)%peakrate = 0.
          
          if (ob(icmd)%rcv_tot > 0) then
          do in = 1, ob(icmd)%rcv_tot
            iob = ob(icmd)%obj_in(in)
            ihyd = ob(icmd)%ihtyp_in(in)
            frac_in = ob(icmd)%frac_in(in)
            ob(icmd)%peakrate = ob(iob)%peakrate
            
            ! if object is not an hru, need ht1, don't need %hin_sur and %hin_lat
            ! don't have to check if it's in an ru - only hru's can be routed over
            if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "hru_lte") then
              ! recieving hru, needs %hin_sur and %hin_lat and %hin_til to route separately in hru_control
              if (ob(icmd)%htyp_in(in) == "tot") then
                ! if total hyd coming in from hru or ru -> add both surface and lateral flows
                ! add to surface runon
                ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(3)
                if (cs_db%num_tot > 0) then
                  obcs(icmd)%hin_sur = obcs(icmd)%hin_sur + frac_in * obcs(iob)%hd(3)
                end if
                ! add to lateral soil runon
                ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(4)
                if (cs_db%num_tot > 0) then
                  obcs(icmd)%hin_lat = obcs(icmd)%hin_lat + frac_in * obcs(iob)%hd(4)
                end if
              else
                ! if hyd in is not a total hyd from an hru or ru -> add the specified hyd typ 
                select case (ob(icmd)%htyp_in(in))
                case ("tot")   ! total flow
                  ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_til = obcs(icmd)%hin_til + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("sur")   ! surface runoff
                  ob(icmd)%hin_sur = ob(icmd)%hin_sur + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_sur = obcs(icmd)%hin_sur + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("lat")   ! lateral soil flow
                  ob(icmd)%hin_lat = ob(icmd)%hin_lat + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_lat = obcs(icmd)%hin_lat + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("til")   ! tile flow
                  ob(icmd)%hin_til = ob(icmd)%hin_til + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_til = obcs(icmd)%hin_til + frac_in * obcs(iob)%hd(ihyd)
                  end if
                case ("aqu")   ! aquifer inflow
                  ob(icmd)%hin_aqu = ob(icmd)%hin_aqu + frac_in * ob(iob)%hd(ihyd)
                  !add constituents
                  if (cs_db%num_tot > 0) then
                    obcs(icmd)%hin_aqu = obcs(icmd)%hin_aqu + frac_in * obcs(iob)%hd(ihyd)
                  end if
                end select
              end if
              
            else
              ! all objects other than hru's
              ! fraction of organics
              ht1 = frac_in * ob(iob)%hd(ihyd)
              ob(icmd)%hin = ob(icmd)%hin + ht1
              ! fraction of constituents
              if (cs_db%num_tot > 0) then
                hcs1 = frac_in * obcs(iob)%hd(ihyd)
                obcs(icmd)%hin = obcs(icmd)%hin + hcs1
              end if
              ob(icmd)%hin_d(in) = ht1        !for hydrograph output
              obcs(icmd)%hcsin_d(in) = hcs1   !for constituent hydrograph output
            end if
            
            !sum subdaily hydrographs
            if (time%step > 0) then
              if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru") then
                select case (ob(icmd)%htyp_in(in))
                case ("tot")   ! total flow
                  hyd_flo = ob(iob)%hyd_flo(iday,:) + ob(iob)%lat_til_flo / time%step
                case ("sur")   ! surface runoff
                  hyd_flo(:) = ob(iob)%hyd_flo(iday,:)
                case ("lat")   ! lateral soil flow
                  hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                case ("til")   ! tile flow
                  hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                case ("aqu")   ! aquifer inflow
                  hyd_flo(:) = ob(iob)%hd(ihyd)%flo / time%step
                end select
              end if
              if (ob(icmd)%typ == "res" .or. ob(icmd)%typ == "sdc") then
                
              end if
            end if
            !  iday = ob(iob)%day_cur
            !  !! iob = inflow object number
            !  if (ob(icmd)%frac_in(ihyd) < .999) then
            !    if (ob(icmd)%obtyp_in(ihyd) == "hru" .or. ob(icmd)%obtyp_in(ihyd) == "ru") then
            !      !! if fraction of an hru/ru - need to calc the flow hydrograph each day
            !      call flow_hyd_ru_hru (ob(iob)%day_cur, ob(iob)%hd(3)%flo, ob(iob)%hd(4)%flo,     &
            !            ob(iob)%hd(5)%flo, ob(icmd)%hin_uh(ihyd)%uh, ob(icmd)%hin_uh(ihyd)%hyd_flo)
            !      hyd_flo = ob(icmd)%hin_uh(ihyd)%hyd_flo(iday,:)
            !    else
            !      !! if entire hru/ru or other object - use the flow hydrograph of the entire object
            !      hyd_flo = ob(iob)%hyd_flo(iday,:)
            !    end if
            !  else
            !    !! if fraction in is 1.0 - always use the flow hydrograph of the entire object
            !    hyd_flo = ob(iob)%hyd_flo(iday,:)
            !  end if
            !  !! add flow hydrographs for each incoming object
            !  ob(icmd)%tsin = ob(icmd)%tsin + hyd_flo
            !end if

          end do    ! in = 1, ob(icmd)%rcv_tot

          !convert to per area basis
          if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "ru") then  !only convert hru and subbasin hyds for routing
            !if (ob(icmd)%ru_tot > 0) then
            !  !object is in a subbasin
            !  ielem = ob(icmd)%elem
            !  iru = ob(icmd)%ru(1)  !can only be in one subbasin if routing over
            !  conv = 100. * ru(iru)%da_km2  !* ru_elem(ielem)%frac
            !else
              conv = ob(icmd)%area_ha
            !end if
            ob(icmd)%hin_sur = ob(icmd)%hin_sur // conv
            ob(icmd)%hin_lat = ob(icmd)%hin_lat // conv
            ob(icmd)%hin_til = ob(icmd)%hin_til // conv
          end if
        end if

        ! select the next command type

        select case (ob(icmd)%typ)
            
          case ("hru")   ! hru
            ihru = ob(icmd)%num
            call hru_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output
                      
          case ("hru_lte")   ! hru_lte
            isd = ob(icmd)%num
            call hru_lte_control (isd)
            !if (ob(icmd)%rcv_tot > 0) call hyddep_output
            
          case ("ru")   ! subbasin
            iru = ob(icmd)%num
            call ru_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output

          case ("gwflow")   ! gwflow
            call gwflow_simulate
            do i_mfl = 1,sp_ob%gwflow
              icmd = icmd + 1
            enddo
            icmd = icmd - 1
            
          case ("aqu")   ! aquifer
            if (ob(icmd)%dfn_tot == 0) then   !1-D use old bf recession
              call aqu_1d_control
            end if
          
          case ("chan")   ! channel
            jrch = ob(icmd)%num
            jrchq = ob(icmd)%props2
            if (ob(icmd)%rcv_tot > 0) then
              call channel_control
            end if

          case ("res")   ! reservoir
            ires = ob(icmd)%num
            if (ob(icmd)%rcv_tot > 0) then
              call res_control (ires)
            end if 
              
          case ("recall")   ! recall hydrograph
            irec = ob(icmd)%num
            select case (recall(irec)%typ)
              case (0)    !subdaily
                ob(icmd)%hyd_flo(ob(icmd)%day_cur,:) = recall(irec)%hyd_flo(1:time%step,time%yrs)
              case (1)    !daily
                if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
                    ob(icmd)%hd(1) = recall(irec)%hd(time%day,time%yrs)
                else
                    ob(icmd)%hd(1) = hz
                end if
              case (2)    !monthly
                if (time%yrc >= recall(irec)%start_yr .and. time%yrc <= recall(irec)%end_yr) then 
                    ob(icmd)%hd(1) = recall(irec)%hd(time%mo,time%yrs)
                else
                    ob(icmd)%hd(1) = hz
                end if
              case (3)    !annual
                if (time%yrc >= recall(irec)%start_yr .or. time%yrc <= recall(irec)%end_yr) then
                  ob(icmd)%hd(1) = recall(irec)%hd(1,time%yrs)
                else
                  ob(icmd)%hd(1) = hz
                end if
              case (4)    !average annual
                ob(icmd)%hd(1) = recall(irec)%hd(1,1)
              end select
              
              rec_d(irec) = ob(icmd)%hd(1)

              if (cs_db%num_tot > 0) then
                obcs(icmd)%hd(1) = hin_csz
              end if

          !case ("exco")   ! export coefficient hyds are set at start

          case ("dr")   ! delivery ratios
            ob(icmd)%hd(1) = ob(icmd)%hin ** dr(ob(icmd)%props) ! ** is an intrinsic function to multiply 
            if (cs_db%num_tot > 0) then
              idr = ob(iob)%props
              
              call constit_hyd_mult (icmd, idr)
            end if
            
          case ("outlet")  !outlet
            ob(icmd)%hd(1) = ob(icmd)%hin
              
          case ("chandeg")  !swatdeg channel
            isdch = ob(icmd)%num
            isd_chsur = ob(icmd)%props2
            if (sd_ch(isdch)%chl > 1.e-3) then
              call sd_channel_control
            else
                !! artificial channel - length=0 - no transformations
                ob(icmd)%hd(1) = ob(icmd)%hin
                
                ch_in_d(isdch) = ht1                        !set inflow om hydrograph
                chsd_d(isdch)%flo_in = ht1%flo / 86400.     !flow for morphology output
                ch_in_d(isdch)%flo = ht1%flo / 86400.       !flow for om output
                ch_out_d(isdch) = ht1                       !set inflow om hydrograph
                ch_out_d(isdch)%flo = ht1%flo / 86400.      !m3 -> m3/s
                !! output channel morphology
                chsd_d(isdch)%flo = ht1%flo / 86400.        !adjust if overbank flooding is moved to landscape
                chsd_d(isdch)%peakr = 0. 
                chsd_d(isdch)%sed_in = ob(icmd)%hin%sed
                chsd_d(isdch)%sed_out = ob(icmd)%hin%sed
                chsd_d(isdch)%washld = 0.
                chsd_d(isdch)%bedld = 0.
                chsd_d(isdch)%dep = 0.
                chsd_d(isdch)%deg_btm = .0
                chsd_d(isdch)%deg_bank = 0.
                chsd_d(isdch)%hc_sed = 0.
                chsd_d(isdch)%width = sd_ch(isdch)%chw
                chsd_d(isdch)%depth = sd_ch(isdch)%chd
                chsd_d(isdch)%slope = sd_ch(isdch)%chs
                chsd_d(isdch)%deg_btm_m = 0.
                chsd_d(isdch)%deg_bank_m = 0.
                chsd_d(isdch)%hc_m = 0.
                if (cs_db%num_tot > 0) then
                  obcs(icmd)%hd(1) = obcs(icmd)%hin
                end if
            end if
            
          end select
        if (pco%fdcout == "y" .and. ob(icmd)%typ == "chandeg") call flow_dur_curve
        
        !print all outflow hydrographs
        if (ob(icmd)%src_tot > 0) then
          do iout = 1, ob(icmd)%src_tot
            ihtyp = ob(icmd)%ihtyp_out(iout)
            ht1 = ob(icmd)%frac_out(iout) * ob(icmd)%hd(ihtyp)
            call hydout_output (iout)
            if (cs_db%num_tot > 0) then
              ! hcs1 is the daily constituent hyd to be printed
              hcs1 =  ob(icmd)%frac_out(iout) * obcs(icmd)%hd(ihtyp)
            end if
          end do
        end if
  
        !set the next command
        icmd = ob(icmd)%cmd_next
        
      end do
      
      !! set demand requirements for water rights objects
      !! call water_demand
      do iwro =1, db_mx%wro_db
        wro(iwro)%demand = 0.
        do iob = 1, wro(iwro)%num_objs
          ob_num = wro(iwro)%field(iob)%ob_num
          wro(iwro)%demand = irrig(ob_num)%demand
        end do
      end do
    
      !! print all output files
      if (time%yrs > pco%nyskip .and. time%step == 0) then
        call obj_output
        
        !! print water allocation output
        do iwro =1, db_mx%wallo_db
          call water_allocation_output (iwro)
        end do
        
        do isd = 1, sp_ob%hru_lte
          call hru_lte_output (isd)
        end do
        
        do ihru = 1, sp_ob%hru
          call hru_output (ihru)
          if (hru(ihru)%dbs%surf_stor > 0) then
            call wetland_output(ihru)
          end if
          if (cs_db%num_tot > 0) then 
            call hru_pesticide_output (ihru)
            call hru_pathogen_output (ihru)
          end if
        end do        
        
        do iaq = 1, sp_ob%aqu
          call aquifer_output (iaq)
          if (cs_db%num_tot > 0) then 
            call aqu_pesticide_output (iaq)
          end if       
        end do
        
        do jrch = 1, sp_ob%chan
          call channel_output (jrch)
        end do
                
        do jrch = 1, sp_ob%chandeg
          call sd_chanmorph_output (jrch)
          call sd_channel_output (jrch)
          if (cs_db%num_tot > 0) then 
            call cha_pesticide_output (jrch)   
            !call ch_pathogen_output (jrch)
          end if         
        end do

        do j = 1, sp_ob%res
          call reservoir_output(j)
         if (cs_db%num_tot > 0) then 
            call res_pesticide_output (j)
            !call res_pathogen_output (j)
          end if       
        end do 
        
        do j = 1, sp_ob%ru
          call ru_output(j)
        end do
        
        do j = 1, sp_ob%recall
          call recall_output (j)
        end do

        call hydin_output   !if all output is no, then don"t call
        !call hcsin_output  gives allocate error
        if (sp_ob%chandeg > 0 .and. cs_db%num_pests > 0) call basin_ch_pest_output  
        if (sp_ob%res > 0 .and. cs_db%num_pests > 0) call basin_res_pest_output     
        if (sp_ob%hru > 0 .and. cs_db%num_pests > 0) call basin_ls_pest_output
        if (sp_ob%aqu > 0 .and. cs_db%num_pests > 0) call basin_aqu_pest_output
        if (db_mx%lsu_elem > 0) call basin_output
        if (db_mx%lsu_out > 0) call lsu_output
        if (db_mx%aqu_elem > 0) call basin_aquifer_output
        if (sp_ob%res > 0) call basin_reservoir_output
        if (sp_ob%chan > 0) call basin_channel_output
        if (sp_ob%chandeg > 0) call basin_chanmorph_output
        if (sp_ob%chandeg > 0) call basin_sdchannel_output
        if (sp_ob%recall > 0) call basin_recall_output
        call soil_nutcarb_output
        !call lsreg_output
        !call region_aquifer_output
        !call region_reservoir_output
        !call region_channel_output
        !call region_recall_output
      end if
      
      return
      end
