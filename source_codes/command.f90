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
      
      implicit none
    
      integer :: isur_in              !              |
      integer :: in                   !              |
      integer :: ielem                !              |  
      integer :: iob                  !              |
      integer :: kk                   !none          |counter
      integer :: iday                 !              |
      real :: conv                    !              |
      real :: hin_s                   !none          |inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
      real :: hin                     !none          |inflow hydrograph for surface runon - sum of all inflow hyds
      integer :: isd                  !none          |counter
      integer :: ires                 !none          |reservoir number
      integer :: irec                 !              |
      integer :: iout                 !none          |counter
      integer :: ihtyp                !              |
      integer :: iaq                  !none          |counter
      integer :: j                    !none          |counter
      integer :: iru_in               !              |
      integer :: ihyd                 !              |
      integer :: idr                  !              |

      icmd = sp_ob1%objs
      do while (icmd /= 0)
        isur_in = 0
        iru_in = 0
        !subdaily - set current day of hydrograph
        if (time%step > 0) then
          !update current day of hydrograph for the object
          ob(icmd)%day_cur = ob(icmd)%day_cur + 1
          if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
        end if
        
        !sum all receiving hydrographs
        if (ob(icmd)%rcv_tot > 0) then
          ob(icmd)%hin = hz
          ob(icmd)%hin_s = hz
          ht1 = hz
          obcs(icmd)%hin = hin_csz
          obcs(icmd)%hin_s = hin_csz
          hcs1 = hin_csz
          hcs2 = hin_csz
          if (time%step > 0) ob(icmd)%tsin(:) = hz
          ob(icmd)%peakrate = 0.
          do in = 1, ob(icmd)%rcv_tot
            !check to see if the object is in a subbasin
            !if routing over an hru, it can only be in one subbasin
            !element type should be an hru if routing over - if element is a channel use incoming directly
            !CEAP uses representative 1st order streams as elements to define upland sub
            if (ob(icmd)%ru_tot > 0) then
              if (ob(icmd)%typ == "hru" .or. ob(icmd)%typ == "hru_lte") then
                ielem = ob(icmd)%elem
                iru = ob(icmd)%ru(1)                          !can only be in one subbasin if routing over
                iru = sp_ob1%ru + iru - 1                    !object number of the subbasin
                iob = ob(iru)%obj_in(in)
                ihyd = ob(iru)%ihtyp_in(in)
                ht1 = ob(iru)%frac_in(in) * ob(iob)%hd(ihyd)   !fraction of hydrograph
                ht1 = ru_elem(ielem)%frac * ht1                 !fraction of hru in subbasin
                !fraction of hru in subbasin for constituents
                if (cs_db%num_tot > 0) then
                  hcs1 = ob(iru)%frac_in(in) * obcs(iob)%hd(ihyd)   !fraction of hydrograph
                  hcs1 = ru_elem(ielem)%frac * hcs1                 !fraction of hru in subbasin
                end if
                ob(iru)%hin_d(in) = ob(iru)%hin_d(in) + ht1
                obcs(iru)%hcsin_d(in) = obcs(iru)%hcsin_d(in) + hcs1
              else
                iob = ob(icmd)%obj_in(in)
                ihyd = ob(icmd)%ihtyp_in(in)
                ht1 = ob(icmd)%frac_in(in) * ob(iob)%hd(ihyd)
                !fraction of constituents
                if (cs_db%num_tot > 0) then
                  hcs1 = ob(icmd)%frac_in(in) * obcs(iob)%hd(ihyd)
                end if
                ob(icmd)%peakrate = ob(iob)%peakrate
              end if
            else
              iob = ob(icmd)%obj_in(in)
              ihyd = ob(icmd)%ihtyp_in(in)
              ht1 = ob(icmd)%frac_in(in) * ob(iob)%hd(ihyd)
              ob(icmd)%peakrate = ob(iob)%peakrate
              !fraction of constituents
              if (cs_db%num_tot > 0) then
                hcs1 = ob(icmd)%frac_in(in) * obcs(iob)%hd(ihyd)
              end if
            end if
            ob(icmd)%hin_d(in) = ht1
            obcs(icmd)%hcsin_d(in) = hcs1    !for constituent hydrograph output

            if (ihyd == 4) then  !route lat flow through soil
              ob(icmd)%hin_s = ob(icmd)%hin_s + ht1
              !add constituents
              if (cs_db%num_tot > 0) then
                obcs(icmd)%hin_s = obcs(icmd)%hin_s + hcs1
              end if
              iru_in = 1
            else
              ob(icmd)%hin = ob(icmd)%hin + ht1
              !add constituents
              if (cs_db%num_tot > 0) then
                obcs(icmd)%hin = obcs(icmd)%hin + hcs1
              end if
              isur_in = 1
            end if

            !sum subdaily hydrographs
            if (time%step > 0) then
              do kk = 1, time%step
                iday = ob(iob)%day_cur
                ob(icmd)%tsin(kk) = ob(icmd)%tsin(kk) + ob(iob)%ts(iday,kk)
              end do
            end if
                        
            !if (isur_in == 1) then
            !  ht1 = ob(icmd)%hin
            !  call hydin_output (in)
            !end if
            !if (iru_in == 1) then
            !  ht1 = ob(icmd)%hin_s
            !  call hydin_output (in)
            !end if
            
          end do    ! in = 1, ob(icmd)%rcv_tot

          !convert to per area basis
          if (ob(icmd)%typ == "ru" .or. ob(icmd)%typ == "hru") then  !only convert hru and subbasin hyds for routing
            if (ob(icmd)%ru_tot > 0) then
              !object is in a subbasin
              ielem = ob(icmd)%elem
              iru = ob(icmd)%ru(1)  !can only be in one subbasin if routing over
              conv = 100. * ru(iru)%da_km2 * ru_elem(ielem)%frac
            else
              conv = ob(icmd)%area_ha
            end if
            ob(icmd)%hin_s = ob(icmd)%hin_s // conv
            ob(icmd)%hin = ob(icmd)%hin // conv
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

          case ("modflow")   ! modflow
            !! call modflow (daily)  **Ryan**
            
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
              case (1)    !daily
                ob(icmd)%hd(1) = recall(irec)%hd(time%day,time%yrs)
              case (2)    !monthly
                ob(icmd)%hd(1) = recall(irec)%hd(time%mo,time%yrs)
              case (3)    !annual
                ob(icmd)%hd(1) = recall(irec)%hd(1,time%yrs)
              end select
              
              rec_d(irec) = ob(icmd)%hd(1)

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
            call sd_channel_control
            
          end select
        if (pco%fdcout == "y") call flow_dur_curve
        
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

      if (time%yrs > pco%nyskip .and. time%step == 0) then
        call obj_output
        
        do isd = 1, sp_ob%hru_lte
          call hru_lte_output (isd)
        end do
        
        do ihru = 1, sp_ob%hru
          call hru_output (ihru)
          if (cs_db%num_tot > 0) then 
              call hru_pesticide_output (ihru)
              call hru_pathogen_output (ihru)
          end if
        end do        
        
        do iaq = 1, sp_ob%aqu
          call aquifer_output (iaq)
        end do
        
        do jrch = 1, sp_ob%chan
          call channel_output (jrch)
        end do
                
        do jrch = 1, sp_ob%chandeg
          call sd_channel_output (jrch)
        end do

        do j = 1, sp_ob%res
          call reservoir_output(j)
        end do 
        
        do j = 1, sp_ob%ru
          call ru_output(j)
        end do
        
        do j = 1, sp_ob%recall
          call recall_output (j)
        end do
        
        do ires = 1, db_mx%wet_dat
          call wetland_output(ires)
        end do

        
        call hydin_output   !if all output is no, then don"t call
        !call hcsin_output  gives allocate error
        if (db_mx%lsu_elem > 0) call basin_output
        if (db_mx%lsu_out > 0) call lsu_output
        if (db_mx%aqu_elem > 0) call basin_aquifer_output
        if (sp_ob%res > 0) call basin_reservoir_output
        if (sp_ob%chan > 0) call basin_channel_output
        if (sp_ob%chandeg > 0) call basin_sdchannel_output
        !if (sp_ob%recall > 0) call basin_recall_output   !!! all recall outputs need to be addressed
        call soil_nutcarb_output
        !call lsreg_output
        !call region_aquifer_output
        !call region_reservoir_output
        !call region_channel_output
        !call region_recall_output
      end if
      
      return
      end