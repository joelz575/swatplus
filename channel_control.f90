      subroutine channel_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates channel routing     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bankst(:)   |m^3 H2O       |bank storage
!!    da_ha       |ha            |area of watershed in hectares
!!    hru_sub(:)  |none          |subbasin number for HRU
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    nhru        |none          |number of HRUs in watershed
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    rchdep      |m             |depth of flow on day
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    revapday    |m^3 H2O       |amount of water moving from bank storage
!!                               |into the soil profile or being taken
!!                               |up by plant roots in the bank storage zone
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: rchinit, rtover, rtday, rtmusk, rthourly, rtsed, rthsed, watqual
!!    SWAT: noqual, hhwatqual, hhnoqual, rtpest, rthpest, rtbact, irr_rch
!!    SWAT: rchuse, reachout

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_parms
      use hydrograph_module
      use basin_module
      use jrw_datalib_module
      use time_module

      integer :: ii
      real :: subwtr
      
      jhyd = ch_dat(jrch)%hyd
      jsed = ch_dat(jrch)%sed
      jnut = ch_dat(jrch)%nut
      jpst = ch_dat(jrch)%pst

      iwst = ob(icmd)%wst
      pet_day = wst(iwst)%weat%pet
      
      !! initialize variables for route command loop
      call ch_rchinit

      !! zero flow out variables
      ob(icmd)%hd(1) = hz
      if (time%step > 0) then
        do ii = 1, time%step ! changed from 24 to nstep for urban modeling by J.Jeong 4/16/2008
          ob(icmd)%ts(1,ii) = hz
        end do
      end if  

      ch(jrch)%vel_chan = 0.
      ch(jrch)%dep_chan = 0.

!! route water through reach
      if (time%step == 0) then
        if (bsn_cc%rte == 0) call ch_rtday
        if (bsn_cc%rte == 1) call ch_rtmusk
      else
        if (bsn_cc%rte == 0) call ch_rthr
        if (bsn_cc%rte == 1) call ch_rthmusk
      endif

!! average daily water depth for sandi doty 09/26/07
      ch(jrch)%dep_chan = rchdep


!! add transmission losses to bank storage/deep aquifer in subbasin
      if (rttlc > 0.) then
!        ch(jrch)%bankst = ch(jrch)%bankst + rttlc * (1. - bsn_prm%trnsrch)
!        if (da_ha > 1.e-9) then 
!          subwtr = rttlc * bsn_prm%trnsrch / (da_ha * sub_fr(jrch) * 10.)
!          do j = hru1(jrch), hru1(jrch) + hrutot(jrch) - 1
!            hru(j)%dpa%deepst = hru(j)%dpa%deepst + subwtr
!          end do
!	  end if
        ch(jrch)%bankst = ch(jrch)%bankst + rttlc * (1.-bsn_prm%trnsrch)
!!!! Jeff add to hydrograph -----------------------------
        rchsep(jrch) = rttlc * bsn_prm%trnsrch
      end if
 
!! compute revap from bank storage
      revapday = 0.6 * pet_day *ch_hyd(jhyd)%l *ch_hyd(jhyd)%w
      revapday = Min(revapday, ch(jrch)%bankst)
      ch(jrch)%bankst = ch(jrch)%bankst - revapday

!! compute contribution of water in bank storage to streamflow
      qdbank = ch(jrch)%bankst * (1. - ch_hyd(jhyd)%alpha_bnk)
      ch(jrch)%bankst = ch(jrch)%bankst - qdbank
      rtwtr = rtwtr + qdbank
      if (time%step > 0) then
        do ii = 1, time%step
          hrtwtr(ii) = hrtwtr(ii) + qdbank / real(time%step)
        end do
      end if


!! perform in-stream sediment calculations
	  sedrch = 0.
	  rch_san = 0.
	  rch_sil = 0.
	  rch_cla = 0.
	  rch_sag = 0.
	  rch_lag = 0.
	  rch_gra = 0.
	  ch(jrch)%orgn = 0.
	  ch(jrch)%orgp = 0.

!! do not perform sediment routing for headwater subbasins when i_subhw = 0
	  if (bsn_cc%i_subhw == 0 .and. inum1 == inum2) then
          if (time%step == 0) then
            if (rtwtr > 0. .and. rchdep > 0.) then
              sedrch  = ob(icmd)%hd(1)%flo 
	        rch_san = ob(icmd)%hd(1)%san 
	        rch_sil = ob(icmd)%hd(1)%sil 
	        rch_cla = ob(icmd)%hd(1)%cla 
	        rch_sag = ob(icmd)%hd(1)%sag 
	        rch_lag = ob(icmd)%hd(1)%lag 
	        rch_gra = ob(icmd)%hd(1)%grv 
            end if
          else
            do ii = 1, time%step
              if (hrtwtr(ii) > 0. .and. hdepth(ii) > 0.) then
                hsedyld(ii) = ob(icmd)%ts(1,ii)%sed 
                sedrch = sedrch + hsedyld(ii)
                rch_san = 0.
	          rch_sil = rch_sil + hsedyld(ii)  !!All are assumed to be silt type particles
                rch_cla = 0.
                rch_sag = 0.
                rch_lag = 0.
	          rch_gra = 0.
              end if
            end do
          end if
        else
            if (time%step == 0) then
            if (ch_sed(jsed)%eqn == 0) call ch_rtsed
            if (ch_sed(jsed)%eqn == 1) call ch_rtsed_bagnold
            if (ch_sed(jsed)%eqn == 2) call ch_rtsed_kodatie
            if (ch_sed(jsed)%eqn == 3) call ch_rtsed_Molinas_Wu
            if (ch_sed(jsed)%eqn == 4) call ch_rtsed_yangsand
          else
            call ch_rthsed
            end if      
        end if

!! perform in-stream nutrient calculations
      if (time%step == 0) then
        if (bsn_cc%wq == 2) call ch_watqual2
        if (bsn_cc%wq == 1) call ch_watqual
        if (bsn_cc%wq == 0) call ch_noqual
      else
        if (bsn_cc%wq == 1) call ch_hhwatqual
        if (bsn_cc%wq == 0) call ch_hhnoqual
      end if

!! perform in-stream pesticide calculations
!!      call ch_biofilm
      
!! perform in-stream pesticide calculations
      if (time%step == 0) then
        call ch_rtpest
      else
        call ch_rthpest
      end if

!! perform in-stream bacteria calculations
      ! call ch_rtbact  dont merge

!! remove water from reach for irrigation
      call ch_irr_rch

!! remove water from reach for consumptive water use
      call ch_rchuse

!! summarize output/determine loadings to next routing unit
      call ch_rtout
      
!   output_channel
      ch_d(jrch)%flo_in = ob(icmd)%hin%flo  / 86400. 
      ch_d(jrch)%flo_out = rtwtr / 86400. 
      ch_d(jrch)%evap = rtevp / 86400.   
      ch_d(jrch)%tloss = rttlc / 86400.  
      ch_d(jrch)%sed_in = ob(icmd)%hin%sed   
      ch_d(jrch)%sed_out = sedrch              
      ch_d(jrch)%sed_conc = sedcon             
      ch_d(jrch)%orgn_in = ob(icmd)%hin%orgn   
      ch_d(jrch)%orgn_out = ob(icmd)%hd(1)%orgn              
      ch_d(jrch)%orgp_in = ob(icmd)%hin%sedp    
      ch_d(jrch)%orgp_out = ob(icmd)%hd(1)%sedp              
      ch_d(jrch)%no3_in = ob(icmd)%hin%no3  
      ch_d(jrch)%no3_out = ob(icmd)%hd(1)%no3                     
      ch_d(jrch)%nh4_in = ob(icmd)%hin%nh3    
      ch_d(jrch)%nh4_out = ob(icmd)%hd(1)%nh3               
      ch_d(jrch)%no2_in = ob(icmd)%hin%no2 
      ch_d(jrch)%no2_out = ob(icmd)%hd(1)%no2                       
      ch_d(jrch)%solp_in = ob(icmd)%hin%solp         
      ch_d(jrch)%solp_out = ob(icmd)%hd(1)%solp                   
      ch_d(jrch)%chla_in = ob(icmd)%hin%chla     
      ch_d(jrch)%chla_out = ob(icmd)%hd(1)%chla                   
      ch_d(jrch)%cbod_in = ob(icmd)%hin%cbod    
      ch_d(jrch)%cbod_out = ob(icmd)%hd(1)%cbod                    
      ch_d(jrch)%dis_in = ob(icmd)%hin%dox         
      ch_d(jrch)%dis_out = ob(icmd)%hd(1)%dox                     
      ch_d(jrch)%solpst_in = ob(icmd)%hin%psol      
      ch_d(jrch)%solpst_out = ob(icmd)%hd(1)%psol                  
      ch_d(jrch)%sorbpst_in = ob(icmd)%hin%psor     
      ch_d(jrch)%sorbpst_out = ob(icmd)%hd(1)%psor                  
      ch_d(jrch)%react = reactw                                 
      ch_d(jrch)%volat = volatpst                           
      ch_d(jrch)%setlpst = setlpst                              
      ch_d(jrch)%resuspst = resuspst                            
      ch_d(jrch)%difus = -difus                                 
      ch_d(jrch)%reactb = reactb                               
      ch_d(jrch)%bury = bury                                    
      ch_d(jrch)%sedpest = sedpest                              
      ch_d(jrch)%bacp = ob(icmd)%hd(1)%bacp                        
      ch_d(jrch)%baclp = ob(icmd)%hd(1)%baclp                       
      ch_d(jrch)%met1 = ob(icmd)%hd(1)%met1                         
      ch_d(jrch)%met2 = ob(icmd)%hd(1)%met2                         
      ch_d(jrch)%met3 = ob(icmd)%hd(1)%met3                          
      ch_d(jrch)%sand_in = ob(icmd)%hin%san 
      ch_d(jrch)%sand_out = ob(icmd)%hd(1)%san                         
      ch_d(jrch)%silt_in = ob(icmd)%hin%sil         
      ch_d(jrch)%silt_out = ob(icmd)%hd(1)%sil                       
      ch_d(jrch)%clay_in = ob(icmd)%hin%cla            
      ch_d(jrch)%clay_out = ob(icmd)%hd(1)%cla                         
      ch_d(jrch)%smag_in = ob(icmd)%hin%sag             
      ch_d(jrch)%smag_out = ob(icmd)%hd(1)%sag                       
      ch_d(jrch)%lag_in = ob(icmd)%hin%lag           
      ch_d(jrch)%lag_out = ob(icmd)%hd(1)%lag                        
      ch_d(jrch)%grvl_in = ob(icmd)%hin%grv          
      ch_d(jrch)%grvl_out = ob(icmd)%hd(1)%grv                      
      ch_d(jrch)%bnk_ero = bnkrte
      ch_d(jrch)%ch_deg = degrte
!!    Channel Deposition (Only new deposits during the current time step)
      if (ch(jrch)%depch >= ch(jrch)%depprch) then
	  ch_d(jrch)%ch_dep = ch(jrch)%depch - ch(jrch)%depprch
	else
	  ch_d(jrch)%ch_dep = 0.
	end if
!!    Floodplain Deposition (Only new deposits during the current time step)
      if (ch(jrch)%depfp >= ch(jrch)%depprfp) then
	  ch_d(jrch)%fp_dep = ch(jrch)%depfp - ch(jrch)%depprfp
	else
	  ch_d(jrch)%fp_dep = 0.
	end if
!!    Total suspended sediments (only silt and clay)
      if (ch_sed(jsed)%eqn == 0) then
        ch_d(jrch)%tot_ssed = sedrch
      else
        ch_d(jrch)%tot_ssed = rch_sil + rch_cla
      endif
      
      if (time%yrs > pco%nyskip) then
        call channel_output
      end if

      return

      end subroutine channel_control