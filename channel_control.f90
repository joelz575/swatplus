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

!! route overland flow
!!      iru_sub = inum4   !!routing unit number
!!      call routels(iru_sub)

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
      
      if (time%yrs > pco%nyskip) then
        call channel_output
      end if

      return

      end subroutine channel_control