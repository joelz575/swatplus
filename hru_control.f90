      subroutine hru_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use parm
      use basin_module
      use organic_mineral_mass_module

      integer :: j, sb, kk, iwst
      real :: tmpk, d, gma, ho, pet_alpha, aphu, phuop

      j = ihru
      if (pcom(j)%npl > 0) idp = pcom(ihru)%plcur(1)%idplt
      ulu = hru(j)%luse%urb_lu
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgen = wst(iwst)%wco%wgn
      ith = hru(j)%dbs%topo
      iadep = wst(iwst)%wco%atmodep
      ipot = hru(ihru)%dbs%surf_stor
      iwgn = wst(iwst)%wco%wgn
      jres = hru(j)%res
      isched = hru(j)%mgt_ops
      
      precipday = wst(iwst)%weat%precip
      tmx(j) = wst(iwst)%weat%tmax
      tmn(j) = wst(iwst)%weat%tmin
      tmpav(j) = (tmx(j) + tmn(j)) / 2.
      hru_ra(j) = wst(iwst)%weat%solrad
      hru_rmx(j) = wst(iwst)%weat%solradmx
      rhd(j) = wst(iwst)%weat%rhum
      u10(j) = wst(iwst)%weat%windsp
      
      !plt => hru(j)%pl(1)
      
      !!by zhang DSSAT tillage
      !!======================
      !!    deptil(:)   |mm  |depth of mixing caused by tillage operation
      !jj is hru number
      if (bsn_cc%cswat == 2) then
          if (tillage_switch(ihru) .eq. 1) then
              if (tillage_days(ihru) .ge. 30) then
                    tillage_switch(ihru) = 0
                    tillage_days(ihru) = 0
              else
                    tillage_days(ihru) = tillage_days(ihru) + 1
              end if                
              !tillage_depth(ihru) = dtil
              !tillage_switch(ihru) = .TRUE. 
          end if
      end if
      !!by zhang DSSAT tillage  
      !!====================== 

      call varinit
      nd_30 = nd_30 + 1
      if (nd_30 > 30) nd_30 = 1
      if (icr(j) <= 0) icr(j) = 1
      
      i_wtrhru = 0
      if (idp /= 0) then
          if (pldb(idp)%plantnm == "watr") then
              i_wtrhru = 1
          end if
      endif
      
	if (i_wtrhru == 1) then
         call water_hru
      else

        !! Add incoming lateral soil flow
        surfqout = 0.
        inflrout = 0.
        !!ht1== deposition: write to deposition.out
        !!ht2== outflow from inflow: added to hru generated flows
        ht1 = hz
        ht2 = hz
        if (ob(icmd)%rcv_tot > 0) then
          !!Route incoming lateral soil flow
          call rls_routesoil (icmd)
        end if
          
        !!add overland flow to precipday to compute runoff
        if (ob(icmd)%hin%flo > 1.e-6) then
          call rls_routesurf (icmd)
        end if
        
        !!add overbank flooding to storage
        if (ob(icmd)%flood_ch_lnk > 0) then
          idat = res_ob(jres)%props
          ihyd = res_dat(idat)%hyd
          ics = ob(icmd)%props2
          !if (ch_sur(ics)%hd(1)%flo > 1.e-6) then
            !res(jres) = res(jres) + ch_sur(ics)%hd
            call rls_routesurf (icmd)
          !end if
        end if
        
        !!Route incoming overland flow
        !if (ch_sur(ics)%hd()%flo > 1.e-6) then
        !  if (ch_sur(ics)%hd()%flo > 1.e-6) then
            !!partition flow-back to stream and downstream
        !  else
            !!all flow back to stream
        !  end if
          !!send partition into subroutine
        !  call rls_routesurf (icmd)
        !end if

        !! check auto operations
        if (sched(isched)%num_autos > 0) then
          do iauto = 1, sched(isched)%num_autos
            id = sched(isched)%num_db(iauto)
            jj = j
            call conditions (id, jj)
            call actions (id, jj)
          end do
        end if
        
        !! update base zero total heat units
        if (tmpav(j) > 0. .and. wgn_pms(iwgn)%phutot > 0.01) then
           phubase(j) = phubase(j) + tmpav(j) / wgn_pms(iwgn)%phutot
        end if

        !! compute total parms for all plants in the community
        sumlai = 0.
        cht_mx(j) = 0.
        sumbm = 0.
        sumrwt = 0.
        strsw_av = 0.
        strsa_av = 0.
        strsn_av = 0.
        strsp_av = 0.
        strstmp_av = 0.
        do ipl = 1, pcom(j)%npl
          sumlai = sumlai + pcom(j)%plg(ipl)%lai
           sumbm = sumbm + pcom(j)%plm(ipl)%mass
          sumrwt = sumrwt + pcom(j)%plg(ipl)%rwt
          cht_mx(j) = Max(0., cht_mx(j))
          strsw_av(j) = strsw_av(j) + pcom(j)%plstr(ipl)%strsw / pcom(j)%npl
          strsa_av = strsa_av + pcom(j)%plstr(ipl)%strsa / pcom(j)%npl
          strsn_av = strsn_av + pcom(j)%plstr(ipl)%strsn / pcom(j)%npl
          strsp_av = strsp_av + pcom(j)%plstr(ipl)%strsp / pcom(j)%npl
          strstmp_av = strstmp_av + pcom(j)%plstr(ipl)%strst / pcom(j)%npl
        end do
        
        !! calculate albedo for day
        call albedo

        !! calculate soil temperature for soil layers
        call stmp_solt
        
        call surface
        
        !! ht2%sed==sediment routed across hru from surface runon
        sedyld(j) = sedyld(j) + ht2%sed

        !! compute effective rainfall (amount that percs into soil)
        !! add infiltration from surface runon (inflrout)
        inflpcp = Max(0.,precipday - surfq(j)) + inflrout
         
        !! perform management operations
        if (yr_skip(j) == 0) call mgt_operatn   
        
        !! perform soil water routing
        call swr_percmain

        !! compute peak rate similar to swat-deg using SCS triangular unit hydrograph
        runoff_m3 = 10. * surfq(j) * hru(j)%area_ha
        bf_m3 = 10. * latq(j) * hru(j)%area_ha
        peakr = 2. * runoff_m3 / (1.5 * tconc(j) * 3600.)
        peakrbf = bf_m3 / 86400.
        peakr = (peakr + peakrbf)     !* prf     
          
        !! compute evapotranspiration
        call et_pot
!        if (pot(j)%vol < 1.e-6) call etact
        call et_act

        !! compute water table depth using climate drivers
        call wattable

        !! new CN method
        if (icn == 1) then 
        sci(j) = sci(j) + pet_day * exp(-hru(j)%hyd%cncoef * sci(j) /      &
           smx(j)) - precipday + qday + qtile + latq(j) + sepbtm(j)
        else if (icn == 2) then 
        sci(j) = sci(j) + pet_day * exp(-hru(j)%hyd%cncoef * sci(j) /      &
           smx(j)) - precipday + qday + latq(j) + sepbtm(j) + qtile
        sci(j) = amin1(sci(j), bsn_prm%smxco * smx(j))
        end if 
        
        !! apply fertilizer/manure in continuous fert operation
        if (icfrt(j) == 1) then
          ndcfrt(j) = ndcfrt(j) + 1
          call pl_confert
          call bac_apply_hrucon
          soil(j)%ly(1)%bacsol(ibac) = sol_bacsol
          soil(j)%ly(1)%bacsor(ibac) = sol_bacsor
        end if
        
        !! apply pesticide in continuous pest operation
        if (icpst(j) == 1) then 
          ndcpst(j) = ndcpst(j) + 1
          call pl_conapply
        end if 
        
        !! remove biomass from grazing and apply manure
        if (igrz(j) == 1) then
          ndeat(j) = ndeat(j) + 1
          call pl_graze
          !call bac_apply_hrucon
          !soil(j)%ly(1)%bacsol(ibac) = sol_bacsol
          !soil(j)%ly(1)%bacsor(ibac) = sol_bacsor
        end if
       
        !! compute crop growth
        call plantmod
     
        !! check for dormancy
        do ipl = 1, pcom(j)%npl
          if (pcom(j)%plcur(ipl)%gro == 1) call mgt_dormant
        end do
               
        !! tropical plants - begin new growing cycle at end of dry season when soil 
        !! moisture of upper 2 layers exceeds input fraction of field capacity
        if (pcom(j)%mseas == 1) then
          call mgt_trop_gro
        end if

        !! compute actual ET for day in HRU
        etday = ep_day + es_day + canev

        !! write daily air and soil temperature file
        !! can be uncommmented if needed by user and also in readfile.f

!      write (120,12112) i,j,tmx(j),tmn(j),(sol_tmp(k,j),k=1,sol_nly(j))
!12112  format (2i4,12f8.2)

        !! compute nitrogen and phosphorus mineralization 

      if (bsn_cc%cswat == 0) then
        call nut_nminrl
	end if
	if (bsn_cc%cswat == 1) then
		call cbn_new
	end if
	
	!! Add by zhang
	!!=================
	if (bsn_cc%cswat == 2) then
	  call cbn_zhang2
	end if
	!! Add by zhang
	!!=================	

        call nut_nitvol
        if (bsn_cc%sol_P_model == 1) then
            call nut_pminrl
        else
            call nut_pminrl2
        end if

!!    compute biozone processes in septic HRUs
!!    if 1)current is septic hru and 2)  soil temperature is above zero
        isep = iseptic(j)
	  if (sep(isep)%opt /= 0. .and. time%yrc >= sep(isep)%yr) then
	   if (soil(j)%phys(i_sep(j))%tmp > 0.) call sep_biozone     
	  endif

        !! compute ground water contribution
!        call gwmod
!        call gwmod_deep

        !! compute pesticide washoff   
        if (precipday >= 2.54) call pst_washp

        !! compute pesticide degradation
        call pst_decay

        !! compute pesticide movement in soil
        call pst_lch

        if (surfq(j) > 0. .and. peakr > 1.e-6) then
          if (precipday > 0.) then
            call pst_enrsb(0)
            if (sedyld(j) > 0.) call pst_pesty(0)

		  if (bsn_cc%cswat == 0) then
			call nut_orgn(0)
	    end if
	    if (bsn_cc%cswat == 1) then
	    
		    call nut_orgnc(0)
		  end if
		  
		  !! Add by zhang
		  !! ====================
		  if (bsn_cc%cswat == 2) then
		    call orgncswat2(0)
		  end if
		  !! Add by zhang
		  !! ====================

            call nut_psed(0)
          end if
        end if

        !! add nitrate in rainfall to soil profile
        call nut_nrain

        !! compute nitrate movement leaching
        call nut_nlch

        !! compute phosphorus movement
        call nut_solp

        !! compute chl-a, CBOD and dissolved oxygen loadings
        call swr_subwq

        !! compute bacteria transport
        call bac_hrucontrol

        !! compute loadings from urban areas
        if (hru(j)%luse%urb_lu > 0) then
	     if(time%step == 0) then
	        call hru_urban ! daily simulation
	     else
		     call hru_urbanhr ! subdaily simulation J.Jeong 4/20/2009
	     endif
	  endif	  

        !! compute sediment loading in lateral flow and add to sedyld
        call swr_latsed

        !! compute nutrient loading in groundwater flow
!        call aqu_minp
!        call aqu_no3

        !! lag nutrients and sediment in surface runoff
        call stor_surfstor

        !! lag subsurface flow and nitrate in subsurface flow
        call swr_substor

        !! add lateral flow that was routed across the landscape on the previous day
      !!  latq(j) = latq(j) + latq_ru(j)
      !!  latq_ru(j) = 0.
        
        !! compute reduction in pollutants due to edge-of-field filter strip
        if (hru(j)%lumv%vfsi > 0.)then
          call smp_filter
          if (filterw(j) > 0.) call smp_buffer
        end if
        if (hru(j)%lumv%vfsi == 0. .and. filterw(j) > 0.) then 
          call smp_filtw
          call smp_buffer
        end if

	 !! compute reduction in pollutants due to in field grass waterway
         if (hru(j)%lumv%grwat_i == 1) then
          call smp_grass_wway
        end if

	 !! compute reduction in pollutants due to in fixed BMP eff
	   if (hru(j)%lumv%bmp_flag == 1) then
          call smp_bmpfixed
        end if


        !! compute water yield for HRU - ht2%flo is outflow from routed runon
        qdr(j) = qday + latq(j) + qtile + ht2%flo
        if (qdr(j) < 0.) qdr(j) = 0.
        if (qdr(j) > 0.) then
          qdfr = qday / qdr(j)
        else
          qdfr = 0.
        end if
      
        !! impounded water - rice paddy, pothole, wetland, etc
        !! do not call if typ=="fpl" (flood plain and no depressional storage)
        if (hru(j)%surfstor > 0) then ! .and. res(jres)%flo > 1.e-6) then
          call stor_surf
        end if
          
        xx = sed_con(j) + soln_con(j) + solp_con(j) + orgn_con(j) + orgp_con(j)
        if (xx > 1.e-6) then
          call hru_urb_bmp
        end if
      endif

      !! perform output summarization
      if (time%yrs > pco%nyskip) then
        call hru_output
      end if

      !! summarize output for multiple HRUs per subbasin
      !! store reach loadings for new fig method
      call hru_hyds
      aird(j) = 0.

      return
      end subroutine hru_control