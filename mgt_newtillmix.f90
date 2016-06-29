      subroutine mgt_newtillmix(jj,bmix)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients during tillage and 
!!    biological mixing
!!    New version developed by Armen R. Kemanian in collaboration with Stefan Julich and Cole Rossi
!!    Mixing was extended to all layers
!!    A subroutine to simulate stimulation of organic matter decomposition was added
!!    March 2009: testing has been minimal and further adjustments are expected
!!    use with caution and report anomalous results to akemanian@brc.tamus.edu and jeff.arnold@ars.usda.edu

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    cnop          |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    curyr         |none          |current year of simulation
!!    deptil(:)     |mm            |depth of mixing caused by tillage
!!                                 |operation
!!    effmix(:)     |none          |mixing efficiency of tillage operation
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    min_res(:)	|kg/ha		   |Min residue allowed due to implementation of 
!!                                 |residue managment in the OPS file.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bmix        |none          |biological mixing efficiency: this 
!!                               |number is zero for tillage operations
!!    dg          |mm            |depth of soil layer
!!    dtil        |mm            |depth of mixing
!!    emix        |none          |mixing efficiency
!!    jj          |none          |HRU number
!!    k           |none          |counter
!!    l           |none          |counter
!!    nl          |none          |number of layers being mixed
!!    smix(:)     |varies        |amount of substance in soil profile
!!                               |that is being redistributed between 
!!                               |mixed layers
!!    thtill(:)   |none          |fraction of soil layer that is mixed
!!    sol_msm					 | sol_mass mixed
!!    sol_msn					 | sol_mass not mixed
!!    maxmix      |none          | maximum mixing eff to preserve specified minimum residue cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max
!!    SWAT: curno 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module

      integer, intent (in) :: jj
      real, intent (in) :: bmix
      integer :: l, k              !CB 12/2/09 nl and a are not used.
      real :: emix, dtil, XX, WW1, WW2, WW3, WW4, maxmix
      !!by zhang
      !!=============   
      real :: smix(22+npmx+12)        !CB 12/2/09 thtill is not used. mjw rev 490
      !!changed the dimension from 22 + npmx to 22 + npmx + 12
      !!by zhang
      !!=============
      real :: sol_mass(soil(jj)%nly)
      real :: sol_thick(soil(jj)%nly), sol_msm(soil(jj)%nly)
      real :: sol_msn(soil(jj)%nly)


      XX = 0.
      WW1 = 0.
      WW2 = 0.
      WW3 = 0.
      WW4 = 0.
      emix = 0.
      dtil = 0.
      if (bmix > 1.e-6) then
        !! biological mixing
        emix = bmix !bmix MJW (rev 412)
        kk = soil(jj)%nly
        dtil = Min(soil(jj)%phys(kk)%d, 50.) ! it was 300.  MJW (rev 412)
      else 
        !! tillage operation
        emix = tilldb(idtill)%effmix
        dtil = tilldb(idtill)%deptil
      end if

      !!by zhang DSSAT tillage
      !!=======================
      !!    deptil(:)   |mm  |depth of mixing caused by tillage operation
      !jj is hru number
      if (bsn_cc%cswat == 2) then
          tillage_days(jj) = 0
          tillage_depth(jj) = dtil
          tillage_switch(jj) = 1
      end if
      !!by zhang DSSAT tillage
      !!=======================


      smix = 0.
      sol_mass = 0.
      sol_thick = 0.
      sol_msm = 0.
      sol_msn = 0.

	!! incorporate bacteria - no mixing - lost from transport
      if (dtil > 10.) then     
        bactpq(jj) = bactpq(jj) * (1. - emix)
        bactps(jj) = bactps(jj) * (1. - emix)
        bactlpq(jj) = bactlpq(jj) * (1. - emix)
        bactlps(jj) = bactlps(jj) * (1. - emix)
	end if
      	
	!! calculate max mixing to preserve target surface residue MJW rev 490
	!! Assume residue in all other layers is negligible to simplify calculation and remove depth dependency
      if (min_res(jj) > 1. .and. bmix < 0.001) then
	  maxmix = 1 - min_res(jj)/soil(jj)%ly(1)%rsd
	  if (maxmix <0.05)  maxmix = 0.05	
	  if (emix > maxmix)  emix = maxmix
      end if


      do l = 1, soil(jj)%nly
        if ( l == 1) then
          sol_thick(l) = soil(jj)%phys(l)%d
        else	
          sol_thick(l) = soil(jj)%phys(l)%d - soil(jj)%phys(l-1)%d
        end if
	
      sol_mass(l) = (sol_thick(l) / 1000.) * 10000. *                    &
          soil(jj)%phys(1)%bd * 1000. * (1.- soil(jj)%phys(l)%rock/ 100.)

      end do

!       do l=1,20+npmx
!         smix(l)=0.
!       end do
      smix = 0.

      if (dtil > 0.) then
!!!  added by Armen 09/10/2010 next line only
        if (dtil < 10.0) dtil = 11.0
	 do l = 1, soil(jj)%nly

          if (soil(jj)%phys(l)%d <= dtil) then
            !! msm = mass of soil mixed for the layer
            !! msn = mass of soil not mixed for the layer		
            sol_msm(l) = emix * sol_mass(l)	
            sol_msn(l) = sol_mass(l) - sol_msm(l)	
          else if (soil(jj)%phys(l)%d > dtil .AND. soil(jj)%phys(l-1)%d      &
              < dtil) then 
            sol_msm(l) = emix * sol_mass(l) *                                &                          
            (dtil - soil(jj)%phys(l-1)%d) / sol_thick(l)
            sol_msn(l) =  sol_mass(l) -  sol_msm(l)
          else
            sol_msm(l) = 0.
            sol_msn(l) = sol_mass(l)
          end if

			
          !! calculate the mass or concentration of each mixed element 
          !! mass based mixing
          WW1 = sol_msm(l)/(sol_msm(l) + sol_msn(l))
          smix(1) = smix(1) + soil(jj)%nut(l)%no3 * WW1
          smix(2) = smix(2) + soil(jj)%nut(l)%orgn * WW1
          smix(3) = smix(3) + soil(jj)%nut(l)%nh3 * WW1
          smix(4) = smix(4) + soil(jj)%nut(l)%solp * WW1
          smix(5) = smix(5) + soil(jj)%nut(l)%orgp * WW1
          smix(6) = smix(6) + soil(jj)%nut(l)%aorgn * WW1
          smix(7) = smix(7) + soil(jj)%nut(l)%actp * WW1
          smix(8) = smix(8) + soil(jj)%nut(l)%fon * WW1
          smix(9) = smix(9) + soil(jj)%nut(l)%fop * WW1
          smix(10) = smix(10) + soil(jj)%nut(l)%stap * WW1
          smix(11) = smix(11) + soil(jj)%ly(l)%rsd * WW1
          smix(12) = smix(12) + soil(jj)%cbn(l)%mc * WW1
          smix(13) = smix(13) + soil(jj)%nut(l)%mn * WW1
          smix(14) = smix(14) + soil(jj)%nut(l)%mp * WW1

		!! concentration based mixing
          WW2 = XX + sol_msm(l)
          smix(15) = (XX * smix(15)+soil(jj)%cbn(l)%cbn *sol_msm(l))/WW2
          smix(16) = (XX * smix(16) + soil(jj)%ly(l)%n*sol_msm(l))/WW2
          smix(17) = (XX * smix(17) + soil(jj)%phys(l)%clay *              &
                  sol_msm(l))/WW2
          smix(18) = (XX * smix(18) + soil(jj)%phys(l)%silt                &
                  * sol_msm(l))/WW2
          smix(19)=(XX*smix(19)+soil(jj)%phys(l)%sand*sol_msm(l))/WW2
		!! mass based distribution - check later
          !do k = 1, npmx
          !  smix(20+k) = smix(20+k) + soil(jj)%ly(1)%pst(k) * WW1
          !end do

            !!by zhang
            !!============== 
            if (bsn_cc%cswat == 2) then         
	        smix(20+npmx+1) = smix(20+npmx+1) +soil(jj)%cbn(l)%lsc* WW1
	        smix(20+npmx+2) = smix(20+npmx+2) +soil(jj)%cbn(l)%lslc* WW1
	        smix(20+npmx+3) = smix(20+npmx+3) +soil(jj)%cbn(l)%lslnc* WW1
	        smix(20+npmx+4) = smix(20+npmx+4) +soil(jj)%cbn(l)%lmc* WW1
	        smix(20+npmx+5) = smix(20+npmx+5) +soil(jj)%cbn(l)%lm* WW1
	        smix(20+npmx+6) = smix(20+npmx+6) +soil(jj)%cbn(l)%lsl* WW1
	        smix(20+npmx+7) = smix(20+npmx+7) +soil(jj)%cbn(l)%ls* WW1  
	        
	        smix(20+npmx+8) = smix(20+npmx+8) +soil(jj)%cbn(l)%lsn* WW1
	        smix(20+npmx+9) = smix(20+npmx+9) +soil(jj)%cbn(l)%lmn* WW1
	        smix(20+npmx+10) = smix(20+npmx+10) +soil(jj)%cbn(l)%bmn* WW1
	        smix(20+npmx+11) = smix(20+npmx+11) +soil(jj)%cbn(l)%hsn* WW1
	        smix(20+npmx+12) = smix(20+npmx+12) +soil(jj)%cbn(l)%hpn* WW1  
	      end if
            !!by zhang 	
            !!=============
			
          XX = XX + sol_msm(l)

        end do

          do l = 1, soil(jj)%nly
			
            ! reconstitute each soil layer 
            WW3 = sol_msn(l) / sol_mass(l)
            WW4 = sol_msm(l) / XX

            soil(jj)%nut(l)%no3=soil(jj)%nut(l)%no3 * WW3 + smix(1)*WW4
            soil(jj)%nut(l)%orgn=soil(jj)%nut(l)%orgn*WW3 + smix(2)*WW4
            soil(jj)%nut(l)%nh3=soil(jj)%nut(l)%nh3 * WW3 + smix(3)*WW4
            soil(jj)%nut(l)%solp=soil(jj)%nut(l)%solp * WW3+smix(4)*WW4
            soil(jj)%nut(l)%orgp = soil(jj)%nut(l)%orgp*WW3+smix(5)*WW4
            soil(jj)%nut(l)%aorgn = soil(jj)%nut(l)%aorgn *                &
                   WW3+smix(6)*WW4
            soil(jj)%nut(l)%actp = soil(jj)%nut(l)%actp*WW3+smix(7)*WW4
            soil(jj)%nut(l)%fon=soil(jj)%nut(l)%fon * WW3 + smix(8)*WW4
            soil(jj)%nut(l)%fop=soil(jj)%nut(l)%fop * WW3 + smix(9)*WW4
            soil(jj)%nut(l)%stap=soil(jj)%nut(l)%stap*WW3+smix(10)*WW4
            soil(jj)%ly(l)%rsd = soil(jj)%ly(l)%rsd * WW3 + smix(11)*WW4
            if (soil(jj)%ly(l)%rsd < 1.e-10) soil(jj)%ly(l)%rsd = 1.e-10
            soil(jj)%cbn(l)%mc = soil(jj)%cbn(l)%mc * WW3 + smix(12)*WW4
            soil(jj)%nut(l)%mn = soil(jj)%nut(l)%mn * WW3 + smix(13)*WW4
            soil(jj)%nut(l)%mp = soil(jj)%nut(l)%mp * WW3 + smix(14)*WW4

            soil(jj)%cbn(l)%cbn=(soil(jj)%cbn(l)%cbn*sol_msn(l)+smix(15)    &
                 * sol_msm(l)) / sol_mass(l)
            soil(jj)%ly(l)%n  = (soil(jj)%ly(l)%n * sol_msn(l)+smix(16)     &
                 * sol_msm(l)) / sol_mass(l)
            soil(jj)%phys(l)%clay = (soil(jj)%phys(l)%clay                  &
                 * sol_msn(l)+smix(17) * sol_msm(l)) / sol_mass(l)
            soil(jj)%phys(l)%silt = (soil(jj)%phys(l)%silt                  &
                 * sol_msn(l)+smix(18) * sol_msm(l)) / sol_mass(l)
            soil(jj)%phys(l)%sand = (soil(jj)%phys(l)%sand                  &
                 * sol_msn(l) + smix(19) * sol_msm(l)) / sol_mass(l)
!pesticide clean up - npmx is number of pest in db - should be in simulation
!            do k = 1, npmx
!              soil(jj)%ly(l)%pst(k) = soil(jj)%ly(l)%pst(k) * WW3         
!     &                                           + smix(20+k) * WW4
!            end do

             !!by zhang
             !!=============
             if (bsn_cc%cswat == 2) then
      soil(jj)%cbn(l)%lsc=soil(jj)%cbn(l)%lsc*WW3+smix(20+npmx+1)*WW4
      soil(jj)%cbn(l)%lslc=soil(jj)%cbn(l)%lslc*WW3+smix(20+npmx+2)*WW4
      soil(jj)%cbn(l)%lslnc=soil(jj)%cbn(l)%lslnc*WW3+smix(20+npmx+3)*WW4
      soil(jj)%cbn(l)%lmc=soil(jj)%cbn(l)%lmc*WW3+smix(20+npmx+4)*WW4
      soil(jj)%cbn(l)%lm=soil(jj)%cbn(l)%lm*WW3+smix(20+npmx+5)*WW4
      soil(jj)%cbn(l)%lsl=soil(jj)%cbn(l)%lsl*WW3 + smix(20+npmx+6)* WW4
       soil(jj)%cbn(l)%ls=soil(jj)%cbn(l)%ls*WW3 + smix(20+npmx+7)* WW4
       soil(jj)%cbn(l)%lsn=soil(jj)%cbn(l)%lsn*WW3 + smix(20+npmx+8)*WW4
       soil(jj)%cbn(l)%lmn=soil(jj)%cbn(l)%lmn*WW3+smix(20+npmx+9)* WW4
       soil(jj)%cbn(l)%bmn=soil(jj)%cbn(l)%bmn*WW3+smix(20+npmx+10)*WW4
       soil(jj)%cbn(l)%hsn=soil(jj)%cbn(l)%hsn*WW3+smix(20+npmx+11)*WW4
       soil(jj)%cbn(l)%hpn=soil(jj)%cbn(l)%hpn*WW3+smix(20+npmx+12)* WW4
             end if
            !!by zhang 
            !!==============

	  end do
	
        if (bsn_cc%cswat == 1) then
            call mgt_tillfactor(jj,bmix,emix,dtil,sol_thick)
        end if
      end if
	
      !! perform final calculations for tillage operation
 
      !! count the tillage only if it is a scheduled operation biomix does not count MJW Rev 490
      if (bmix <= 1.e-6) then
        ntil(jj) = ntil(jj) + 1
      end if
      if (cnop > 1.e-4) call curno(cnop,jj)
      
      !ntil(jj) = ntil(jj) + 1 ' orig code

      return
      end subroutine mgt_newtillmix