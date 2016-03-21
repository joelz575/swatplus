      subroutine cbn_day

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)       |mm H2O        |amount of water applied to HRU on current
!!                                 |day
!!    bactrolp      |# colonies/ha |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# colonies/ha |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# colonies/ha |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# colonies/ha |persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    cfertn        |kg N/ha       |amount of nitrogen added to soil in
!!                                 |continuous fertilizer operation on day
!!    cfertp        |kg P/ha       |amount of phosphorus added to soil in
!!                                 |continuous fertilizer operation on day
!!    cnday(:)      |none          |curve number for current day, HRU and at
!!                                 |current soil moisture
!!    etday         |mm H2O        |actual amount of evapotranspiration that
!!                                 |occurs on day in HRU
!!    gw_q(:)       |mm H2O        |groundwater contribution to streamflow from
!!                                 |HRU on current day
!!    gwseep        |mm H2O        |amount of water recharging deep aquifer on
!!                                 |current day
!!    hru_ha(:)     |ha            |area of HRU in hectares
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_ra(:)     |MJ/m^2        |solar radiation for the day in HRU
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    iida          |julian date   |current day of simulation
!!    ihru          |none          |HRU number
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!                                 |the day
!!    latq(:)       |mm H2O        |amount of water in lateral flow in HRU for
!!                                 |the day
!!    pet_day       |mm H2O        |potential evapotranspiration for day in HRU
!!    qday          |mm H2O        |surface runoff loading to main channel for
!!                                 |day in HRU
!!    qdr(:)        |mm H2O        |total amount of water entering main channel
!!                                 |for day from HRU
!!    rchrg(:)      |mm H2O        |amount of water recharging both aquifers on
!!                                 |current day in HRU
!!    revapday      |mm H2O        |amount of water moving from the shallow
!!                                 |aquifer into the soil profile or being taken
!!                                 |up by plant roots in the shallow aquifer
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sepbtm(:)     |mm H2O        |seepage leaving the bottom of the soil
!!                                 |profile on day in HRU
!!    snofall       |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow on day in HRU
!!    snomlt        |mm H2O        |amount of water in snow melt for the day in
!!                                 |HRU
!!    surfq(:)      |mm H2O        |surface runoff generated on day in HRU
!!    tloss         |mm H2O        |amount of water removed from surface runoff
!!                                 |via transmission losses on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use time_module
      use basin_module

      integer :: j, sb, ii, iflag
      character (len=4) :: cropname

      !!by zhang print out soil water
      !!===============================    
      integer :: ly
      real :: sumwater, sumwfsc, sumdepth, sat, wc, dp
      real :: ssoilwater(100), swfsc(100)
      real :: soilwater(11), wfsc(11), sum_depth(11) !10, 100, 200, 300, 400, ..., 1000 mm
      !!by zhang print out soil water
      !!===============================

      !!by zhang print out soil water
      !!===============================
      if (bsn_cc%cswat == 2) then
          soilwater(1) = 0.
          wfsc(1) = 0.
          sum_depth(1) = 10.
          do k = 2, 11
            soilwater(k) = 0.
            wfsc(k) = 0.
            sum_depth(k) = 100. * (k -1)
          end do
          
          wc = soil(ihru)%phys(1)%st + soil(ihru)%phys(1)%wpmm
          sat = soil(ihru)%phys(1)%ul + soil(ihru)%phys(1)%wpmm
          soilwater(1) = wc      
          wfsc(1) = soil(ihru)%phys(1)%por * (wc / sat)   ! fraction
          
          if (hru(ihru)%sol%nly .ge. 2) then
              do k = 2, 11
                sumwater = 0.
                sumwfsc = 0.
                sumdepth = 0.
                do ly = 2, hru(ihru)%sol%nly
                    if (soil(ihru)%phys(ly-1)%d.ge. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .le. sum_depth(k)) then

                              dp = soil(ihru)%phys(ly)%d - soil(ihru)%phys(ly-1)%d
                              if (dp .gt. 0.) then
                                  wc = soil(ihru)%phys(ly)%st + soil(ihru)%phys(ly)%wpmm*(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  sat = soil(ihru)%phys(ly)%ul + soil(ihru)%phys(ly)%wpmm*(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + soil(ihru)%phys(ly)%por * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                    
                    elseif ((soil(ihru)%phys(ly-1)%d .gt. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .gt. sum_depth(k)) &
                            .or. (soil(ihru)%phys(ly-1)%d .ge. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .gt. sum_depth(k)) &
                            .or. (soil(ihru)%phys(ly-1)%d .gt. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .ge. sum_depth(k))) &
                             then
                            if (soil(ihru)%phys(ly-1)%d .le. sum_depth(k)) then 
                              dp = sum_depth(k) - soil(ihru)%phys(ly-1)%d
                              if (dp .gt. 0.) then
                                  wc = (soil(ihru)%phys(ly)%st + soil(ihru)%phys(ly)%wpmm) *(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  sat = (soil(ihru)%phys(ly)%ul + soil(ihru)%phys(ly)%wpmm) *(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + soil(ihru)%phys(ly)%por * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                            end if
                    elseif ((soil(ihru)%phys(ly-1)%d .lt. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .lt. sum_depth(k)) & 
                            .or. (soil(ihru)%phys(ly-1)%d .le. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .lt. sum_depth(k)) & 
                            .or. (soil(ihru)%phys(ly-1)%d .lt. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .le. sum_depth(k))) &
                             then
                            if (soil(ihru)%phys(ly)%d .ge. sum_depth(k-1)) then
                              dp = soil(ihru)%phys(ly)%d - sum_depth(k-1)
                              if (dp .gt. 0.) then
                                  wc = (soil(ihru)%phys(ly)%st + soil(ihru)%phys(ly)%wpmm)*(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  sat = (soil(ihru)%phys(ly)%ul + soil(ihru)%phys(ly)%wpmm) *(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + soil(ihru)%phys(ly)%por * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                            end if
                    
                    elseif ((soil(ihru)%phys(ly-1)%d .lt. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .gt. sum_depth(k)) & 
                             .or. (soil(ihru)%phys(ly-1)%d .le. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .gt. sum_depth(k)) & 
                             .or. (soil(ihru)%phys(ly-1)%d .lt. sum_depth(k-1) .and. soil(ihru)%phys(ly)%d .ge. sum_depth(k))) &
                              then 
                              dp = sum_depth(k) - sum_depth(k-1)
                              if (dp .gt. 0.) then
                                  wc = (soil(ihru)%phys(ly)%st + soil(ihru)%phys(ly)%wpmm)*(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  sat = (soil(ihru)%phys(ly)%ul + soil(ihru)%phys(ly)%wpmm)*(dp/(soil(ihru)%phys(ly)%d-soil(ihru)%phys(ly-1)%d))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + soil(ihru)%phys(ly)%por * (wc / sat) * dp
                                  sumdepth = sumdepth + dp    
                              end if
                    end if
                end do !!End lyr
                
                if (sumdepth .gt. 0.) then
                      soilwater(k) = sumwater / sumdepth     
                      wfsc(k) = sumwfsc / sumdepth   ! fraction                
                end if
                
              end do !!end k
              
              
          end if
      end if
      !!by zhang print out soil water
      !!===============================

      !!add by zhang
      !!output carbon realted variables
      !!=================================
      if (bsn_cc%cswat == 2) then
          if (j == 1) then
          tot_mass = 0.
          tot_cmass = 0.
          tot_nmass = 0.
          tot_LSC = 0.
          tot_LMC = 0.
          tot_HSC = 0.
          tot_HPC = 0.
          tot_BMC = 0.
          tot_pmass = 0. 
          tot_solp = 0.
          tot_no3_nh3 =0.
          do k = 1, hru(j)%sol%nly 
              sol_mass = 0.
              if (k == 1) then
 		        sol_mass = (10) / 1000.* 10000. * soil(j)%phys(k)%bd* 1000. * (1- soil(j)%phys(k)%rock / 100.)            
              else
		        sol_mass = (soil(j)%phys(k)%d - soil(j)%phys(k-1)%d) / 1000.* 10000. * soil(j)%phys(k)%bd * 1000. *	(1- soil(j)%phys(k)%rock / 100.)
	         end if       
          sol_cmass = 0.
          sol_cmass = soil(j)%cbn(k)%lsc+soil(j)%cbn(k)%lmc+soil(j)%cbn(k)%hpc+soil(j)%cbn(k)%hsc +soil(j)%cbn(k)%bmc
          sol_nmass = 0. 
          sol_nmass = soil(j)%cbn(k)%lsn+soil(j)%cbn(k)%lmn+soil(j)%cbn(k)%hpn+soil(j)%cbn(k)%hsn +soil(j)%cbn(k)%bmn     
          write (98,9000) time%yrc, i, k, j, sol_mass,sol_cmass,                       &
             sol_nmass,soil(j)%cbn(k)%ls,soil(j)%cbn(k)%lm,                                &
             soil(j)%cbn(k)%lsc,soil(j)%cbn(k)%lmc,soil(j)%cbn(k)%hsc,soil(j)%cbn(k)%hpc,      &
             soil(j)%cbn(k)%bmc,soil(j)%cbn(k)%lsn,soil(j)%cbn(k)%lmn,soil(j)%cbn(k)%hpn,    &
             soil(j)%cbn(k)%hsn,soil(j)%cbn(k)%bmn,soil(j)%nut(k)%no3,soil(j)%nut(k)%fop,  &
             soil(j)%nut(k)%orgp,soil(j)%nut(k)%solp   
         
           tot_mass = tot_mass + sol_mass
           tot_cmass = tot_cmass + sol_cmass 
           tot_nmass = tot_nmass + sol_nmass
           tot_LSC = tot_LSC + soil(j)%cbn(k)%lsc
           tot_LMC = tot_LMC + soil(j)%cbn(k)%lmc
           tot_HSC = tot_HSC + soil(j)%cbn(k)%hsc
           tot_HPC = tot_HPC + soil(j)%cbn(k)%hpc
           tot_BMC = tot_BMC + soil(j)%cbn(k)%bmc
           tot_pmass =tot_pmass+ soil(j)%nut(k)%orgp + soil(j)%nut(k)%fop +  soil(j)%nut(k)%solp
           tot_solp = tot_solp + soil(j)%nut(k)%solp
           
           tot_no3_nh3 = tot_no3_nh3  + soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3
          end do      

          write (100,9001) time%yrc, i, j, rsdc_d(j), sedc_d(j), percc_d(j),        &
              latc_d(j),emitc_d(j), grainc_d(j), surfqc_d(j), stoverc_d(j),         &
              NPPC_d(j), foc_d(j),rspc_d(j),tot_mass,tot_cmass,tot_nmass,           &
              tot_LSC,tot_LMC,tot_HSC,tot_HPC,tot_BMC,                              &
              sumbm*0.42, sumrwt, tot_no3_nh3,wdntl,etday,tillage_factor(j),        &
              (soilwater(ii), ii = 1, 11), (wfsc(ii), ii = 1, 11)     
          end if  
      end if
      !!add by zhang
      !!output carbon related variables
      !!=================================

      return

9000  format(i4,i4,i2,i8,21(f16.3))
9001  format(i4,i4,i8,48(f16.3))
      end subroutine cbn_day