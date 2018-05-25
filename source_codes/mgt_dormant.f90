      subroutine mgt_dormant

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai_min(:)    |m**2/m**2     |minimum LAI during winter dormant period
!!    bio_leaf(:)    |none          |fraction of biomass that drops during
!!                                  |dormancy (for trees only)
!!    daylmn(:)      |hours         |shortest daylength occurring during the
!!                                  |year
!!    dormhr(:)      |hour          |time threshold used to define dormant
!!                                  |period for plant (when daylength is within
!!                                  |the time specified by dormhr from the minimum
!!                                  |daylength for the area, the plant will go
!!                                  |dormant)
!!    ihru           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use hydrograph_module
      use plant_data_module
      use organic_mineral_mass_module
      use hru_module, only : hru, dormhr, phubase, sol_sumno3, sol_sumsolp, ipl, ihru,  &
         sol_sumno3, sol_sumsolp
      use soil_module
      use plant_module
      use carbon_module
      use time_module
      
      implicit none

      real :: resnew                !              |
      real :: resnew_n              !              |
      integer :: j                  !none          |HRU number
      integer :: idp                !              |
      integer :: iob                !              |
      integer :: iwgn               !              |
      real :: dorm_flag             !              |
      real :: xx                    !varies        |variable to hold calculation results 
      real :: rln                   !              |  
      real :: rlr                   !fraction      |fraction of lignin in the added residue

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgn = wst(iwst)%wco%wgn

!! check for beginning of dormant season
      if (pldb(idp)%idc == 'warm_annual_legume' .or. pldb(idp)%idc == 'warm_annual') return
      if (pcom(j)%plcur(ipl)%idorm == 'n' .and. wst(iwst)%weat%daylength - dormhr(j) < wgn_pms(iwgn)%daylmn) then
                
          !! make sure all operations are scheduled during growing season of warm season annual
          ! case (1,4)
          if (pldb(idp)%idc == 'warm_annual_legume' .or. pldb(idp)%idc == 'warm_annual') then 
            dorm_flag = 1
            call mgt_operatn
            dorm_flag = 0
          end if

          !! beginning of forest dormant period
          if (pldb(idp)%idc == 'trees') then
            pcom(j)%plcur(ipl)%idorm = 'y'
            resnew = pcom(j)%plm(ipl)%mass * pcom(j)%plg(ipl)%bio_leaf
            resnew_n = resnew * pcom(j)%plm(ipl)%n_fr
            call mgt_bio_drop (resnew, resnew_n)
          end if

          !! beginning of perennial (pasture/alfalfa) dormant period
          if (pldb(idp)%idc == 'perennial_legume' .or. pldb(idp)%idc == 'perennial') then
            pcom(j)%plcur(ipl)%idorm = 'y'
            resnew = pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%mass 
            resnew_n = pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%nmass
            call mgt_bio_drop (resnew, resnew_n)
          end if

          !! beginning of cool season annual dormant period
          ! case (2,5)
          if (pldb(idp)%idc == 'cold_annual_legume' .or. pldb(idp)%idc == 'cold_annual') then
            if (pcom(j)%plcur(ipl)%phuacc < 0.75) then
              pcom(j)%plcur(ipl)%idorm = 'y'
              pcom(j)%plstr(ipl)%strsw = 1.
            end if 
          end if

           if (pco%mgtout ==  'y') then
            write (2612, 1000) j, time%yrc, time%mo, time%day,           &
              pldb(idp)%plantnm, "START-DORM", phubase(j),               & 
              pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,                     &
              pcom(j)%plm(ipl)%mass,soil(j)%ly(1)%rsd,                   &
              sol_sumno3(j), sol_sumsolp(j)
           end if           
         end if

        !! check if end of dormant period
        if (pcom(j)%plcur(ipl)%idorm == 'y' .and. wst(iwst)%weat%daylength - dormhr(j) >=   &
            wgn_pms(iwgn)%daylmn) then

         if (pldb(idp)%idc == 'perennial_legume' .or. pldb(idp)%idc == 'perennial' .or.   &
           pldb(idp)%idc == 'trees') then
           !! end of perennial dormant period
           pcom(j)%plcur(ipl)%idorm = 'n'
         end if

         !! end of cool season annual dormant period
         if (pldb(idp)%idc == 'cold_annual_legume' .or. pldb(idp)%idc == 'cold_annual') then
           pcom(j)%plcur(ipl)%idorm = 'n'
           pcom(j)%plcur(ipl)%phuacc = 0.
         end if
            
          if (pco%mgtout == 'y') then
            write (2612,1000) j, time%yrc, time%mo, time%day,           & 
              pldb(idp)%plantnm, "END-DORM", phubase(j),                &               
              pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,                    &
              pcom(j)%plm(ipl)%mass,soil(j)%ly(1)%rsd,                  &
              sol_sumno3(j), sol_sumsolp(j)
          end if

        end if

1000  format (4i6,5x,2a15,7f10.2)
      return
      end subroutine mgt_dormant