      subroutine ch_rtpest
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine computes the daily stream pesticide balance
!!     (soluble and sorbed)     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chpst_conc(:) |mg/(m**3)     |initial pesticide concentration in reach
!!    chpst_koc(:)  |m**3/g        |pesticide partition coefficient between
!!                                 |water and sediment in reach
!!    chpst_mix(:)  |m/day         |mixing velocity (diffusion/dispersion) for
!!                                 |pesticide in reach
!!    chpst_rea(:)  |1/day         |pesticide reaction coefficient in reach
!!    chpst_rsp(:)  |m/day         |resuspension velocity in reach for pesticide
!!                                 |sorbed to sediment
!!    chpst_stl(:)  |m/day         |settling velocity in reach for pesticide
!!                                 |sorbed to sediment
!!    chpst_vol(:)  |m/day         |pesticide volatilization coefficient in 
!!                                 |reach
!!    drift(:)      |kg            |amount of pesticide drifting onto main
!!                                 |channel in subbasin
!!    rchdep        |m             |depth of flow on day
!!    rchwtr        |m^3 H2O       |water stored in reach at beginning of day
!!    rtwtr         |m^3 H2O       |water leaving reach on day
!!    sedpst_rea(:) |1/day         |pesticide reaction coefficient in river bed
!!                                 |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    difus       |mg pst        |diffusion of pesticide from sediment to reach
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    reactw      |mg pst        |amount of pesticide in reach that is lost
!!                               |through reactions
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |reach due to resuspension
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    solpesto    |mg pst/m^3    |soluble pesticide concentration in outflow
!!                               |on day
!!    sorpesto    |mg pst/m^3    |sorbed pesticide concentration in outflow
!!                               |on day
!!    volatpst    |mg pst        |amount of pesticide in reach lost by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chpstmass   |mg pst        |mass of pesticide in reach
!!    depth       |m             |depth of water in reach
!!    fd2         |
!!    frsol       |none          |fraction of pesticide in reach that is soluble
!!    frsrb       |none          |fraction of pesticide in reach that is sorbed
!!    jrch        |none          |reach number
!!    pstin       |mg pst        |total pesticide transported into reach
!!                               |during time step
!!    sedcon      |g/m^3         |sediment concentration
!!    sedpstmass  |mg pst        |mass of pesticide in bed sediment
!!    solpstin    |mg pst        |soluble pesticide entering reach during 
!!                               |time step
!!    sorpstin    |mg pst        |sorbed pesticide entering reach during
!!                               |time step
!!    tday        |days          |flow duration
!!    wtrin       |m^3 H2O       |volume of water entering reach during time
!!                               |step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module
      use channel_module
      use ch_pesticide_module
      use hydrograph_module, only : ob, jrch, ht1, ch_stor
      use constituent_mass_module
      
      implicit none
      
      integer :: ipest          !none          |pesticide counter
      real :: solpstin          !mg pst        |soluble pesticide entering reach during 
                                !              |time step
      real :: sorpstin          !mg pst        |sorbed pesticide entering reach during
                                !              |time step
      real :: pstin             !mg pst        |total pesticide transported into reach
                                !              |during time step
      real :: depth             !m             |depth of water in reach
      real :: chpstmass         !mg pst        |mass of pesticide in reach
      real :: frsol             !none          |fraction of pesticide in reach that is soluble
      real :: frsrb             !none          |fraction of pesticide in reach that is sorbed
      real :: sedpstmass        !mg pst        |mass of pesticide in bed sediment
      real :: fd2               !units         |description
      real :: solmax            !units         |description
      real :: sedcon            !g/m^3         |sediment concentration 
      real :: tday              !none          |flow duration (fraction of 24 hr)
      real :: rchwtr            !m^3 H2O       |water stored in reach at beginning of day

      !! zero outputs
      chpst_d(jrch) = chpstz
      
      !! initialize depth of water for pesticide calculations
      if (rchdep < 0.01) then
        depth = .01
      else
        depth = rchdep
      endif

      do ipest = 1, cs_db%num_pests

        !! volume of water entering reach and stored in reach
        wtrin = ht1%flo + ch_stor(jrch)%flo
         
        !! pesticide transported into reach during day
        solpstin = hcs1%pest(ipest)%sol 
        sorpstin = hcs1%pest(ipest)%sor
        pstin = solpstin + sorpstin

        !! calculate mass of pesticide in reach
        chpstmass = pstin + (ch_water(jrch)%pest(ipest)%sol + ch_water(jrch)%pest(ipest)%sor) * wtrin
      
        !! calculate mass of pesticide in bed sediment
        sedpstmass = ch_benthic(jrch)%pest(ipest)%sol + ch_benthic(jrch)%pest(ipest)%sor

        if (chpstmass + sedpstmass < 1.e-6) then
          ch_water(jrch)%pest(ipest)%sol = 0.
          ch_water(jrch)%pest(ipest)%sor = 0.
          ch_benthic(jrch)%pest(ipest)%sol = 0.
          ch_benthic(jrch)%pest(ipest)%sor = 0.
        end if
        if (chpstmass + sedpstmass < 1.e-6) return

        !!in-stream processes
        if (rtwtr / 86400. > 0.002) then
          !! calculated sediment concentration
          sedcon = sedrch / rtwtr * 1.e6

          !! calculate fraction of soluble and sorbed pesticide
          if (solpstin + sorpstin > 1.e-6) then
            if (ch_pst(jpst)%pst_koc > 0.) then
              frsol = 1. / (1. + ch_pst(jpst)%pst_koc* sedcon)
            else
              frsol = solpstin / (solpstin + sorpstin)
            end if
            frsrb = 1. - frsol
          else
            !!drifting pesticide is only pesticide entering and none is sorbed
            frsol = 1.
            frsrb = 0.
          end if

          !! ASSUME POR=0.5; DENSITY=2.6E6; KD2=KD1
          fd2 = 1. / (.5 + ch_pst(jpst)%pst_koc)

          !! calculate flow duration
          tday = rttime / 24.0
          if (tday > 1.0) tday = 1.0
          tday = 1.0

          !! calculate amount of pesticide that undergoes chemical or
          !! biological degradation on day in reach
          !! MFW, 3/12/12: modify decay to be 1st order
          !! reactw = chpst_rea(jrch) * chpstmass * tday
          chpst%pest(jrch)%react = chpstmass - (chpstmass * EXP(-1. * ch_pst(jpst)%pst_rea * tday))
          chpstmass = chpstmass - chpst%pest(jrch)%react

          !! calculate amount of pesticide that volatilizes from reach
          chpst%pest(jrch)%volat = ch_pst(jpst)%pst_vol * frsol * chpstmass * tday / depth
          if (chpst%pest(jrch)%volat > frsol * chpstmass) then
            chpst%pest(jrch)%volat = frsol * chpstmass 
            chpstmass = chpstmass - chpst%pest(jrch)%volat
          else
            chpstmass = chpstmass - chpst%pest(jrch)%volat
          end if

          !! calculate amount of pesticide removed from reach by settling
          chpst%pest(jrch)%settle = ch_pst(jpst)%pst_rsp * frsrb * chpstmass * tday / depth
          if (chpst%pest(jrch)%settle >  frsrb * chpstmass) then
            chpst%pest(jrch)%settle = frsrb * chpstmass
            chpstmass = chpstmass - chpst%pest(jrch)%settle
          else
            chpstmass = chpstmass - chpst%pest(jrch)%settle
          end if
          sedpstmass = sedpstmass + chpst%pest(jrch)%settle

          !! calculate resuspension of pesticide in reach
          chpst%pest(jrch)%resus = ch_pst(jpst)%pst_rsp * sedpstmass * tday / depth
          if (chpst%pest(jrch)%resus > sedpstmass) then
            chpst%pest(jrch)%resus = sedpstmass
            sedpstmass = 0.
          else
            sedpstmass = sedpstmass - chpst%pest(jrch)%resus
          end if
          chpstmass = chpstmass + chpst%pest(jrch)%resus

          !! calculate diffusion of pesticide between reach and sediment
          chpst%pest(jrch)%difus = ch_pst(jpst)%pst_mix * (fd2 * sedpstmass - frsol * chpstmass) * tday / depth
          if (chpst%pest(jrch)%difus > 0.) then
            if (chpst%pest(jrch)%difus > sedpstmass) then
              chpst%pest(jrch)%difus = sedpstmass
              sedpstmass = 0.
            else
              sedpstmass = sedpstmass - Abs(chpst%pest(jrch)%difus)
            end if
            chpstmass = chpstmass + Abs(chpst%pest(jrch)%difus)
          else
            if (Abs(chpst%pest(jrch)%difus) > chpstmass) then
              chpst%pest(jrch)%difus = -chpstmass
              chpstmass = 0.
            else
              chpstmass = chpstmass - Abs(chpst%pest(jrch)%difus)
            end if
            sedpstmass = sedpstmass + Abs(chpst%pest(jrch)%difus)
          end if

          !! calculate removal of pesticide from active sediment layer by burial
          chpst%pest(jrch)%bury = ch_pst(jpst)%sedpst_bry * sedpstmass / ch_pst(jpst)%sedpst_act
          if (chpst%pest(jrch)%bury > sedpstmass) then
            chpst%pest(jrch)%bury = sedpstmass
            sedpstmass = 0.
          else
            sedpstmass = sedpstmass - chpst%pest(jrch)%bury
          end if

          !! verify that water concentration is at or below solubility
          solmax = ch_pst(jpst)%pst_solub * wtrin
          if (solmax < chpstmass * frsol) then
            sedpstmass = sedpstmass + (chpstmass * frsol - solmax)
            chpstmass = chpstmass - (chpstmass * frsol - solmax)
          end if
        
        else   
          !!insignificant flow
          sedpstmass = sedpstmass + chpstmass
          chpstmass = 0.
        end if

        !! sediment processes
        !! calculate loss of pesticide from bed sediments by reaction
        chpst%pest(jrch)%react_bot = ch_pst(jpst)%sedpst_rea * sedpstmass
        if (chpst%pest(jrch)%react_bot > sedpstmass) then
          chpst%pest(jrch)%react_bot = sedpstmass
          sedpstmass = 0.
        else
          sedpstmass = sedpstmass - chpst%pest(jrch)%react_bot
        end if

        !! set new pesticide mass of (in + store) after processes
        if (rchwtr + wtrin > 1.e-6) then
          hcs1%pest(ipest)%sol = frsol * chpstmass
          hcs1%pest(ipest)%sor = frsrb * chpstmass
        else
          sedpstmass = sedpstmass + chpstmass
        end if
        ch_benthic(jrch)%pest(ipest)%sol = 0.
        ch_benthic(jrch)%pest(ipest)%sor = sedpstmass

      end do

      return
      end subroutine ch_rtpest