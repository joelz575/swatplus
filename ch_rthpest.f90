      subroutine ch_rthpest
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine computes the hourly stream pesticide balance 
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
!!    hdepth(:)     |m             |depth of flow in hour
!!    hru_sub(:)    |none          |subbasin number where reach is located
!!    rchwtr        |m^3 H2O       |water stored in reach at beginning of day
!!    rtwtr         |m^3 H2O       |water leaving reach on day
!!    sedpst_act(:) |m             |depth of active sediment layer in reach for
!!                                 |pesticide
!!    sedpst_bry(:) |m/day         |pesticide burial velocity in river bed
!!                                 |sediment
!!    sedpst_conc(:)|mg/(m**3)     |inital pesticide concentration in river bed
!!                                 |sediment
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
!!    hsolpst(:)  |mg pst/m^3    |soluble pesticide concentration in outflow
!!                               |on day
!!    hsorpst(:)  |mg pst/m^3    |sorbed pesticide concentration in outflow
!!                               |on day
!!    volatpst    |mg pst        |amount of pesticide in reach lost by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bedvol      |m^3           |volume of river bed sediment
!!    chpstmass   |mg pst        |mass of pesticide in reach
!!    depth       |m             |depth of water in reach
!!    fd2         |
!!    frsol       |none          |fraction of pesticide in reach that is soluble
!!    frsrb       |none          |fraction of pesticide in reach that is sorbed
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    pstin       |mg pst        |total pesticide transported into reach
!!                               |during time step
!!    sedcon      |g/m^3         |sediment concentration
!!    sedpstmass  |mg pst        |mass of pesticide in bed sediment
!!    solpstin    |mg pst        |soluble pesticide entering reach during 
!!                               |time step
!!    sorpstin    |mg pst        |sorbed pesticide entering reach during
!!                               |time step
!!    thour       |hour          |flow duration
!!    wtrin       |m^3 H2O       |volume of water entering reach during time
!!                               |step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use jrw_datalib_module
      use time_module
      use parm, only : pest_sol

      integer :: ii
      real :: solpstin, sorpstin, pstin, depth, chpstmass, frsol, frsrb
      real :: sedpstmass, bedvol, fd2, wtrin, solmax, sedcon, tday

!! calculate volume of active river bed sediment layer
      bedvol = 0.
      bedvol =ch_hyd(jhyd)%w * ch_hyd(jhyd)%l*1000.*                     &
                    ch_pst(jpst)%sedpst_act

      do ii = 1, time%step
!! initialize depth of water for pesticide calculations
      depth = 0.
      if (hdepth(ii) < 0.1) then
        depth = .1
      else
        depth = hdepth(ii)
      endif

!! calculate volume of water entering reach
      wtrin = 0.
      wtrin = ob(icmd)%ts(1,ii)%flo 
         
!! pesticide transported into reach during day
      solpstin = 0.
      sorpstin = 0.
      pstin = 0.
      solpstin = ob(icmd)%ts(1,ii)%psol 
      sorpstin = ob(icmd)%ts(1,ii)%psor 
      pstin = solpstin + sorpstin

!! add pesticide drifting from HRUs in subbasin to reach
!      if (rtwtr > 0.) then
!        pstin = pstin + (drift(jrch) * 1.e6)
!      else
!        sedpst_conc(jrch) = sedpst_conc(jrch) + drift(jrch) * 1.e6 /    
!     &                                                            bedvol
!      endif
 
      !! calculate mass of pesticide in reach
      chpstmass = 0.
      chpstmass = pstin + ch(jrch)%pst_conc * hrchwtr(ii)
      
      !! calculate mass of pesticide in bed sediment
      sedpstmass = 0.
      sedpstmass = ch_pst(jpst)%sedpst_conc * bedvol

      if (chpstmass + sedpstmass < 1.e-6) then
        ch(jrch)%pst_conc = 0.
        ch_pst(jpst)%sedpst_conc = 0.
      end if
      if (chpstmass + sedpstmass < 1.e-6) return

!!in-stream processes
      if (hrtwtr(ii) / (time%dtm*60.) > 0.01) then
        !! calculated sediment concentration
        sedcon = 0.
        sedcon = hsedyld(ii) / hrtwtr(ii) * 1.e6

        !! calculate fraction of soluble and sorbed pesticide
        frsol = 0.
        frsrb = 0.
        if (solpstin + sorpstin > 1.e-6) then
          if (ch_pst(jpst)%pst_koc > 0.) then
            frsol = 1. / (1. + ch_pst(jpst)%pst_koc)
          else
            frsol = solpstin / (solpstin + sorpstin)
          end if
!         frsol = 1. / (1. + chpst_koc(jrch) * sedcon)
          frsrb = 1. - frsol
        else
          !!drifting pesticide is only pesticide entering
          !!and none is sorbed
          frsol = 1.
          frsrb = 0.
        end if

        !! ASSUME POR=0.5; DENSITY=2.6E6; KD2=KD1
        fd2 = 1. / (.5 + ch_pst(jpst)%pst_koc)

        !! calculate flow duration
         thour = 0.
         thour = hhtime(ii)
         if (thour > 1.0) thour = 1.0
         thour = 1.0

        !! calculate amount of pesticide that undergoes chemical or
        !! biological degradation on day in reach
        reactw = ch_pst(jpst)%pst_rea * chpstmass * thour / 24.
        chpstmass = chpstmass - reactw

        !! calculate amount of pesticide that volatilizes from reach
        volatpst = ch_pst(jpst)%pst_vol * frsol * chpstmass * thour      &
                                                        / (depth * 24.)
        if (volatpst > chpstmass) then
          volatpst = chpstmass
          chpstmass = 0.
        else
          chpstmass = chpstmass - volatpst
        end if

        !! calculate amount of pesticide removed from reach by
        !! settling
        setlpst = ch_pst(jpst)%pst_rsp * frsrb * chpstmass * thour        &
                                                        / (depth * 24.)
        if (setlpst > chpstmass) then
          setlpst = chpstmass
          chpstmass = 0.
        else
          chpstmass = chpstmass - setlpst
        end if
        sedpstmass = sedpstmass + setlpst

        !! calculate resuspension of pesticide in reach
        resuspst = ch_pst(jpst)%pst_rsp * sedpstmass * thour/(depth*24.)
        if (resuspst > sedpstmass) then
          resuspst = sedpstmass
          sedpstmass = 0.
        else
          sedpstmass = sedpstmass - resuspst
        end if
        chpstmass = chpstmass + resuspst

        !! calculate diffusion of pesticide between reach and sediment
        difus =ch_pst(jpst)%pst_mix * (fd2 * sedpstmass - frsol *       & 
                                     chpstmass) * thour / (depth * 24.)
        if (difus > 0.) then
          if (difus > sedpstmass) then
            difus = sedpstmass
            sedpstmass = 0.
          else
            sedpstmass = sedpstmass - Abs(difus)
          end if
          chpstmass = chpstmass + Abs(difus)
        else
          if (Abs(difus) > chpstmass) then
            difus = -chpstmass
            chpstmass = 0.
          else
            chpstmass = chpstmass - Abs(difus)
          end if
          sedpstmass = sedpstmass + Abs(difus)
        end if

        !! calculate removal of pesticide from active sediment layer
        !! by burial
        bury = ch_pst(jpst)%sedpst_bry*sedpstmas/                        &
                (ch_pst(jpst)%sedpst_act * 24.)
        if (bury > sedpstmass) then
          bury = sedpstmass
          sedpstmass = 0.
        else
          sedpstmass = sedpstmass - bury
        end if

        !! verify that water concentration is at or below solubility
        solmax = 0.
        solmax = pest_sol * (rchwtr + wtrin)
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
      reactb = ch_pst(jpst)%sedpst_rea * sedpstmass / 24.
      if (reactb > sedpstmass) then
        reactb = sedpstmass
        sedpstmass = 0.
      else
        sedpstmass = sedpstmass - reactb
      end if

!! calculate pesticide concentrations at end of hour
      ch(jrch)%pst_conc = 0.
      ch_pst(jpst)%sedpst_conc = 0.
      if (hrchwtr(ii) + wtrin > 1.e-6) then
        ch(jrch)%pst_conc = chpstmass / (hrchwtr(ii) + wtrin)
      else
        sedpstmass = sedpstmass + chpstmass
      end if
      ch_pst(jpst)%sedpst_conc = sedpstmass / bedvol

!! calculate amount of pesticide transported out of reach
      if (hrtwtr(ii) > 0.001) then
        hsolpst(ii) = ch(jrch)%pst_conc * frsol
        hsorpst(ii) = ch(jrch)%pst_conc * frsrb
      else
        hsolpst(ii) = 0.
        hsorpst(ii) = 0.
      end if
      end do

      return
      end subroutine ch_rthpest