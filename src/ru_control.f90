      subroutine ru_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!

      use hru_module, only : hhsurfq, ihru, qday 
      use ru_module
      use hydrograph_module
      use time_module
      use constituent_mass_module
      
      implicit none 
      
      character (len=3) :: ihtyp         !            |
      integer :: iday                    !            |
      integer :: ihdmx                   !            |
      real :: sumfrac                    !            |
      real :: sumarea                    !            |
      integer :: ielem                   !none        |counter
      integer :: ise                     !none        |counter
      integer :: iob                     !            |
      integer :: isd
      integer :: ihtypno                 !            |
      real :: ef                         !            | 
      real :: cnv_m3                     !            |
      integer :: ii                      !none        |counter
      integer :: iday_prev               !            | 
      real :: ssq                        !            |
      integer :: kk                      !            |
      real :: sumflo                     !            |
      integer :: itot                    !            |
      integer :: ib                      !none        |counter
      real :: rto                        !none        |cloud cover factor
      integer :: iadj                    !            | 
      integer :: istep                   !            |
      integer :: ipest                   !            |
      real :: sumflo_day                 !            |
      real :: ratio                      !frac        |fraction change in precipitation due to 
                                         !            |elevation changes
      real :: wtmp                       !deg C       |temperature of water in reach
      
           
      iday = 1
      ihdmx = 2
      cnv_m3 = 1000. * ru(iru)%da_km2

      ru_d(iru) = hz
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      ob(icmd)%hd(3) = hz
      ob(icmd)%hd(4) = hz
      ob(icmd)%hd(5) = hz
      if (cs_db%num_tot > 0) then
        obcs(icmd)%hd(1) = hin_csz
        obcs(icmd)%hd(2) = hin_csz
        obcs(icmd)%hd(3) = hin_csz
        obcs(icmd)%hd(4) = hin_csz
        obcs(icmd)%hd(5) = hin_csz
      end if
      
      sumfrac = 0.
      sumarea = 0.
      
      do ielem = 1, ru_def(iru)%num_tot
        ise = ru_def(iru)%num(ielem)
        iob = ru_elem(ise)%obj
        ihru = ru_elem(ise)%obtypno
        ht1 = hz
        ht2 = hz
        ht3 = hz
        ht4 = hz
        ht5 = hz
        delrto = hz
        
        sumfrac = sumfrac + ru_elem(ise)%frac
        sumarea = sumarea + ob(iob)%area_ha
        
        !define delivery ratio - all variables are hyd_output type

        !calculated dr = f(tconc element/ tconc sub)
        delrto = ru_elem(ise)%dr

        if (ru_elem(ise)%obtyp == "exc") then
          !! compute hyds for export coefficients-ht1==surface,ht2==groundwater
          ht1 = exco(ob(iob)%props) ** delrto
          ht2 = hz
          if (ob(iob)%area_ha > .01) then
            !per area units - mm, t/ha, kg/ha (ie hru, apex)
            ef = ru_elem(ise)%frac * ru(iru)%da_km2 / (ob(iob)%area_ha / 100.)
            ht1 = ef * ht1
          end if
          
        else
          !! for routing units, channels, reservoir, and recall objects use fraction
          ef = ru_elem(ise)%frac
          !! for hru's use define expansion factor to surface/soil and recharge
          if (ob(iob)%typ == "hru" .or. ob(iob)%typ == "hru_lte") then
            !! if frac is 1.0, then the hru is not part of ru and use entire hru output
            if (ef < .99999) then
              ef = ef * ru(iru)%da_km2 / (ob(iob)%area_ha / 100.)
            end if
          end if
          
          !compute all hyd"s needed for routing
          do ihtypno = 1, ob(iob)%nhyds
            if (ihtypno /=2) then
              !! apply dr to tot, surf, lat and tile
              ht1 = ob(iob)%hd(ihtypno) ** delrto
              do ipest = 1, cs_db%num_pests
                hcs1%pest(ipest) = obcs(iob)%hd(ihtypno)%pest(ipest)    !* delrto - don't apply dr to pests
              end do
            else
              !! don't apply dr to recharge
              ht1 = ob(iob)%hd(ihtypno)
              !! set constituents
              do ipest = 1, cs_db%num_pests
                hcs1%pest(ipest) = obcs(iob)%hd(ihtypno)%pest(ipest)
              end do
            end if
            ht1 = ef * ht1
            ob(icmd)%hd(ihtypno) = ob(icmd)%hd(ihtypno) + ht1
            ru_d(iru) = ru_d(iru) + ht1
            !! set constituents
            do ipest = 1, cs_db%num_pests
              obcs(icmd)%hd(ihtypno)%pest(ipest) = obcs(icmd)%hd(ihtypno)%pest(ipest) + hcs1%pest(ipest)
            end do
          end do
          
        end if      !ru_elem(ise)%obtyp == "exc"
  
        ! sum subdaily hydrographs for each object
        !if (time%step > 0) then
        !  select case (ru_elem(ise)%obtyp)
        !  case ("hru")
        !    do ii = 1, time%step
        !      !hyd_flo(ii) = hyd_flo(ii) + hhsurfq(ihru,ii) * ru_elem(ise)%frac * delrto%flo * cnv_m3
        !    end do
        !  end select
        !end if

      end do  !element loop
      
      !! set subdaily hydrographs
      if (time%step > 0) then
        call flow_hyd_ru_hru (ob(icmd)%day_cur, ob(icmd)%hd(3)%flo, ob(icmd)%hd(4)%flo,     &
                                        ob(icmd)%hd(5)%flo, ob(icmd)%uh, ob(icmd)%hyd_flo)
      end if

	return

      end subroutine ru_control
