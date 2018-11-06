      subroutine res_pest (jres)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the lake hydrologic pesticide balance.

      use reservoir_data_module
      use reservoir_module
      use hydrograph_module, only : res, ob
      use constituent_mass_module
      
      implicit none      
      
      real :: tpest1                !mg pst        |amount of pesticide in water
      real :: tpest2                !mg pst        |amount of pesticide in benthic sediment
      real :: fd1                   !              |frac of soluble pesticide in water column
      real :: fd2                   !              |frac of sorbed pesticide in water column
      real :: fp1                   !              |frac of soluble pesticide in benthic column
      real :: fp2                   !              |frac of sorbed pesticide in benthic column
      real :: depth                 !              |average depth of reservoir
      real :: bedvol                !m^3           |volume of river bed sediment
      integer :: jres               !none          |reservoir number  
      integer :: ipst               !none          |counter
      integer :: icmd               !none          |
      integer :: jpst               !none          |counter
      integer :: idb                !none          |

      if (res(jres)%flo > 1.) then
          
      do ipst = 1, cs_db%num_pests
        icmd = res_ob(jres)%ob
        idb = ob(icmd)%props
        jpst = res_dat(idb)%pst
        solpesti = obcs(icmd)%hin%pest(ipst)%sol
        sorpesti = obcs(icmd)%hin%pest(ipst)%sor
        tpest1 = solpesti + sorpesti + res_water(jres)%pest(ipst)%sol + res_water(jres)%pest(ipst)%sor
        bedvol = 1000. * res_om_d(jres)%area_ha * res_pst(jpst)%spst_act + .01
        tpest2 = (res_benthic(jres)%pest(ipst)%sol + res_benthic(jres)%pest(ipst)%sor) * bedvol

        !! calculate average depth of reservoir
        depth = res(jres)%flo / (res_om_d(jres)%area_ha * 10000.)
        !! sor conc/sol conc = Koc *frac_oc -> (sor mass/mass sed) / (sol mass/mass water) = Koc * frac_oc
        !! -> sor mass/sol mass = Koc * frac_oc * sed conc --> sol mass/tot mass = 1 / (1 + Koc * frac_oc)
        !! assume fraction organic = 1% ; sed conc = kg/L = t/m^3
        fd1 = 1. / (1. + .01 * res_pst(jpst)%pst_koc * res(jres)%sed)
        fd1 = amin1 (1., fd1)
        fp1 = 1. - fd1
        !! assume; fraction organic = 1%;\; por=0.8; density=2.6 t/m^3
        fd2 = 1. / (.8 + .026 * res_pst(jpst)%pst_koc)
        fd2 = amin1 (1., fd2)
        fp2 = 1. - fd2

        !! add incoming pesticide to pesticide in water layer
        respesti = solpesti + sorpesti
        tpest1 = tpest1 + respesti

        !! determine pesticide lost through reactions in water layer
        reactw = res_pst(jpst)%pst_rea * tpest1
        tpest1 = tpest1 - reactw

        !! determine pesticide lost through volatilization
        volatpst = res_pst(jpst)%pst_vol * fd1 * tpest1 / depth
        if (volatpst > tpest1) then
          volatpst = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - volatpst
        end if

        !! determine amount of pesticide settling to sediment layer
        setlpst = res_pst(jpst)%pst_stl * fp1 * tpest1 / depth
        if (setlpst > tpest1) then
          setlpst = tpest1
          tpest1 = 0.
          tpest2 = tpest2 + setlpst
        else
          tpest1 = tpest1 - setlpst
          tpest2 = tpest2 + setlpst
        end if

        !! determine pesticide resuspended into lake water
        resuspst = res_pst(jpst)%pst_rsp * tpest2/res_pst(jpst)%spst_act
        if (resuspst > tpest2) then
          resuspst = tpest2
          tpest2 = 0.
          tpest1 = tpest1 + resuspst
        else
          tpest2 = tpest2 - resuspst
          tpest1 = tpest1 + resuspst
        end if

        !! determine pesticide diffusing from sediment to water
        difus = res_pst(jpst)%pst_mix *                                 &                                
              (fd2 * tpest2 / res_pst(jpst)%spst_act-fd1*tpest1 / depth)
        if (difus > 0.) then
          if (difus > tpest2) then
            difus = tpest2
            tpest2 = 0.
          else
            tpest2 = tpest2 - Abs(difus)
          end if
          tpest1 = tpest1 + Abs(difus)
        else
          if (Abs(difus) > tpest1) then
            difus = -tpest1
            tpest1 = 0.
          else
            tpest1 = tpest1 - Abs(difus)
          end if
          tpest2 = tpest2 + Abs(difus)
        end if

        !! determine pesticide lost from sediment by reactions
        reactb = res_pst(jpst)%spst_rea * tpest2
        if (reactb > tpest2) then
          reactb = tpest2
          tpest2 = 0.
        else
          tpest2 = tpest2 - reactb
        end if

        !! determine pesticide lost from sediment by burial
        bury = res_pst(jpst)%spst_bry * tpest2 / res_pst(jpst)%spst_act
        if (bury > tpest2) then
          bury = tpest2
          tpest2 = 0.
        else
          tpest2 = tpest2 - bury
        end if

        !! calculate soluble pesticide transported out of reservoir
        solpesto = resflwo * fd1 * tpest1 / res(jres)%flo
        if (solpesto > tpest1) then
          solpesto = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - solpesto
        end if

        !! calculate sorbed pesticide transported out of reservoir
        sorpesto = resflwo * fp1 * tpest1 / res(jres)%flo
        if (sorpesto > tpest1) then
          sorpesto = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - sorpesto
        end if

        !! update concentration of pesticide in lake water and sediment
        if (tpest1 < 1.e-10) tpest1 = 0.0
        if (tpest2 < 1.e-10) tpest2 = 0.0
        res_water(jres)%pest(ipst)%sol = fd1 * tpest1 / res(jres)%flo
        res_water(jres)%pest(ipst)%sol = fp1 * tpest1 / res(jres)%flo
        res_benthic(jres)%pest(ipst)%sol = fd2 * tpest2 / bedvol
        res_benthic(jres)%pest(ipst)%sol = fp2 * tpest2 / bedvol

      end do
      end if

      return
      end subroutine res_pest