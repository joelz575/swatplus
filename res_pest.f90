      subroutine res_pest (jres, ipst)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the lake hydrologic pesticide balance.

      use jrw_datalib_module
      use parm, only : respesti
      
      real :: tpest1, tpest2, fd1, fp1, fd2, dlake, fp2

      tpest1 = res(jres)%psor
      tpest2 = res(jres)%psor

      if (res(jres)%flo > 1.) then
        !! calculate depth of lake
        dlake = 0.
        dlake = res(jres)%flo / (res_ob(jres)%area_ha * 10000.)

        fd1 = 1. / (1. + res_pst(ipst)%pst_koc * res(jres)%sed * 1.e6)
        fp1 = 1. - fd1
        !! ASSUME POR=0.8; DENSITY=2.6E6, then concsed = 5.2e5; KD2=KD1
        fd2 = 1. / (.8 + 5.2e5 * res_pst(ipst)%pst_koc)
        fp2 = 1. - fd2

        !! add incoming pesticide to pesticide in water layer
        respesti = solpesti + sorpesti
        tpest1 = tpest1 + respesti

        !! determine pesticide lost through reactions in water layer
        reactw = res_pst(ipst)%pst_rea * tpest1
        tpest1 = tpest1 - reactw

        !! determine pesticide lost through volatilization
        volatpst = res_pst(ipst)%pst_vol * fd1 * tpest1 / dlake
        if (volatpst > tpest1) then
          volatpst = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - volatpst
        end if

        !! determine amount of pesticide settling to sediment layer
        setlpst = res_pst(ipst)%pst_stl * fp1 * tpest1 / dlake
        if (setlpst > tpest1) then
          setlpst = tpest1
          tpest1 = 0.
          tpest2 = tpest2 + setlpst
        else
          tpest1 = tpest1 - setlpst
          tpest2 = tpest2 + setlpst
        end if

        !! determine pesticide resuspended into lake water
        resuspst = res_pst(ipst)%pst_rsp * tpest2/res_pst(ipst)%spst_act
        if (resuspst > tpest2) then
          resuspst = tpest2
          tpest2 = 0.
          tpest1 = tpest1 + resuspst
        else
          tpest2 = tpest2 - resuspst
          tpest1 = tpest1 + resuspst
        end if

        !! determine pesticide diffusing from sediment to water
        difus = res_pst(ipst)%pst_mix *                                 &                                
              (fd2 * tpest2 / res_pst(ipst)%spst_act-fd1*tpest1 / dlake)
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
        reactb = res_pst(ipst)%spst_rea * tpest2
        if (reactb > tpest2) then
          reactb = tpest2
          tpest2 = 0.
        else
          tpest2 = tpest2 - reactb
        end if

        !! determine pesticide lost from sediment by burial
        bury = res_pst(ipst)%spst_bry * tpest2 / res_pst(ipst)%spst_act
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
        res(jres)%psor = tpest1
        res(jres)%psor = tpest2
        res_pst(ipst)%pst_conc = tpest1 / res(jres)%flo
        res_pst(ipst)%spst_conc = tpest2 /                               &
                         (res_pst(ipst)%spst_act * res_ob(jres)%area_ha * 10000. + 1.)
      else
        solpesto = 0.
        sorpesto = 0.
      end if

      return
      end subroutine res_pest