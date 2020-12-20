      subroutine hru_hyds
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for subbasins with multiple HRUs and
!!    prints the daily output.hru file

      use hru_module, only : cbodu, chl_a, clayld, doxq, hhsurfq, hru, ihru, itb, lagyld, latq, peakr, percn, qday,  &
         sagyld, sanyld, silyld, sedminpa, sedminps, sedorgn, sedorgp, sepbtm, surqno3, surqsolp, tileno3,     &
         tmpav, sedyld, latno3, qtile
      use hydrograph_module
      use basin_module
      use time_module
      use constituent_mass_module
      use output_ls_pesticide_module
      
      implicit none

      integer :: j                   !none          |same as ihru (hru number)
      real :: cnv_m3                 !              |
      real :: cnv_kg                 !              |
      integer :: iob                 !              |
      integer :: ihyd                !none          |counter
      integer :: ipest               !none          |counter
      integer :: ipath               !none          |counter 

      j = ihru
      cnv_m3 = hru(j)%area_ha * 10.
      cnv_kg = hru(j)%area_ha
      
      !! assign reach loadings for subbasin
      !! zero out hydrograph storage locations
      iob = icmd 
      ob(icmd)%hd(3) = hz

      !! surface runoff hydrograph (3)
      ob(icmd)%peakrate = peakr
      ob(icmd)%hd(3)%temp = 5. + .75 * tmpav(j)       !!wtmp
      ob(icmd)%hd(3)%flo = qday * cnv_m3              !!qdr m3/d
      ob(icmd)%hd(3)%sed = sedyld(j)                  !!sedyld
      ob(icmd)%hd(3)%orgn = sedorgn(j) * cnv_kg       !!sedorgn
      ob(icmd)%hd(3)%sedp = (sedorgp(j) + sedminpa(j) +                 &
                        sedminps(j)) * cnv_kg         !!sedorgp & sedminps
      ob(icmd)%hd(3)%no3 = surqno3(j) * cnv_kg        !!surqno3 & latno3 & no3gw
      ob(icmd)%hd(3)%solp = surqsolp(j) * cnv_kg      !!surqsolp & sedminpa
      ob(icmd)%hd(3)%chla = chl_a(j) *cnv_kg          !!chl_a
      ob(icmd)%hd(3)%nh3 = 0.                         !! NH3
      ob(icmd)%hd(3)%no2 = 0.                         !! NO2
      ob(icmd)%hd(3)%cbod = cbodu(j) * cnv_kg         !!cbodu
      ob(icmd)%hd(3)%dox = doxq(j) *cnv_kg            !!doxq & soxy

      ob(icmd)%hd(3)%san = sanyld(j)                  !! detached sand
      ob(icmd)%hd(3)%sil = silyld(j)                  !! detached silt
      ob(icmd)%hd(3)%cla = clayld(j)                  !! detached clay
      ob(icmd)%hd(3)%sag = sagyld(j)                  !! detached small aggregates
      ob(icmd)%hd(3)%lag = lagyld(j)                  !! detached large aggregates
      
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(3)%pest(ipest) = (hpestb_d(j)%pest(ipest)%surq + hpestb_d(j)%pest(ipest)%sed) * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(3)%path(ipath) = 0
      end do
      
      !recharge hydrograph (2)
      ob(icmd)%hd(2)%flo = sepbtm(j) * cnv_m3           !! recharge flow
      ob(icmd)%hd(2)%no3 = percn(j) * cnv_kg            !! recharge nitrate
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(2)%pest(ipest) = hpestb_d(j)%pest(ipest)%perc * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(2)%path(ipath) = 0
      end do
      
      !lateral soil flow hydrograph (4)
      ob(icmd)%hd(4)%flo = latq(j) * cnv_m3             !! lateral flow
      ob(icmd)%hd(4)%no3 = latno3(j) * cnv_kg
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(4)%pest(ipest) = hpestb_d(j)%pest(ipest)%latq * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(4)%path(ipath) = 0
      end do
      
      !tile flow hydrograph (5)
      ob(icmd)%hd(5)%flo = qtile * cnv_m3               !! tile flow
      ob(icmd)%hd(5)%no3 = tileno3(j) * cnv_kg          !! tile flow nitrate 
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(5)%pest(ipest) = hpestb_d(j)%pest(ipest)%tileq * cnv_kg
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(5)%path(ipath) = 0.
      end do
      
      !sum to obtain the total outflow hydrograph (1)
      ob(icmd)%hd(1) = hz
      do ihyd = 3, 5
        ob(icmd)%hd(1) = ob(icmd)%hd(1) + ob(icmd)%hd(ihyd)
      end do
      !set constituents
      do ipest = 1, cs_db%num_pests
        obcs(icmd)%hd(1)%pest(ipest) = obcs(icmd)%hd(3)%pest(ipest) + obcs(icmd)%hd(4)%pest(ipest) +    &
                                                                      obcs(icmd)%hd(5)%pest(ipest)
      end do
      do ipath = 1, cs_db%num_paths
        obcs(icmd)%hd(1)%path(ipath) = 0
      end do
      
      !! set subdaily hydrographs
      if (time%step > 0) then
        call flow_hyd_ru_hru (ob(icmd)%day_cur, ob(icmd)%hd(3)%flo, ob(icmd)%hd(4)%flo,     &
                                        ob(icmd)%hd(5)%flo, ob(icmd)%uh, ob(icmd)%hyd_flo)
      end if

      return   
      end subroutine hru_hyds
