      subroutine res_sediment (jres, ihyd, ised)

      use reservoir_data_module
      use reservoir_module
      use conditional_module
      use climate_module
      use time_module
      use hydrograph_module, only : res
      
      implicit none
            
	  real :: sed                 !              |
      real :: san                 !              |
      real :: sil                 !              |
      real :: cla                 !              |
      real :: sag                 !              |
      real :: lag                 !              |
      real :: gra                 !              |
      integer :: jres             !none          |reservoir number
      real :: velofl              !              |  
      integer :: ised             !none          |counter
      real :: trapres             !              |
      real :: susp                !              |
      real :: vol                 !m^3 H2O       |volume of water applied in irrigation 
      real :: ressedc             !              |
      real :: sed_ppm, sil_ppm, cla_ppm
      integer :: ihyd             !none          |counter 

      if (res(jres)%flo < 1.e-6) then
        ! reservoir is empty
        res(jres)%sed = 0.
      else
        sed = res(jres)%sed

        !! Storage by particle sizes
        san = res(jres)%san
        sil = res(jres)%sil
        cla = res(jres)%cla
        sag = res(jres)%sag
        lag = res(jres)%lag
	    gra = res(jres)%grv

        !! compute new sediment concentration in reservoir
	    if (ressedi < 1.e-6) ressedi = 0.0      
        !! velsetl = 1.35 for clay particle m/d
	    if (res_om_d(jres)%area_ha > 1.e-6) then
          velofl = (res(jres)%flo / res_om_d(jres)%area_ha) / 10000.  ! m3/d / ha * 10000. = m/d
	      trapres = res_sed(ised)%velsetlr / velofl
	      if (trapres > 1.) trapres = 1.
	      susp = 1. - trapres
	    else
	      susp = 0.
        end if

        !! compute concentrations
	    if (res(jres)%flo > 0.) then
          vol = res(jres)%flo
          sed_ppm = 1000000. * (ressedi * susp + sed) / vol
          sed_ppm = Max(1.e-6, sed_ppm)
          sil_ppm = 1000000. * (ressili * susp + sil) / vol
          sil_ppm = Max(1.e-6, sil_ppm)
          cla_ppm = 1000000. * (resclai * susp + cla) / vol
          cla_ppm = Max(1.e-6, cla_ppm)
	    else
          sed_ppm = 1.e-6
          sil_ppm = 1.e-6
          cla_ppm = 1.e-6
	    endif
        
        !! compute change in sediment concentration due to settling 
        if (sed_ppm > res_sed(ised)%nsed) then
          sed_ppm = (sed_ppm - res_sed(ised)%nsed) * res_sed(ised)%sed_stlr + res_sed(ised)%nsed
          res(jres)%sed = sed_ppm * vol / 1000000.      ! ppm -> t
          
          sil_ppm = (sil_ppm - res_sed(ised)%nsed) * res_sed(ised)%sed_stlr + res_sed(ised)%nsed
          res(jres)%sil = sil_ppm * vol / 1000000.      ! ppm -> t
          
          cla_ppm = (cla_ppm - res_sed(ised)%nsed) * res_sed(ised)%sed_stlr + res_sed(ised)%nsed
          res(jres)%cla = cla_ppm * vol / 1000000.      ! ppm -> t

          !! assume all settles
          res(jres)%san = 0.
          res(jres)%sag = 0.
          res(jres)%lag = 0.
          res(jres)%grv = 0.
        end if

        !! compute sediment leaving reservoir - ppm -> t
        ressedo = sed_ppm * resflwo / 1000000.
        ressilo = sil_ppm * resflwo / 1000000.
        resclao = cla_ppm * resflwo / 1000000.
        ressano = 0.
        ressago = 0.
        reslago = 0.
	    resgrao = 0.

        !! net change in amount of sediment in reservoir for day
        ressedc = vol * sed + ressedi - ressedo - res(jres)%sed * res(jres)%flo
      end if

      return
      end subroutine res_sediment