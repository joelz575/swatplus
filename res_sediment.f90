      subroutine res_sediment (jres, ihyd, ised)

      use jrw_datalib_module
      use reservoir_module
      use conditional_module
      !use hydrograph_module
      use climate_parms
      use time_module

	real :: sed,san,sil,cla,sag,lag,gra
	real :: inised, finsed, setsed, remsetsed

      !! store initial values
	inised = 0.
	finsed = 0.
	setsed = 0.
	remsetsed = 0.

      if (res(jres)%flo < 1.e-6) then
        !reservoir is empty
        res(jres)%sed = 0.
      else
      sed = res(jres)%sed

      !!    Storage by particle sizes
      san = res(jres)%san
      sil = res(jres)%sil
      cla = res(jres)%cla
      sag = res(jres)%sag
      lag = res(jres)%lag
	gra = res(jres)%grv


        !! compute new sediment concentration in reservoir
	  if (ressedi < 1.e-6) ressedi = 0.0      
	  if (res_ob(jres)%area_ha == 0.) res_ob(jres)%area_ha = 1.e-6     !! MJW added 040711
	  velofl = (resflwo / res_ob(jres)%area_ha) / 10000.  !!m3/d / ha * 10000. = m/d
!!	  velsetl = 1.35      !! for clay particle m/d
	  if (velofl > 1.e-6) then
	    trapres = res_sed(ised)%velsetlr / velofl
	    if (trapres > 1.) trapres = 1.  !! set to mres
	    susp = 1. - trapres
	  else
	    susp = 0.
	  end if

	if (res(jres)%flo > 0.) then                         !!MJW added 040811
        res(jres)%sed = (ressedi * susp + sed * vol) / res(jres)%flo
        res(jres)%san = (ressani + san * vol) / res(jres)%flo
        res(jres)%sil = (ressili + sil * vol) / res(jres)%flo
        res(jres)%cla = (resclai + cla * vol) / res(jres)%flo
        res(jres)%sag = (ressagi + sag * vol) / res(jres)%flo
        res(jres)%lag = (reslagi + lag * vol) / res(jres)%flo
        res(jres)%grv = (resgrai + gra * vol) / res(jres)%flo

        res(jres)%sed = Max(1.e-6,res(jres)%sed)
        res(jres)%san = Max(1.e-6,res(jres)%san)
        res(jres)%sil = Max(1.e-6,res(jres)%sil)
        res(jres)%cla = Max(1.e-6,res(jres)%cla)
        res(jres)%sag = Max(1.e-6,res(jres)%sag)
        res(jres)%lag = Max(1.e-6,res(jres)%lag)
        res(jres)%grv = Max(1.e-6,res(jres)%grv)
	else
        res(jres)%sed = 1.e-6             !!MJW added 040711
        res(jres)%san = 1.e-6
        res(jres)%sil = 1.e-6
        res(jres)%cla = 1.e-6
        res(jres)%sag = 1.e-6
        res(jres)%lag = 1.e-6
        res(jres)%grv = 1.e-6
	endif
        
        !! compute change in sediment concentration due to settling
        if (res(jres)%sed < 1.e-6) res(jres)%sed = 0.0   
        if (res(jres)%sed > res_sed(ised)%nsed) then
	    inised = res(jres)%sed
          res(jres)%sed = (res(jres)%sed -res_sed(ised)%nsed) *         &        
                         res_sed(ised)%sed_stlr + res_sed(ised)%nsed
	    finsed = res(jres)%sed
	    setsed = inised - finsed

        if (res(jres)%grv >= setsed) then
	    res(jres)%grv = res(jres)%grv - setsed
	    remsetsed = 0.
	  else
	    remsetsed = setsed - res(jres)%grv
          res(jres)%grv = 0.
		if (res(jres)%lag >= remsetsed) then
	      res(jres)%lag = res(jres)%lag - remsetsed
	      remsetsed = 0.
	    else
	      remsetsed = remsetsed - res(jres)%lag
	      res(jres)%lag = 0.
	      if (res(jres)%san >= remsetsed) then
	        res(jres)%san = res(jres)%san - remsetsed
	        remsetsed = 0.
	      else
	        remsetsed = remsetsed - res(jres)%san
	        res(jres)%san = 0.
              if (res(jres)%sag >= remsetsed) then
	          res(jres)%sag = res(jres)%sag - remsetsed
	          remsetsed = 0.
	        else
	          remsetsed = remsetsed - res(jres)%sag
	          res(jres)%sag = 0.
                if (res(jres)%sil >= remsetsed) then
  	            res(jres)%sil = res(jres)%sil - remsetsed
	            remsetsed = 0.
	          else
	            remsetsed = remsetsed - res(jres)%sil
	            res(jres)%sil = 0.
                  if (res(jres)%cla >= remsetsed) then
	              res(jres)%cla = res(jres)%cla - remsetsed
	              remsetsed = 0.
	            else
	              remsetsed = remsetsed - res(jres)%cla
	              res(jres)%cla = 0.
	            end if
                end if
	        end if
	      end if
	    end if
	  endif

        end if

        !! compute sediment leaving reservoir
        ressedo = res(jres)%sed * resflwo
        ressano = res(jres)%san * resflwo
        ressilo = res(jres)%sil * resflwo
        resclao = res(jres)%cla * resflwo
        ressago = res(jres)%sag * resflwo
        reslago = res(jres)%lag * resflwo
	  resgrao = res(jres)%grv * resflwo

        !! net change in amount of sediment in reservoir for day
        ressedc = vol * sed + ressedi - ressedo - res(jres)%sed *       &       
                                                          res(jres)%flo
      end if

      return
      end subroutine res_sediment