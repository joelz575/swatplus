      subroutine drainage_area
    
      use hydrograph_module
      use basin_module
      
      do i = 1, sp_ob%objs
        !! compute basin drainage area by summing hru areas if hru's are routed somewhere
        !! or if they are not routed and not part of a subbasin
        if (ob(i)%typ == "hru" .or. ob(i)%typ == "hru_lte") then
          if (ob(i)%src_tot > 0 .or. ob(i)%src_tot + ob(i)%subs_tot == 0) then
            bsn%area_ha = bsn%area_ha + ob(i)%area_ha
          end if
        end if
      end do
      return
      end subroutine drainage_area