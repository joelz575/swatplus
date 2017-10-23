      subroutine dr_ru
    
      use hydrograph_module
      use hru_lte_module
      use ru_module
      use parm, only : ihru, tconc
    
     ! compute delivery ratio for each hru in the sub
      sumn = 0.
      do isub = 1, sp_ob%sub
        do ii = 1, ru_def(isub)%num_tot
          ielem = ru_def(isub)%num(ii)
          ihru = ru_elem(ielem)%obtypno

          if (ru_elem(ielem)%idr == 0) then
            allocate (ru_elem(ielem)%dr(0:0))
            select case (ru_elem(ielem)%obtyp)
            case ("hru")
              rto = tconc(ihru) / ru_tc(isub)
            case ("hlt")
              rto = (hlt_db(ihru)%tc / 3600.) / ru_tc(isub)
            case ("sdc")
              rto = 1.
            case ("sub")
              rto = 1.
            end select
            
            rto = amin1(1.0, rto ** .5)
            ru_elem(ielem)%dr(0) = rto .add. hz
          end if
        end do
        ru_n(isub) = sumn / ru(isub)%da_km2
      end do
      return
      end subroutine dr_ru
      