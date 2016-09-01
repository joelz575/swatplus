      subroutine dr_sub
    
      use hydrograph_module
      use sd_hru_module
      use subbasin_module
    
     ! compute delivery ratio for each hru in the sub
      sumn = 0.
      do isub = 1, sp_ob%sub
        do ii = 1, sub_d(isub)%num_tot
          ielem = sub_d(isub)%num(ii)
          ihru = sub_elem(ielem)%obtypno

          if (sub_elem(ielem)%idr == 0) then
            allocate (sub_elem(ielem)%dr(0:0))
            select case (sub_elem(ielem)%obtyp)
            case ("hru")
              rto = tconc(ihru) / sub_tc(isub)
            case ("hlt")
              rto = (sd_db(ihru)%tc / 3600.) / sub_tc(isub)
            case ("sdc")
              rto = 1.
            case ("sub")
              rto = 1.
            end select
            
            rto = amin1(.95,rto ** .5)
            sub_elem(ielem)%dr(0) = rto .add. hz
          end if
        end do
        sub_n(isub) = sumn / sub(isub)%da_km2
      end do
      return
      end subroutine dr_sub