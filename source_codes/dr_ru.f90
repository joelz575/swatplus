      subroutine dr_ru
    
      use hydrograph_module
      use hru_lte_module
      use ru_module
      use hru_module, only : ihru, tconc
      
      implicit none
      
      real :: sumn                  !kg N/ha       |total amount of nitrate stored in soil profile
      integer :: ii                 !none          |counter
      integer :: ielem              !none          |counter
      real :: rto                   !none          |cloud cover factor
      
      
    
     ! compute delivery ratio for each hru in the sub
      sumn = 0.
      do iru = 1, sp_ob%ru
        do ii = 1, ru_def(iru)%num_tot
          ielem = ru_def(iru)%num(ii)
          ihru = ru_elem(ielem)%obtypno

          if (ru_elem(ielem)%idr == 0) then
            allocate (ru_elem(ielem)%dr(0:0))
            select case (ru_elem(ielem)%obtyp)
            case ("hru")
              rto = tconc(ihru) / ru_tc(iru)
            case ("hlt")
              rto = (hlt_db(ihru)%tc / 3600.) / ru_tc(iru)
            case ("sdc")
              rto = 1.
            case ("ru")
              rto = 1.
            end select
            
            rto = amin1(1.0, rto ** .5)
            ru_elem(ielem)%dr(0) = rto .add. hz
          end if
        end do
        ru_n(iru) = sumn / ru(iru)%da_km2
      end do
      return
      end subroutine dr_ru
      