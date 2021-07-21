      subroutine water_allocation
      
      use water_allocation_module
      use hydrograph_module
      use reservoir_module
      use mgt_operations_module
      use hru_module
      
      implicit none 

      integer :: j                  !none       |hru number
      integer :: idmd               !none       |demand object number
      integer :: isrc               !none       |source object number
      integer :: isrco              !none       |other source object number
      integer :: iwallo             !none       |water allocation number
      integer :: irrop              !none       |irrigation database number
      real :: dmd_m3                !m3         |water demand
      real :: dmd_mm                !mm         |water demand
      real :: res_min               !m3         |min reservoir volume for withdrawal
      real :: res_vol               !m3         |reservoir volume after withdrawal
      character (len=1), dimension (2) :: avail    !none       |y=available, n=not available from source

      iwallo = 1    !may need only one allocation object
      
      !! determine demand for each municipal and hru source
      do idmd = 1, wallo(iwallo)%dmd_obs
          
        !! comput demand for both sources
        do isrc = 1, 2
          select case (wallo(iwallo)%dmd(idmd)%ob_typ)
          !! minicipal demand
          case ("muni")
            wallod_out(idmd)%src(isrc)%demand = wallo(iwallo)%dmd(idmd)%amount  !ha-m     
        
          !! irrigation demand
          case ("hru")
            j = wallo(iwallo)%dmd(idmd)%obtyp_num
            !! if there is demand, use amount from water allocation file
            if (irrig(j)%demand > 0.) then
              wallod_out(idmd)%src(isrc)%demand = wallo(iwallo)%dmd(idmd)%amount * hru(j)%area_ha / 1000. !ha-m = mm * ha / 1000.
            else
              wallod_out(idmd)%src(isrc)%demand = 0.
            end if
          end select
          wallod_out(idmd)%src(isrc)%demand = wallo(iwallo)%dmd(idmd)%src(isrc)%frac * wallod_out(idmd)%src(isrc)%demand
        end do
        
        !! check if water is available from the source
        do isrc = 1, 2
          !! m3 = 10,000 * ha-m
          dmd_m3 = 10000. * wallod_out(idmd)%src(isrc)%demand
          
          select case (wallo(iwallo)%dmd(idmd)%src(isrc)%ob_typ)
          !! reservor source
          case ("res") 
            j = wallo(iwallo)%dmd(idmd)%src(isrc)%obtyp_num
            res_min = wallo(iwallo)%res_lim * res_ob(j)%pvol
            res_vol = res(j)%flo - dmd_m3
            if (res_vol > res_min) then
              avail(isrc) = "y"
              res(j)%flo = res(j)%flo - dmd_m3
              wallod_out(idmd)%src(isrc)%withdr = dmd_m3 / 10000.  ! m3 -> ha-m
              wallod_out(idmd)%src(isrc)%unmet = 0.
            else
              avail(isrc) = "n"
              wallod_out(idmd)%src(isrc)%withdr = 0.
              wallod_out(idmd)%src(isrc)%unmet = dmd_m3 / 10000.  ! m3 -> ha-m
            end if
        
          !! unlimited groundwater source
          case ("gwu")
            avail(isrc) = "y"
            wallod_out(idmd)%src(isrc)%withdr = dmd_m3 / 10000.  ! m3 -> ha-m
            wallod_out(idmd)%src(isrc)%unmet = 0.
          end select
        end do
        
        !! if compensation is allowed, check the other source
        if (wallo(iwallo)%comp == "y") then
        do isrc = 1, 2
          if (avail(isrc) == "n") then
          !! set isrco to the other source
          if (isrc == 1) isrco = 2
          if (isrc == 2) isrco = 1
          !! m3 = 10,000 * ha-m
          dmd_m3 = 10000. * wallod_out(idmd)%src(isrc)%demand
          
          select case (wallo(iwallo)%dmd(idmd)%src(isrco)%ob_typ)
          !! reservor source
          case ("res") 
            j = wallo(iwallo)%dmd(idmd)%src(isrco)%obtyp_num
            res_min = wallo(iwallo)%res_lim * res_ob(j)%pvol
            res_vol = res(j)%flo - dmd_m3
            if (res_vol > res_min) then
              avail(isrc) = "y"
              res(j)%flo = res(j)%flo - dmd_m3
              wallod_out(idmd)%src(isrco)%withdr = wallod_out(idmd)%src(isrco)%withdr + dmd_m3 / 10000.  ! m3 -> ha-m
              wallod_out(idmd)%src(isrco)%unmet = 0.
            else
              avail(isrc) = "n"
              wallod_out(idmd)%src(isrco)%withdr = 0.
              wallod_out(idmd)%src(isrco)%unmet = dmd_m3 / 10000.  ! m3 -> ha-m
            end if
        
          !! unlimited groundwater source
          case ("gwu")
            avail(isrc) = "y"
            wallod_out(idmd)%src(isrco)%withdr = wallod_out(idmd)%src(isrco)%withdr + dmd_m3 / 10000.  ! m3 -> ha-m
            wallod_out(idmd)%src(isrco)%unmet = 0.
          end select
          end if
        end do
        end if  !comp == "y"
        
        !! set irrigation applied and runoff
        if (wallo(iwallo)%dmd(idmd)%ob_typ == "hru") then
          j = wallo(iwallo)%dmd(idmd)%obtyp_num
          !! mm = ha-m * 1000 mm/m / ha
          dmd_mm = (wallod_out(idmd)%src(1)%withdr + wallod_out(idmd)%src(2)%withdr) * 1000. / hru(j)%area_ha
          !! apply what is available to withdraw
          if (dmd_mm > 0.) then
            irrop = wallo(iwallo)%dmd(idmd)%irr_typ
            irrig(j)%applied = dmd_mm * irrop_db(irrop)%eff * (1. - irrop_db(irrop)%surq)
            irrig(j)%runoff = dmd_mm * irrop_db(irrop)%surq
          end if
        end if
        
      end do    !idmd
      return
      end subroutine water_allocation
