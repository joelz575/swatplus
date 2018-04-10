      subroutine swr_substor
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine stores and lags lateral soil flow and nitrate

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bss(1,:)      |mm H2O       |amount of lateral flow lagged
!!    bss(2,:)      |kg N/ha      |amount of nitrate in lateral flow lagged

!!    bss(3,:)      |mm           |amount of tile flow lagged
!!    bss(4,:)      |kg N/ha      |amount of nitrate in tile flow lagged
!!    hrupest(:)    |none         |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru          |none         |HRU number
!!    lat_pst(:)    |kg pst/ha    |amount of pesticide in lateral flow in HRU
!!                                |for the day
!!    lat_ttime(:)  |none         |Exponential of the lateral flow travel time
!!    latq(:)       |mm H2O       |amount of water in lateral flow in HRU for
!!                                |the day
!!    qtile(:)      |mm H2O       |amount of water in tile flow in HRU for the day
!!    pst_lag(:,3,:)|kg pst/ha    |amount of pesticide lagged
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bss(1,:)      |mm H2O        |amount of lateral flow lagged
!!    bss(2,:)      |kg N/ha       |amount of nitrate in lateral flow lagged
!!    bss(3,:)      |mm            |amount of tile flow lagged
!!    bss(4,:)      |kg N/ha       |amount of nitrate in tile flow lagged
!!    lat_pst(:)    |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                                 |for the day
!!    latq(:)       |mm H2O        |amount of water in lateral flow in HRU for
!!                                 |the day
!!    pst_lag(:,3,:)|kg pst/ha     |amount of pesticide lagged
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use pesticide_data_module
      use hru_module, only : hru, ihru, latq, latno3, qtile, bss, pst_lag, tileno3, hrupest, npno, npmx
      use constituent_mass_module
      
      integer :: j, k

      j = 0
      j = ihru

      bss(1,j) = bss(1,j) + latq(j)
      bss(2,j) = bss(2,j) + latno3(j)
      bss(3,j) = bss(3,j) + qtile
      bss(4,j) = bss(4,j) + tileno3(j)
        if (bss(1,j) < 1.e-6) bss(1,j) = 0.0
        if (bss(2,j) < 1.e-6) bss(2,j) = 0.0
        if (bss(3,j) < 1.e-6) bss(3,j) = 0.0
        if (bss(4,j) < 1.e-6) bss(4,j) = 0.0

      if (hrupest(j) == 1) then
                    
        npmx = cs_db%num_pests
        do k = 1, npmx
          if (pst_lag(k,3,j) < 1.e-6) pst_lag(k,3,j) = 0.0
          !MFW, 3/3/12: Modified lagged pesticide to include decay in lag
          pst_lag(k,3,j) = (pst_lag(k,3,j) * pstcp(npno(k))%decay_s)     & 
                           + hru(j)%pst(k)%latq
          ! pst_lag(k,3,j) = pst_lag(k,3,j) + lat_pst(k)
        end do
      end if

      latq(j) = bss(1,j) * hru(j)%hyd%lat_ttime
      latno3(j) = bss(2,j) * hru(j)%hyd%lat_ttime
      qtile = bss(3,j) * hru(j)%lumv%tile_ttime
      tileno3(j) = bss(4,j) * hru(j)%lumv%tile_ttime
      if (latq(j) < 1.e-6) latq(j) = 0.
      if (latno3(j) < 1.e-6) latno3(j) = 0.
      if (qtile < 1.e-6) qtile = 0.
      if (tileno3(j) < 1.e-6) tileno3(j) = 0.
      if (hrupest(j) == 1) then
        do k = 1, npmx
         hru(j)%pst(k)%latq = hru(j)%pst(k)%latq *           &      
               hru(j)%hyd%lat_ttime
        end do
      end if

      bss(1,j) = bss(1,j) - latq(j)
      bss(2,j) = bss(2,j) - latno3(j)
      bss(3,j) = bss(3,j) - qtile
      bss(4,j) = bss(4,j) - tileno3(j)
      if (hrupest(j) == 1) then
        do k = 1, npmx
          pst_lag(k,3,j) = pst_lag(k,3,j) - hru(j)%pst(k)%latq
        end do
      end if

      return
      end subroutine swr_substor