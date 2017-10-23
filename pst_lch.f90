      subroutine pst_lch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates pesticides leached through each layer,
!!    pesticide transported with lateral subsurface flow, and pesticide
!!    transported with surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)   |none          |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru         |none          |HRU number
!!    npmx         |none          |number of different pesticides used in
!!                                |the simulation
!!    npno(:)      |none          |array of unique pesticides used in watershed
!!    pst_wsol(:)  |mg/L (ppm)    |solubility of chemical in water
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer
!!    surfq(:)     |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lat_pst(:)   |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                                |for the day
!!    pst_surq(:,:)|kg/ha         |amount of pesticide type lost in surface
!!                                |runoff on current day in HRU
!!    zdb(:,:)     |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    co          |kg/mm-ha      |concentration of pesticide in water
!!    cocalc      |kg/mm-ha      |calc concentration of pesticide in water
!!    csurf       |kg/mm-ha      |concentration of pesticide in surq and latq
!!    dg          |mm            |depth of soil layer
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from pest.dat
!!    ly          |none          |counter (soil layers)
!!    qsurf       |mm H2O        |surface runoff for layer
!!    vf          |
!!    xx          |kg/ha         |amount of pesticide removed from soil layer
!!    yy          |
!!    zdb1        |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module, only : pestdb
      use basin_module
      use parm, only : soil, hru, hrupest, surfq, ihru, npmx
      use constituent_mass_module
      
      integer :: j, ly, k, kk
      real :: dg, yy, qsurf, vf, zdb1, xx, co, csurf, cocalc
 
      j = 0
      j = ihru

      if (hrupest(j) /= 0) then

        do ly = 1, soil(j)%nly
          if (ly == 1) then
            yy = 0.
          else
            yy = 0.
            yy = soil(j)%phys(ly-1)%d
          end if
          dg = 0.
          dg = soil(j)%phys(ly)%d - yy
          
          npmx = obcs(icmd)%num_pests
          do k = 1, npmx
            kk = hru(j)%pst(k)%num_db

            if (kk > 0) then
              qsurf = 0.
              if (ly == 1) then
                qsurf = surfq(j)
              else
                qsurf = 0.
              endif

              zdb1 = 0.0
!!              zdb1 = soil(j)%phys(ly)%ul + sol_kp(k,j,ly) * soil(j)%phys(1)%bd*dg
              zdb1 = soil(j)%phys(ly)%ul + soil(j)%ly(ly)%kp(k) *          &
                                                   soil(j)%phys(1)%bd*dg
              !! units: mm + (m^3/ton)*(ton/m^3)*mm = mm
              if (ly == 1) hru(j)%pst(k)%zdb = zdb1

              vf = 0.
              vf = qsurf + soil(j)%ly(ly)%prk + soil(j)%ly(ly)%flat

              if (soil(j)%ly(ly)%pst(k) >= 0.0001 .and. vf > 0.) then
                xx = 0.
                xx =  soil(j)%ly(ly)%pst(k) * (1. - Exp(-vf /             &          
                                                      (zdb1 + 1.e-6)))
               
                cocalc = 0.
                co = 0.
                if (ly == 1) then
                  cocalc = xx /                                          &                                         
                 (soil(j)%ly(ly)%prk + bsn_prm%percop *                  &
                   (qsurf + soil(j)%ly(ly)%flat) + 1.e-6)
                else
                  cocalc = xx/(soil(j)%ly(ly)%prk+soil(j)%ly(ly)%flat +  &
                    1.e-6)
                end if
                co = Min(pestdb(kk)%pst_wof / 100., cocalc)
               
                !! calculate concentration of pesticide in surface
                !! runoff and lateral flow
                csurf = 0.
                if (ly == 1) then
                  csurf = bsn_prm%percop * co
                else
                  csurf = co
                end if

                !! calculate pesticide leaching
                xx = 0.
                xx = co * soil(j)%ly(ly)%prk
                if (xx > soil(j)%ly(ly)%pst(k)) xx = soil(j)%ly(ly)%pst(k)
                 
                soil(j)%ly(ly)%pst(k) = soil(j)%ly(ly)%pst(k) - xx

                if (ly < soil(j)%nly) then
                  soil(j)%ly(ly+1)%pst(k) = soil(j)%ly(ly+1)%pst(k) + xx
                  
                else
 !                 pstsol(k) = xx
                end if

                !! calculate pesticide lost in surface runoff
                if (ly == 1) then
                  yy = 0.
                  yy = csurf * surfq(j)
                  if (yy >  soil(j)%ly(ly)%pst(k)) yy =                  &                 
                                                 soil(j)%ly(ly)%pst(k)
                 
                   soil(j)%ly(ly)%pst(k) =  soil(j)%ly(ly)%pst(k) - yy
                  hru(j)%pst(k)%surq = yy 
                endif


                !! calculate pesticide lost in lateral flow
                yy = 0.
                yy = csurf * soil(j)%ly(ly)%flat
                if (yy > soil(j)%ly(ly)%pst(k)) yy = soil(j)%ly(ly)%pst(k)
                 
                soil(j)%ly(ly)%pst(k) = soil(j)%ly(ly)%pst(k) - yy
                hru(j)%pst(k)%latq = hru(j)%pst(k)%latq + yy 

              end if

            end if
          end do
        end do
      end if

      return
      end subroutine pst_lch