      subroutine pst_lch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates pesticides leached through each layer,
!!    pesticide transported with lateral subsurface flow, and pesticide
!!    transported with surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru         |none          |HRU number
!!    pst_wsol(:)  |mg/L (ppm)    |solubility of chemical in water
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer
!!    surfq(:)     |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lat_pst(:)   |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                                |for the day
!!    zdb(:,:)     |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use pesticide_data_module
      use basin_module
      use hru_module, only : hru, surfq, ihru
      use soil_module
      use constituent_mass_module
      use output_ls_pesticide_module
      
      implicit none        
      
      integer :: j         !none          |HRU number
      integer :: k         !none          |counter
      integer :: ipest_db  !none          |pesticide number from pest.dat
      integer :: ly        !none          |counter (soil layers)
      real :: yy           !              |
      real :: qsurf        !mm H2O        |surface runoff for layer
      real :: vf           !              |
      real :: xx           !kg/ha         |amount of pesticide removed from soil layer
      real :: zdb1         !              |
      real :: co           !kg/mm-ha      |concentration of pesticide in water
      real :: cocalc       !kg/mm-ha      |calc concentration of pesticide in water
      real :: csurf        !kg/mm-ha      |concentration of pesticide in surq and latq
      integer :: icmd      !              | 

      j = ihru

      if (cs_db%num_pests /= 0) then
        do k = 1, cs_db%num_pests
          hpest_bal(j)%pest(k)%perc = 0.
          hpest_bal(j)%pest(k)%surq = 0.
          hpest_bal(j)%pest(k)%latq = 0.
        end do

        do ly = 1, soil(j)%nly

          do k = 1, cs_db%num_pests
            ipest_db = cs_db%pest_num(k)
            if (ipest_db > 0) then
              if (ly == 1) then
                qsurf = surfq(j)
              else
                qsurf = 0.
              endif

              zdb1 = soil(j)%phys(ly)%ul + soil(j)%ly(ly)%kp(k) *                           &
                                                soil(j)%phys(1)%bd * soil(j)%phys(1)%thick
              !! units: mm + (m^3/ton)*(ton/m^3)*mm = mm

              vf = qsurf + soil(j)%ly(ly)%prk + soil(j)%ly(ly)%flat
              if (soil(j)%ly(ly)%pst(k) >= 0.0001 .and. vf > 0.) then
                xx =  soil(j)%ly(ly)%pst(k) * (1. - Exp(-vf / (zdb1 + 1.e-6)))
                if (ly == 1) then
                  cocalc = xx / (soil(j)%ly(ly)%prk + bsn_prm%percop *                  &
                                               (qsurf + soil(j)%ly(ly)%flat) + 1.e-6)
                else
                  cocalc = xx/(soil(j)%ly(ly)%prk+soil(j)%ly(ly)%flat + 1.e-6)
                end if
                co = Min(pestdb(ipest_db)%pst_wof / 100., cocalc)
               
                !! calculate concentration of pesticide in surface
                !! runoff and lateral flow
                if (ly == 1) then
                  csurf = bsn_prm%percop * co
                else
                  csurf = co
                end if

                !! calculate pesticide leaching
                xx = co * soil(j)%ly(ly)%prk
                if (xx > soil(j)%ly(ly)%pst(k)) xx = soil(j)%ly(ly)%pst(k)
                 
                soil(j)%ly(ly)%pst(k) = soil(j)%ly(ly)%pst(k) - xx

                if (ly < soil(j)%nly) then
                  soil(j)%ly(ly+1)%pst(k) = soil(j)%ly(ly+1)%pst(k) + xx
                else
                  hpest_bal(j)%pest(k)%perc = xx
                end if

                !! calculate pesticide lost in surface runoff
                if (ly == 1) then
                  yy = csurf * surfq(j)
                  if (yy >  soil(j)%ly(ly)%pst(k)) yy = soil(j)%ly(ly)%pst(k)
                  soil(j)%ly(ly)%pst(k) =  soil(j)%ly(ly)%pst(k) - yy
                  hpest_bal(j)%pest(k)%surq = yy 
                endif

                !! calculate pesticide lost in lateral flow
                yy = csurf * soil(j)%ly(ly)%flat
                if (yy > soil(j)%ly(ly)%pst(k)) yy = soil(j)%ly(ly)%pst(k)
                 
                soil(j)%ly(ly)%pst(k) = soil(j)%ly(ly)%pst(k) - yy
                hpest_bal(j)%pest(k)%latq = hpest_bal(j)%pest(k)%latq + yy 

              end if

            end if
          end do
        end do
      end if

      return
      end subroutine pst_lch