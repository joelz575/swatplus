      subroutine irr_res

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the irrigation operation when the water
!!    source is a reservoir

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)         |mm H2O        |amount of water applied to HRU on current
!!                                   |day
!!    auto_wstr(:)    |none or mm    |water stress factor which triggers auto
!!                                   |irrigation
!!    wstrs_id(:)     |none          |water stress identifier:
!!                                   |1 plant water demand
!!                                   |2 soil water deficit
!!    flag                           |1 = manual 2 = auto
!!    irramt(:)       |mm H2O        |depth of irrigation water applied to
!!                                   |HRU
!!    irrno(:)        |none          |irrigation source location
!!                                   |if IRR=1, IRRNO is the number of the
!!                                   |          reach
!!                                   |if IRR=2, IRRNO is the number of the
!!                                   |          reservoir
!!                                   |if IRR=3, IRRNO is the number of the
!!                                   |          subbasin
!!                                   |if IRR=4, IRRNO is the number of the
!!                                   |          subbasin
!!                                   |if IRR=5, not used
!!    irrsc(:)        |none          |irrigation source code:
!!                                   |1 divert water from reach
!!                                   |2 divert water from reservoir
!!                                   |3 divert water from shallow aquifer
!!                                   |4 divert water from deep aquifer
!!                                   |5 divert water from source outside
!!                                   |  watershed
!!    nhru            |none          |number of HRUs in watershed
!!    res_vol(:)      |m**3          |reservoir volume
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    res_vol(:)  |m**3          |reservoir volume
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm => m^3)
!!    flag        |none          |irrigation flag:
!!                               |0 no irrigation operation on current day
!!                               |1 scheduled irrigation
!!                               |2 auto irrigation
!!    jres        |none          |reservoir number
!!    k           |none          |HRU number
!!    vmm         |mm H2O        |depth of irrigation water over HRU
!!    vmxi        |mm H2O        |amount of water specified in irrigation
!!                               |operation
!!    vol         |m^3 H2O       |volume of water applied in irrigation 
!!                               |operation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs, Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : soil, hru, irrsc, irrno, irr_flag, auto_wstr, wstrs_id, strsw_av, irrsq,  &
         irr_sc, irr_no, irr_asq, irr_noa, irr_sca, irramt, irr_mx, aird, irr_eff, nhru
      use jrw_datalib_module, only : potdb
      use reservoir_module
      use hydrograph_module, only : res

      integer :: k, flag
      real :: cnv, vmm, vol, vmxi, sq_rto

      do k = 1, nhru
        if (irrsc(k) == 2 .and. irrno(k) == jres) then

          !! check for timing of irrigation operation
          flag = 0
          flag = irr_flag(k)
          if (auto_wstr(k) > 0.) then
            if (wstrs_id(k) == 1 .and. strsw_av(k) < auto_wstr(k))      &
                                                           flag = 2
            if (wstrs_id(k) == 2 .and. soil(k)%sumfc-soil(k)%sw >    &
                                             auto_wstr(k)) flag = 2
          end if

          if (flag == 1) then 
            sq_rto = irrsq(k)
            irrsc(k) = irr_sc(k)
            irrno(k) = irr_no(k)                 
          else
            sq_rto = irr_asq(k) 
            irrsc(k) = irr_sca(k)
            irrno(k) = irr_noa(k)           
          endif

          if (flag > 0) then
            cnv = 0.
            cnv = hru(k)%area_ha * 10.

            !! compute maximum amount of water available for irrigation
            !! from reach
            vmm = 0.
            vmm = res(jres)%flo / cnv

            !! check available against set amount in scheduled operation
            if (flag == 1) then
              vmxi = 0.
              vmxi = irramt(k)                      
              if (vmxi < 1.e-6) vmxi = soil(k)%sumfc
              if (vmm > vmxi) vmm = vmxi
            end if
            if (flag == 2) then
              vmxi = 0.
              vmxi = irr_mx(k)
              if (vmm > vmxi) vmm = vmxi
            end if

 !           if (vmm > 0.) then
 !             vol = 0.
 !             vol = vmm * cnv
 !             if (potdb(ipot)%frac > 1.e-6) then
 !               pot(k)%vol = pot(k)%vol + vol
 !             else
 !               call pl_irrigate(k,vmm)
 !             end if

              !! subtract irrigation from reservoir volume
              if (potdb(ipot)%frac > 1.e-6) then
                vol = 0.
                vol = aird(k) * cnv
              end if
              vol = vol / irr_eff(k)		     !! BN inserted to account for irr. efficiency
              res(jres)%flo = res(jres)%flo - vol
              if (res(jres)%flo < 0.) res(jres)%flo = 0.

            end if
          end if
!        end if
      end do

      return
      end