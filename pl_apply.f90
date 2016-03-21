      subroutine pl_apply
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies pesticide

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)     |none             |application efficiency (0-1)
!!    curyr        |none             |current year of simulation
!!    drift(:)     |kg               |amount of pesticide drifting onto main 
!!                                   |channel in subbasin
!!    driftco(:)   |none             |coefficient for pesticide drift directly
!!                                   |onto stream
!!    hru_dafr(:)  |km**2/km**2      |fraction of watershed area in HRU
!!    hru_km(:)    |km**2            |area of HRU in square kilometers
!!    ihru         |none             |HRU number
!!    ipest        |none             |pesticide identification number from
!!                                   |pest.dat
!!    nope(:)      |none             |sequence number of pesticide in NPNO(:)
!!    plt_pst(:,:) |kg/ha            |pesticide on plant foliage
!!    pst_dep      |kg/ha          |depth of pesticide in soil
!!    pst_kg       |kg/ha            |amount of pesticide applied to HRU
!!    sol_pst(:,:,1)|kg/ha           |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drift(:)    |kg            |amount of pesticide drifting onto main 
!!                               |channel in subbasin
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gc          |none          |fraction of ground covered by plant foliage
!!    j           |none          |HRU number
!!    jj          |none          |subbasin number
!!    k           |none          |sequence number of pesticide in NPNO(:)
!!    kk          |none          |pesticide identification number from
!!                               |pest.dat
!!    xx          |kg/ha         |amount of pesticide applied to HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      
      integer :: j, kk, k, jj
      real :: xx, gc

      j = 0
      j = ihru

      !! initialize local variables
      k = mgt%op1                !! sequential hru pesticide number
      kk = hru(j)%pst(mgt%op1)%num_db  !! database number from pest.dat
      xx = pst_kg * pestdb(kk)%ap_ef

!! calculate amount of pesticide drifting onto main channel in subbasin
!      if (k == bsn_cc%rtpest) then
!        drift(jj) = drift(jj) + xx * hru(j)%km * 100. * driftco(j) *    
!     *                                                              1.e6
!      end if
!      xx = xx * ap_ef(kk) * (1. - driftco(j))
      xx = xx * pestdb(kk)%ap_ef

      ! added for pesticide incorporation 3/31/08 gsm
      if (pst_dep > 1.e-6) then
       do nly = 1, hru(j)%sol%nly
         if (nly == 1) then
         if (pst_dep < soil(j)%phys(nly)%d) then
            hru(j)%ly(1)%pst(k) =  hru(j)%ly(1)%pst(k) + xx
           exit
         endif
       else
        if (pst_dep>soil(j)%phys(nly-1)%d .and. pst_dep <           &
          soil(j)%phys(nly)%d)then
             hru(j)%ly(nly)%pst(k) = hru(j)%ly(nly)%pst(k) + xx
           exit
           endif
         endif
       enddo
      else
      ! added above for pesticide incorporation 3/31/08 gsm

      !! calculate ground cover
      gc = 0.
      gc = (1.99532 - erfc(1.333 * sumlai - 2.)) / 2.1
      if (gc < 0.) gc = 0.

      !! update pesticide levels on ground and foliage
      hru(j)%pst(k)%plt = hru(j)%pst(k)%plt + gc * xx
      hru(j)%ly(1)%pst(k) = hru(j)%ly(1)%pst(k) + (1. - gc) * xx
      
      !! added endif for pesticide incorporation 3/31/08 gsm
      endif

      return
      end subroutine pl_apply