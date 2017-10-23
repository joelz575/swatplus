      subroutine pl_conapply
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies continuous pesticide

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)     |none             |application efficiency (0-1)
!!    drift(:)     |kg               |amount of pesticide drifting onto main 
!!                                   |channel in subbasin
!!    driftco(:)   |none             |coefficient for pesticide drift directly
!!                                   |onto stream
!!    hru_km(:)    |km**2            |area of HRU in square kilometers
!!    ihru         |none             |HRU number
!!    ipest(:,:,:) |none             |pesticide identification number from
!!                                   |pest.dat
!!    plt_pst(:,:) |kg/ha            |pesticide on plant foliage
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

      use jrw_datalib_module, only : pestdb
      use basin_module
      use parm, only : hru, soil, pcom, iday_pest, icpst, ndcpst, ipst_freq, cpst_id, cpst_kg, phubase,  &
        sol_sumno3, sol_sumsolp, pest_days, ihru, driftco, ipl, sumlai
      
      integer :: j, kk, k, jj
      real :: xx, gc

      j = 0
      j = ihru

!! if continuous pesticide not currently on, check to see if it is time
!! to initialize continuous pesticide

      if (iday_pest(j) == ipst_freq(j)) then
        !! apply pesticide
        !! reset frequency counter
        iday_pest(j) = 1

        !! initialize local variables
        kk = 0
        k = 0
        jj = 0
        xx = 0.
        
        k = cpst_id(j)             !! sequential hru number
        kk = hru(j)%pst(k)%num_db  !! database number from pest.dat
        xx = cpst_kg(j)

  !! calculate amount of pesticide drifting onto main channel in subbasin
!       if (k == bsn_cc%rtpest) then
!         drift(jj) = drift(jj) + xx * hru(j)%km * 100. * driftco(j) *   
!     *                                                            1.e6
!       end if
!       xx = xx * ap_ef(kk) * (1. - driftco(j))
        xx = xx * pestdb(kk)%ap_ef

!! calculate ground cover
        gc = 0.
        gc = (1.99532 - erfc(1.333 * sumlai - 2.)) / 2.1
        if (gc < 0.) gc = 0.

!! update pesticide levels on ground and foliage
        hru(j)%pst(k)%plt = hru(j)%pst(k)%plt + gc * xx
        soil(j)%ly(1)%pst(k) = soil(j)%ly(1)%pst(k) + (1. - gc) * xx
        
 
        if (pco%mgtout ==  'year') then
         write (2612, 1000) j, time%yrc, time%mo, time%day,               & 
         "         ", "CONT PEST", phubase(j),pcom(j)%plcur(ipl)%phuacc,  &
            soil(j)%sw,pcom(j)%plm(ipl)%mass,                             &
            soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j), cpst_kg(j)
        end if
           
          
      else
        iday_pest(j) = iday_pest(j) + 1
      end if

!! check to set if continuous pesticide period is over
      if (ndcpst(j) == pest_days(j)) then
        icpst(j) = 0
        ndcpst(j) = 0
        iday_pest(j) = 0
!        ncpest(j) = ncpest(j) + 1
      end if

1000  format (4i6,5x,2a15,57f10.2,20x,f10.2) 
      return
      end subroutine pl_conapply