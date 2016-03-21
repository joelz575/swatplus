      subroutine mgt_autoirr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the auto-irrigation operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)        |mm H2O        |amount of water applied to HRU on current
!!                                  |day
!!    auto_wstr(:)   |none or mm    |water stress threshold that triggers irrigation
!!    hru_sub(:)     |none          |subbasin in which HRU is located
!!    wstrs_id(:)    |none          |water stress identifier:
!!                                  |1 plant water demand
!!                                  |2 soil water deficit
!!    ihru           |none          |HRU number
!!    irrno(:)       |none          |irrigation source location
!!                                  |if IRR=1, IRRNO is the number of the
!!                                  |          reach
!!                                  |if IRR=2, IRRNO is the number of the
!!                                  |          reservoir
!!                                  |if IRR=3, IRRNO is the number of the
!!                                  |          subbasin
!!                                  |if IRR=4, IRRNO is the number of the
!!                                  |          subbasin
!!                                  |if IRR=5, not used
!!    irrsc(:)       |none          |irrigation source code:
!!                                  |1 divert water from reach
!!                                  |2 divert water from reservoir
!!                                  |3 divert water from shallow aquifer
!!                                  |4 divert water from deep aquifer
!!                                  |5 divert water from source outside
!!                                  |  watershed
!!    nhru           |none          |number of HRUs in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    vmm         |mm H2O        |maximum amount of water to be applied
!!    vmma        |mm H2O        |amount of water in source
!!    vmmd        |m^3 H2O       |total amount of water in subbasin deep
!!                               |aquifer
!!    vmms        |m^3 H2O       |total amount of water in subbasin shallow 
!!                               |aquifer
!!    vol         |mm H2O        |volume of water applied to HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: j, k
      real :: vmma, vmm, cnv, vol, vmms, vmmd

      j = ihru
      ipl = iplt_airr(j)

      if ((wstrs_id(j) == 1 .and. pcom(j)%plstr(ipl)%strsw<auto_wstr(j)   &
       .or.(wstrs_id(j)==2.and.hru(j)%sol%sumfc-hru(j)%sol%sw >           &
       auto_wstr(j))))                                                    &   
             then                                                       
        call mgt_irrsub

       if (pco%mgtout ==  1) then
            write (143, 1000) j, time%yrc, i_mo, iida,                   & 
            "         ",  " AUTOIRR", phubase(j),                        & 
       pcom(j)%plcur(ipc)%phuacc,                                        &
       hru(j)%sol%sw,pcom(j)%plm(ipl)%mass,                              &
       soil(j)%ly(1)%rsd,sol_sumno3(j),sol_sumsolp(j),aird(j),irrsc(j),  &
       irrno(j)
       end if

      end if 
        
1000  format (4i6,2a15,7f10.2,10x,f10.2,70x,i10,10x,i10) 

      return
      end subroutine mgt_autoirr