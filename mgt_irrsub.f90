      subroutine mgt_irrsub

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the irrigation operation when the source is 
!!    the shallow or deep aquifer or a source outside the watershed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)        |mm H2O        |amount of water applied to HRU on current
!!                                  |day
!!    hru_sub(:)     |none          |subbasin in which HRU is located
!!    ihru           |none          |HRU number
!!    ipot(:)        |none          |number of HRU (in subbasin) that is ponding
!!                                  |water--the HRU that the surface runoff from
!!                                  |current HRU drains into. This variable is
!!                                  |used only for rice paddys or closed
!!                                  |depressional areas
!!    irramt(:)      |mm H2O        |depth of irrigation water applied to
!!                                  |HRU
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
!!    nirr(:)        |none          |sequence number of irrigation application
!!                                  |within the year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nirr(:)     |none          |sequence number of irrigation application
!!                               |within the year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    vmm         |mm H2O        |maximum amount of water to be applied
!!    vmma        |mm H2O        |amount of water in source
!!    vmmd        |m^3 H2O       |amount of water in deep aquifer
!!    vmms        |m^3 H2O       |amount of water in shallow aquifer
!!    vmxi        |mm H2O        |amount of water specified in irrigation
!!                               |operation
!!    vol         |m^3 H2O       |volume of water to be applied in irrigation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
!      use wateruse_module

      integer :: j, k, flag
      real :: vmma, vmm, cnv, vmxi, vol, vmms, vmmd

      j = ihru
      
      select case (irrop%src)
      case (1)   !! reach source
        hyd_no = hru(j)%irrsrc%chan(irrop%src_num)
!        wus_tran(hyd_no)%aird = 10. * irrop%amt_mm * hru(j)%area_ha
        
      case (2)   !! reservoir source
        hyd_no = hru(j)%irrsrc%res(irrop%src_num)
!       wus_tran(hyd_no)%aird = 10. * irrop%amt_mm * hru(j)%area_ha
                  
      case (3)   !! shallow aquifer source
        hyd_no = hru(j)%irrsrc%shal(irrop%src_num)
!        wus_tran(hyd_no)%aird = 10. * irrop%amt_mm * hru(j)%area_ha
        
      case (4)   !! deep aquifer source
        hyd_no = hru(j)%irrsrc%deep(irrop%src_num)
!        wus_tran(hyd_no)%aird = 10. * irrop%amt_mm * hru(j)%area_ha
        
      case (5)   !! unlimited source
!        wus_tran(hyd_no)%aird = 10. * hru(j)%sol%sumfc * hru(j)%area_ha
        
      case (6)   !! pond source
        hyd_no = hru(j)%irrsrc%pond(irrop%src_num)
!        wus_tran(hyd_no)%aird = 10. * irrop%amt_mm * hru(j)%area_ha
        
      end select

      return
      end subroutine mgt_irrsub