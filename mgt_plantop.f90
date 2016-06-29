      subroutine mgt_plantop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the plant operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lai_init       |none          |initial leaf area index of transplants
!!    bio_init       |kg/ha         |initial biomass of transplants
!!    cnop           |none          |SCS runoff curve number for moisture 
!!                                  |condition II
!!    icr(:)         |none          |sequence number of crop grown within the
!!                                  |current year
!!    ihru           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      integer :: j
      
      j = 0
      j = ihru
      idp = pcom(j)%plcur(1)%idplt

      pcom(j)%plcur(ipl)%gro = 1
      pcom(j)%plcur(ipl)%idorm = 0
      pcom(j)%plcur(ipl)%phuacc = 0.
      pcom(j)%plm(ipl)%nmass = 0.
      pcom(ihru)%plm(ipl)%pmass = 0.
      pcom(j)%plg(ipl)%plet = 0.
      pcom(j)%plg(ipl)%plpet = 0.                         
      pcom(j)%plg(ipl)%laimxfr = 0.
      pcom(j)%plg(ipl)%hvstiadj = 0.
      pcom(j)%plg(ipl)%olai = 0.
      pcom(j)%plg(ipl)%rwt = 0.
      
 !!   added for Srini in output.mgt per JGA by gsm 9/8/2011     
      strsw_sum = 0.
      strstmp_sum = 0.
      strsn_sum = 0.
      strsp_sum = 0.
      strsa_sum = 0.  

      !! initialize transplant variables
      if (lai_init > 0.) then
          pcom(j)%plg(ipl)%lai = lai_init
          pcom(j)%plm(ipl)%mass = bio_init
      endif
      
      !! compare maximum rooting depth in soil to maximum rooting depth of plant
      nly = soil(j)%nly
      soil(ihru)%zmx = soil(j)%phys(nly)%d
      plt_zmx = 1000. * pldb(ipl)%rdmx
      soil(ihru)%zmx = Min(soil(ihru)%zmx,plt_zmx)
      
      !! reset curve number if given in .mgt file
      if (cnop > 0.) call curno(cnop,j)

      return
      end subroutine mgt_plantop