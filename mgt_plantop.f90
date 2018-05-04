      subroutine mgt_plantop
    
      use hru_module, only: pcom, soil, bio_init, cnop, ihru, ipl, lai_init, plstrz
      use plant_data_module
  
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the plant operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_init       |kg/ha         |initial biomass of transplants
!!    cnop           |none          |SCS runoff curve number for moisture 
!!                                  |condition II
!!    ihru           |none          |HRU number
!!    lai_init       |none          |initial leaf area index of transplants
!!    pcom%plcur     
!!         idorm     |none          |dormancy status code; 0=land cover growing 1=land cover dormant
!!         phuacc    |fraction      |fraction of plant heat unit accumulated
!!    pcom%plm 
!!         nmass     |kg/ha         |nitrogen mass
!!         pmass     |kg/ha         |phosphorus mass
!!    pcom%plg
!!         plet      |mm H2O        |actual ET simulated during life of plant
!!         plpet     |mm H2O        |potential ET simulated during life of plant
!!         laimxfr   | 
!!         hvstiadj  |(kg/ha)/(kg/ha)  |optimal harvest index for current time during growing season
!!         olai      |
!!         rwt       |none          |fraction of total plant biomass that is in roots
!!    soil()%zmx     |mm            |maximum rooting depth of soil 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnop        |none          |SCS runoff curve number for moisture condII
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      implicit none

      integer :: j            !none          |HRU number
      integer :: nly          !none          |number of soil layer
      integer :: min          !              | 
      real :: plt_zmx         !mm            |rooting depth of plant
      

      j = ihru
      !idp = pcom(j)%plcur(1)%idplt

      pcom(j)%plcur(ipl)%gro = "y"
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
      pcom(j)%plstr(ipl) = plstrz 

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