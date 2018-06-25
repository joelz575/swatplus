      subroutine pl_waterup
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes potential plant evaporation through
!!    the root zone and calculates actual plant water use based on soil
!!    water availability. Also estimates water stress factor.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs
!!                               |on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |
!!    ir          |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    reduc       |none          |fraction of water uptake by plants achieved
!!                               |where the reduction is caused by low water
!!                               |content
!!    sum         |
!!    sump        |
!!    wuse        |mm H2O        |water uptake by plants in each soil layer
!!    xx          |mm H2O        |water uptake by plants from all layers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use basin_module
      use hru_module, only : hru, ihru, epmax, ipl, ep_day, uptake
      use soil_module
      use plant_module
      
      implicit none
      
      integer :: j           !none      |hru number
      integer :: k           !none      |counter 
      integer :: ir          !none      |flag to denote bottom of root zone reached
      integer :: idp         !          |   
      real :: sum            !          |
      real :: xx             !mm H2O    |water uptake by plants from all layers          |
      real :: reduc          !none      |fraction of water uptake by plants achieved
                             !          |where the reduction is caused by low water
                             !          |content
      real :: sump           !          |
      real :: gx             !mm        |lowest depth in layer from which nitrogen
                             !          |may be removed
      real :: wuse           !mm H2O    |water uptake by plants in each soil layer
      real :: satco          !          | 
      real :: pl_aerfac      !          |
      real :: scparm         !          |  
      real :: uobw           !none      |water uptake normalization parameter
                             !          |This variable normalizes the water uptake so
                             !          |that the model can easily verify that uptake
                             !          |from the different soil layers sums to 1.0
      real :: ubw            !          |the uptake distribution for water is hardwired
      real :: yy             !          | 
      

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt

      if (epmax(ipl) <= 0.01) then
        pcom(j)%plstr(ipl)%strsw = 1.
      else
        !! initialize variables
        gx = 0.
        ir = 0
        sump = 0.
        wuse = 0.
        xx = 0.
 
!!  compute aeration stress
        if (soil(j)%sw > soil(j)%sumfc) then
          satco=(soil(j)%sw-soil(j)%sumfc) / (soil(j)%sumul -   &
                                                  soil(j)%sumfc)
          pl_aerfac = .85
          scparm = 100. * (satco - pl_aerfac) / (1.0001 - pl_aerfac)
          if (scparm > 0.) then
            pcom(j)%plstr(ipl)%strsa = 1. - (scparm /                    &
              (scparm + Exp(2.9014 - .03867 * scparm)))
          else
            pcom(j)%plstr(ipl)%strsa = 1.
          end if
        end if

        do k = 1, soil(j)%nly
          if (ir > 0) exit

          if (pcom(j)%plg(ipl)%root_dep <= soil(j)%phys(k)%d) then
            gx = pcom(j)%plg(ipl)%root_dep
            ir = k
          else
            gx = soil(j)%phys(k)%d
          end if

          if (pcom(j)%plg(ipl)%root_dep <= 0.01) then
            sum = epmax(ipl) / uptake%water_norm
          else
            sum = epmax(ipl) * (1. - Exp(-uptake%water_dis * gx / pcom(j)%plg(ipl)%root_dep)) / uptake%water_norm
          end if

          wuse = sum - sump + yy * hru(j)%hyd%epco
          wuse = sum - sump + (sump - xx) * hru(j)%hyd%epco
          sump = sum

!!! commented aeration stress out !!!
          !! adjust uptake if sw is greater than 90% of plant available water
          !! aeration stress
!         yy = air_str(idp)
!         satco = 100. * (sol_st(k,j) / sol_ul(k,j) - yy) / (1. - yy)
!         if (satco > 0.) then 
!           strsa(j) = 1. - (1. - (satco / (satco + Exp(5.1 - .082 * 
!    &                                                      satco))))
!         else
!           strsa(j) = 1.
!         end if
!         wuse = strsa(j) * wuse
          
!         if (iwatable(j) > 0) then
!           yy = sol_sumfc(j) + .08 * (sol_sumul(j) - sol_sumfc(j))
!           yy = sol_fc(k,j) + .01 * (sol_ul(k,j) - sol_fc(k,j))
!           if (sol_sw(j) > yy) then
!             wuse = 0.
!           endif
!         endif

          !! adjust uptake if sw is less than 25% of plant available water
          if (soil(j)%phys(k)%st < soil(j)%phys(k)%fc / 4.) then
            reduc = Exp(5. * (4. * soil(j)%phys(k)%st / soil(j)%phys(k)%fc - 1.))
          else
            reduc = 1.
          endif
          reduc = 1.
          wuse = wuse * reduc

          if (soil(j)%phys(k)%st < wuse) then
            wuse = soil(j)%phys(k)%st
          end if

          soil(j)%phys(k)%st = Max(1.e-6, soil(j)%phys(k)%st - wuse)
          xx = xx + wuse
          end do

        !! update total soil water in profile
        soil(j)%sw = 0.
        do k = 1, soil(j)%nly
          soil(j)%sw = soil(j)%sw + soil(j)%phys(k)%st
        end do

        pcom(j)%plstr(ipl)%strsw = xx / epmax(ipl)
        ep_day = ep_day + xx
      end if

      return
      end subroutine pl_waterup