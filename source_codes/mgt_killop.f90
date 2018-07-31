      subroutine mgt_killop (jj, iplant)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr        |none          |current year of simulation
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru         |none          |HRU number
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:) |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~



!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : hru, hrupest, ihru, ipl, npmx
      use soil_module
      use plant_module
      use constituent_mass_module
      
      implicit none
   
      integer :: j                     !none           |HRU number
      integer :: k                     !none           |counter
      integer, intent (in) :: jj       !none           |counter
      integer, intent (in) :: iplant   !               |plant number xwalked from hlt_db()%plant and plants.plt
      real :: resnew                   !               | 
      real :: resnew_n                 !               | 
      integer :: orgc_f                !fraction       |fraction of organic carbon in fertilizer
      real :: rtresnew                 !               |
      real :: ff1                      !               |
      real :: ff2                      !               |
      real :: yieldn                   !               |
      real :: yieldp                   !               |  
      real :: xx                       !varies         |variable to hold calculation results 
      real :: rln                      !               | 
      real :: rlr                      !fraction       |fraction of lignin in the added residue
      real :: l                        !none           |counter  
      real :: rtfr                     !none           |root fraction
      integer :: hiad1                 !none           |actual harvest index (adj for water/growth)
      integer :: icmd                  !               |

      resnew = 0.
      resnew_n = 0.

      j = jj
      ipl = iplant

	  !! 22 January 2008	
      resnew = pcom(j)%plm(ipl)%mass * (1. - pcom(j)%plg(ipl)%root_frac)
	  rtresnew = pcom(j)%plm(ipl)%mass * pcom(j)%plg(ipl)%root_frac
	  call pl_rootfr

	  !! update residue, N, P on soil surface
      ff1 = (1 - hiad1) / (1 - hiad1 + pcom(j)%root(ipl)%mass)
      rsd1(j)%tot(ipl)%m = resnew + rsd1(j)%tot(ipl)%m
      
      rsd1(j)%tot(ipl)%n = rsd1(j)%tot(ipl)%n + ff1 *     &
                                  (pcom(j)%plm(ipl)%nmass - yieldn)
      rsd1(j)%tot(ipl)%p = rsd1(j)%tot(ipl)%p + ff1 *     & 
                                  (pcom(j)%plm(ipl)%pmass - yieldp)
      rsd1(j)%tot(ipl)%m = Max(rsd1(j)%tot(ipl)%m, 0.)
	  rsd1(j)%tot(ipl)%n = Max(rsd1(j)%tot(ipl)%n, 0.)
	  rsd1(j)%tot(ipl)%p = Max(rsd1(j)%tot(ipl)%p, 0.)
      
      resnew = resnew
      resnew_n = ff1 * (pcom(j)%plm(ipl)%nmass- yieldn)
      call pl_leaf_drop (resnew, resnew_n)

	!! allocate dead roots, N, P to soil layers
	do l = 1, soil(j)%nly
	 soil(j)%ly(l)%rsd = soil(j)%ly(l)%rsd + soil(j)%ly(l)%rtfr * rtresnew
	 soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + soil(j)%ly(l)%rtfr *            &
          pcom(j)%plm(ipl)%nmass * pcom(j)%root(ipl)%mass
	 soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + soil(j)%ly(l)%rtfr *          &
          pcom(ihru)%plm(ipl)%pmass * pcom(j)%root(ipl)%mass
     
     resnew = soil(j)%ly(l)%rtfr * rtresnew 
     resnew_n = soil(j)%ly(l)%rtfr * ff2 * (pcom(j)%plm(ipl)%nmass - yieldn)
     call pl_leaf_drop (resnew, resnew_n)
	end do

      if (hrupest(j) == 1) then
        npmx = cs_db%num_pests
        do k = 1, npmx
           soil(j)%ly(1)%pst(k) = soil(j)%ly(1)%pst(k) + pcom(j)%pest(k)
           pcom(j)%pest(k) = 0.
        end do
      end if
      
	!! reset variables
      pcom(j)%plg(ipl) = plgz
      pcom(j)%plm(ipl) = plmz
      pcom(j)%plstr(ipl) = plstrz
      !! can't reset entire plcur - harv_num can't be zero'd
      pcom(j)%plcur(ipl)%gro = "n"
      pcom(j)%plcur(ipl)%idorm = "n"
      pcom(j)%plcur(ipl)%phuacc = 0.
      pcom(j)%plcur(ipl)%curyr_mat = 1

	  rtfr = 0. ! Resetting roots fraction per layer array
	 
      return
      end subroutine mgt_killop