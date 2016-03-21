      subroutine mgt_operatn
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations             

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dayl(:)     |hours         |day length for current day
!!    daylmn(:)   |hours         |shortest daylength occurring during the
!!                               |year
!!    dormhr(:)   |hours         |time threshold used to define dormant
!!                               |period for plant (when daylength is within
!!                               |the time specified by dormhr from the minimum
!!                               |daylength for the area, the plant will go
!!                               |dormant)
!!    phubase(:)  |heat units    |base zero total heat units (used when no
!!                               |land cover is growing
!!    icr(:)      |none          |sequence number of crop grown within a year
!!    iida        |julian date   |day being simulated (current julian date)
!!    idc(:)      |none          |crop/landcover category:
!!                               |1 warm season annual legume
!!                               |2 cold season annual legume
!!                               |3 perennial legume
!!                               |4 warm season annual
!!                               |5 cold season annual
!!                               |6 perennial
!!                               |7 trees
!!    ihru        |none          |HRU number
!!    iop(:,:,:)  |julian date   |date of tillage operation
!!    ntil(:)     |none          |sequence number of tillage operation within
!!                               |current year
!!    phut(:,:,:) |none          |fraction of heat units (base zero or plant)
!!                               |at which tillage occurs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aphu        |heat units    |fraction of total heat units accumulated
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: plantop, dormant, harvkillop, harvestop, killop, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module

      integer :: j
      real :: aphu, tillphu

      j = ihru
      isched = hru(j)%mgt_ops
      if (sched(isched)%num_ops < 1) return
      
      mgt = sched(isched)%mgt_ops(nop(j))

        do while(mgt%jday > 0 .and. iida == mgt%jday)
          call mgt_sched (isched)
          if (yr_skip(j) == 1) exit
        end do

        ipl = amax1(mgt%op2, 1)
        if (pcom(j)%plcur(ipl)%gro == 0) then
          aphu = phubase(j)
        else
          aphu = pcom(j)%plcur(ipl)%phuacc
        end if 
        if (dorm_flag == 1) aphu = 999.
        do while (mgt%husc > 0. .and. aphu > mgt%husc)
          call mgt_sched (isched)
          ipl = amax1(mgt%op2, 1)
          if (pcom(j)%plcur(ipl)%gro == 0) then
            aphu = phubase(j)
          else
            aphu = pcom(j)%plcur(ipl)%phuacc
          end if
          if (dorm_flag == 1) aphu = 999.
          if (mgt%op == "skip") then
	      call mgt_sched (isched)
          end if
          if (yr_skip(j) == 1) exit
        end do
         
      return
1000  format (4i10,a10)
      end subroutine mgt_operatn