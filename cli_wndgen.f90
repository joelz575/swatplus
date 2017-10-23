      subroutine cli_wndgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates wind speed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    j           |none          |HRU number
!!    i_mo        |none          |month being simulated
!!    rndseed(:,:)|none          |random number seeds
!!    wndav(:,:)  |m/s           |average wind speed for the month in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    v6          |none          |random number between 0.0 and 1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log
!!    SWAT: Aunif

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, only : i_mo
      
 
      real :: v6, v7, rdir2, pi2
      integer :: idir, idir1, idir2, mo

      pi2 = 6.283185
      mo = time%mo
      
      !! Generate wind speed !!
      v6 = Aunif(rndseed(idg(5),iwgn))
      wst(iwst)%weat%windsp = wgn(iwgn)%windav(i_mo) * (-Log(v6)) ** 0.3
      
      !! Generate wind direction !!
      iwndir = wst(iwst)%wco%wndir
      if (iwndir > 0) then
      idir1 = 16
      idir2 = 15
      v7 = Aunif(rndseed(idg(5),iwgn))
      do idir = 1, 16
        if (wnd_dir(iwndir)%dir(mo,idir) > v7) then
          idir1 = idir
          idir2 = idir - 1
          exit
        end if
      end do
      if (idir1 == 1) then
        g = v7 / wnd_dir(iwndir)%dir(mo,idir1)
      else
        g = (v7 - wnd_dir(iwndir)%dir(mo,idir2)) /                        &
           (wnd_dir(iwndir)%dir(mo,idir1) - wnd_dir(iwndir)%dir(mo,idir2))
      end if

      rdir2 = float (idir2)
      wst(iwst)%weat%wndir = pi2 * (g + rdir2 - .5) / 16.
      wst(iwst)%weat%wndir = pi2 + wst(iwst)%weat%wndir
      end if
      
      return
      end subroutine cli_wndgen