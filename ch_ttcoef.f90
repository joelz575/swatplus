      subroutine ch_ttcoef(k)
      use channel_module
      use jrw_datalib_module
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes travel time coefficients for routing
!!    along the main channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    chsslope(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    phi(1,:)    |m^2           |cross-sectional area of flow at bankfull
!!                               |depth
!!    phi(2,:)    |none          |
!!    phi(3,:)    |none          |
!!    phi(4,:)    |none          |
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    phi(6,:)    |m             |bottom width of main channel
!!    phi(7,:)    |m             |depth of water when reach is at bankfull
!!                               |depth
!!    phi(8,:)    |m/s           |average velocity when reach is at 
!!                               |bankfull depth
!!    phi(9,:)    |m/s           |wave celerity when reach is at
!!                               |bankfull depth
!!    phi(10,:)   |hr            |storage time constant for reach at
!!                               |bankfull depth (ratio of storage to
!!                               |discharge)
!!    phi(11,:)   |m/s           |average velocity when reach is at
!!                               |0.1 bankfull depth (low flow)
!!    phi(12,:)   |m/s           |wave celerity when reach is at
!!                               |0.1 bankfull depth (low flow)
!!    phi(13,:)   |hr            |storage time constant for reach at
!!                               |0.1 bankfull depth (low flow) (ratio
!!                               |of storage to discharge)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    aa          |none          |area/area=1 (used to calculate velocity with
!!                               |Manning's equation)
!!    a           |m^2           |cross-sectional area of channel
!!    b           |m             |bottom width of channel
!!    d           |m             |depth of flow 
!!    fps         |none          |change in horizontal distance per unit
!!                               |change in vertical distance on floodplain side
!!                               |slopes; always set to 4 (slope=1/4)
!!    jj          |none          |counter
!!    k           |none          |dummy argument (HRU number)
!!    p           |m             |wetting perimeter
!!    qq1         |m^3/s         |flow rate for a specified depth
!!    rh          |m             |hydraulic radius of channel
!!    tt1         |km s/m        |time coefficient for specified depth
!!    tt2         |km s/m        |time coefficient for bankfull depth
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use jrw_datalib_module

      integer, intent (in) :: k
      integer :: jj
      real :: fps, d, b, p, a, qq1, rh, tt1, tt2, aa

      do jj = 1, 13
        ch(k)%phi(jj) = 0.
      end do

      aa = 1.
      b = 0.
      d = 0.
!!    If side slope is not set in .rte file then assume this default
!!    If it is main reach default side slope to 2:1 if it is a waterway default to 8:1
      if (ch_hyd(k)%side <= 1.e-6) then
         chsslope = 2.
      else
         chsslope = ch_hyd(k)%side
      end if

      fps = 4.
      d = ch_hyd(k)%d
      b = ch_hyd(k)%w - 2. * d * chsslope


!!    check if bottom width (b) is < 0
      if (b <= 0.) then
        b = 0.
        chsslope = 0.
        b = .5 * ch_hyd(k)%w
        b = Max(0., b)
        chsslope = (ch_hyd(k)%w - b) / (2. * d)
      end if
      ch(k)%phi(6) = b
      ch(k)%phi(7) = d

!!    compute flow and travel time at bankfull depth
      p = 0.
      a = 0.
      rh = 0.
      tt2 = 0.
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      ch(k)%phi(1) = a
      ch(k)%phi(5) = Qman(a, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      ch(k)%phi(8) = Qman(aa, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      ch(k)%phi(9) = ch(k)%phi(8) * 5. / 3.
      ch(k)%phi(10) = ch_hyd(k)%l / ch(k)%phi(9) / 3.6
      tt2 = ch_hyd(k)%l * a / ch(k)%phi(5)

!!    compute flow and travel time at 1.2 bankfull depth
      d = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 1.2 * ch_hyd(k)%d
      a = a + (ch_hyd(k)%w * ch_hyd(k)%d + fps * (d - ch_hyd(k)%d) ** 2)
      p = p + 4. * ch_hyd(k)%w + (0.4 * ch_hyd(k)%d * Sqrt(fps * fps + 1.))
      rh = a / p
      qq1 = Qman(a, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      tt1 = ch_hyd(k)%l * a / qq1

!!    compute flow and travel time at 0.1 bankfull depth
      a = 0.
      d = 0.
      p = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 0.1 * ch_hyd(k)%d
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      qq1 = Qman(a, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      tt1 = ch_hyd(k)%l * a / qq1
      ch(k)%phi(11) = Qman(aa, rh, ch_hyd(k)%n, ch_hyd(k)%s)
      ch(k)%phi(12) = ch(k)%phi(11) * 5. / 3.
      ch(k)%phi(13) = ch_hyd(k)%l / ch(k)%phi(12) / 3.6

      return
      end subroutine ch_ttcoef