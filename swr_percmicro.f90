      subroutine swr_percmicro(ly1)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes percolation and lateral subsurface flow
!!    from a soil layer when field capacity is exceeded

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru         |none          |HRU number
!!    slsoil(:)    |m             |slope length for lateral subsurface flow
!!    sw_excess    |mm H2O        |amount of water in soil that exceeds field 
!!                                |capacity (gravity drained water)
!!    tdrain(:)    |hrs           |time to drain soil to field capacity
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latlyr       |mm H2O        |lateral subsurface flow in layer
!!    lyrtile      |mm H2O        |drainage tile flow in layer for day in HRU
!!    sepday       |mm H2O        |percolation from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adjf         |none          |adjustment factor for lateral flow
!!    dg           |mm            |depth of soil layer
!!    ho           |none          |variable to hold intermediate calculation
!!                                |result
!!    j            |none          |HRU number
!!    ly1          |none          |soil layer number
!!    ratio        |none          |ratio of seepage to (latq + sepday)
!!    yy           |mm            |depth to top of soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module

      integer, intent (in) :: ly1
      integer :: j
      real :: adjf, yy, dg, ho, ratio, sol_k_sep

      j = ihru
      adjf = 1.

      !! if temperature of layer is 0 degrees C or below
      !! there is no water flow
      if (soil(j)%phys(ly1)%tmp <= 0.) then
        sepday = 0.
        return
      end if

        !! COMPUTE LATERAL FLOW USING HILLSLOPE STORAGE METHOD
        if (ly1 == 1) then
          yy = 0.
        else
          yy = soil(j)%phys(ly1-1)%d
        end if

        dg = soil(j)%phys(ly1)%d - yy
        if (soil(j)%phys(ly1)%ul - soil(j)%phys(ly1)%fc == 0.) then
          ho = 0.
        else
          ho = 2. * sw_excess / ((soil(j)%phys(ly1)%ul - soil(j)%phys(ly1)%fc) / dg)
        end if
        latlyr = adjf * ho * soil(j)%phys(ly1)%k * hru(j)%topo%slope / hru(j)%topo%lat_len * .024

      if (latlyr < 0.) latlyr = 0. 
      if (latlyr > sw_excess) latlyr = sw_excess

      soil(j)%phys(ly1)%hk = (soil(j)%phys(ly1)%ul - soil(j)%phys(ly1)%fc) / soil(j)%phys(ly1)%k

      !! septic changes 1/28/09 
      if (ly1 == i_sep(j)) then
         if (sep(isep)%opt  == 1) then !active system
           sol_k_sep = soil(j)%phys(ly1)%k * (soil(j)%phys(ly1)%st - soil(j)%phys(ly1)%fc) /     &
                                              (soil(j)%phys(ly1)%ul - soil(j)%phys(ly1)%fc)
           sol_k_sep = Max(1.e-6, sol_k_sep)
           sol_k_sep = Min(soil(j)%phys(ly1)%k, sol_k_sep)
           soil(j)%phys(ly1)%hk = (soil(j)%phys(ly1)%hk - soil(j)%phys(ly1)%fc) / sol_k_sep
         
         elseif (sep(isep)%opt  == 2) then !failing system
           soil(j)%phys(ly1)%hk = 1.e10
         endif
      endif 
      !!  septic changes 1/28/09
      
      soil(j)%phys(ly1)%hk = Max(2., soil(j)%phys(ly1)%hk)

      !! compute seepage to the next layer
      sepday = (soil(j)%phys(ly1)%st - soil(j)%phys(ly1)%fc * hru(j)%hyd%perco) * (1. - Exp(-24. / soil(j)%phys(ly1)%hk))
      sepday = Max(0., sepday)
      
      !! limit maximum seepage from biozone layer below potential perc amount
	  if(ly1 == i_sep(j).and.sep(isep)%opt ==1) then
	    sepday = min(sepday,sol_k_sep *24.)
	    bz_perc(j) = sepday
	  end if
      
      !! restrict seepage if next layer is saturated
      if (ly1 == soil(j)%nly) then
        xx = (hru(j)%hyd%dep_imp - soil(j)%phys(ly1)%d) / 1000.
        if (xx < 1.e-4) then
          sepday = 0.
        else
          sepday = sepday * xx / (xx + Exp(8.833 - 2.598 * xx))
        end if
      end if

      !! check mass balance
      if (sepday + latlyr > sw_excess) then
        ratio = sepday / (latlyr + sepday)
        sepday = sw_excess * ratio
        latlyr = sw_excess * (1. - ratio)
      endif
      if (sepday + lyrtile > sw_excess) then
        sepday = sw_excess - lyrtile
      endif

      return
      end subroutine swr_percmicro