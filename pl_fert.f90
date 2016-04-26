      subroutine pl_fert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies N and P specified by date and
!!    amount in the management file (.mgt)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)   |none          |fraction of bacteria in solution (the
!!                                 |remaining fraction is sorbed to soil
!!                                 |particles)
!!    bactlpdb(:)   |# cfu/g   frt |concentration of less persistent bacteria
!!                                 |in fertilizer
!!    bactpdb(:)    |# cfu/g   frt |concentration of persistent bacteria in
!!                                 |fertilizer
!!    bactlpq(:)    |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)    |# cfu/m^2     |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)     |# cfu/m^2     |persistent bacteria attached to soil 
!!                                 |particles
!!    curyr         |none          |current year of simulation
!!    fminn(:)      |kg minN/kg frt|fraction of fertilizer that is mineral N
!!                                 |(NO3 + NH4)
!!    fminp(:)      |kg minP/kg frt|fraction of fertilizer that is mineral P
!!    fnh3n(:)      |kgNH3-N/kgminN|fraction of mineral N in fertilizer that
!!                                 |is NH3-N
!!    forgn(:)      |kg orgN/kg frt|fraction of fertilizer that is organic N
!!    forgp(:)      |kg orgP/kg frt|fraction of fertilizer that is organic P
!!    frt_kg        |kg/ha         |amount of fertilizer applied to HRU
!!    frt_surface   |none          |fraction of fertilizer which is applied to
!!                                 |the top 10 mm of soil (the remaining
!!                                 |fraction is applied to first soil layer)
!!    hru_dafr(:)   |km2/km2       |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    nfert(:)      |none          |sequence number of fertilizer application
!!                                 |within the year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# cfu/m^2    |less persistent bacteria in soil solution
!!    bactlps(:)    |# cfu/m^2    |less persistent bacteria attached to soil
!!                                |particles
!!    bactpq(:)     |# cfu/m^2    |persistent bacteria in soil solution
!!    bactps(:)     |# cfu/m^2    |persistent bacteria attached to soil 
!!                                |particles
!!    nfert(:)      |none         |sequence number of fertilizer application
!!                                |within the year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units        |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frt_t        |
!!    gc           |
!!    gc1          |
!!    j            |none         |HRU number
!!    l            |none         |counter (soil layer #)
!!    rtof         |none         |weighting factor used to partition the 
!!                               |organic N & P content of the fertilizer
!!                               |between the fresh organic and the active 
!!                               |organic pools
!!    xx           |none         |fraction of fertilizer applied to layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      
      real, parameter :: rtof=0.5
      integer :: j, l, ifrt
      real :: xx, gc, gc1, swf, frt_t

      !!added by zhang
      !!======================
      real :: X1, X8, X10, XXX, YY, ZZ, XZ, YZ, RLN, orgc_f
      X1 = 0.
      X8 = 0.
      X10 = 0.
      XXX = 0.
      YY = 0.
      ZZ = 0.
      XZ = 0.
      YZ = 0.
      RLN = 0.
      orgc_f = 0.
      !!added by zhang
      !!======================  

      j = ihru

      ifrt = mgt%op4
      frt_kg = fertop%amt_kgh

      do l = 1, 2
        xx = 0.
        if (l == 1) then
          xx = fertop%surface                       
        else
          xx = 1. - fertop%surface                     
        endif

        soil(j)%nut(l)%no3 = soil(j)%nut(l)%no3 + xx * frt_kg *          &
            (1. - fertdb(ifrt)%fnh3n) * fertdb(ifrt)%fminn

        if (bsn_cc%cswat == 0) then
        soil(j)%nut(l)%fon = soil(j)%nut(l)%fon + rtof * xx * frt_kg *   &
                       fertdb(ifrt)%forgn
        soil(j)%nut(l)%aorgn = soil(j)%nut(l)%aorgn + (1. - rtof) * xx * &
            frt_kg * fertdb(ifrt)%forgn
        soil(j)%nut(l)%fop = soil(j)%nut(l)%fop + rtof * xx * frt_kg *   &
                       fertdb(ifrt)%forgp
        soil(j)%nut(l)%orgp=soil(j)%nut(l)%orgp+(1. - rtof)*xx*frt_kg *  &
                       fertdb(ifrt)%forgp
        end if
	  if (bsn_cc%cswat == 1) then
	  soil(j)%cbn(l)%mc = soil(j)%cbn(l)%mc + xx * frt_kg *            &
      		fertdb(ifrt)%forgn * 10.
	  soil(j)%nut(l)%mn = soil(j)%nut(l)%mn + xx * frt_kg *            &
      		fertdb(ifrt)%forgn
	  soil(j)%nut(l)%mp = soil(j)%nut(l)%mp + xx * frt_kg *            &
      		fertdb(ifrt)%forgp
	  end if

        !!By Zhang for C/N cycling 
        !!===========================
	  if (bsn_cc%cswat == 2) then
        soil(j)%nut(l)%fop = soil(j)%nut(l)%fop + rtof * xx *           &
            frt_kg * fertdb(ifrt)%forgp
        soil(j)%nut(l)%orgp = soil(j)%nut(l)%orgp + (1. - rtof) * xx *  &
            frt_kg * fertdb(ifrt)%forgp
        
        !!Allocate organic fertilizer to Slow (SWAT_active) N pool;
          soil(j)%cbn(l)%hsn = soil(j)%cbn(l)%hsn + (1. - rtof) * xx *  &
                        frt_kg * fertdb(ifrt)%forgn
          soil(j)%nut(l)%aorgn = soil(j)%cbn(l)%hsn


          
          !orgc_f is the fraction of organic carbon in fertilizer
          !for most fertilziers this value is set to 0.
          orgc_f = 0.0
          !X1 is fertlizer applied to layer (kg/ha)
          !xx is fraction of fertilizer applied to layer
          X1 = xx * frt_kg 
          !X8: organic carbon applied (kg C/ha)
          X8 = X1 * orgc_f
          !RLN is calculated as a function of C:N ration in fertilizer          
          RLN = .175 *(orgc_f)/(fertdb(ifrt)%fminn + fertdb(ifrt)%forgn  & 
                                                               + 1.e-5)
          
          !X10 is the fraction of carbon in fertilizer that is allocated to metabolic litter C pool
          X10 = .85-.018*RLN
          if (X10<0.01) then
            X10 = 0.01
          else
            if (X10 > .7) then
                X10 = .7
            end if
          end if
          
          !XXX is the amount of organic carbon allocated to metabolic litter C pool
          XXX = X8 * X10
          soil(j)%cbn(l)%lmc = soil(j)%cbn(l)%lmc + XXX
          !YY is the amount of fertilizer (including C and N) allocated into metabolic litter SOM pool
          YY = X1 * X10
          soil(j)%cbn(l)%lm = soil(j)%cbn(l)%lm + YY
          
          !ZZ is amount of organic N allocated to metabolic litter N pool
          ZZ = X1 *rtof * fertdb(ifrt)%forgn * X10
          
          
          soil(j)%cbn(l)%lmn = soil(j)%cbn(l)%lmn + ZZ
           
          !!remaining organic N is llocated to structural litter N pool
          soil(j)%cbn(l)%lsn = soil(j)%cbn(l)%lsn + X1                   &
                            * fertdb(ifrt)%forgn - ZZ
          !XZ is the amount of organic carbon allocated to structural litter C pool   
          XZ = X1 *orgc_f-XXX
          soil(j)%cbn(l)%lsc = soil(j)%cbn(l)%lsc + XZ
          
          !assuming lignin C fraction of organic carbon to be 0.175; updating lignin amount in strucutral litter pool
          soil(j)%cbn(l)%lslc = soil(j)%cbn(l)%lslc + XZ * .175
          !non-lignin part of the structural litter C is also updated;
          soil(j)%cbn(l)%lslnc = soil(j)%cbn(l)%lslnc + XZ * (1.-.175) 
          
          !YZ is the amount of fertilizer (including C and N) allocated into strucutre litter SOM pool
          YZ = X1 - YY
          soil(j)%cbn(l)%ls = soil(j)%cbn(l)%ls + YZ
          !assuming lignin fraction of the organic fertilizer allocated into structure litter SOM pool to be 0.175;
          !update lignin weight in structural litter.
          soil(j)%cbn(l)%lsl = soil(j)%cbn(l)%lsl + YZ*.175
          soil(j)%nut(l)%fon = soil(j)%cbn(l)%lmn + soil(j)%cbn(l)%lsn
          
          !end if
      
	  end if
        !!By Zhang for C/N cycling 
        !!=========================== 

        soil(j)%nut(l)%nh3 = soil(j)%nut(l)%nh3 + xx * frt_kg *          &
            fertdb(ifrt)%fnh3n * fertdb(ifrt)%fminn

        soil(j)%nut(l)%solp = soil(j)%nut(l)%solp + xx * frt_kg *        & 
            fertdb(ifrt)%fminp

      end do 

!! summary calculations
      fertno3 = frt_kg * fertdb(ifrt)%fminn * (1. - fertdb(ifrt)%fnh3n)
      fertnh3 = frt_kg * (fertdb(ifrt)%fminn * fertdb(ifrt)%fnh3n)
      fertorgn = frt_kg * fertdb(ifrt)%forgn
      fertsolp = frt_kg * fertdb(ifrt)%fminp
      fertorgp = frt_kg * fertdb(ifrt)%forgp  
      fertn = fertn + (frt_kg + cfertn) * (fertdb(ifrt)%fminn +         &         
                                                   fertdb(ifrt)%forgn)

      fertp = fertp + (frt_kg + cfertp) * (fertdb(ifrt)%fminp +         &         
                                                   fertdb(ifrt)%forgp)

      tfertn(j) = tfertn(j) + fertn
      tfertp(j) = tfertp(j) + fertp

!! increase fertilizer sequence number by one
      nfert(j) = nfert(j) + 1

      return
      end subroutine pl_fert