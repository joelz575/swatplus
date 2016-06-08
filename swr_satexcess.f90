      subroutine swr_satexcess(j1)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    nn          |none          |number of soil layers
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latlyr      |mm H2O        |lateral flow in soil layer for the day
!!    latq        |mm H2O        |total lateral flow in soil profile for the 
!!                               |day in HRU
!!    lyrtile     |mm H2O        |drainage tile flow in soil layer for day
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sep         |mm H2O        |micropore percolation from soil layer
!!    sepbtm      |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module

      integer :: j, j1, ii, isp
      real:: ul_excess,qlyr,pormm,rtof

      j = 0
      j = ihru
     	isp = sep(isep)%typ 	   !! J.Jeong 3/09/09
      rtof = 0.5

 	if (sep(isep)%opt ==2.and.j1==i_sep(j)) then
	  
	  ii = j1 
	  qlyr = soil(j)%phys(ii)%st
	  
	  ! distribute excess STE to upper soil layers 
	  do while (qlyr>0.and.ii>1)
	    
		 ! distribute STE to soil layers above biozone layer
		 if (soil(j)%phys(ii)%st > soil(j)%phys(ii)%ul) then
	      qlyr = soil(j)%phys(ii)%st - soil(j)%phys(ii)%ul 	! excess water moving to upper layer
	      soil(j)%phys(ii)%st = soil(j)%phys(ii)%ul  ! layer saturated
	      soil(j)%phys(ii-1)%st = soil(j)%phys(ii-1)%st + qlyr ! add excess water to upper layer
	   else 
	      qlyr = 0.
	   endif
	  
	    ! Add surface ponding to the 10mm top layer when the top soil layer is saturated
		 !  and surface ponding occurs.
		 if (ii==2) then
	     qlyr = soil(j)%phys(1)%st - soil(j)%phys(1)%ul
	     ! excess water makes surface runoff
	     if (qlyr>0) then
	         soil(j)%phys(1)%st = soil(j)%phys(1)%ul
             cbodu(j) = (cbodu(j) * surfq(j) + sepdb(sep(isep)%typ)%bodconcs * qlyr) / (qday + qlyr) !add septic effluent cbod (mg/l) concentration to HRU runoff, Jaehak Jeong 2016
	         surfq(j) = surfq(j) + qlyr 
		       qvol = qlyr * hru(j)%area_ha * 10.
		       ! nutrients in surface runoff
		       xx = qvol / hru(j)%area_ha / 1000.
	         surqno3(j) = surqno3(j) + xx * (sepdb(sep(isep)%typ)%no3concs +        &           
                       sepdb(sep(isep)%typ)%no2concs) 
               surqsolp(j) =  surqsolp(j) +  xx * sepdb(sep(isep)%typ)%minps 
               
	         ! Initiate counting the number of days the system fails and makes surface ponding of STE
	         if(sep_tsincefail(j)==0) sep_tsincefail(j) = 1
	     endif
	     qlyr = 0.
           !nutrients in the first 10mm layer
		   qvol = soil(j)%phys(1)%st * hru(j)%area_ha * 10.
		   xx = qvol / hru(j)%area_ha / 1000.
           soil(j)%nut(1)%no3 = soil(j)%nut(1)%no3 + xx *                &   
            (sepdb(sep(isep)%typ)%no3concs +                             &                      
                       sepdb(sep(isep)%typ)%no2concs)  
           soil(j)%nut(1)%nh3 = soil(j)%nut(1)%nh3 + xx *                &
                         sepdb(sep(isep)%typ)%nh4concs                  
           soil(j)%nut(1)%orgn=soil(j)%nut(1)%orgn + xx *                &   
                     sepdb(sep(isep)%typ)%orgnconcs * rtof
           soil(j)%nut(1)%fon = soil(j)%nut(1)%fon + xx *                &
                     sepdb(sep(isep)%typ)%orgnconcs * (1.-rtof)
           soil(j)%nut(1)%orgp=soil(j)%nut(1)%orgp+xx *                  &    
                   sepdb(sep(isep)%typ)%orgps * rtof
           soil(j)%nut(1)%fop=soil(j)%nut(1)%fop+xx*                     &
                   sepdb(sep(isep)%typ)%orgps*(1.-rtof)
           soil(j)%nut(1)%solp = soil(j)%nut(1)%solp + xx *              &    
                   sepdb(sep(isep)%typ)%minps  
     
		 endif

         ! volume water in the current layer: m^3
         qvol = soil(j)%phys(ii)%st * hru(j)%area_ha * 10. 
         
		 ! add nutrient to soil layer
		 xx = qvol / hru(j)%area_ha / 1000.
		 soil(j)%nut(ii)%no3 = soil(j)%nut(ii)%no3 + xx *        &
                        sepdb(sep(isep)%typ)%no3concs              &
                        + sepdb(sep(isep)%typ)%no2concs
	   soil(j)%nut(ii)%nh3=soil(j)%nut(ii)%nh3 + xx *            &
                        sepdb(sep(isep)%typ)%nh4concs
	   soil(j)%nut(ii)%orgn=soil(j)%nut(ii)%orgn + xx *          &        
                        sepdb(sep(isep)%typ)%orgnconcs*rtof
         soil(j)%nut(ii)%fon=soil(j)%nut(ii)%fon + xx *            &
                         sepdb(sep(isep)%typ)%orgnconcs * (1.-rtof)
         soil(j)%nut(ii)%orgp=soil(j)%nut(ii)%orgp+xx *            &
                         sepdb(sep(isep)%typ)%orgps * rtof
	   soil(j)%nut(ii)%fop = soil(j)%nut(ii)%fop + xx *          &
                         sepdb(sep(isep)%typ)%orgps*(1.-rtof)
         soil(j)%nut(ii)%solp = soil(jj)%nut(l)%solp + xx *        &
                         sepdb(sep(isep)%typ)%minps

	    ii = ii - 1
	  end do
	endif
	       
      if (sep(isep)%opt ==0) then
      if (j1 < hru(j)%sol%nly) then
        if (soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul > 1.e-4) then
          sepday = sepday + (soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul)
          soil(j)%phys(j1)%st = soil(j)%phys(j1)%ul
        end if
      else

        if (soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul > 1.e-4) then
          ul_excess = soil(j)%phys(j1)%st - soil(j)%phys(j1)%ul
          soil(j)%phys(j1)%st = soil(j)%phys(j1)%ul
          nn = hru(j)%sol%nly
          do ly = nn - 1, 1, -1
            soil(j)%phys(ly)%st = soil(j)%phys(ly)%st + ul_excess
            if (soil(j)%phys(ly)%st > soil(j)%phys(ly)%ul) then
              ul_excess = soil(j)%phys(ly)%st - soil(j)%phys(ly)%ul
              soil(j)%phys(ly)%st = soil(j)%phys(ly)%ul
            else
              ul_excess = 0.
              exit
            end if
            if (j1 == 1 .and. ul_excess > 0.) then
              !! add ul_excess to depressional storage and then to surfq
              pot(j)%vol = pot(j)%vol + ul_excess
            end if
          end do
          !compute tile flow again after saturation redistribution
        end if
      end if
      end if

      return
      end subroutine swr_satexcess