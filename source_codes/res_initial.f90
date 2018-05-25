      subroutine res_initial
      
      use reservoir_module
      use maximum_data_module
      use reservoir_data_module
      use hydrograph_module
      
      implicit none
      
      integer :: ires        !none          |counter
      integer :: iprop       !              |     
      integer :: ihyd        !none          |counter 
      integer :: lnvol       !              |
      integer :: iires       !              | 
      real :: resdif         !              |
      integer :: i           !none          |counter
      integer :: init        !              | 
      real :: cnv            !              |
      

      do ires = 1, db_mx%res
        !! set initial volumes for res and hru types
        !! convert units
        iprop = res_ob(ires)%props
        ihyd = res_dat(iprop)%hyd
        res_ob(ires)%evol = res_hyd(ihyd)%evol * 10000.       !! ha-m => m**3
        res_ob(ires)%pvol = res_hyd(ihyd)%pvol * 10000.       !! ha-m => m**3
        res_ob(ires)%esa = res_hyd(ihyd)%esa
        res_ob(ires)%psa = res_hyd(ihyd)%psa
        
        !! calculate shape parameters for surface area equation
        resdif = res_hyd(ihyd)%evol - res_hyd(ihyd)%pvol
        if ((res_hyd(ihyd)%esa - res_hyd(ihyd)%psa) > 0. .and. resdif > 0.) then
          lnvol = Log10(res_ob(ires)%evol) - Log10(res_ob(ires)%pvol)
          if (lnvol > 1.e-4) then
            res_ob(ires)%br2 = (Log10(res_ob(ires)%esa) - Log10(res_ob(ires)%psa)) / lnvol
          else  
            res_ob(ires)%br2 = (Log10(res_ob(ires)%esa) - Log10(res_ob(ires)%psa)) / 0.001
          end if
          if (res_ob(ires)%br2 > 0.9) then
            res_ob(ires)%br2 = 0.9
            res_ob(ires)%br1 = (res_ob(ires)%psa / res_ob(ires)%pvol) ** 0.9
          else
            res_ob(ires)%br1 = (res_ob(ires)%esa / res_ob(ires)%evol) ** res_ob(iires)%br2
          end if  
        else
          res_ob(ires)%br2 = 0.9
          if (res_ob(ires)%pvol > 1.e-6) then
            res_ob(ires)%br1 = (res_ob(ires)%psa / res_ob(ires)%pvol) ** 0.9
          else
            res_ob(ires)%br1 = .1
          end if
        end if
        
      end do
      
      do ires = 1, db_mx%res
        !!set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg
        !!                                       ppm = t/m^3 * 10^6
        i = res_ob(ires)%props
        ihyd = res_dat(i)%hyd
        init = res_dat(i)%init
      
        cnv = res(ires)%flo / 1000.
        res(ires)%flo = res_init(init)%vol * res_ob(ires)%pvol
        res(ires)%sed = res_init(init)%sed * cnv
        res(ires)%orgn= res_init(init)%orgn * cnv
        res(ires)%no3 = res_init(init)%no3 * cnv
        res(ires)%no2 = res_init(init)%no2 * cnv
        res(ires)%nh3 = res_init(init)%nh3 * cnv
        res(ires)%sedp = res_init(init)%orgp * cnv
        res(ires)%solp = res_init(init)%solp * cnv
        res_ob(ires)%seci = res_init(init)%seci * cnv
        res(ires)%san = res_init(init)%san * cnv
        res(ires)%sil = res_init(init)%sil * cnv
        res(ires)%cla = res_init(init)%cla * cnv
        res(ires)%sag = res_init(init)%sag * cnv
        res(ires)%lag = res_init(init)%lag * cnv
        res(ires)%grv = res_init(init)%gra * cnv
        res(ires)%chla = res_init(init)%chla * cnv
        res(ires)%psor = res_init(init)%psor * cnv
        res(ires)%psor = res_init(init)%psor * cnv
        res(ires)%baclp = res_init(init)%bactlp * cnv
        res(ires)%bacp = res_init(init)%bactp * cnv
        
        !! calculate initial surface area       
        res_ob(ires)%area_ha = res_ob(ires)%br1 * res(ires)%flo ** res_ob(ires)%br2

      end do
      close(105)

      return
      end subroutine res_initial