      subroutine aqu_initial 
    
      use aquifer_module  
      use hydrograph_module
      use constituent_mass_module
      
      implicit none
      
      character (len=500) :: header    !header for output file
      character (len=80) :: titldum    !title 
      integer :: iaq                   !none      |counter
      integer :: iob                   !          | 
      integer :: iaqdb                 !          | 
      integer :: ipest                 !none      |counter
      integer :: ipath                 !          | 
      integer :: isalt                 !          | 
      integer :: i                     !none      |counter
      integer :: init                  !          | 
      integer :: idat

      !allocate objects for each aquifer
      allocate (aqu_d(sp_ob%aqu))
      allocate (aqu_prm(sp_ob%aqu))
      allocate (aqu_m(sp_ob%aqu))
      allocate (aqu_y(sp_ob%aqu))
      allocate (aqu_a(sp_ob%aqu))
      allocate (cs_aqu(sp_ob%aqu))
      
      do iaq = 1, sp_ob%aqu
        !! allocate constituents
        allocate (cs_aqu(iaq)%pest(cs_db%num_pests))
        allocate (cs_aqu(iaq)%path(cs_db%num_paths))
        allocate (cs_aqu(iaq)%hmet(cs_db%num_metals))
        allocate (cs_aqu(iaq)%salt(cs_db%num_salts))
          
        iob = sp_ob1%aqu + iaq - 1
        iaqdb = ob(iob)%props

        !! initialize parameters
        aqu_prm(iaq)%alpha = aqudb(iaqdb)%alpha
        aqu_prm(iaq)%flo_min = aqudb(iaqdb)%flo_min
        aqu_prm(iaq)%revap_co = aqudb(iaqdb)%revap_co
        aqu_prm(iaq)%revap_min = aqudb(iaqdb)%revap_min
        aqu_prm(iaq)%alpha_e = Exp(-aqudb(iaqdb)%alpha)
        aqu_prm(iaq)%bf_max = aqudb(iaq)%bf_max
        aqu_prm(iaq)%nloss = Exp(-.693 / (aqudb(iaqdb)%hlife_n + .1))
        aqu_d(iaq)%flo = aqudb(iaqdb)%flo
        aqu_d(iaq)%dep_wt = aqudb(iaqdb)%dep_wt
        aqu_d(iaq)%stor = 1000. * (aqudb(iaqdb)%dep_bot - aqu_d(iaqdb)%dep_wt) * aqudb(iaqdb)%spyld
        aqu_d(iaq)%no3 = aqudb(iaqdb)%no3
        aqu_d(iaq)%minp = aqudb(iaqdb)%minp
        aqu_d(iaq)%orgn = aqudb(iaqdb)%orgn
        aqu_d(iaq)%orgp = aqudb(iaqdb)%orgp
      end do

      return
      end subroutine aqu_initial         