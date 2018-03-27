      subroutine aqu_initial 
    
      use aquifer_module  
      use hydrograph_module
      
      implicit none
      
      character (len=500) :: header    !header for output file
      character (len=80) :: titldum    !title 
      integer :: maqu_sp               !          | 
      integer :: iaq                   !none      |counter
      integer :: iob                   !          | 
      integer :: iaqdb                 !          | 
       
      maqu_sp = sp_ob%aqu
      
      sumrchrg = 0.
      sumflo = 0.
      sumseep = 0.
      sumrevap = 0.

      !allocate objects for each aquifer
      allocate (aqu(maqu_sp))
      allocate (aqu_st(maqu_sp))
      allocate (aqu_m(maqu_sp))
      allocate (aqu_y(maqu_sp))
      allocate (aqu_a(maqu_sp))
      
      do iaq = 1, maqu_sp
        iob = sp_ob1%aqu + iaq - 1
        aqu_st(iaq)%obj_no = iob
        aqu_st(iaq)%props = ob(iob)%props
        iaqdb = aqu_st(iaq)%props
        !! initialize parameters
        aqu_prm(iaqdb)%alpha_e = Exp(-aqudb(iaqdb)%alpha)
        if(aqudb(iaqdb)%delay < .1) aqudb(iaqdb)%delay = .1
        aqu_prm(iaqdb)%delay_e = Exp(-1./(aqudb(iaqdb)%delay + 1.e-6))
        
        !! 
        aqu_prm(iaqdb)%nloss = Exp(-.693 / (aqudb(iaqdb)%hlife_n + .1))
        aqu(iaq)%flo = aqudb(iaqdb)%flo
        aqu(iaq)%stor = aqudb(iaqdb)%stor    !* ob(iob)%area_ha * 10.  !convert mm to m^3
        aqu(iaq)%hgt = aqudb(iaqdb)%hgt
        aqu(iaq)%no3 = aqudb(iaqdb)%no3
        aqu(iaq)%minp = aqudb(iaqdb)%minp
        aqu(iaq)%orgn = aqudb(iaqdb)%orgn
        aqu(iaq)%orgp = aqudb(iaqdb)%orgp
        aqu_st(iaq)%flo_min = aqudb(iaqdb)%flo_min
        aqu_st(iaq)%revap_co = aqudb(iaqdb)%revap
        aqu_st(iaq)%revap_min = aqudb(iaqdb)%revap_min
      end do

      return
      end subroutine aqu_initial         