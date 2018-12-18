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
      allocate (aqu_prm(maqu_sp))
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
        aqu_prm(iaq)%alpha = aqudb(iaqdb)%alpha
        aqu_prm(iaq)%delay = aqudb(iaqdb)%delay
        aqu_prm(iaq)%flo_min = aqudb(iaqdb)%flo_min
        aqu_prm(iaq)%revap_co = aqudb(iaqdb)%revap_co
        aqu_prm(iaq)%revap_min = aqudb(iaqdb)%revap_min
        aqu_prm(iaq)%alpha_e = Exp(-aqudb(iaqdb)%alpha)
        if(aqudb(iaq)%delay < .1) aqudb(iaqdb)%delay = .1
        aqu_prm(iaq)%delay_e = Exp(-1./(aqudb(iaqdb)%delay + 1.e-6))
        aqu_prm(iaq)%nloss = Exp(-.693 / (aqudb(iaqdb)%hlife_n + .1))
        aqu(iaq)%flo = aqudb(iaqdb)%flo
        aqu(iaq)%dep_wt = aqudb(iaqdb)%dep_wt
        aqu(iaq)%stor = 1000. * (aqudb(iaqdb)%dep_bot - aqu(iaqdb)%dep_wt) * aqudb(iaqdb)%spyld
        aqu(iaq)%no3 = aqudb(iaqdb)%no3
        aqu(iaq)%minp = aqudb(iaqdb)%minp
        aqu(iaq)%orgn = aqudb(iaqdb)%orgn
        aqu(iaq)%orgp = aqudb(iaqdb)%orgp
      end do

      return
      end subroutine aqu_initial         