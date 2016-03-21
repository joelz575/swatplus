      subroutine aqu_initial 
      
      character (len=500) :: header
      character (len=80) :: titldum
       
      maqu_sp = sp_ob%aqu

      !allocate objects for each aquifer
      allocate (aqu(maqu_sp))
      allocate (aqu_st(maqu_sp))
      allocate (aqu_m(maqu_sp))
      allocate (aqu_y(maqu_sp))
      allocate (aqu_a(maqu_sp))
      
      do iaq = 1, maqu_sp
        iob = sp_ob1%aqu + iaq - 1
        iaqdb = ob(iob)%props
        !! initialize parameters
        aqu_prm(iaqdb)%alpha_e = Exp(-aqudb(iaqdb)%alpha)
        if(aqudb(iaqdb)%delay < .1) aqudb(iaqdb)%delay = .1
        aqu_prm(iaqdb)%delay_e = Exp(-1./(aqudb(iaqdb)%delay + 1.e-6))
            
        aqu_prm(iaqdb)%nloss = Exp(-.693 / (aqudb(iaqdb)%hlife_n + .1))
        aqu(iaq)%flo = aqudb(iaqdb)%flo           ! * ob(iob)%area_ha * 10.  !convert mm to m^3
        aqu(iaq)%stor = aqudb(iaqdb)%stor
        aqu(iaq)%hgt = aqudb(iaqdb)%hgt
        aqu(iaq)%no3 = aqudb(iaqdb)%no3
        aqu(iaq)%minp = aqudb(iaqdb)%minp
        aqu(iaq)%orgn = aqudb(iaqdb)%orgn
        aqu(iaq)%orgp = aqudb(iaqdb)%orgp
      end do

      return
      end subroutine aqu_initial         