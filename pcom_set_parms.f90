      subroutine pcom_set_parms (init)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT:

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use hru_module
      use jrw_datalib_module
      use conditional_module
      use organic_mineral_mass_module
      
      integer, intent (in) :: init
         
      j = ihru
      
      !!assign land use pointers for the hru
        hru(j)%land_use_mgt = ilu
        hru(j)%plant_cov = lum_str(ilu)%plant_cov
        icom = hru(j)%plant_cov
        hru(j)%mgt_ops = lum_str(ilu)%mgt_ops
        hru(j)%tiledrain = lum_str(ilu)%tiledrain
        hru(j)%septic = lum_str(ilu)%septic
        hru(j)%fstrip = lum_str(ilu)%fstrip
        hru(j)%grassww = lum_str(ilu)%grassww
        hru(j)%terrace = lum_str(ilu)%terrace
        hru(j)%contour = lum_str(ilu)%contour
        hru(j)%stcrop = lum_str(ilu)%stcrop
        hru(j)%bmpuser = lum_str(ilu)%bmpuser
        hru(j)%luse%cn_lu = lum_str(ilu)%cn_lu
        hru(j)%luse%usle_p = 1.
        hru(j)%luse%urb_lu = lum(ilu)%urb_lu
        hru(j)%luse%ovn = lum(ilu)%ovn

      !! allocate plants
        ipl_com(j) = icom
        if (icom == 0) then
          pcom(j)%npl = 0
        else
        if (init > 0) then
          deallocate (pcom(j)%plg) 
          deallocate (pcom(j)%plm) 
          deallocate (pcom(j)%plstr) 
          deallocate (pcom(j)%plcur) 
          deallocate (hru(j)%veg_ag)
          deallocate (hru(j)%grain)
          deallocate (hru(j)%root)
          
          deallocate (hru(j)%rsd_flt)
          deallocate (rsd1(j)%tot)
        end if
        
        pcom(j)%npl = pcomdb(icom)%plants_com
        nop(j) = 1
        ipl = pcom(j)%npl
        allocate (pcom(j)%plg(ipl)) 
        allocate (pcom(j)%plm(ipl)) 
        allocate (pcom(j)%plstr(ipl)) 
        allocate (pcom(j)%plcur(ipl)) 
        allocate (hru(j)%veg_ag(ipl))
        allocate (hru(j)%grain(ipl))
        allocate (hru(j)%root(ipl))
        
        allocate (hru(j)%rsd_flt(ipl))
        allocate (rsd1(j)%tot(ipl))

        cvm_com(j) = 0.
        blai_com(j) = 0.
        tnylda(j) = 0.
        rsdco_plcom(j) = 0.
        pcom(j)%pcomdb = icom
        do ipl = 1, pcom(j)%npl
          pcom(j)%plg(ipl)%cpnm = pcomdb(icom)%pl(ipl)%cpnm
          pcom(j)%plcur(ipl)%gro = pcomdb(icom)%pl(ipl)%igro
          pcom(j)%plcur(ipl)%idorm = 1
          idp = pcomdb(icom)%pl(ipl)%db_num
          rsd1(j)%tot(ipl)%m = pcomdb(icom)%pl(ipl)%rsdin
          !set fresh organic pools--assume cn ratio = 57 and cp ratio = 300
          rsd1(j)%tot(ipl)%n = 0.43 * rsd1(j)%tot(ipl)%m / 57.
          rsd1(j)%tot(ipl)%p = 0.43 * rsd1(j)%tot(ipl)%m / 300.
          pcom(j)%plg(ipl)%phumat = pcomdb(icom)%pl(ipl)%phu
          pcom(j)%plg(ipl)%lai = pcomdb(icom)%pl(ipl)%lai
          pcom(j)%plm(ipl)%mass = pcomdb(icom)%pl(ipl)%bioms
          pcom(j)%plcur(ipl)%phuacc = pcomdb(icom)%pl(ipl)%phuacc
          pcom(j)%plcur(ipl)%curyr_mat = pcomdb(icom)%pl(ipl)%yrmat
          cvm_com(j) = plcp(idp)%cvm + cvm_com(j)
          rsdco_plcom(j) = rsdco_plcom(j) + pldb(idp)%rsdco_pl
          pcom(j)%plcur(ipl)%idplt = pcomdb(icom)%pl(ipl)%db_num
          idp = pcom(j)%plcur(ipl)%idplt
          pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1-pldb(idp)%pltpfr3)*   &
          (1. - pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +     &
           Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *                           &
           pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltpfr3
            pcom(j)%plm(ipl)%nmass=pcom(j)%plm(ipl)%n_fr *                 &
           pcom(j)%plm(ipl)%mass                  
          pcom(j)%plm(ipl)%n_fr =                                          &
           (pldb(idp)%pltnfr1- pldb(idp)%pltnfr3) *                        &
           (1.- pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +     &
           Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *                           &
           pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltnfr3
          pcom(j)%plm(ipl)%pmass = pcom(j)%plm(ipl)%p_fr *                 &
                pcom(j)%plm(ipl)%mass
          tnylda(j) = tnylda(j) + 350. * pldb(idp)%cnyld *                 &
                pldb(idp)%bio_e / pcom(j)%npl
          if (pcom(j)%plcur(ipl)%pop_com < 1.e-6) then
            pcom(j)%plg(ipl)%laimx_pop = pldb(idp)%blai
          else
            xx = pcom(j)%plcur(ipl)%pop_com / 1001.
            pcom(j)%plg(ipl)%laimx_pop = pldb(idp)%blai * xx / (xx +     &
                    exp(pldb(idp)%pop1 - pldb(idp)%pop2 * xx))
          end if
        end do
        end if

        !! set initial curve number parameters
        icn = lum_str(ilu)%cn_lu
        select case (sol(isol)%s%hydgrp)
        case ('A')
          cn2(j) = cn(icn)%cn(1)
        case ('B')
          cn2(j) = cn(icn)%cn(2)
        case ('C')
          cn2(j) = cn(icn)%cn(3)
        case ('D')
          cn2(j) = cn(icn)%cn(4)
        end select
 
        call curno(cn2(j),j)
       
        !! set parameters for structural land use/managment
        if (lum(ilu)%tiledrain /= 'null') then
          call structure_set_parms('tiledrain       ', lum_str(ilu)%tiledrain, j)
        end if
      
        if (lum(ilu)%septic /= 'null') then
          call structure_set_parms('septic          ', lum_str(ilu)%septic, j)
        end if
        
        if (lum(ilu)%fstrip /= 'null') then
          call structure_set_parms('fstrip          ', lum_str(ilu)%fstrip, j)
        end if
        
        if (lum(ilu)%grassww /= 'null') then
          call structure_set_parms('grassww         ', lum_str(ilu)%grassww, j)
        end if
        
        if (lum(ilu)%terrace /= 'null') then
          call structure_set_parms('terrace         ', lum_str(ilu)%terrace, j)
        end if
        
        if (lum(ilu)%contour /= 'null') then
          call structure_set_parms('contour         ', lum_str(ilu)%contour, j)
        end if
        
        if (lum(ilu)%stcrop /= 'null') then
          call structure_set_parms('stcrop          ', lum_str(ilu)%stcrop, j)
        end if
        
        if (lum(ilu)%bmpuser /= 'null') then
          call structure_set_parms('bmpuser         ', lum_str(ilu)%bmpuser, j)
        end if

      !! set linked-list array for all hru's with unlimited source irrigation (hru_irr_nosrc)
      db_mx%irr_nosrc = 0
        isched = hru(j)%mgt_ops
        icmd = hru(j)%obj_no
        if (sched(isched)%irr > 0 .and. ob(icmd)%wr_ob == 0) then
          hru(j)%irrsrc = 1
          db_mx%irr_nosrc = db_mx%irr_nosrc + 1
        end if
  
      return
      end subroutine pcom_set_parms