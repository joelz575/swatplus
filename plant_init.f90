      subroutine plant_init (init)

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

      use hru_module, only : blai_com, cn2, cvm_com, hru, idp, ihru, ipl, isol, nop, pcom,  &
         rsdco_plcom, sol, ilu 
      use hydrograph_module
      use climate_module, only : wst, wgn
      use time_module
      use hru_lte_module
      use jrw_datalib_module, only : lum_str, lum, lum_grp, pcomdb, pldb, plcp, cn, cons_prac, db_mx,  &
         urbdb, overland_n, sched
      use conditional_module
      use organic_mineral_mass_module
      
      integer, intent (in) :: init
      integer :: icom
  
      j = ihru
      
      !!assign land use pointers for the hru
        hru(j)%land_use_mgt = ilu
        hru(j)%plant_cov = lum_str(ilu)%plant_cov
        hru(j)%lum_group_c = lum(ilu)%cal_group
        do ilug = 1, lum_grp%num
          if (hru(j)%lum_group_c == lum_grp%name(ilum)) then
            hru(j)%lum_group =  ilug
          end if
        end do
        icom = hru(j)%plant_cov
        iob = hru(j)%obj_no
        iwst = ob(iob)%wst
        iwgn = wst(iwst)%wco%wgn
        hru(j)%mgt_ops = lum_str(ilu)%mgt_ops
        hru(j)%tiledrain = lum_str(ilu)%tiledrain
        hru(j)%septic = lum_str(ilu)%septic
        hru(j)%fstrip = lum_str(ilu)%fstrip
        hru(j)%grassww = lum_str(ilu)%grassww
        hru(j)%bmpuser = lum_str(ilu)%bmpuser
        hru(j)%luse%cn_lu = lum_str(ilu)%cn_lu
        hru(j)%luse%cons_prac = lum_str(ilu)%cons_prac

      !! allocate plants
        if (icom == 0) then
          pcom(j)%npl = 0
        else
          if (init > 0) then
            deallocate (pcom(j)%plg) 
            deallocate (pcom(j)%plm) 
            deallocate (pcom(j)%plstr) 
            deallocate (pcom(j)%plcur) 
            deallocate (rsd1(j)%tot)
          end if
        
        pcom(j)%npl = pcomdb(icom)%plants_com
        nop(j) = 1
        ipl = pcom(j)%npl
        allocate (pcom(j)%plg(ipl)) 
        allocate (pcom(j)%plm(ipl)) 
        allocate (pcom(j)%plstr(ipl)) 
        allocate (pcom(j)%plcur(ipl)) 
        allocate (rsd1(j)%tot(ipl))

        cvm_com(j) = 0.
        blai_com(j) = 0.
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
          
          ! set hu to maturity
          if (pldb(idp)%phu > 1.e-6) then
            pcom(j)%plcur(ipl)%phumat = pldb(idp)%phu
          else
            mo = 1
            imo = 2
            phutot = 0.
            do iday = 1, 365
              if (iday > ndays(imo)) then
                imo = imo + 1
                mo = mo + 1
              end if
              if (iday > grow_start .and. iday < grow_end) then
                tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
                iplt = hlt(i)%iplant
                phuday = tave - pldb(iplt)%t_base
                if (phuday > 0.) then
                  phutot = phutot + phuday
                end if
              end if
            end do
            ! set initial heat units and heat units to maturity
            pcom(j)%plcur(ipl)%phuacc = pcomdb(icom)%pl(ipl)%phuacc * phutot
            pcom(j)%plcur(ipl)%phumat = .9 * phutot
            pcom(j)%plcur(ipl)%phumat = Max(500., pcom(j)%plcur(ipl)%phumat)
            if (pldb(iplt)%idc <= 2 .or. pldb(iplt)%idc == 4 .or. pldb(iplt)%idc == 5) then
              pcom(j)%plcur(ipl)%phumat = Min(2000., pcom(j)%plcur(ipl)%phumat)
            end if
          end if
          
          pcom(j)%plg(ipl)%lai = pcomdb(icom)%pl(ipl)%lai
          pcom(j)%plm(ipl)%mass = pcomdb(icom)%pl(ipl)%bioms
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
          if (pcom(j)%plcur(ipl)%pop_com < 1.e-6) then
            pcom(j)%plcur(ipl)%laimx_pop = pldb(idp)%blai
          else
            xx = pcom(j)%plcur(ipl)%pop_com / 1001.
            pcom(j)%plcur(ipl)%laimx_pop = pldb(idp)%blai * xx / (xx +     &
                    exp(pldb(idp)%pop1 - pldb(idp)%pop2 * xx))
          end if
        end do
        end if

        ilum = hru(ihru)%land_use_mgt
        !! set initial curve number parameters
        icn = lum_str(ilum)%cn_lu
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
        
        !! set p factor and slope length (ls factor)
        icp = lum_str(ilum)%cons_prac
        xm = .6 * (1. - Exp(-35.835 * hru(ihru)%topo%slope))
        sin_sl = Sin(Atan(hru(ihru)%topo%slope))
        sl_len = amin1 (hru(ihru)%topo%slope_len, cons_prac(icp)%sl_len_mx)
        hru(ihru)%lumv%usle_ls = (hru(ihru)%topo%slope_len / 22.128) ** xm *          & 
                      (65.41 * sin_sl * sin_sl + 4.56 * sin_sl + .065)
        hru(ihru)%lumv%usle_p = cons_prac(icp)%pfac
        
        !! xwalk urban land use type with urban name in urban.urb
        hru(ihru)%luse%urb_ro = lum(ilum)%urb_ro
        do idb = 1, db_mx%urban
          if (lum(ilum)%urb_lu == urbdb(idb)%urbnm) then
            hru(ihru)%luse%urb_lu = idb
            exit
          endif
        end do
        
        !! xwalk overland n with name in ovn_table.lum
        do idb = 1, db_mx%ovn
          if (lum(ilum)%ovn == overland_n(idb)%name) then
            hru(ihru)%luse%ovn = overland_n(idb)%ovn
            exit
          endif
        end do
        
        !! set parameters for structural land use/managment
        if (lum(ilum)%tiledrain /= 'null') then
          call structure_set_parms('tiledrain       ', lum_str(ilum)%tiledrain, j)
        end if
      
        if (lum(ilum)%septic /= 'null') then
          call structure_set_parms('septic          ', lum_str(ilum)%septic, j)
        end if
        
        if (lum(ilum)%fstrip /= 'null') then
          call structure_set_parms('fstrip          ', lum_str(ilum)%fstrip, j)
        end if
        
        if (lum(ilum)%grassww /= 'null') then
          call structure_set_parms('grassww         ', lum_str(ilum)%grassww, j)
        end if

        if (lum(ilum)%bmpuser /= 'null') then
          call structure_set_parms('bmpuser         ', lum_str(ilum)%bmpuser, j)
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
    end subroutine plant_init