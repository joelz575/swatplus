      subroutine plant_init (init)

      use hru_module, only : blai_com, cn2, cvm_com, hru, ihru, ipl, isol, rsdco_plcom, ilu
      use soil_module
      use plant_module
      use hydrograph_module
      use climate_module, only : wst, wgn
      use time_module
!      use hru_lte_module
      use maximum_data_module
      use plant_data_module
      use landuse_data_module
      use mgt_operations_module
      use urban_data_module
      use conditional_module
      use organic_mineral_mass_module
      
      implicit none
      
      integer, intent (in) :: init   !           |
      integer :: icom                !           |
      integer :: idp                 !           |
      integer :: j                   !none       |counter
      integer :: ilug                !none       |counter 
      integer :: iob                 !           |
      integer :: iwgn                !           |
      integer :: mo                  !none       |counter 
      integer :: iday                !none       |counter 
      integer :: iplt                !none       |counter 
      integer :: i                   !none       |counter  
      integer :: icn                 !none       |counter 
      integer :: icp                 !none       |counter 
      integer :: ilum                !none       |counter 
      integer :: idb                 !none       |counter 
      integer :: isched              !           |
      integer :: iop                 !none       |management operation counter 
      integer :: irot                !none       |rotation year counter 
      integer :: igrow               !none       |julian day growth begins
      integer :: iday_sum            !none       |day for southern hemisphere (182-181)
      integer :: iday_sh             !none       |julian day growth begins in souther hemisphere
      real :: phutot                 !heat unit  |total potential heat units for year (used
                                     !           |when no crop is growing)
      real :: grow_start             !           |
      real :: grow_end               !           | 
      real :: tave                   !           |
      real :: phuday                 !           |
      real :: xx                     !           |
      real :: xm                     !           |
      real :: sin_sl                 !           |
      real :: sl_len                 !           | 
      real :: phu0                   !deg C      |base zero heat units for year

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
        isched = hru(j)%mgt_ops
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
            deallocate (pcom(j)%ab_gr)
            deallocate (pcom(j)%leaf)
            deallocate (pcom(j)%stem)
            deallocate (pcom(j)%seed)
            deallocate (pcom(j)%root)
            deallocate (pcom(j)%plstr) 
            deallocate (pcom(j)%plcur) 
            deallocate (rsd1(j)%tot)
          end if
        
        pcom(j)%npl = pcomdb(icom)%plants_com
        ipl = pcom(j)%npl
        allocate (pcom(j)%plg(ipl)) 
        allocate (pcom(j)%plm(ipl)) 
        allocate (pcom(j)%ab_gr(ipl))
        allocate (pcom(j)%leaf(ipl))
        allocate (pcom(j)%stem(ipl))
        allocate (pcom(j)%seed(ipl))
        allocate (pcom(j)%root(ipl))
        allocate (pcom(j)%plstr(ipl)) 
        allocate (pcom(j)%plcur(ipl)) 
        allocate (rsd1(j)%tot(ipl))

        cvm_com(j) = 0.
        blai_com(j) = 0.
        rsdco_plcom(j) = 0.
        pcom(j)%pcomdb = icom
        pcom(j)%rot_yr = 1
        do ipl = 1, pcom(j)%npl
          pcom(j)%plg(ipl)%cpnm = pcomdb(icom)%pl(ipl)%cpnm
          pcom(j)%plcur(ipl)%gro = pcomdb(icom)%pl(ipl)%igro
          pcom(j)%plcur(ipl)%idorm = "y"
          idp = pcomdb(icom)%pl(ipl)%db_num
          rsd1(j)%tot(ipl)%m = pcomdb(icom)%pl(ipl)%rsdin
          !set fresh organic pools--assume cn ratio = 57 and cp ratio = 300
          rsd1(j)%tot(ipl)%n = 0.43 * rsd1(j)%tot(ipl)%m / 57.
          rsd1(j)%tot(ipl)%p = 0.43 * rsd1(j)%tot(ipl)%m / 300.
          
          ! set heat units to maturity
          ! first compute base0 units for entire year
          phu0 = 0.
          do iday = 1, 365
            time%day = iday
            call xmon
            mo = time%mo
            tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
            if (tave > 0.) phu0 = phu0 + tave
          end do
          
          wgn(iwgn)%lat = -wgn(iwgn)%lat
          ! if days to maturity are not input - assume the plant is active during entire growing season
          if (pldb(icom)%days_mat < 1.e-6) then
            pcom(j)%plcur(ipl)%phumat = .9 * phutot
          else
            ! caculate planting day for annuals
            iday_sum = 181
            phutot = 0.
            phu0 = 0.15 * phu0    !assume planting at 0.15 base 0 heat units
            do iday = 1, 365
              if (wgn(iwgn)%lat > 0.) then
                time%day = iday
              else
                ! find Southern Hemisphere day
                iday_sum = iday_sum + 1
                if (iday_sum > 365) then
                  iday_sh = iday_sum - 365
                else
                  iday_sh = iday_sum
                end if
                time%day = iday_sh
              end if
              call xmon
              mo = time%mo
              tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
              phuday = tave
              if (phuday > 0.) then
                phutot = phutot + phuday
              end if
              if (phutot > phu0) exit
            end do
            
            ! calculate heat units from plant day (iday) to maturity (add days to maturity)
            if (wgn(iwgn)%lat > 0.) then
              igrow = iday
            else
              igrow = iday_sh
            end if
            do iday = igrow, igrow + pldb(iplt)%days_mat
              if (wgn(iwgn)%lat > 0.) then
                time%day = iday
              else
                ! find Southern Hemisphere day
                if (iday > 365) then
                  iday_sh = iday - 365
                else
                  iday_sh = iday
                end if
                time%day = iday_sh
              end if
              call xmon
              mo = time%mo
              tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
              phuday = tave - pldb(iplt)%t_base
              if (phuday > 0.) then
                phutot = phutot + phuday
              end if
            end do
            pcom(j)%plcur(ipl)%phumat = phutot
          end if
          
          ! set initial operation for date scheduling
          if (sched(isched)%num_ops > 0) then
          if (sched(isched)%mgt_ops(1)%jday > 0) then
            irot = 1
            do iop = 1, sched(isched)%num_ops
              if (irot == pcomdb(icom)%rot_yr_ini .and. time%day_start <= sched(isched)%mgt_ops(iop)%jday) then
                exit
              else
                if (sched(isched)%mgt_ops(iop)%name == "skip") then
                  irot = irot + 1
                end if
              end if
            end do
            sched(isched)%first_op = min (iop, sched(isched)%num_ops)
          else
            sched(isched)%first_op = 1
          end if
          end if
          sched(isched)%cur_op = sched(isched)%first_op

          ! set initial rotation year for dtable scheduling
          pcom(j)%rot_yr = pcomdb(icom)%rot_yr_ini
          
          ! set initial heat units and other data
          pcom(j)%plcur(ipl)%phuacc = pcomdb(icom)%pl(ipl)%phuacc
          pcom(j)%plg(ipl)%laimxfr = pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +     &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc))
          pcom(j)%plg(ipl)%lai = pcomdb(icom)%pl(ipl)%lai
          pcom(j)%plm(ipl)%mass = pcomdb(icom)%pl(ipl)%bioms
          pcom(j)%plcur(ipl)%curyr_mat = pcomdb(icom)%pl(ipl)%yrmat
          cvm_com(j) = plcp(idp)%cvm + cvm_com(j)
          rsdco_plcom(j) = rsdco_plcom(j) + pldb(idp)%rsdco_pl
          pcom(j)%plcur(ipl)%idplt = pcomdb(icom)%pl(ipl)%db_num
          idp = pcom(j)%plcur(ipl)%idplt
          pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1-pldb(idp)%pltpfr3)*        &
          (1. - pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +          &
           Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *                                &
           pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltpfr3
          pcom(j)%plm(ipl)%nmass=pcom(j)%plm(ipl)%n_fr * pcom(j)%plm(ipl)%mass                  
          pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1- pldb(idp)%pltnfr3) *      &
           (1.- pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +          &
           Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *                                &
          pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltnfr3
           pcom(j)%plm(ipl)%pmass = pcom(j)%plm(ipl)%p_fr * pcom(j)%plm(ipl)%mass
          if (pcom(j)%plcur(ipl)%pop_com < 1.e-6) then
            pcom(j)%plcur(ipl)%laimx_pop = pldb(idp)%blai
          else
            xx = pcom(j)%plcur(ipl)%pop_com / 1001.
            pcom(j)%plcur(ipl)%laimx_pop = pldb(idp)%blai * xx / (xx +          &
                    exp(pldb(idp)%pop1 - pldb(idp)%pop2 * xx))
          end if
          
          !! initialize plant mass
          call pl_root_gro
          call pl_seed_gro
          call pl_partition

        end do   ! ipl loop
        end if   ! icom > 0

        ilum = hru(ihru)%land_use_mgt
        !! set initial curve number parameters
        icn = lum_str(ilum)%cn_lu
        select case (sol(isol)%s%hydgrp)
        case ("A")
          cn2(j) = cn(icn)%cn(1)
        case ("B")
          cn2(j) = cn(icn)%cn(2)
        case ("C")
          cn2(j) = cn(icn)%cn(3)
        case ("D")
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
        if (lum(ilum)%tiledrain /= "null") then
          call structure_set_parms("tiledrain       ", lum_str(ilum)%tiledrain, j)
        end if
      
        if (lum(ilum)%septic /= "null") then
          call structure_set_parms("septic          ", lum_str(ilum)%septic, j)
        end if
        
        if (lum(ilum)%fstrip /= "null") then
          call structure_set_parms("fstrip          ", lum_str(ilum)%fstrip, j)
        end if
        
        if (lum(ilum)%grassww /= "null") then
          call structure_set_parms("grassww         ", lum_str(ilum)%grassww, j)
        end if

        if (lum(ilum)%bmpuser /= "null") then
          call structure_set_parms("bmpuser         ", lum_str(ilum)%bmpuser, j)
        end if

    return
    end subroutine plant_init