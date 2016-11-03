      subroutine hru_soil_init (mres)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nhru          |none          |total number of HRUs in the watershed
!!    npmx          |none          |total number of pesticides modeled in
!!                                 |in watershed plus 1
!!    nope(:)       |none          |sequence number of pesticide in NPNO(:)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i             |none          |number of specific reservoir or HRU
!!    ndays(:)      |julian date   |julian date for last day of preceding 
!!                                 |month (where the array location is the 
!!                                 |number of the month) The dates are for
!!                                 |leap years
!!    npmx          |none          |total number of pesticides modeled in
!!                                 |watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: soil_chem, soil_phys, rteinit, h2omgt_init, hydro_init,

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use channel_module
      use hru_module
      use basin_module
      use jrw_datalib_module
      use conditional_module

      integer :: eof, isched
      character (len=80) :: titldum

      !!Section i
      !!assign database pointers for the hru
      imp = mres
      do ihru = 1, mhru
        iob = sp_ob1%hru + ihru - 1
        ihru_db = ob(iob)%props    !points to hru.dat
        hru(ihru)%dbs = hru_db(ihru_db)%dbs
        hru(ihru)%dbsc = hru_db(ihru_db)%dbsc
        hru(ihru)%parms = hru_db(ihru_db)%parms
        if (hru(ihru)%dbs%surf_stor > 0) then
          imp = imp + 1
          hru(ihru)%surfstor = imp
        else
          hru(ihru)%surfstor = 0
        end if
      end do
      !! use the same res object for resrvoirs and landscape storage
      !! allocate res and other types later in res_init
      mres = imp

      !!assign topography and hyd paramters
      do ihru = 1, mhru
        itopo_db = hru(ihru)%dbs%topo
        ihyd_db = hru(ihru)%dbs%hyd
        itopohd_db = hru(ihru)%dbs%topo
        ihyd_db = hru(ihru)%dbs%hyd
        hru(ihru)%topo%name = topo_db(itopo_db)%name
        hru(ihru)%topo%elev = topo_db(itopohd_db)%elev
        hru(ihru)%topo%slope = topo_db(itopohd_db)%slope
        hru(ihru)%topo%slope_len = topo_db(itopohd_db)%slope_len
        hru(ihru)%hyd%name = hyd_db(ihyd_db)%name
        hru(ihru)%hyd%lat_ttime = hyd_db(ihyd_db)%lat_ttime
        hru(ihru)%hyd%lat_sed = hyd_db(ihyd_db)%lat_sed / 1000. !mg/l => g/l ; mm * km2 * g/l = t
        hru(ihru)%topo%lat_len = topo_db(itopohd_db)%lat_len
        hru(ihru)%hyd%canmx = hyd_db(ihyd_db)%canmx
        hru(ihru)%hyd%esco = hyd_db(ihyd_db)%esco
        hru(ihru)%hyd%epco = hyd_db(ihyd_db)%epco
        hru(ihru)%hyd%erorgn = hyd_db(ihyd_db)%erorgn
        hru(ihru)%hyd%erorgp = hyd_db(ihyd_db)%erorgp
        hru(ihru)%hyd%cn3_swf = hyd_db(ihyd_db)%cn3_swf
        hru(ihru)%hyd%perco = hyd_db(ihyd_db)%perco
        hru(ihru)%topo%dis_stream = topo_db(itopohd_db)%dis_stream
        hru(ihru)%hyd%biomix = hyd_db(ihyd_db)%biomix
        hru(ihru)%hyd%dep_imp = hyd_db(ihyd_db)%dep_imp
        if (hru(ihru)%hyd%dep_imp < 1.e-6) hru(ihru)%hyd%dep_imp = 6000.
        hru(ihru)%hyd%lat_orgn = hyd_db(ihyd_db)%lat_orgn
        hru(ihru)%hyd%lat_orgp = hyd_db(ihyd_db)%lat_orgp
        ! set field data
        hru(ihru)%field%length = field_db(ifield_db)%length
        hru(ihru)%field%wid = field_db(ifield_db)%wid
        hru(ihru)%field%ang = field_db(ifield_db)%ang
        hru(ihru)%topo%dep_co = topo_db(itopohd_db)%dep_co
      end do
      
      !!Section 1
      !!this section sets, allocates, and initializes the original soil database
       msoils = Max(0,db_mx%soil)
       allocate (sol(0:msoils))
      do isol = 1, msoils
        sol(isol)%s%snam = soildb(isol)%s%snam
        sol(isol)%s%nly = soildb(isol)%s%nly + 1    !add 10 mm layer
        sol(isol)%s%hydgrp = soildb(isol)%s%hydgrp
        sol(isol)%s%zmx = soildb(isol)%s%zmx                      
        sol(isol)%s%anion_excl = soildb(isol)%s%anion_excl
        sol(isol)%s%crk = soildb(isol)%s%crk                  
        sol(isol)%s%texture = soildb(isol)%s%texture
        mlyr = sol(isol)%s%nly
        allocate (sol(isol)%ly(mlyr))
        allocate (sol(isol)%phys(mlyr))
        allocate (sol(isol)%nut(mlyr))   !!  nbs
        allocate (sol(isol)%cbn(mlyr))   !!  nbs
        !!set first 10 mm layer
        sol(isol)%phys(1)%d = 10.
        sol(isol)%phys(1)%bd = soildb(isol)%ly(1)%bd
        sol(isol)%phys(1)%awc = soildb(isol)%ly(1)%awc
        sol(isol)%phys(1)%k = soildb(isol)%ly(1)%k
        sol(isol)%cbn(1)%cbn = soildb(isol)%ly(1)%cbn
        sol(isol)%phys(1)%clay = soildb(isol)%ly(1)%clay
        sol(isol)%phys(1)%silt = soildb(isol)%ly(1)%silt
        sol(isol)%phys(1)%sand = soildb(isol)%ly(1)%sand
        sol(isol)%phys(1)%rock = soildb(isol)%ly(1)%rock
        sol(isol)%ly(1)%alb = soildb(isol)%ly(1)%alb
        sol(isol)%ly(1)%usle_k = soildb(isol)%ly(1)%usle_k
        sol(isol)%ly(1)%ec = soildb(isol)%ly(1)%ec
        sol(isol)%ly(1)%cal = soildb(isol)%ly(1)%cal
        sol(isol)%ly(1)%ph = soildb(isol)%ly(1)%ph
        !!set remaining layers
        do j = 2, mlyr
          sol(isol)%phys(j)%d = soildb(isol)%ly(j-1)%z
          sol(isol)%phys(j)%bd = soildb(isol)%ly(j-1)%bd
          sol(isol)%phys(j)%awc = soildb(isol)%ly(j-1)%awc
          sol(isol)%phys(j)%k = soildb(isol)%ly(j-1)%k
          sol(isol)%cbn(j)%cbn = soildb(isol)%ly(j-1)%cbn
          sol(isol)%phys(j)%clay = soildb(isol)%ly(j-1)%clay
          sol(isol)%phys(j)%silt = soildb(isol)%ly(j-1)%silt
          sol(isol)%phys(j)%sand = soildb(isol)%ly(j-1)%sand
          sol(isol)%phys(j)%rock = soildb(isol)%ly(j-1)%rock
          sol(isol)%ly(1)%alb = soildb(isol)%ly(j-1)%alb
          sol(isol)%ly(1)%usle_k = soildb(isol)%ly(j-1)%usle_k
          sol(isol)%ly(j)%ec = soildb(isol)%ly(j-1)%ec
          sol(isol)%ly(j)%cal = soildb(isol)%ly(j-1)%cal
          sol(isol)%ly(j)%ph = soildb(isol)%ly(j-1)%ph
        end do 
      end do

      if (bsn_cc%rtpest > 0) irtpest = pstcp(bsn_cc%rtpest)%nope

      do isol = 1, msoils
        call hru_soil_chem(isol)      !! initialize soil chemical parameters
        call hru_soil_phys(isol)      !! initialize soil physical parameters
      end do
      
      !!Section 2
      !!this section sets hru soils to appropriate soil database
      
      do ihru = 1, mhru
        !! allocate soil layers
        isol = hru(ihru)%dbs%soil
        wfsh(ihru) = 10. * Exp(6.5309 - 7.32561* sol(isol)%phys(1)%por +    &
      3.809479 * sol(isol)%phys(1)%por**2+0.001583 *                        &
      sol(isol)%phys(1)%clay **2 + 0.000344 * sol(isol)%phys(1)%sand*       &
      sol(isol)%phys(1)%clay - 0.049837 * sol(isol)%phys(1)%por *           &
      sol(isol)%phys(1)%sand + 0.001608*sol(isol)%phys(1)%por ** 2 *        &
      sol(isol)%phys(1)%sand ** 2+0.001602*sol(isol)%phys(1)%por ** 2 *     &
      sol(isol)%phys(1)%clay**2-0.0000136*sol(isol)%phys(1)%sand ** 2       &
      * sol(isol)%phys(1)%clay-0.003479*sol(isol)%phys(1)%clay ** 2 *       &
      sol(isol)%phys(1)%por - 0.000799 * sol(isol)%phys(1)%sand ** 2 *      & 
      sol(isol)%phys(1)%por)
        soil(ihru) = sol(isol)%s
        nly = soil(ihru)%nly
        allocate (soil(ihru)%ly(nly))
        allocate (soil(ihru)%phys(nly))
        allocate (soil(ihru)%nut(nly))    !!  nbs
        allocate (soil(ihru)%cbn(nly))    !!  nbs
        !! set hru soils to appropriate database soil layers
        do ly = 1, nly
          soil(ihru)%phys(ly) = sol(isol)%phys(ly)
          soil(ihru)%nut(ly) = sol(isol)%nut(ly)
          soil(ihru)%cbn(ly) = sol(isol)%cbn(ly)
          soil(ihru)%ly(ly) = sol(isol)%ly(ly)
          !! set arrays that are layer and plant dependent - residue and roots
          allocate (soil(ihru)%ly(ly)%rs(pcom(ihru)%npl))
        end do
      end do
      
      do ihru = 1, mhru
        isolt = hru(ihru)%dbs%soil_nutr_init
        if (isolt > 0) then
          call hru_soiltest_update(ihru, isolt)
        end if
      end do
      
      !!Section 3
      !!this section sets parameters related to soil and other processes

      !! dimension hru output variables
      allocate (hwb_d(mhru))
      allocate (hwb_m(mhru))
      allocate (hwb_y(mhru))
      allocate (hwb_a(mhru))
      allocate (hnb_d(mhru))
      allocate (hnb_m(mhru))
      allocate (hnb_y(mhru))
      allocate (hnb_a(mhru))
      allocate (hls_d(mhru))
      allocate (hls_m(mhru))
      allocate (hls_y(mhru))
      allocate (hls_a(mhru))
      allocate (hpw_d(mhru))
      allocate (hpw_m(mhru))
      allocate (hpw_y(mhru))
      allocate (hpw_a(mhru))

      do ihru = 1, mhru

      !! read initial bacteria data
      !! compute sequential basin number for routing
      !!this needs to be cleaned up and look like pesticides!!
      !mbac_db = bsn%num_bac
      !do mbac = 1, mbac_db
      !  do ibac = 1, bsn%num_bac
      !    if (bsn%bac(ibac) == bact(mbac)%bac(ibac)%num_db) then
      !      bact(mbac)%bac(ibac)%num_bsn = ibac
      !    end if
      !  end do
      !end do          
    
            !do ihru = 1, mhru
              !!Convert QSTE from volume to depth unit, mm
              !isep = hru(ihru)%dbs%septic
              ! if (sep(isep)%area > 1.e-6) then
              ! qstemm(ihru) = sepdb(sep(isep)%typ)%qs * sep(isep)%cap /
    !&	                        sep(isep)%area * 1000.
              ! end if
           ! end do
        
        !! allocate bacteria
        icmd = hru(ihru)%obj_no
        mbac = obcs(icmd)%num_paths
        if (mbac > 0) then
        !! allocate bacteria associated with
        do ly = 1, soil(ihru)%nly
          allocate (soil(ihru)%ly(ly)%bacsol(mbac))
          allocate (soil(ihru)%ly(ly)%bacsor(mbac))
        end do
        do ibac = 1, mbac
          if (ly == 1) then
            soil(ihru)%ly(1)%bacsol(ibac) = bact(ibac_db)%bac(ibac)%sol
            soil(ihru)%ly(1)%bacsor(ibac) = bact(ibac_db)%bac(ibac)%sor
          else
            soil(ihru)%ly(1)%bacsol(ibac) = 0.
            soil(ihru)%ly(1)%bacsor(ibac) = 0.
          end if
        end do   
        !! allocate bacteria associated with plant
        mbac = obcs(icmd)%num_paths
        if (mbac > 0) then
          do ipl = 1, pcom(j)%npl
            allocate (pcom(ihru)%plg(ipl)%bac(mbac))
          end do
        end if
        
        end if

        !! allocate pesticides
        npmx = obcs(icmd)%num_pests
        if (npmx > 0) then
        allocate (hru(ihru)%pst(mpst))
        npmx = obcs(icmd)%num_pests
        if (npmx > 0) then
          do ly = 1, soil(ihru)%nly
            allocate (soil(ihru)%ly(ly)%kp(npmx))
            allocate (soil(ihru)%ly(ly)%pst(npmx))
          end do
        end if

        npmx = obcs(icmd)%num_pests
        do ipest = 1, npmx
         hru(ihru)%pst(ipest)%num_db = pesti_db(ipest_db)%pesti(ipest)%num_db
         hru(ihru)%pst(ipest)%plt = pesti_db(ipest_db)%pesti(ipest)%plt
         soil(ihru)%ly(1)%pst(ipest) = pesti_db(ipest_db)%pesti(ipest)%soil
         hru(ihru)%pst(ipest)%enr = pesti_db(ipest_db)%pesti(ipest)%enr
        end do
        end if
        !!  topohyd defaults
        hru(ihru)%topo%lat_len = 50.
        xm = .6 * (1. - Exp(-35.835 * hru(ihru)%topo%slope))
        sin_sl = Sin(Atan(hru(ihru)%topo%slope))
        hru(ihru)%lumv%usle_ls = (hru(ihru)%topo%slope_len / 22.128) ** xm *          & 
                      (65.41 * sin_sl * sin_sl + 4.56 * sin_sl + .065)
      
        sol_cov(ihru) = soil(ihru)%ly(1)%rsd
        
      end do    !hru loop
                                   
      !!assign land use pointers for the hru
      !!allocate and initialize land use and management
      do ihru = 1, mhru
        !!ihru, ilu and isol are in modparm
        ilu = hru(ihru)%dbs%land_use_mgt
        isol = hru(ihru)%dbs%soil
        !send 0 value in when initializing- 1 for updating to deallocate
        call pcom_set_parms (0)
      end do

      call hydroinit        !! initialize hydrology parameters

      return
      end subroutine hru_soil_init