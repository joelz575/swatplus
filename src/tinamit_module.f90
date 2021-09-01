module tinamit_module

    use landuse_data_module, ONLY : lum
    use hru_module, ONLY : ihru, isol, hru, hru_db
    use plant_module
    use hru_lte_module, ONLY : hlt
    use channel_module, ONLY : ch
    use sd_channel_module, ONLY : sd_ch
    use hydrograph_module, ONLY : sp_ob, ob, sp_ob1, ch_out_y
    use time_module
    use soil_module
    use plant_data_module
    use mgt_operations_module
    use conditional_module

    save
    integer :: MAX_BUFFER_LEN = 20000
    integer cliente_obj
    logical dynamic
    integer :: dias = 0
    integer :: t = 1
contains

    subroutine abre (arg1, arg2)

        character(len = 32), intent(in) :: arg1, arg2
        character(len = 256) :: host_num
        integer :: port_num

        host_num = arg2
        write (*, *) "host_num= ", host_num
        READ(arg1, '(I5)') port_num
        print *, "port_Num = ", port_num

        !Opening a socket
        write(*, *) 'Opening Socket now...'
        CAll opensocket(port_num, host_num, cliente_obj)
        print *, "cliente_obj=", cliente_obj

    end subroutine

    subroutine recibe ()
        !character, dimension(:, :), allocatable :: charBuffer
        character(len = 7) :: command
        character(len = 21) :: var = "                     "
        character(len = 5) :: tipo_contents
        integer :: tmn_contents, nPasos, i, shape
        real, allocatable, dimension(:) :: realBuffer(:)
        integer, allocatable, dimension(:) :: intBuffer(:)

        tmn_shape = 1

        call receive (cliente_obj, command, var, tipo_contents, nPasos, shape) !charBuffer

        if (command == "cambiar")then

            if(tipo_contents=="float")then
                if(allocated(realBuffer)) deallocate(realBuffer)
                allocate(realBuffer(shape))
                call recvfloat (cliente_obj, realBuffer, shape)

            elseif(tipo_contents=="int".or.tipo_contents=="int64")then
                if(allocated(intBuffer)) deallocate(intBuffer)
                allocate(intBuffer(shape))
                call recvint (cliente_obj, intBuffer, shape)

            end if

        elseif (command == "leer")then

            print *, "Variable Name: ", trim(var)

        end if

        call evaluar(command, trim(var), shape, intBuffer, realBuffer, nPasos)

    end subroutine recibe

    subroutine evaluar (orden, var, shape, intBuffer, realBuffer, nPasos)

        character(len = :), allocatable :: senderBuffer
        character(*) :: var, orden
        integer :: nPasos, t_final
        integer :: shape
        integer, dimension(shape) :: intBuffer
        real, dimension(shape) :: realBuffer

        !make this a case statement
        if(trim(orden) == 'cerrar')then
            call closesock(cliente_obj)
            print *, "The socket was successfully closed"
            dynamic = .false.

        elseif(trim(orden) == 'incr')then
            dias = nPasos
            print *, "Number of Passes: ", nPasos
            !No further action required

        elseif(trim(orden) == 'cambiar')then
            call tomar (var, shape, intBuffer, realBuffer)

        elseif(trim(orden) == 'leer') then
            call obtener (var)

        else
            print *, "The command: ", trim(orden), "is not recognized"

        end if

    end subroutine evaluar

    subroutine tomar (variable_Name, shape, intBuffer, floatBuffer)
        character (*) :: variable_Name
        character(len = :), allocatable :: senderBuffer
        integer :: index, i, isched
        integer :: shape
        integer, dimension(shape) :: intBuffer
        real, dimension(shape) :: floatBuffer
        integer :: f = 1
        character(len = 8) :: lu_prev
        real :: rock

        if (sp_ob%hru > 0) then
            print *, "there are full hru's"
        else
            print *, "there are only lite hru's"
        end if

        if (size(ch) > 0) then
            print *, "There are full channels defined"
        else
            print *, "there are only lite channels defined"

        end if

        select case (trim(variable_Name))
        !-----------SD-Channel Variables----------------------------------------------------------------------------------------
        case("sd_props")
            sd_ch%props = intBuffer

        case("sd_obj_no")
            sd_ch%obj_no = intBuffer

        case("sd_aqu_link")
            !aquifer the channel is linked to
            sd_ch%aqu_link = intBuffer

        case("sd_aqu_link_ch")
            !sequential channel number in the aquifer
            sd_ch%aqu_link_ch = intBuffer

        case("sd_chw")
            !m          |channel width
            sd_ch%chw = floatBuffer

        case("sd_chd")
            !m          |channel depth
            sd_ch%chd = floatBuffer

        case("sd_chs")
            !m/m        |channel slope
            sd_ch%chs = floatBuffer

        case("sd_chl")
            !km         |channel length
            sd_ch%chl = floatBuffer

        case("sd_chn")
            !           |channel Manning's n
            sd_ch%chn = floatBuffer

        case("sd_cov")
            !0-1        |channel cover factor
            sd_ch%cov = floatBuffer

        case("sd_cherod")
            !           |channel erodibility
            sd_ch%cherod = floatBuffer

        case("sd_shear_bnk")
            !0-1        |bank shear coefficient - fraction of bottom shear
            sd_ch%shear_bnk = floatBuffer

        case("sd_hc_erod")
            !           |headcut erodibility
            sd_ch%hc_erod = floatBuffer

        case("sd_hc_co")
            !m/m        |proportionality coefficient for head cut
            sd_ch%hc_co = floatBuffer

        case("sd_hc_len")
            !m          |length of head cut
            sd_ch%hc_len = floatBuffer

        case("sd_hc_hgt")
            !m          |headcut height
            sd_ch%hc_hgt = floatBuffer

        case("sd_stor")
            !m3         |water stored in reach at end of the day
            sd_ch%stor = floatBuffer

        case("sd_kd")
            !           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD kd is not yet supported"


        case("sd_aq_mix")
            ! m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD aq mix is not yet supported"

!-----------Lite HRU Variables------------------------------------------------------------------------------------------
        case("lte_props")
            hlt%props = intBuffer

        case("lte_obj_no")
            hlt%obj_no = intBuffer

        case("lte_plant")
            !character(len=16) :: plant
            !              |plant type (as listed in plants.plt)
            print *, "lte_plant is not yet supported for transfer as it is a character value, use iplant instead."
            print *, "lte_plant will then be updated accordinly"

        case("lte_iplant")
            !              |plant number xwalked from hlt_db()%plant and plants.plt
            hlt%iplant = intBuffer

        case("lte_km2")
            !km^2          |drainage area
            hlt%km2 = floatBuffer

        case("lte_cn2")
            !              |condition II curve number (used in calibration)
            hlt%cn2 = floatBuffer

        case("lte_cn3_swf")
            !none          |soil water factor for cn3 (used in calibration)
            hlt%cn3_swf = floatBuffer

        case("lte_soildep")                                     !              |0 = fc; 1 = saturation (porosity)
            !mm            |soil profile depth
            hlt%soildep = floatBuffer

        case("lte_etco")
            !              |et coefficient - use with pet and aet (used in calibration)
            hlt%etco = floatBuffer

        case("lte_revapc")
            !m/m           |revap from aquifer (used in calibration)
            hlt%revapc = floatBuffer

        case("lte_perco")
            !              |soil percolation coefficient (used in calibration)
            hlt%perco = floatBuffer

        case("lte_tdrain")
            !hr            |design subsurface tile drain time (used in calibration)
            hlt%tdrain = floatBuffer

        case("lte_stress")
            !frac          |plant stress - pest, root restriction, soil quality, nutrient,
            hlt%stress = floatBuffer

        case("lte_uslefac")
            !              |USLE slope length factor
            hlt%uslefac = floatBuffer

        case("lte_wrt1")
            hlt%wrt1 = floatBuffer

        case("lte_wrt2")
            hlt%wrt2 = floatBuffer

        case("lte_smx")
            hlt%smx = floatBuffer

        case("lte_hk")
            hlt%hk = floatBuffer

        case("lte_yls")
            hlt%yls = floatBuffer

        case("lte_ylc")
            hlt%ylc = floatBuffer

        case("lte_awc")
            !mm/mm        |available water capacity of soil
            hlt%awc = floatBuffer

        case("lte_g")

            hlt%g = floatBuffer

        case("lte_hufh")
            hlt%hufh = floatBuffer

        case("lte_phu")
            hlt%phu = floatBuffer

        case("lte_por")
            hlt%por = floatBuffer

        case("lte_sc")
            hlt%sc = floatBuffer

        case("lte_sw")
            !mm/mm         |initial soil water storage
            hlt%sw = floatBuffer

        case("lte_gw")
            !mm            |initial shallow aquifer storage
            hlt%gw = floatBuffer

        case("lte_snow")
            !mm            |initial water content of snow
            hlt%snow = floatBuffer

        case("lte_gwflow")
            !mm            |initial groundwater flow
            hlt%gwflow = floatBuffer

        case("lte_dm")
            !t/ha          |plant biomass
            hlt%dm = floatBuffer

        case("lte_alai")
            !              |leaf area index
            hlt%alai = floatBuffer

        case("lte_yield")
            !t/ha          |plant yield
            hlt%yield = floatBuffer

        case("lte_npp")
            !t/ha          |net primary productivity
            hlt%npp = floatBuffer

        case("lte_lai_mx")
            !              |maximum leaf area index
            hlt%lai_mx = floatBuffer

        case("lte_gwdeep")
            !mm            |deep aquifer storage
            hlt%gwdeep = floatBuffer

        case("lte_aet")
            !mm            |sum of actual et during growing season (for hi water stress)
            hlt%aet = floatBuffer

        case("lte_pet")
            !mm            |sum of potential et during growing season (for hi water stress)
            hlt%pet = floatBuffer

        case("lte_start")
            hlt%start = intBuffer

        case("lte_end")
            hlt%end = intBuffer

!-------Channel Variables-----------------------------------------------------------------------------------------------
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            ch%algae = floatBuffer

        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            ch%flwin = floatBuffer

!-------HRU Variables---------------------------------------------------------------------------------------------------
        case("hru_obj_no")
            hru%obj_no = intBuffer

        case("hru_area_ha")
            hru%area_ha = floatBuffer

        case("hru_km")
            hru%km = floatBuffer

        case("hru_surf_stor")
            !points to res() for surface storage
            hru%surf_stor = intBuffer

        case("hru_land_use_mgt")
            hru%land_use_mgt = intBuffer

        !    character(len=16) :: land_use_mgt_c
        case("hru_land_use_mgt_c")
            hru%land_use_mgt = intBuffer

        case("hru_lum_group")
            hru%lum_group = intBuffer

        !    character(len=16) :: lum_group_c        !land use group for soft cal and output
        case("hru_lum_group_c")
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_lum_group instead"
            hru%lum_group = intBuffer

        !    character(len=16) :: region
        case("hru_region")
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%field "
            hru%dbs%field = intBuffer

        case("hru_plant_cov")
            hru%plant_cov = intBuffer

        case("hru_mgt_ops")
            hru%mgt_ops = intBuffer

        case("hru_tiledrain")
            hru%tiledrain = intBuffer

        case("hru_septic")
            hru%septic = intBuffer

        case("hru_fstrip")
            hru%fstrip = intBuffer

        case("hru_grassww")
            hru%grassww = intBuffer

        case("hru_bmpuser")
            hru%bmpuser = intBuffer

        case("hru_crop_reg")
            hru%crop_reg = intBuffer

        case("hru_cur_op")
            hru%cur_op = intBuffer

        !case("hru_strsa")
        !    print *, "CANNOT RETURN CHARACTER VALUE, SENDING... "
        !    hru%strsa = intBuffer

        case("hru_sno_mm")
            !mm H2O        |amount of water in snow on current day
            hru%sno_mm = floatBuffer

        case("hru_water_fr")
            hru%water_fr = floatBuffer

        case("hru_water_seep")
            hru%water_seep = floatBuffer

        case("hru_water_evap")
            floatBuffer = hru%water_evap

        case("hru_ich_flood")
            hru%ich_flood= intBuffer

        case("hru_luse%name")
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"
            do i= 1,sp_ob%hru
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do

        case("hru_luse%cn_lu")
            do i= 1,sp_ob%hru
                hru(i)%luse%cn_lu = intBuffer(i)
            end do

        case("hru_luse%cons_prac")
            do i= 1,sp_ob%hru
                hru(i)%luse%cons_prac = intBuffer(i)
            end do

        case("hru_luse%usle_p")
            do i= 1,sp_ob%hru
                hru(i)%luse%usle_p = intBuffer(i)
            end do

        case("hru_luse%urb_ro")
            do i= 1,sp_ob%hru
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_luse%urb_lu")
            do i= 1,sp_ob%hru
                hru(i)%luse%urb_lu = intBuffer(i)
            end do

        case("hru_luse%ovn")
            do i= 1,sp_ob%hru
                hru(i)%luse%ovn = intBuffer(i)
            end do

        !case("hru_dbs%name")
        !    do i= 1,sp_ob%hru
        !        print *, hru(i)%dbs%name
        !        hru%obj_no = intBuffer(i)
        !    end do

        !    print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbs%topo")
            do i= 1,sp_ob%hru
                hru(i)%dbs%topo = intBuffer(i)
            end do

        case("hru_dbs%hyd")
            do i= 1,sp_ob%hru
                hru(i)%dbs%hyd = intBuffer(i)
            end do

        case("hru_dbs%soil")
            do i= 1,sp_ob%hru
                hru(i)%dbs%soil = intBuffer(i)
            end do

        case("hru_dbs%land_use_mgt")
            do i= 1,sp_ob%hru
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do

        case("hru_dbs%soil_plant_init")
            do i= 1,sp_ob%hru
                hru(i)%dbs%soil_plant_init = intBuffer(i)
            end do

        case("hru_dbs%surf_stor")
            do i= 1,sp_ob%hru
                hru(i)%dbs%surf_stor = intBuffer(i)
            end do

        case("hru_dbs%snow")
            do i= 1,sp_ob%hru
                hru(i)%dbs%snow = intBuffer(i)
            end do

        case("hru_dbs%field")
            do i= 1,sp_ob%hru
                hru(i)%dbs%hyd = intBuffer(i)
            end do

        !case("hru_dbsc%name")
        !    do i= 1,sp_ob%hru

        !    end do

        !    hru%obj_no = intBuffer(i)
        !    print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbsc%topo")
            do i= 1,sp_ob%hru
                hru(i)%dbs%topo = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%topo"

        case("hru_dbsc%hyd")
            do i= 1,sp_ob%hru
                hru(i)%dbs%hyd = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%hyd"

        case("hru_dbsc%soil")
            do i= 1,sp_ob%hru
                hru(i)%dbs%soil = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil"

        case("hru_dbsc%land_use_mgt")
            do i= 1,sp_ob%hru
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_dbsc%soil_plant_init")
            do i= 1,sp_ob%hru
                hru(i)%dbs%soil_plant_init = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil_plant_init"

        case("hru_dbsc%surf_stor")
            do i= 1,sp_ob%hru
                hru(i)%dbs%surf_stor = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%surf_stor"

        case("hru_dbsc%snow")
            do i= 1,sp_ob%hru
                hru(i)%dbs%snow = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%snow"

        case("hru_dbsc%field")
            do i= 1,sp_ob%hru
                hru(i)%dbs%field = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%field"

        case("hru_lumv%usle_p")
            do i= 1,sp_ob%hru
                hru(i)%lumv%usle_p = intBuffer(i)
            end do

        case("hru_lumv%usle_ls")
            do i= 1,sp_ob%hru
                hru(i)%lumv%usle_ls = intBuffer(i)
            end do

        case("hru_lumv%usle_mult")
            do i= 1,sp_ob%hru
                hru(i)%lumv%usle_mult = intBuffer(i)
            end do

        case("hru_lumv%sdr_dep")
            do i= 1,sp_ob%hru
                hru(i)%lumv%sdr_dep = intBuffer(i)
            end do

        case("hru_lumv%ldrain")
            do i= 1,sp_ob%hru
                hru(i)%lumv%ldrain = intBuffer(i)
            end do

        case("hru_lumv%tile_ttime")
            do i= 1,sp_ob%hru
                hru(i)%lumv%tile_ttime = intBuffer(i)
            end do

        case("hru_lumv%vfsi")
            do i= 1,sp_ob%hru
                hru(i)%lumv%vfsi = intBuffer(i)
            end do

        case("hru_lumv%vfsratio")
            do i= 1,sp_ob%hru
                hru(i)%lumv%vfsratio = intBuffer(i)
            end do

        case("hru_lumv%vfscon")
            do i= 1,sp_ob%hru
                hru(i)%lumv%vfscon = intBuffer(i)
            end do

        case("hru_lumv%vfsch")
            do i= 1,sp_ob%hru
                hru(i)%lumv%vfsch = intBuffer(i)
            end do

        case("hru_lumv%ngrwat")
            do i= 1,sp_ob%hru
                hru(i)%lumv%ngrwat = intBuffer(i)
            end do

        case("hru_lumv%grwat_i")
            do i= 1,sp_ob%hru
                hru(i)%lumv%grwat_i = intBuffer(i)
            end do

        case("hru_lumv%grwat_n")
            do i= 1,sp_ob%hru
                hru(i)%lumv%grwat_n = intBuffer(i)
            end do

        case("hru_lumv%grwat_spcon")
            do i= 1,sp_ob%hru
                hru(i)%lumv%grwat_spcon = intBuffer(i)
            end do

        case("hru_lumv%grwat_d")
            do i= 1,sp_ob%hru
                hru(i)%lumv%grwat_d = intBuffer(i)
            end do

        case("hru_lumv%grwat_w")
            do i= 1,sp_ob%hru
                hru(i)%lumv%grwat_w = intBuffer(i)
            end do

        case("hru_lumv%grwat_l")
            do i= 1,sp_ob%hru
                hru(i)%lumv%grwat_l = intBuffer(i)
            end do

        case("hru_lumv%grwat_s")
            do i= 1,sp_ob%hru
                hru(i)%lumv%grwat_s = intBuffer(i)
            end do

        case("hru_lumv%bmp_flag")
            do i= 1,sp_ob%hru
                hru(i)%lumv%bmp_flag = intBuffer(i)
            end do

        case("hru_lumv%bmp_sed")
            do i= 1,sp_ob%hru
                hru(i)%lumv%bmp_sed = intBuffer(i)
            end do

        case("hru_lumv%bmp_pp")
            do i= 1,sp_ob%hru
                hru(i)%lumv%bmp_pp = intBuffer(i)
            end do

        case("hru_lumv%bmp_sp")
            do i= 1,sp_ob%hru
                hru(i)%lumv%bmp_sp = intBuffer(i)
            end do

        case("hru_lumv%bmp_pn")
            do i= 1,sp_ob%hru
                hru(i)%lumv%bmp_pn = intBuffer(i)
            end do

        case("hru_lumv%bmp_sn")
            do i= 1,sp_ob%hru
                hru(i)%lumv%bmp_sn = intBuffer(i)
            end do

        case("hru_lumv%bmp_bac")
            do i= 1,sp_ob%hru
                hru(i)%lumv%bmp_bac = intBuffer(i)
            end do

        case("luse")
            print *, "HRU luse registered"
            do i= 1, sp_ob%hru
                ihru = i
                ilu = intBuffer(i)

                !Changing landuse in databases, if necessary
                if((ilu/=hru(i)%land_use_mgt))then
                    hru(i)%dbs%land_use_mgt = ilu
                    hru(i)%dbsc%land_use_mgt = lum(ilu)%name !from line 72 and 73 in hru_read
                    lu_prev = hru(i)%land_use_mgt_c
                    hru(i)%land_use_mgt_c = lum(ilu)%name
                    hru(i)%land_use_mgt = ilu
                    isol = hru(i)%dbs%soil
                    call hru_lum_init (i)
                    call plant_init(1)
                    call cn2_init (i)
                !! reset composite usle value - in hydro_init
                    rock = Exp(-.053 * soil(i)%phys(1)%rock)
                    hru(i)%lumv%usle_mult = rock * soil(i)%ly(1)%usle_k *       &
                                 hru(i)%lumv%usle_p * hru(i)%lumv%usle_ls * 11.8
                !! write to new landuse change file
                    write (3612,*) i, time%yrc, time%mo, time%day_mo,  "    LU_CHANGE ",        &
                        lu_prev, hru(i)%land_use_mgt_c, "   0   0"

                    !! initialize pcom(ihru)dtbl in case the hru didn't have one before
                    isched = hru(ihru)%mgt_ops
                    if (sched(isched)%num_autos > 0) then
                        if(allocated(pcom(ihru)%dtbl)) deallocate(pcom(ihru)%dtbl)
                        allocate (pcom(ihru)%dtbl(sched(isched)%num_autos))

                        do iauto = 1, sched(isched)%num_autos
                            id = sched(isched)%num_db(iauto)
                            allocate (pcom(ihru)%dtbl(iauto)%num_actions(dtbl_lum(id)%acts))
                            pcom(ihru)%dtbl(iauto)%num_actions = 1
                            allocate (pcom(ihru)%dtbl(iauto)%days_act(dtbl_lum(id)%acts))
                            pcom(ihru)%dtbl(iauto)%days_act = 0
                        end do
                    end if

                    do ipl = 1, pcom(ihru)%npl
                        do ipl_bsn = 1, basin_plants
                            if (pcom(ihru)%pl(ipl) == plants_bsn(ipl_bsn)) then
                                pcom(ihru)%plcur(ipl)%bsn_num = ipl_bsn
                                exit
                            end if
                        end do
                    end do
                end if
            end do
!            j = d_tbl%act(iac)%ob_num
!            if (j == 0) j = ob_cur
!            ilu = d_tbl%act_typ(iac)
!            hru(j)%dbs%land_use_mgt = ilu
!            lu_prev = hru(j)%land_use_mgt_c
!            hru(j)%land_use_mgt_c = d_tbl%act(iac)%file_pointer
!            isol = hru(j)%dbs%soil


        CASE default
            print *, "Unused variable: ", variable_Name
            error stop
        end select

        call recibe()
    end subroutine tomar

    subroutine obtener (varNombre)
        character(*) :: varNombre
        character(len = 6) :: shapeBuffer
        integer, dimension(:), allocatable :: intBuffer
        real, dimension(:), allocatable :: floatBuffer
        shapeBuffer = ""

        if(allocated(intBuffer))deallocate(intBuffer)
        if(allocated(floatBuffer))deallocate(floatBuffer)

        select case (trim(varNombre))

!--------Calibration/Initialization-------------------------------------------------------------------------------------

!-----------Landuse Variables-------------------------------------------------------------------------------------------
        case ("total_ch_out_y%no3")
            allocate(intBuffer(0))
            allocate(floatBuffer(1))
            do i = 1, sp_ob%chandeg
                floatBuffer(1) = floatBuffer(1) + ch_out_y(i)%no3
            end do

        case ("total_ch_out_y%flo")
            allocate(intBuffer(0))
            allocate(floatBuffer(1))
            do i = 1, sp_ob%chandeg
                floatBuffer(1) = floatBuffer(1) + ch_out_y(i)%flo
            end do

        case ("bsn_crop_yld")
            allocate(intBuffer(0))
            allocate(floatBuffer(basin_plants))
            do i = 1, basin_plants
                floatBuffer(i) = bsn_crop_yld(i)%yield
                end do

!-----------Water flow, contaminants, (P o and ao then N, K)------------------------------------------------------------
            !ch(:)%
        CASE("t")
            allocate(intBuffer(1))
            allocate(floatBuffer(0))
            intBuffer = t

!-----------SD-Channel Variables----------------------------------------------------------------------------------------
        case("sd_props")
            allocate(intBuffer(size(sd_ch%props)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%props

        case("sd_obj_no")
            allocate(intBuffer(size(sd_ch%obj_no)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%obj_no

        case("sd_aqu_link")
            !aquifer the channel is linked to
            allocate(intBuffer(size(sd_ch%aqu_link)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%aqu_link

        case("sd_aqu_link_ch")
            !sequential channel number in the aquifer
            allocate(intBuffer(size(sd_ch%aqu_link_ch)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%aqu_link_ch
        case("sd_chw")
            !m          |channel width
            allocate(floatBuffer(size(sd_ch%chw)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chw

        case("sd_chd")
            !m          |channel depth
            allocate(floatBuffer(size(sd_ch%chd)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chd

        case("sd_chs")
            !m/m        |channel slope
            allocate(floatBuffer(size(sd_ch%chs)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chs

        case("sd_chl")
            !km         |channel length
            allocate(floatBuffer(size(sd_ch%chl)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chl

        case("sd_chn")
            !           |channel Manning's n
            allocate(floatBuffer(size(sd_ch%chn)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chn

        case("sd_cov")
            !0-1        |channel cover factor
            allocate(floatBuffer(size(sd_ch%cov)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%cov

        case("sd_cherod")
            !           |channel erodibility
            allocate(floatBuffer(size(sd_ch%cherod)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%cherod

        case("sd_shear_bnk")
            !0-1        |bank shear coefficient - fraction of bottom shear
            allocate(floatBuffer(size(sd_ch%shear_bnk)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%shear_bnk

        case("sd_hc_erod")
            !           |headcut erodibility
            allocate(floatBuffer(size(sd_ch%hc_erod)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_erod

        case("sd_hc_co")
            !m/m        |proportionality coefficient for head cut
            allocate(floatBuffer(size(sd_ch%hc_co)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_co

        case("sd_hc_len")
            !m          |length of head cut
            allocate(floatBuffer(size(sd_ch%hc_len)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_len

        case("sd_hc_hgt")
            !m          |headcut height
            allocate(floatBuffer(size(sd_ch%hc_hgt)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_hgt

        case("sd_stor")
            !m3         |water stored in reach at end of the day
            allocate(floatBuffer(size(sd_ch%stor)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%stor

        case("sd_kd")
            !           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD kd is not yet supported"


        case("sd_aq_mix")
            ! m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD aq mix is not yet supported"

!-----------Lite HRU Variables------------------------------------------------------------------------------------------
        case("lte_props")
            allocate(intBuffer(size(hlt%props)))
            allocate(floatBuffer(0))
            intBuffer = hlt%props

        case("lte_obj_no")
            allocate(intBuffer(size(hlt%obj_no)))
            allocate(floatBuffer(0))
            intBuffer = hlt%obj_no

        case("lte_plant")
            !character(len=16) :: plant         !   |plant type (as listed in plants.plt)
            print *, "lte_plant is not yet supported for transfer as it is a character value but we are looking into it"

        case("lte_iplant")
            !              |plant number xwalked from hlt_db()%plant and plants.plt
            allocate(intBuffer(size(hlt%iplant)))
            allocate(floatBuffer(0))
            intBuffer = hlt%iplant

        case("lte_km2")
            !km^2          |drainage area
            allocate(floatBuffer(size(hlt%km2)))
            allocate(intBuffer(0))
            floatBuffer = hlt%km2

        case("lte_cn2")
            !              |condition II curve number (used in calibration)
            allocate(floatBuffer(size(hlt%cn2)))
            allocate(intBuffer(0))
            floatBuffer = hlt%cn2

        case("lte_cn3_swf")
            !none          |soil water factor for cn3 (used in calibration)
            allocate(floatBuffer(size(hlt%cn3_swf)))
            allocate(intBuffer(0))
            floatBuffer = hlt%cn3_swf

        case("lte_soildep")                                     !              |0 = fc; 1 = saturation (porosity)
            !mm            |soil profile depth
            allocate(floatBuffer(size(hlt%soildep)))
            allocate(intBuffer(0))
            floatBuffer = hlt%soildep

        case("lte_etco")
            !              |et coefficient - use with pet and aet (used in calibration)
            allocate(floatBuffer(size(hlt%etco)))
            allocate(intBuffer(0))
            floatBuffer = hlt%etco

        case("lte_revapc")
            !m/m           |revap from aquifer (used in calibration)
            allocate(floatBuffer(size(hlt%revapc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%revapc

        case("lte_perco")
            !              |soil percolation coefficient (used in calibration)
            allocate(floatBuffer(size(hlt%perco)))
            allocate(intBuffer(0))
            floatBuffer = hlt%perco

        case("lte_tdrain")
            !hr            |design subsurface tile drain time (used in calibration)
            allocate(floatBuffer(size(hlt%tdrain)))
            allocate(intBuffer(0))
            floatBuffer = hlt%tdrain

        case("lte_stress")
            !frac          |plant stress - pest, root restriction, soil quality, nutrient,
            allocate(floatBuffer(size(hlt%stress)))
            allocate(intBuffer(0))
            floatBuffer = hlt%stress

        case("lte_uslefac")
            !              |USLE slope length factor
            allocate(floatBuffer(size(hlt%uslefac)))
            allocate(intBuffer(0))
            floatBuffer = hlt%uslefac

        case("lte_wrt1")
            allocate(floatBuffer(size(hlt%wrt1)))
            allocate(intBuffer(0))
            floatBuffer = hlt%wrt1

        case("lte_wrt2")
            allocate(floatBuffer(size(hlt%wrt2)))
            allocate(intBuffer(0))
            floatBuffer = hlt%wrt2

        case("lte_smx")
            allocate(floatBuffer(size(hlt%smx)))
            allocate(intBuffer(0))
            floatBuffer = hlt%smx

        case("lte_hk")
            allocate(floatBuffer(size(hlt%hk)))
            allocate(intBuffer(0))
            floatBuffer = hlt%hk

        case("lte_yls")
            allocate(floatBuffer(size(hlt%yls)))
            allocate(intBuffer(0))
            floatBuffer = hlt%yls

        case("lte_ylc")
            allocate(floatBuffer(size(hlt%ylc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%ylc

        case("lte_awc")
            !mm/mm        |available water capacity of soil
            allocate(floatBuffer(size(hlt%awc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%awc

        case("lte_g")
            allocate(floatBuffer(size(hlt%g)))
            allocate(intBuffer(0))
            floatBuffer = hlt%g

        case("lte_hufh")
            allocate(floatBuffer(size(hlt%hufh)))
            allocate(intBuffer(0))
            floatBuffer = hlt%hufh

        case("lte_phu")
            allocate(floatBuffer(size(hlt%phu)))
            allocate(intBuffer(0))
            floatBuffer = hlt%phu

        case("lte_por")
            allocate(floatBuffer(size(hlt%por)))
            allocate(intBuffer(0))
            floatBuffer = hlt%por

        case("lte_sc")
            allocate(floatBuffer(size(hlt%sc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%sc

        case("lte_sw")
            !mm/mm         |initial soil water storage
            allocate(floatBuffer(size(hlt%sw)))
            allocate(intBuffer(0))
            floatBuffer = hlt%sw

        case("lte_gw")
            !mm            |initial shallow aquifer storage
            allocate(floatBuffer(size(hlt%gw)))
            allocate(intBuffer(0))
            floatBuffer = hlt%gw

        case("lte_snow")
            !mm            |initial water content of snow
            allocate(floatBuffer(size(hlt%snow)))
            allocate(intBuffer(0))
            floatBuffer = hlt%snow

        case("lte_gwflow")
            !mm            |initial groundwater flow
            allocate(floatBuffer(size(hlt%gwflow)))
            allocate(intBuffer(0))
            floatBuffer = hlt%gwflow

        case("lte_dm")
            !t/ha          |plant biomass
            allocate(floatBuffer(size(hlt%dm)))
            allocate(intBuffer(0))
            floatBuffer = hlt%dm

        case("lte_alai")
            !              |leaf area index
            allocate(floatBuffer(size(hlt%alai)))
            allocate(intBuffer(0))
            floatBuffer = hlt%alai

        case("lte_yield")
            !t/ha          |plant yield
            allocate(floatBuffer(size(hlt%yield)))
            allocate(intBuffer(0))
            floatBuffer = hlt%yield

        case("lte_npp")
            !t/ha          |net primary productivity
            allocate(floatBuffer(size(hlt%npp)))
            allocate(intBuffer(0))
            floatBuffer = hlt%npp

        case("lte_lai_mx")
            !              |maximum leaf area index
            allocate(floatBuffer(size(hlt%lai_mx)))
            allocate(intBuffer(0))
            floatBuffer = hlt%lai_mx

        case("lte_gwdeep")
            !mm            |deep aquifer storage
            allocate(floatBuffer(size(hlt%gwdeep)))
            allocate(intBuffer(0))
            floatBuffer = hlt%gwdeep

        case("lte_aet")
            !mm            |sum of actual et during growing season (for hi water stress)
            allocate(floatBuffer(size(hlt%aet)))
            allocate(intBuffer(0))
            floatBuffer = hlt%aet

        case("lte_pet")
            !mm            |sum of potential et during growing season (for hi water stress)
            allocate(floatBuffer(size(hlt%pet)))
            allocate(intBuffer(0))
            floatBuffer = hlt%pet

        case("lte_start")
            allocate(intBuffer(size(hlt%start)))
            allocate(floatBuffer(0))
            intBuffer = hlt%start

        case("lte_end")
            allocate(intBuffer(size(hlt%end)))
            allocate(floatBuffer(0))
            intBuffer = hlt%end

!-------Channel Variables-----------------------------------------------------------------------------------------------
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            allocate(floatBuffer(size(ch%algae)))
            allocate(intBuffer(0))
            floatBuffer = ch%algae

        CASE("ammonian")        ! mg N/L        |ammonia concentration in reach
            allocate(floatBuffer(size(ch%ammonian)))
            allocate(intBuffer(0))
            floatBuffer = ch%ammonian

        CASE("bankst")          ! m^3 H2O       |bank storage
            allocate(floatBuffer(size(ch%bankst)))
            allocate(intBuffer(0))
            floatBuffer = ch%bankst

        CASE("li")              ! km            |initial length of main channel
            allocate(floatBuffer(size(ch%li)))
            allocate(intBuffer(0))
            floatBuffer = ch%li

        CASE("orgn")            !               |organic nitrogen contribution from channel erosion
            allocate(floatBuffer(size(ch%orgn)))
            allocate(intBuffer(0))
            floatBuffer = ch%orgn

        CASE("orgp")            !               |organic phosphorus contribution from channel erosion
            allocate(floatBuffer(size(ch%orgp)))
            allocate(intBuffer(0))
            floatBuffer = ch%orgp

        CASE("si")              !(m/n)          |slope of main channel
            allocate(floatBuffer(size(ch%si)))
            allocate(intBuffer(0))
            floatBuffer = ch%si

        CASE("wi")              !(m)            |width of main channel at top of bank
            allocate(floatBuffer(size(ch%wi)))
            allocate(intBuffer(0))
            floatBuffer = ch%wi

        CASE("di")              !(m)            |depth of main channel from top of bank to bottom
            allocate(floatBuffer(size(ch%di)))
            allocate(intBuffer(0))
            floatBuffer = ch%di

        CASE("chlora")          ! mg chl-a/L    |chlorophyll-a concentration in reach
            allocate(floatBuffer(size(ch%chlora)))
            allocate(intBuffer(0))
            floatBuffer = ch%chlora

        CASE("pst_conc")        ! mg/(m**3)     |initial pesticide concentration in reach
            allocate(floatBuffer(size(ch%pst_conc)))
            allocate(intBuffer(0))
            floatBuffer = ch%pst_conc

        CASE("dep_chan")        ! m             |average daily water depth in channel
            allocate(floatBuffer(size(ch%dep_chan)))
            allocate(intBuffer(0))
            floatBuffer = ch%dep_chan

        CASE("disolvp")         ! mg P/L        |dissolved P concentration in reach
            allocate(floatBuffer(size(ch%disolvp)))
            allocate(intBuffer(0))
            floatBuffer = ch%disolvp

        CASE("drift")           ! kg            |amount of pesticide drifting onto main channel in subbasin
            allocate(floatBuffer(size(ch%drift)))
            allocate(intBuffer(0))
            floatBuffer = ch%drift

        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            allocate(floatBuffer(size(ch%flwin)))
            allocate(intBuffer(0))
            floatBuffer = ch%flwin

        CASE("flwout")          ! m^3 H2O       |flow out of reach on previous day
            allocate(floatBuffer(size(ch%flwout)))
            allocate(intBuffer(0))
            floatBuffer = ch%flwout

        CASE("nitraten")        ! mg N/L        |nitrate concentration in reach
            allocate(floatBuffer(size(ch%nitraten)))
            allocate(intBuffer(0))
            floatBuffer = ch%nitraten

        CASE("nitriten")        ! mg N/L        |nitrite concentration in reach
            allocate(floatBuffer(size(ch%nitriten)))
            allocate(intBuffer(0))
            floatBuffer = ch%nitriten

        CASE("organicn")        ! mg N/L        |organic nitrogen concentration in reach
            allocate(floatBuffer(size(ch%organicn)))
            allocate(intBuffer(0))
            floatBuffer = ch%organicn

        CASE("organicp")        ! mg P/L        |organic phosphorus concentration in reach
            allocate(floatBuffer(size(ch%organicp)))
            allocate(intBuffer(0))
            floatBuffer = ch%organicp

        CASE("rch_bactlp")      ! # cfu/100ml   |less persistent bacteria stored in reach
            allocate(floatBuffer(size(ch%rch_bactlp)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_bactlp

        CASE("rch_bactp")       ! # cfu/100ml   |persistent bacteria stored in reach
            allocate(floatBuffer(size(ch%rch_bactp)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_bactp

        CASE("rch_cbod")        ! mg O2/L       |carbonaceous biochemical oxygen demand in reach
            allocate(floatBuffer(size(ch%rch_cbod)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_cbod

        CASE("rch_dox")         ! mg O2/L       |dissolved oxygen concentration in reach
            allocate(floatBuffer(size(ch%rch_dox)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_dox

        CASE("rchstor")         ! m^3 H2O       |water stored in reach
            allocate(floatBuffer(size(ch%rchstor)))
            allocate(intBuffer(0))
            floatBuffer = ch%rchstor

        CASE("sedst")           ! metric tons   |amount of sediment stored in reach
            allocate(floatBuffer(size(ch%sedst)))
            allocate(intBuffer(0))
            floatBuffer = ch%sedst

        CASE("vel_chan")        ! m/s           |average flow velocity in channel
            allocate(floatBuffer(size(ch%vel_chan)))
            allocate(intBuffer(0))
            floatBuffer = ch%vel_chan

        CASE("bed_san")
            allocate(floatBuffer(size(ch%bed_san)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_san

        CASE("bed_sil")
            allocate(floatBuffer(size(ch%bed_sil)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_sil

        CASE("bed_cla")
            allocate(floatBuffer(size(ch%bed_cla)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_cla

        CASE("bed_gra")
            allocate(floatBuffer(size(ch%bed_gra)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_gra

        CASE("bnk_san")
            allocate(floatBuffer(size(ch%bnk_san)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_san

        CASE("bnk_sil")
            allocate(floatBuffer(size(ch%bnk_sil)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_sil

        CASE("bnk_cla")
            allocate(floatBuffer(size(ch%bnk_cla)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_cla

        CASE("bnk_gra")
            allocate(floatBuffer(size(ch%bnk_gra)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_gra

        CASE("depfp")
            allocate(floatBuffer(size(ch%depfp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depfp

        CASE("depprfp")
            allocate(floatBuffer(size(ch%depprfp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depprfp

        CASE("depsilfp")
            allocate(floatBuffer(size(ch%depsilfp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsilfp

        CASE("depclafp")
            allocate(floatBuffer(size(ch%depclafp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depclafp

        CASE("depch")
            allocate(floatBuffer(size(ch%depch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depch

        CASE("depprch")
            allocate(floatBuffer(size(ch%depprch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depprch

        CASE("depsanch")
            allocate(floatBuffer(size(ch%depsanch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsanch

        CASE("depsilch")
            allocate(floatBuffer(size(ch%depsilch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsilch

        CASE("depclach")
            allocate(floatBuffer(size(ch%depclach)))
            allocate(intBuffer(0))
            floatBuffer = ch%depclach

        CASE("depsagch")
            allocate(floatBuffer(size(ch%depsagch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsagch

        CASE("deplagch")
            allocate(floatBuffer(size(ch%deplagch)))
            allocate(intBuffer(0))
            floatBuffer = ch%deplagch

        CASE("depgrach")
            allocate(floatBuffer(size(ch%depgrach)))
            allocate(intBuffer(0))
            floatBuffer = ch%depgrach

        CASE("sanst")
            allocate(floatBuffer(size(ch%sanst)))
            allocate(intBuffer(0))
            floatBuffer = ch%sanst

        CASE("silst")
            allocate(floatBuffer(size(ch%silst)))
            allocate(intBuffer(0))
            floatBuffer = ch%silst

        CASE("clast")
            allocate(floatBuffer(size(ch%clast)))
            allocate(intBuffer(0))
            floatBuffer = ch%clast

        CASE("sagst")
            allocate(floatBuffer(size(ch%sagst)))
            allocate(intBuffer(0))
            floatBuffer = ch%sagst

        CASE("lagst")
            allocate(floatBuffer(size(ch%lagst)))
            allocate(intBuffer(0))
            floatBuffer = ch%lagst

        CASE("grast")
            allocate(floatBuffer(size(ch%grast)))
            allocate(intBuffer(0))
            floatBuffer = ch%grast

        CASE("wattemp")
            allocate(floatBuffer(size(ch%wattemp)))
            allocate(intBuffer(0))
            floatBuffer = ch%wattemp

        CASE("bactp")
            allocate(floatBuffer(size(ch%bactp)))
            allocate(intBuffer(0))
            floatBuffer = ch%bactp

        CASE("chfloodvol")
            allocate(floatBuffer(size(ch%chfloodvol)))
            allocate(intBuffer(0))
            floatBuffer = ch%chfloodvol

        CASE("bactlp")
            allocate(floatBuffer(size(ch%bactlp)))
            allocate(intBuffer(0))
            floatBuffer = ch%bactlp

!-------HRU Variables---------------------------------------------------------------------------------------------------
        case("hru_obj_no")
            allocate(intBuffer(size(hru%obj_no)))
            allocate(floatBuffer(0))
            intBuffer = hru%obj_no

        case("hru_area_ha")
            allocate(floatBuffer(size(hru%area_ha)))
            allocate(intBuffer(0))
            floatBuffer = hru%area_ha

        case("hru_km")
            allocate(floatBuffer(size(hru%km)))
            allocate(intBuffer(0))
            floatBuffer = hru%km

        case("hru_surf_stor")
            !points to res() for surface storage
            allocate(intBuffer(size(hru%surf_stor)))
            allocate(floatBuffer(0))
            intBuffer = hru%surf_stor

        case("hru_land_use_mgt")
            allocate(intBuffer(size(hru%land_use_mgt)))
            allocate(floatBuffer(0))
            intBuffer = hru%land_use_mgt

        !    character(len=16) :: land_use_mgt_c
        case("hru_land_use_mgt_c")
            print *, "CANNOT SEND CHARACTER DATA, SENDING hru%land_use_mgt instead"
            allocate(intBuffer(size(hru%land_use_mgt)))
            allocate(floatBuffer(0))
            intBuffer = hru%land_use_mgt

        case("hru_lum_group")
            allocate(intBuffer(size(hru%lum_group)))
            allocate(floatBuffer(0))
            intBuffer = hru%lum_group

        !    character(len=16) :: lum_group_c        !land use group for soft cal and output
        case("hru_lum_group_c")
            print *, "CANNOT SEND CHARACTER DATA, SENDING hru%lum_group instead"
            allocate(intBuffer(size(hru%lum_group)))
            allocate(floatBuffer(0))
            intBuffer = hru%lum_group

        !    character(len=16) :: region
        case("hru_region")
            print *, "CANNOT SEND CHARACTER DATA, SENDING 0 instead"
            allocate(intBuffer(1))
            allocate(floatBuffer(0))
            intBuffer = 0


        case("hru_plant_cov")
            allocate(intBuffer(size(hru%plant_cov)))
            allocate(floatBuffer(0))
            intBuffer = hru%plant_cov

        case("hru_mgt_ops")
            allocate(intBuffer(size(hru%mgt_ops)))
            allocate(floatBuffer(0))
            intBuffer = hru%mgt_ops

        case("hru_tiledrain")
            allocate(intBuffer(size(hru%tiledrain)))
            allocate(floatBuffer(0))
            intBuffer = hru%tiledrain

        case("hru_septic")
            allocate(intBuffer(size(hru%septic)))
            allocate(floatBuffer(0))
            intBuffer = hru%septic

        case("hru_fstrip")
            allocate(intBuffer(size(hru%fstrip)))
            allocate(floatBuffer(0))
            intBuffer = hru%fstrip

        case("hru_grassww")
            allocate(intBuffer(size(hru%grassww)))
            allocate(floatBuffer(0))
            intBuffer = hru%grassww

        case("hru_bmpuser")
            allocate(intBuffer(size(hru%bmpuser)))
            allocate(floatBuffer(0))
            intBuffer = hru%bmpuser

        case("hru_crop_reg")
            allocate(intBuffer(size(hru%crop_reg)))
            allocate(floatBuffer(0))
            intBuffer = hru%crop_reg

        case("hru_cur_op")
            allocate(intBuffer(size(hru%cur_op)))
            allocate(floatBuffer(0))
            intBuffer = hru%cur_op

        case("hru_strsa")
            allocate(floatBuffer(size(hru%strsa)))
            allocate(intBuffer(0))
            floatBuffer = hru%strsa

        case("hru_sno_mm")
            !mm H2O        |amount of water in snow on current day
            allocate(floatBuffer(size(hru%sno_mm)))
            allocate(intBuffer(0))
            floatBuffer = hru%sno_mm

        case("hru_water_fr")
            allocate(floatBuffer(size(hru%water_fr)))
            allocate(intBuffer(0))
            floatBuffer = hru%water_fr

        case("hru_water_seep")
            allocate(floatBuffer(size(hru%water_seep)))
            allocate(intBuffer(0))
            floatBuffer = hru%water_seep

        case("hru_water_evap")
            allocate(floatBuffer(size(hru%water_evap)))
            allocate(intBuffer(0))
            floatBuffer = hru%water_evap

        case("hru_ich_flood")
            allocate(intBuffer(size(hru%ich_flood)))
            allocate(floatBuffer(0))
            intBuffer = hru%ich_flood

        case("hru_luse%name")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do

        case("hru_luse%cn_lu")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%luse%cn_lu
            end do

        case("hru_luse%cons_prac")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%luse%cons_prac
            end do

        case("hru_luse%usle_p")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%luse%usle_p
            end do

        case("hru_luse%urb_ro")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_luse%urb_lu")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                print *, hru(i)%luse%urb_lu
                intBuffer(i) = hru(i)%luse%urb_lu
            end do

        case("hru_luse%ovn")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%luse%ovn
            end do

        case("hru_dbs%name")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            intBuffer = hru%obj_no
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbs%topo")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%topo
            end do

        case("hru_dbs%hyd")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%hyd
            end do

        case("hru_dbs%soil")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) =hru(i)%dbs%soil
            end do

        case("hru_dbs%land_use_mgt")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do

        case("hru_dbs%soil_plant_in")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%soil_plant_init
            end do

        case("hru_dbs%surf_stor")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) =  hru(i)%dbs%surf_stor
            end do

        case("hru_dbs%snow")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) =hru(i)%dbs%snow
            end do

        case("hru_dbs%field")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%hyd
            end do

        case("hru_dbsc%name")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            intBuffer = hru%obj_no
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbsc%topo")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%topo
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%topo"

        case("hru_dbsc%hyd")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%hyd
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%hyd"

        case("hru_dbsc%soil")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%soil
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil"

        case("hru_dbsc%land_use_mgt")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_dbsc%soil_plant_i")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%soil_plant_init
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil_plant_init"

        case("hru_dbsc%surf_stor")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%surf_stor
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%surf_stor"

        case("hru_dbsc%snow")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%snow
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%snow"

        case("hru_dbsc%field")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%field
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%field"

        case("hru_lumv%usle_p")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%usle_p
            end do

        case("hru_lumv%usle_lhru_luses")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) =hru(i)%lumv%usle_ls
            end do

        case("hru_lumv%usle_mult")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%usle_mult
            end do

        case("hru_lumv%sdr_dep")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%sdr_dep
            end do

        case("hru_lumv%ldrain")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%lumv%ldrain
            end do

        case("hru_lumv%tile_ttime")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%tile_ttime
            end do

        case("hru_lumv%vfsi")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%vfsi
            end do

        case("hru_lumv%vfsratio")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%vfsratio
            end do

        case("hru_lumv%vfscon")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%vfscon
            end do

        case("hru_lumv%vfsch")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%vfsch
            end do

        case("hru_lumv%ngrwat")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%lumv%ngrwat
            end do

        case("hru_lumv%grwat_i")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%grwat_i
            end do

        case("hru_lumv%grwat_n")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%grwat_n
            end do

        case("hru_lumv%grwat_spcon")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%grwat_spcon
            end do

        case("hru_lumv%grwat_d")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%grwat_d
            end do

        case("hru_lumv%grwat_w")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%grwat_w
            end do

        case("hru_lumv%grwat_l")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%grwat_l
            end do

        case("hru_lumv%grwat_s")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%grwat_s
            end do

        case("hru_lumv%bmp_flag")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%bmp_flag
            end do

        case("hru_lumv%bmp_sed")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%bmp_sed
            end do

        case("hru_lumv%bmp_pp")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%bmp_pp
            end do

        case("hru_lumv%bmp_sp")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%bmp_sp
            end do

        case("hru_lumv%bmp_pn")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%bmp_pn
            end do

        case("hru_lumv%bmp_sn")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) =hru(i)%lumv%bmp_sn
            end do

        case("hru_lumv%bmp_bac")
            allocate(floatBuffer(sp_ob%hru))
            allocate(intBuffer(0))
            do i= 1,sp_ob%hru
                floatBuffer(i) = hru(i)%lumv%bmp_bac
            end do

        !type (landuse) :: luse
        case("luse")
            allocate(intBuffer(sp_ob%hru))
            allocate(floatBuffer(0))
            do i= 1,sp_ob%hru
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do

        !Other possible variables that could be added to this list for hru-objects:
        !type (topography) :: topo
        !type (field) :: field
        !type (hydrology) :: hyd
        !type (land_use_mgt_variables) :: lumv
        !type (subsurface_drainage_parameters) :: sdr
        !type (snow_parameters) :: sno
        !type (hru_databases) :: dbs             !database pointers
        !type (hru_databases_char) :: dbsc       !database pointers
        !type (hru_parms_db) :: parms            !calibration parameters

        CASE default
            print *, "Unknown variable: ", varNombre
            error stop
        end select

        if(.not.(SIZE(intBuffer)==0))then
            if(.not.allocated(floatBuffer)) allocate(floatBuffer(0))
        else
            if(.not.allocated(intBuffer)) allocate(intBuffer(0))
        end if

        if(shapeBuffer == "")then
            shapeBuffer = "-1" // char(0)
        else
            shapeBuffer = shapeBuffer // char(0)
        end if

        !-------------------This is temporary code----------------------------------------------------------------------
        if(size(intBuffer) == 0)then
            deallocate(intBuffer)
            allocate(intBuffer(size(floatBuffer)))
            do i = 1,size(floatBuffer)
                intBuffer(i) = ceiling(floatBuffer(i))
            end do
            deallocate(floatBuffer)
            allocate(floatBuffer(0))
        end if
        !-------------------This is temporary code----------------------------------------------------------------------

        call sendr(cliente_obj, intBuffer, floatBuffer, shapeBuffer, size(intBuffer), size(floatBuffer))

        call recibe()

    end subroutine obtener

end module tinamit_module