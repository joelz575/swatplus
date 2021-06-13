module tinamit_module

    use landuse_data_module, ONLY : lum
    use hru_module, ONLY : hru, hru_db
    use hru_lte_module, ONLY : hlt
    use channel_module, ONLY : ch
    use sd_channel_module, ONLY : sd_ch
    use hydrograph_module, ONLY : sp_ob1, ob

    save
    integer :: MAX_BUFFER_LEN = 20000
    integer cliente_obj
    logical dynamic
    integer :: dias = 1
    integer :: t = 0
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

        print *, "About to Recieve..."
        tmn_shape = 1
        print *, "cliente_obj: ", cliente_obj
        print *, "command: ", command
        print *, "var: ", var
        print *, "tipo_contents: ", tipo_contents
        print *, "nPasos: ", nPasos
        print *, "shape: ", shape

        call receive (cliente_obj, command, var, tipo_contents, nPasos, shape) !charBuffer

        print *, "Cliente Obj: ", cliente_obj
        print *, "Command: ", command
        if (command == "cambiar")then
            print *, "Variable Name: ", var
            print *, "Content Data Type: ", tipo_contents
            print*, "Shape of array: ", shape

            if(tipo_contents=="float")then
                print *, "about to allocate realBuffer"
                allocate(realBuffer(shape))
                call recvfloat (cliente_obj, realBuffer, shape)

            elseif(tipo_contents=="int".or.tipo_contents=="int64")then
                print *, "about to allocate intBuffer"
                allocate(intBuffer(shape))
                call recvint (cliente_obj, intBuffer, shape)

            end if

        elseif (command == "leer")then

            print *, "Variable Name: ", var

        end if

        print *, "intBuffer contents: ", intBuffer
        print *, "realBuffer contents: ", realBuffer

        call evaluar(command, var, shape, intBuffer, realBuffer, nPasos)

    end subroutine recibe

    subroutine evaluar (orden, var, shape, intBuffer, realBuffer, nPasos)

        character(len = :), allocatable :: senderBuffer
        character(*) :: var, orden
        integer :: nPasos, t_final
        integer :: shape
        integer, dimension(shape) :: intBuffer
        real, dimension(shape) :: realBuffer

        print *, "Command: ", orden
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
        integer :: index, i
        integer :: shape
        integer, dimension(shape) :: intBuffer
        real, dimension(shape) :: floatBuffer
        integer :: f = 1

        if (size(hru) > 1) then
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
            !if(.not.allocated(sd_ch%chd)) allocate(sd_ch%chd(shape))
            sd_ch%chd = floatBuffer

        case("sd_chs")
            !m/m        |channel slope
            !if(.not.allocated(sd_ch%chs)) allocate(sd_ch%chs(shape))
            sd_ch%chs = floatBuffer

        case("sd_chl")
            !km         |channel length
            !if(.not.allocated(sd_ch%chl)) allocate(sd_ch%chl(shape))
            sd_ch%chl = floatBuffer

        case("sd_chn")
            !           |channel Manning's n
            !if(.not.allocated(sd_ch%chn)) allocate(sd_ch%chn(shape))
            sd_ch%chn = floatBuffer

        case("sd_cov")
            !0-1        |channel cover factor
            !if(.not.allocated(sd_ch%cov)) allocate(sd_ch%cov(shape))
            sd_ch%cov = floatBuffer

        case("sd_cherod")
            !           |channel erodibility
            !if(.not.allocated(sd_ch%cherod)) allocate(sd_ch%cherod(shape))
            sd_ch%cherod = floatBuffer

        case("sd_shear_bnk")
            !0-1        |bank shear coefficient - fraction of bottom shear
            !if(.not.allocated(sd_ch%shear_bnk)) allocate(sd_ch%shear_bnk(shape))
            sd_ch%shear_bnk = floatBuffer

        case("sd_hc_erod")
            !           |headcut erodibility
            !if(.not.allocated(sd_ch%hc_erod)) allocate(sd_ch%hc_erod(shape))
            sd_ch%hc_erod = floatBuffer

        case("sd_hc_co")
            !m/m        |proportionality coefficient for head cut
            !if(.not.allocated(sd_ch%hc_co)) allocate(sd_ch%hc_co(shape))
            sd_ch%hc_co = floatBuffer

        case("sd_hc_len")
            !m          |length of head cut
            !if(.not.allocated(sd_ch%hc_len)) allocate(sd_ch%hc_len(shape))
            sd_ch%hc_len = floatBuffer

        case("sd_hc_hgt")
            !m          |headcut height
            !if(.not.allocated(sd_ch%hc_hgt)) allocate(sd_ch%hc_hgt(shape))
            sd_ch%hc_hgt = floatBuffer

        case("sd_stor")
            !m3         |water stored in reach at end of the day
            !if(.not.allocated(sd_ch%stor)) allocate(sd_ch%stor(shape))
            sd_ch%stor = floatBuffer

        case("sd_kd")
            !           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD kd is not yet supported"


        case("sd_aq_mix")
            ! m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD aq mix is not yet supported"

!-----------Lite HRU Variables------------------------------------------------------------------------------------------
        case("lte_props")
            !if(.not.allocated(hlt%props)) allocate(hlt%props(shape))
            hlt%props = intBuffer

        case("lte_obj_no")
            !if(.not.allocated(hlt%obj_no)) allocate(hlt%obj_no(shape))
            hlt%obj_no = intBuffer

        case("lte_plant")
            !character(len=16) :: plant
            !              |plant type (as listed in plants.plt)
            print *, "lte_plant is not yet supported for transfer as it is a character value, use iplant instead."
            print *, "lte_plant will then be updated accordinly"

        case("lte_iplant")
            !              |plant number xwalked from hlt_db()%plant and plants.plt
            !if(.not.allocated(hlt%iplant)) allocate(hlt%iplant(shape))
            hlt%iplant = intBuffer

        case("lte_km2")
            !km^2          |drainage area
            !if(.not.allocated(hlt%km2)) allocate(hlt%km2(shape))
            hlt%km2 = floatBuffer

        case("lte_cn2")
            !              |condition II curve number (used in calibration)
            !if(.not.allocated(hlt%cn2)) allocate(hlt%cn2(shape))
            hlt%cn2 = floatBuffer

        case("lte_cn3_swf")
            !none          |soil water factor for cn3 (used in calibration)
            !if(.not.allocated(hlt%cn3_swf)) allocate(hlt%cn3_swf(shape))
            hlt%cn3_swf = floatBuffer

        case("lte_soildep")                                     !              |0 = fc; 1 = saturation (porosity)
            !mm            |soil profile depth
            !if(.not.allocated(hlt%soildep)) allocate(hlt%soildep(shape))
            hlt%soildep = floatBuffer

        case("lte_etco")
            !              |et coefficient - use with pet and aet (used in calibration)
            !if(.not.allocated(hlt%etco)) allocate(hlt%etco(shape))
            hlt%etco = floatBuffer

        case("lte_revapc")
            !m/m           |revap from aquifer (used in calibration)
            !if(.not.allocated(hlt%revapc)) allocate(hlt%revapc(shape))
            hlt%revapc = floatBuffer

        case("lte_perco")
            !              |soil percolation coefficient (used in calibration)
            !if(.not.allocated(hlt%perco)) allocate(hlt%perco(shape))
            hlt%perco = floatBuffer

        case("lte_tdrain")
            !hr            |design subsurface tile drain time (used in calibration)
            !if(.not.allocated(hlt%tdrain)) allocate(hlt%tdrain(shape))
            hlt%tdrain = floatBuffer

        case("lte_stress")
            !frac          |plant stress - pest, root restriction, soil quality, nutrient,
            !if(.not.allocated(hlt%stress)) allocate(hlt%stress(shape))
            hlt%stress = floatBuffer

        case("lte_uslefac")
            !              |USLE slope length factor
            !if(.not.allocated(hlt%uslefac)) allocate(hlt%uslefac(shape))
            hlt%uslefac = floatBuffer

        case("lte_wrt1")
            !if(.not.allocated(hlt%wrt1)) allocate(hlt%wrt1(shape))
            hlt%wrt1 = floatBuffer

        case("lte_wrt2")
            !if(.not.allocated(hlt%wrt2)) allocate(hlt%wrt2(shape))
            hlt%wrt2 = floatBuffer

        case("lte_smx")
            !if(.not.allocated(hlt%smx)) allocate(hlt%smx(shape))
            hlt%smx = floatBuffer

        case("lte_hk")
            !if(.not.allocated(hlt%hk)) allocate(hlt%hk(shape))
            hlt%hk = floatBuffer

        case("lte_yls")
            !if(.not.allocated(hlt%yls)) allocate(hlt%yls(shape))
            hlt%yls = floatBuffer

        case("lte_ylc")
            !if(.not.allocated(hlt%ylc)) allocate(hlt%ylc(shape))
            hlt%ylc = floatBuffer

        case("lte_awc")
            !mm/mm        |available water capacity of soil
            !if(.not.allocated(hlt%awc)) allocate(hlt%awc(shape))
            hlt%awc = floatBuffer

        case("lte_g")
            !if(.not.allocated(hlt%g)) allocate(hlt%g(shape))
            hlt%g = floatBuffer

        case("lte_hufh")
            !if(.not.allocated(hlt%hufh)) allocate(hlt%hufh(shape))
            hlt%hufh = floatBuffer

        case("lte_phu")
            !if(.not.allocated(hlt%phu)) allocate(hlt%phu(shape))
            hlt%phu = floatBuffer

        case("lte_por")
            !if(.not.allocated(hlt%por)) allocate(hlt%por(shape))
            hlt%por = floatBuffer

        case("lte_sc")
            !if(.not.allocated(hlt%sc)) allocate(hlt%sc(shape))
            hlt%sc = floatBuffer

        case("lte_sw")
            !mm/mm         |initial soil water storage
            !if(.not.allocated(hlt%sw)) allocate(hlt%sw(shape))
            hlt%sw = floatBuffer

        case("lte_gw")
            !mm            |initial shallow aquifer storage
            !if(.not.allocated(hlt%gw)) allocate(hlt%gw(shape))
            hlt%gw = floatBuffer

        case("lte_snow")
            !mm            |initial water content of snow
            !if(.not.allocated(hlt%snow)) allocate(hlt%snow(shape))
            hlt%snow = floatBuffer

        case("lte_gwflow")
            !mm            |initial groundwater flow
            !if(.not.allocated(hlt%gwflow)) allocate(hlt%gwflow(shape))
            hlt%gwflow = floatBuffer

        case("lte_dm")
            !t/ha          |plant biomass
            !if(.not.allocated(hlt%dm)) allocate(hlt%dm(shape))
            hlt%dm = floatBuffer

        case("lte_alai")
            !              |leaf area index
            !if(.not.allocated(hlt%alai)) allocate(hlt%alai(shape))
            hlt%alai = floatBuffer

        case("lte_yield")
            !t/ha          |plant yield
            !if(.not.allocated(hlt%yield)) allocate(hlt%yield(shape))
            hlt%yield = floatBuffer

        case("lte_npp")
            !t/ha          |net primary productivity
            !if(.not.allocated(hlt%npp)) allocate(hlt%npp(shape))
            hlt%npp = floatBuffer

        case("lte_lai_mx")
            !              |maximum leaf area index
            !if(.not.allocated(hlt%lai_mx)) allocate(hlt%lai_mx(shape))
            hlt%lai_mx = floatBuffer

        case("lte_gwdeep")
            !mm            |deep aquifer storage
            !if(.not.allocated(hlt%gwdeep)) allocate(hlt%gwdeep(shape))
            hlt%gwdeep = floatBuffer

        case("lte_aet")
            !mm            |sum of actual et during growing season (for hi water stress)
            !if(.not.allocated(hlt%aet)) allocate(hlt%aet(shape))
            hlt%aet = floatBuffer

        case("lte_pet")
            !mm            |sum of potential et during growing season (for hi water stress)
            !if(.not.allocated(hlt%pet)) allocate(hlt%pet(shape))
            hlt%pet = floatBuffer

        case("lte_start")
            !if(.not.allocated(hlt%start)) allocate(hlt%start(shape))
            hlt%start = intBuffer

        case("lte_end")
            !if(.not.allocated(hlt%end)) allocate(hlt%end(shape))
            hlt%end = intBuffer

!-------Channel Variables-----------------------------------------------------------------------------------------------
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            print *, "CH algae before: ", ch%algae
            ch%algae = floatBuffer
            print *, "CH algae after: ", ch%algae

        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            print *, "CH algae before: ", ch%flwin
            ch%flwin = floatBuffer
            print *, "CH algae after: ", ch%flwin
!-------HRU Variables---------------------------------------------------------------------------------------------------
            case("hru_obj_no")
            print *, "HRU obj_no: ", hru%obj_no
            hru%obj_no = intBuffer

        case("hru_area_ha")
            print *, "HRU area_ha: ", hru%area_ha
             hru%area_ha = floatBuffer

        case("hru_km")
            print *, "HRU km: ", hru%km
             hru%km = floatBuffer

        case("hru_surf_stor")
            !points to res() for surface storage
            print *, "HRU surf_stor: ", hru%surf_stor
            hru%surf_stor = intBuffer

        case("hru_land_use_mgt")
            print *, "HRU land_use_mgt: ", hru%land_use_mgt
            hru%land_use_mgt = intBuffer

        !    character(len=16) :: land_use_mgt_c
        case("hru_land_use_mgt_c")
            print *, "HRU land_use_mgt_c: ", hru%land_use_mgt_c
            hru%land_use_mgt = intBuffer

        case("hru_lum_group")
            print *, "HRU lum_group: ", hru%lum_group
            hru%lum_group = intBuffer

        !    character(len=16) :: lum_group_c        !land use group for soft cal and output
        !case("hru_lum_group_c")
        !    print *, "HRU lum_group_c", hru%lum_group_c
        !    print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_lum_group instead"
        !    hru%lum_group = intBuffer

        !    character(len=16) :: region
        case("hru_region")
            print *, "HRU region", hru%region
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%field "
            hru%dbs%field = intBuffer

        case("hru_plant_cov")
            print *, "HRU plant_cov: ", hru%plant_cov
            hru%plant_cov = intBuffer

        case("hru_mgt_ops")
            print *, "HRU mgt_ops: ", hru%mgt_ops
            hru%mgt_ops = intBuffer

        case("hru_tiledrain")
            print *, "HRU tiledrain: ", hru%tiledrain
            hru%tiledrain = intBuffer

        case("hru_septic")
            print *, "HRU septic: ", hru%septic
            hru%septic = intBuffer

        case("hru_fstrip")
            print *, "HRU fstrip: ", hru%fstrip
            hru%fstrip = intBuffer

        case("hru_grassww")
            print *, "HRU grassww: ", hru%grassww
            hru%grassww = intBuffer

        case("hru_bmpuser")
            print *, "HRU bmpuser: ", hru%bmpuser
            hru%bmpuser = intBuffer

        case("hru_crop_reg")
            print *, "HRU crop_reg: ", hru%crop_reg
            hru%crop_reg = intBuffer

        case("hru_cur_op")
            print *, "HRU cur_op: ", hru%cur_op
            hru%cur_op = intBuffer

        case("hru_strsa")
            print *, "HRU strsa", hru%strsa
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING... "
            hru%strsa = intBuffer

        case("hru_sno_mm")
            !mm H2O        |amount of water in snow on current day
            print *, "HRU sno_mm: ", hru%sno_mm
            hru%sno_mm = floatBuffer

        case("hru_water_fr")
            print *, "HRU water_fr: ", hru%water_fr
            hru%water_fr = floatBuffer

        case("hru_water_seep")
            print *, "HRU water_seep: ", hru%water_seep
            hru%water_seep = floatBuffer

        case("hru_water_evap")
            print *, "HRU water_evap: ", hru%water_evap
            floatBuffer = hru%water_evap

        case("hru_ich_flood")
            print *, "HRU ich_flood: ", hru%ich_flood
            hru%ich_flood= intBuffer

        case("hru_luse%name")
            print *, "HRU luse%name"
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"
            do i= 1,size(hru)
                print *, hru(i)%luse%name
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do

        case("hru_luse%cn_lu")
            print *, "HRU cn_luse: "
            do i= 1,size(hru)
                print *, hru(i)%luse%cn_lu
                hru(i)%luse%cn_lu = intBuffer(i)
            end do

        case("hru_luse%cons_prac")
            print *, "HRU luse%cons_prac"
            do i= 1,size(hru)
                print *, hru(i)%luse%cons_prac
                hru(i)%luse%cons_prac = intBuffer(i)
            end do

        case("hru_luse%usle_p")
            print *, "HRU luse%usle_p"
            do i= 1,size(hru)
                print *, hru(i)%luse%usle_p
                hru(i)%luse%usle_p = intBuffer(i)
            end do

        case("hru_luse%urb_ro")
            print *, "HRU luse%urb_ro"
            do i= 1,size(hru)
                print *, hru(i)%luse%urb_ro
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_luse%urb_lu")
            print *, "HRU luse%urb_lu"
            do i= 1,size(hru)
                print *, hru(i)%luse%urb_lu
                hru(i)%luse%urb_lu = intBuffer(i)
            end do

        case("hru_luse%ovn")
            print *, "HRU luse%ovn"
            do i= 1,size(hru)
                print *, hru(i)%luse%ovn
                hru(i)%luse%ovn = intBuffer(i)
            end do

        case("hru_dbs%name")
            print *, "HRU dbs%name"
            do i= 1,size(hru)
                print *, hru(i)%dbs%name
            end do
            hru%obj_no = intBuffer(i)
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbs%topo")
            print *, "HRU dbs%topo"
            do i= 1,size(hru)
                print *, hru(i)%dbs%topo
                hru(i)%dbs%topo = intBuffer(i)
            end do

        case("hru_dbs%hyd")
            print *, "HRU dbs%hyd"
            do i= 1,size(hru)
                print *, hru(i)%dbs%hyd
                hru(i)%dbs%hyd = intBuffer(i)
            end do

        case("hru_dbs%soil")
            print *, "HRU dbs%soil"
            do i= 1,size(hru)
                print *, hru(i)%dbs%soil
                hru(i)%dbs%soil = intBuffer(i)
            end do

        case("hru_dbs%land_use_mgt")
            print *, "HRU dbs%land_use_mgt"
            do i= 1,size(hru)
                print *, hru(i)%dbs%land_use_mgt
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do

        case("hru_dbs%soil_plant_init")
            print *, "HRU dbs%soil_plant_init"
            do i= 1,size(hru)
                print *, hru(i)%dbs%soil_plant_init
                hru(i)%dbs%soil_plant_init = intBuffer(i)
            end do

        case("hru_dbs%surf_stor")
            print *, "HRU dbs%surf_stor"
            do i= 1,size(hru)
                print *, hru(i)%dbs%surf_stor
                hru(i)%dbs%surf_stor = intBuffer(i)
            end do

        case("hru_dbs%snow")
            print *, "HRU dbs%snow"
            do i= 1,size(hru)
                print *, hru(i)%dbs%snow
                hru(i)%dbs%snow = intBuffer(i)
            end do

        case("hru_dbs%field")
            print *, "HRU dbs%hyd"
            do i= 1,size(hru)
                print *, hru(i)%dbs%hyd
                hru(i)%dbs%hyd = intBuffer(i)
            end do

        case("hru_dbsc%name")
            print *, "HRU dbsc%name"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%name

            end do

            hru%obj_no = intBuffer(i)
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbsc%topo")
            print *, "HRU dbsc%topo"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%topo
                hru(i)%dbs%topo = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%topo"

        case("hru_dbsc%hyd")
            print *, "HRU dbsc%hyd"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%hyd
                hru(i)%dbs%hyd = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%hyd"

        case("hru_dbsc%soil")
            print *, "HRU dbsc%soil"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%soil
                hru(i)%dbs%soil = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil"

        case("hru_dbsc%land_use_mgt")
            print *, "HRU dbsc%land_use_mgt"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%land_use_mgt
                hru(i)%dbs%land_use_mgt = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_dbsc%soil_plant_init")
            print *, "HRU dbsc%soil_plant_init"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%soil_plant_init
                hru(i)%dbs%soil_plant_init = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil_plant_init"

        case("hru_dbsc%surf_stor")
            print *, "HRU dbsc%surf_stor"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%surf_stor
                hru(i)%dbs%surf_stor = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%surf_stor"

        case("hru_dbsc%snow")
            print *, "HRU dbsc%snow"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%snow
                hru(i)%dbs%snow = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%snow"

        case("hru_dbsc%field")
            print *, "HRU dbsc%field"
            do i= 1,size(hru)
                print *, hru(i)%dbsc%field
                hru(i)%dbs%field = intBuffer(i)
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%field"

        case("hru_lumv%usle_p")
            print *, "HRU lumv%usle_p"
            do i= 1,size(hru)
                print *, hru(i)%lumv%usle_p
                hru(i)%lumv%usle_p = intBuffer(i)
            end do

        case("hru_lumv%usle_ls")
            print *, "HRU lumv%usle_ls"
            do i= 1,size(hru)
                print *, hru(i)%lumv%usle_ls
                hru(i)%lumv%usle_ls = intBuffer(i)
            end do

        case("hru_lumv%usle_mult")
            print *, "HRU lumv%usle_mult"
            do i= 1,size(hru)
                print *, hru(i)%lumv%usle_mult
                hru(i)%lumv%usle_mult = intBuffer(i)
            end do

        case("hru_lumv%sdr_dep")
            print *, "HRU lumv%sdr_dep"
            do i= 1,size(hru)
                print *, hru(i)%lumv%sdr_dep
                hru(i)%lumv%sdr_dep = intBuffer(i)
            end do

        case("hru_lumv%ldrain")
            print *, "HRU lumv%ldrain"
            do i= 1,size(hru)
                print *, hru(i)%lumv%ldrain
                hru(i)%lumv%ldrain = intBuffer(i)
            end do

        case("hru_lumv%tile_ttime")
            print *, "HRU lumv%tile_ttime"
            do i= 1,size(hru)
                print *, hru(i)%lumv%tile_ttime
                hru(i)%lumv%tile_ttime = intBuffer(i)
            end do

        case("hru_lumv%vfsi")
            print *, "HRU lumv%vfsi"
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfsi
                hru(i)%lumv%vfsi = intBuffer(i)
            end do

        case("hru_lumv%vfsratio")
            print *, "HRU lumv%vfsratio"
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfsratio
                hru(i)%lumv%vfsratio = intBuffer(i)
            end do

        case("hru_lumv%vfscon")
            print *, "HRU lumv%vfscon"
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfscon
                hru(i)%lumv%vfscon = intBuffer(i)
            end do

        case("hru_lumv%vfsch")
            print *, "HRU lumv%vfsch"
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfsch
                hru(i)%lumv%vfsch = intBuffer(i)
            end do

        case("hru_lumv%ngrwat")
            print *, "HRU lumv%ngrwat"
            do i= 1,size(hru)
                print *, hru(i)%lumv%ngrwat
                hru(i)%lumv%ngrwat = intBuffer(i)
            end do

        case("hru_lumv%grwat_i")
            print *, "HRU lumv%grwat_i"
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_i
                hru(i)%lumv%grwat_i = intBuffer(i)
            end do

        case("hru_lumv%grwat_n")
            print *, "HRU lumv%grwat_n"
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_n
                hru(i)%lumv%grwat_n = intBuffer(i)
            end do

        case("hru_lumv%grwat_spcon")
            print *, "HRU lumv%grwat_spcon"
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_spcon
                hru(i)%lumv%grwat_spcon = intBuffer(i)
            end do

        case("hru_lumv%grwat_d")
            print *, "HRU lumv%grwat_d"
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_d
                hru(i)%lumv%grwat_d = intBuffer(i)
            end do

        case("hru_lumv%grwat_w")
            print *, "HRU lumv%grwat_w"
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_w
                hru(i)%lumv%grwat_w = intBuffer(i)
            end do

        case("hru_lumv%grwat_l")
            print *, "HRU lumv%grwat_l"
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_l
                hru(i)%lumv%grwat_l = intBuffer(i)
            end do

        case("hru_lumv%grwat_s")
            print *, "HRU lumv%grwat_s"
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_s
                hru(i)%lumv%grwat_s = intBuffer(i)
            end do

        case("hru_lumv%bmp_flag")
            print *, "HRU lumv%bmp_flag"
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_flag
                hru(i)%lumv%bmp_flag = intBuffer(i)
            end do

        case("hru_lumv%bmp_sed")
            print *, "HRU lumv%bmp_sed"
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_sed
                hru(i)%lumv%bmp_sed = intBuffer(i)
            end do

        case("hru_lumv%bmp_pp")
            print *, "HRU lumv%bmp_pp"
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_pp
                hru(i)%lumv%bmp_pp = intBuffer(i)
            end do

        case("hru_lumv%bmp_sp")
            print *, "HRU lumv%bmp_sp"
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_sp
                hru(i)%lumv%bmp_sp = intBuffer(i)
            end do

        case("hru_lumv%bmp_pn")
            print *, "HRU lumv%bmp_pn"
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_pn
                hru(i)%lumv%bmp_pn = intBuffer(i)
            end do

        case("hru_lumv%bmp_sn")
            print *, "HRU lumv%bmp_sn"
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_sn
                hru(i)%lumv%bmp_sn = intBuffer(i)
            end do

        case("hru_lumv%bmp_bac")
            print *, "HRU lumv%bmp_bac"
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_bac
                hru(i)%lumv%bmp_bac = intBuffer(i)
            end do

        case("luse")
            print *, "HRU luse: "
            do i= 1,size(intBuffer)
                print *, i, " "
                print *, hru(i)%luse%name
                j = i
                jj = i
                ihru = i
                ilu = intBuffer(i)
                isol = hru(j)%dbs%soil

                if((ilu/=hru(j)%land_use_mgt))then

                !Changing landuse in databases
                hru(j)%dbs%land_use_mgt = ilu
                hru(j)%dbsc%land_use_mgt = lum(ilu)%name !from line 72 and 73 in hru_read

                iob = hru(j)%obj_no
                ihru_db = ob(iob)%props
                hru_db(ihru_db)%dbs = hru(ihru)%dbs
                hru_db(ihru_db)%dbsc = hru(ihru)%dbsc

                hru(j)%land_use_mgt = ilu

                print *, "Calling plant_init(1)"
                call plant_init(1)

                ! How it is done in actions
                !!land use change
                !          case ("lu_change")
                !            j = d_tbl%act(iac)%ob_num
                !            if (j == 0) j = ob_cur
                !            ilu = d_tbl%act_typ(iac)
                !            hru(j)%dbs%land_use_mgt = ilu
                !            hru(j)%land_use_mgt_c = d_tbl%act(iac)%file_pointer
                !            isol = hru(j)%dbs%soil
                !            call plant_init (1)
                end if
            end do

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
        character(len = 16) :: temp_shapeBuffer
        shapeBuffer = ""

        print *, "Var nombre in obtener: ", varNombre

        !if(allocated(intBuffer)) deallocate(intBuffer)
        !if(allocated(floatBuffer)) deallocate(floatBuffer)

        select case (trim(varNombre))

!--------Calibration/Initialization-------------------------------------------------------------------------------------

!-----------Landuse Variables-------------------------------------------------------------------------------------------


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
            print *, "SD chs: ", sd_ch%chs
            allocate(floatBuffer(size(sd_ch%chs)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chs

        case("sd_chl")
            !km         |channel length
            print *, "SD chl: ", sd_ch%chl
            allocate(floatBuffer(size(sd_ch%chl)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chl

        case("sd_chn")
            !           |channel Manning's n
            print *, "SD chn: ", sd_ch%chn
            allocate(floatBuffer(size(sd_ch%chn)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chn

        case("sd_cov")
            !0-1        |channel cover factor
            print *, "SD cov: ", sd_ch%cov
            allocate(floatBuffer(size(sd_ch%cov)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%cov

        case("sd_cherod")
            !           |channel erodibility
            print *, "SD cherod: ", sd_ch%cherod
            allocate(floatBuffer(size(sd_ch%cherod)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%cherod

        case("sd_shear_bnk")
            !0-1        |bank shear coefficient - fraction of bottom shear
            print *, "SD shear bnk: ", sd_ch%shear_bnk
            allocate(floatBuffer(size(sd_ch%shear_bnk)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%shear_bnk

        case("sd_hc_erod")
            !           |headcut erodibility
            print *, "SD hc_erod: ", sd_ch%hc_erod
            allocate(floatBuffer(size(sd_ch%hc_erod)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_erod

        case("sd_hc_co")
            !m/m        |proportionality coefficient for head cut
            print *, "SD hc co: ", sd_ch%hc_co
            allocate(floatBuffer(size(sd_ch%hc_co)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_co

        case("sd_hc_len")
            !m          |length of head cut
            print *, "SD hc_len: ", sd_ch%hc_len
            allocate(floatBuffer(size(sd_ch%hc_len)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_len

        case("sd_hc_hgt")
            !m          |headcut height
            print *, "SD hc_hgt: ", sd_ch%hc_hgt
            allocate(floatBuffer(size(sd_ch%hc_hgt)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%hc_hgt

        case("sd_stor")
            !m3         |water stored in reach at end of the day
            print *, "SD stor: ", sd_ch%stor
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
            print *, "HLT props: ", hlt%props
            allocate(intBuffer(size(hlt%props)))
            allocate(floatBuffer(0))
            intBuffer = hlt%props

        case("lte_obj_no")
            print *, "HLT obj_no: ", hlt%obj_no
            allocate(intBuffer(size(hlt%obj_no)))
            allocate(floatBuffer(0))
            intBuffer = hlt%obj_no

        case("lte_plant")
            !character(len=16) :: plant         !   |plant type (as listed in plants.plt)
            print *, "lte_plant is not yet supported for transfer as it is a character value but we are looking into it"

        case("lte_iplant")
            !              |plant number xwalked from hlt_db()%plant and plants.plt
            print *, "HLT iplant: ", hlt%iplant
            allocate(intBuffer(size(hlt%iplant)))
            allocate(floatBuffer(0))
            intBuffer = hlt%iplant

        case("lte_km2")
            !km^2          |drainage area
            print *, "HLT km2: ", hlt%km2
            allocate(floatBuffer(size(hlt%km2)))
            allocate(intBuffer(0))
            floatBuffer = hlt%km2

        case("lte_cn2")
            !              |condition II curve number (used in calibration)
            print *, "HLT cn2: ", hlt%cn2
            allocate(floatBuffer(size(hlt%cn2)))
            allocate(intBuffer(0))
            floatBuffer = hlt%cn2

        case("lte_cn3_swf")
            !none          |soil water factor for cn3 (used in calibration)
            print *, "HLT cn3_swf: ", hlt%cn3_swf
            allocate(floatBuffer(size(hlt%cn3_swf)))
            allocate(intBuffer(0))
            floatBuffer = hlt%cn3_swf

        case("lte_soildep")                                     !              |0 = fc; 1 = saturation (porosity)
            !mm            |soil profile depth
            print *, "HLT soildep: ", hlt%soildep
            allocate(floatBuffer(size(hlt%soildep)))
            allocate(intBuffer(0))
            floatBuffer = hlt%soildep

        case("lte_etco")
            !              |et coefficient - use with pet and aet (used in calibration)
            print *, "HLT etco: ", hlt%etco
            allocate(floatBuffer(size(hlt%etco)))
            allocate(intBuffer(0))
            floatBuffer = hlt%etco

        case("lte_revapc")
            !m/m           |revap from aquifer (used in calibration)
            print *, "HLT revapc: ", hlt%revapc
            allocate(floatBuffer(size(hlt%revapc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%revapc

        case("lte_perco")
            !              |soil percolation coefficient (used in calibration)
            print *, "HLT perco: ", hlt%perco
            allocate(floatBuffer(size(hlt%perco)))
            allocate(intBuffer(0))
            floatBuffer = hlt%perco

        case("lte_tdrain")
            !hr            |design subsurface tile drain time (used in calibration)
            print *, "HLT tdrain: ", hlt%tdrain
            allocate(floatBuffer(size(hlt%tdrain)))
            allocate(intBuffer(0))
            floatBuffer = hlt%tdrain

        case("lte_stress")
            !frac          |plant stress - pest, root restriction, soil quality, nutrient,
            print *, "HLT stress: ", hlt%stress
            allocate(floatBuffer(size(hlt%stress)))
            allocate(intBuffer(0))
            floatBuffer = hlt%stress

        case("lte_uslefac")
            !              |USLE slope length factor
            print *, "HLT uslefac: ", hlt%uslefac
            allocate(floatBuffer(size(hlt%uslefac)))
            allocate(intBuffer(0))
            floatBuffer = hlt%uslefac

        case("lte_wrt1")
            print *, "HLT wrt1: ", hlt%wrt1
            allocate(floatBuffer(size(hlt%wrt1)))
            allocate(intBuffer(0))
            floatBuffer = hlt%wrt1

        case("lte_wrt2")
            print *, "HLT wrt2: ", hlt%wrt2
            allocate(floatBuffer(size(hlt%wrt2)))
            allocate(intBuffer(0))
            floatBuffer = hlt%wrt2

        case("lte_smx")
            print *, "HLT smx: ", hlt%smx
            allocate(floatBuffer(size(hlt%smx)))
            allocate(intBuffer(0))
            floatBuffer = hlt%smx

        case("lte_hk")
            print *, "HLT hk: ", hlt%hk
            allocate(floatBuffer(size(hlt%hk)))
            allocate(intBuffer(0))
            floatBuffer = hlt%hk

        case("lte_yls")
            print *, "HLT yls: ", hlt%yls
            allocate(floatBuffer(size(hlt%yls)))
            allocate(intBuffer(0))
            floatBuffer = hlt%yls

        case("lte_ylc")
            print *, "HLT ylc: ", hlt%ylc
            allocate(floatBuffer(size(hlt%ylc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%ylc

        case("lte_awc")
            !mm/mm        |available water capacity of soil
            print *, "HLT awc: ", hlt%awc
            allocate(floatBuffer(size(hlt%awc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%awc

        case("lte_g")
            print *, "HLT g: ", hlt%g
            allocate(floatBuffer(size(hlt%g)))
            allocate(intBuffer(0))
            floatBuffer = hlt%g

        case("lte_hufh")
            print *, "HLT hufh: ", hlt%hufh
            allocate(floatBuffer(size(hlt%hufh)))
            allocate(intBuffer(0))
            floatBuffer = hlt%hufh

        case("lte_phu")
            print *, "HLT phu: ", hlt%phu
            allocate(floatBuffer(size(hlt%phu)))
            allocate(intBuffer(0))
            floatBuffer = hlt%phu

        case("lte_por")
            print *, "HLT por: ", hlt%por
            allocate(floatBuffer(size(hlt%por)))
            allocate(intBuffer(0))
            floatBuffer = hlt%por

        case("lte_sc")
            print *, "HLT sc: ", hlt%sc
            allocate(floatBuffer(size(hlt%sc)))
            allocate(intBuffer(0))
            floatBuffer = hlt%sc

        case("lte_sw")
            !mm/mm         |initial soil water storage
            print *, "HLT sw: ", hlt%sw
            allocate(floatBuffer(size(hlt%sw)))
            allocate(intBuffer(0))
            floatBuffer = hlt%sw

        case("lte_gw")
            !mm            |initial shallow aquifer storage
            print *, "HLT gw: ", hlt%gw
            allocate(floatBuffer(size(hlt%gw)))
            allocate(intBuffer(0))
            floatBuffer = hlt%gw

        case("lte_snow")
            !mm            |initial water content of snow
            print *, "HLT snow: ", hlt%snow
            allocate(floatBuffer(size(hlt%snow)))
            allocate(intBuffer(0))
            floatBuffer = hlt%snow

        case("lte_gwflow")
            !mm            |initial groundwater flow
            print *, "HLT gwflow: ", hlt%gwflow
            allocate(floatBuffer(size(hlt%gwflow)))
            allocate(intBuffer(0))
            floatBuffer = hlt%gwflow

        case("lte_dm")
            !t/ha          |plant biomass
            print *, "HLT dm: ", hlt%dm
            allocate(floatBuffer(size(hlt%dm)))
            allocate(intBuffer(0))
            floatBuffer = hlt%dm

        case("lte_alai")
            !              |leaf area index
            print *, "HLT alai: ", hlt%alai
            allocate(floatBuffer(size(hlt%alai)))
            allocate(intBuffer(0))
            floatBuffer = hlt%alai

        case("lte_yield")
            !t/ha          |plant yield
            print *, "HLT yield: ", hlt%yield
            allocate(floatBuffer(size(hlt%yield)))
            allocate(intBuffer(0))
            floatBuffer = hlt%yield

        case("lte_npp")
            !t/ha          |net primary productivity
            print *, "HLT npp: ", hlt%npp
            allocate(floatBuffer(size(hlt%npp)))
            allocate(intBuffer(0))
            floatBuffer = hlt%npp

        case("lte_lai_mx")
            !              |maximum leaf area index
            print *, "HLT lai_mx: ", hlt%lai_mx
            allocate(floatBuffer(size(hlt%lai_mx)))
            allocate(intBuffer(0))
            floatBuffer = hlt%lai_mx

        case("lte_gwdeep")
            !mm            |deep aquifer storage
            print *, "HLT gwdeep: ", hlt%gwdeep
            allocate(floatBuffer(size(hlt%gwdeep)))
            allocate(intBuffer(0))
            floatBuffer = hlt%gwdeep

        case("lte_aet")
            !mm            |sum of actual et during growing season (for hi water stress)
            print *, "HLT aet: ", hlt%aet
            allocate(floatBuffer(size(hlt%aet)))
            allocate(intBuffer(0))
            floatBuffer = hlt%aet

        case("lte_pet")
            !mm            |sum of potential et during growing season (for hi water stress)
            print *, "HLT pet: ", hlt%pet
            allocate(floatBuffer(size(hlt%pet)))
            allocate(intBuffer(0))
            floatBuffer = hlt%pet

        case("lte_start")
            print *, "HLT start: ", hlt%start
            allocate(intBuffer(size(hlt%start)))
            allocate(floatBuffer(0))
            intBuffer = hlt%start

        case("lte_end")
            print *, "HLT end: ", hlt%end
            allocate(intBuffer(size(hlt%end)))
            allocate(floatBuffer(0))
            intBuffer = hlt%end

!-------Channel Variables-----------------------------------------------------------------------------------------------
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            print *, "Channel algae: ", ch%algae
            allocate(floatBuffer(size(ch%algae)))
            allocate(intBuffer(0))
            floatBuffer = ch%algae

        CASE("ammonian")        ! mg N/L        |ammonia concentration in reach
            print *, "Channel ammonian: ", ch%ammonian
            allocate(floatBuffer(size(ch%ammonian)))
            allocate(intBuffer(0))
            floatBuffer = ch%ammonian

        CASE("bankst")          ! m^3 H2O       |bank storage
            print *, "Channel bankst: ", ch%bankst
            allocate(floatBuffer(size(ch%bankst)))
            allocate(intBuffer(0))
            floatBuffer = ch%bankst

        CASE("li")              ! km            |initial length of main channel
            print *, "Channel li: ", ch%li
            allocate(floatBuffer(size(ch%li)))
            allocate(intBuffer(0))
            floatBuffer = ch%li

        CASE("orgn")            !               |organic nitrogen contribution from channel erosion
            print *, "Channel orgn: ", ch%orgn
            allocate(floatBuffer(size(ch%orgn)))
            allocate(intBuffer(0))
            floatBuffer = ch%orgn

        CASE("orgp")            !               |organic phosphorus contribution from channel erosion
            print *, "Channel orgp: ", ch%orgp
            allocate(floatBuffer(size(ch%orgp)))
            allocate(intBuffer(0))
            floatBuffer = ch%orgp

        CASE("si")              !(m/n)          |slope of main channel
            print *, "Channel bankst: ", ch%si
            allocate(floatBuffer(size(ch%si)))
            allocate(intBuffer(0))
            floatBuffer = ch%si

        CASE("wi")              !(m)            |width of main channel at top of bank
            print *, "Channel wi: ", ch%wi
            allocate(floatBuffer(size(ch%wi)))
            allocate(intBuffer(0))
            floatBuffer = ch%wi

        CASE("di")              !(m)            |depth of main channel from top of bank to bottom
            print *, "Channel bankst: ", ch%di
            allocate(floatBuffer(size(ch%di)))
            allocate(intBuffer(0))
            floatBuffer = ch%di

        CASE("chlora")          ! mg chl-a/L    |chlorophyll-a concentration in reach
            print *, "Channel bankst: ", ch%chlora
            allocate(floatBuffer(size(ch%chlora)))
            allocate(intBuffer(0))
            floatBuffer = ch%chlora

        CASE("pst_conc")        ! mg/(m**3)     |initial pesticide concentration in reach
            print *, "Channel pst_conc: ", ch%pst_conc
            allocate(floatBuffer(size(ch%pst_conc)))
            allocate(intBuffer(0))
            floatBuffer = ch%pst_conc

        CASE("dep_chan")        ! m             |average daily water depth in channel
            print *, "Channel dep_chan: ", ch%dep_chan
            allocate(floatBuffer(size(ch%dep_chan)))
            allocate(intBuffer(0))
            floatBuffer = ch%dep_chan

        CASE("disolvp")         ! mg P/L        |dissolved P concentration in reach
            print *, "Channel disolvp: ", ch%disolvp
            allocate(floatBuffer(size(ch%disolvp)))
            allocate(intBuffer(0))
            floatBuffer = ch%disolvp

        CASE("drift")           ! kg            |amount of pesticide drifting onto main channel in subbasin
            print *, "Channel drift: ", ch%drift
            allocate(floatBuffer(size(ch%drift)))
            allocate(intBuffer(0))
            floatBuffer = ch%drift

        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            print *, "Channel flwin: ", ch%flwin
            allocate(floatBuffer(size(ch%flwin)))
            allocate(intBuffer(0))
            floatBuffer = ch%flwin

        CASE("flwout")          ! m^3 H2O       |flow out of reach on previous day
            print *, "Channel flwout: ", ch%flwout
            allocate(floatBuffer(size(ch%flwout)))
            allocate(intBuffer(0))
            floatBuffer = ch%flwout

        CASE("nitraten")        ! mg N/L        |nitrate concentration in reach
            print *, "Channel nitraten: ", ch%nitraten
            allocate(floatBuffer(size(ch%nitraten)))
            allocate(intBuffer(0))
            floatBuffer = ch%nitraten

        CASE("nitriten")        ! mg N/L        |nitrite concentration in reach
            print *, "Channel nitriten: ", ch%nitriten
            allocate(floatBuffer(size(ch%nitriten)))
            allocate(intBuffer(0))
            floatBuffer = ch%nitriten

        CASE("organicn")        ! mg N/L        |organic nitrogen concentration in reach
            print *, "Channel organicn: ", ch%organicn
            allocate(floatBuffer(size(ch%organicn)))
            allocate(intBuffer(0))
            floatBuffer = ch%organicn

        CASE("organicp")        ! mg P/L        |organic phosphorus concentration in reach
            print *, "Channel organicp: ", ch%organicp
            allocate(floatBuffer(size(ch%organicp)))
            allocate(intBuffer(0))
            floatBuffer = ch%organicp

        CASE("rch_bactlp")      ! # cfu/100ml   |less persistent bacteria stored in reach
            print *, "Channel rch_bactlp: ", ch%rch_bactlp
            allocate(floatBuffer(size(ch%rch_bactlp)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_bactlp

        CASE("rch_bactp")       ! # cfu/100ml   |persistent bacteria stored in reach
            print *, "Channel rch_bactp: ", ch%rch_bactp
            allocate(floatBuffer(size(ch%rch_bactp)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_bactp

        CASE("rch_cbod")        ! mg O2/L       |carbonaceous biochemical oxygen demand in reach
            print *, "Channel rch_cbod: ", ch%rch_cbod
            allocate(floatBuffer(size(ch%rch_cbod)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_cbod

        CASE("rch_dox")         ! mg O2/L       |dissolved oxygen concentration in reach
            print *, "Channel rch_dox: ", ch%rch_dox
            allocate(floatBuffer(size(ch%rch_dox)))
            allocate(intBuffer(0))
            floatBuffer = ch%rch_dox

        CASE("rchstor")         ! m^3 H2O       |water stored in reach
            print *, "Channel rchstor: ", ch%rchstor
            allocate(floatBuffer(size(ch%rchstor)))
            allocate(intBuffer(0))
            floatBuffer = ch%rchstor

        CASE("sedst")           ! metric tons   |amount of sediment stored in reach
            print *, "Channel sedst: ", ch%sedst
            allocate(floatBuffer(size(ch%sedst)))
            allocate(intBuffer(0))
            floatBuffer = ch%sedst

        CASE("vel_chan")        ! m/s           |average flow velocity in channel
            print *, "Channel vel_chan: ", ch%vel_chan
            allocate(floatBuffer(size(ch%vel_chan)))
            allocate(intBuffer(0))
            floatBuffer = ch%vel_chan

        CASE("bed_san")
            print *, "Channel bed_san: ", ch%bed_san
            allocate(floatBuffer(size(ch%bed_san)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_san

        CASE("bed_sil")
            print *, "Channel bed_sil: ", ch%bed_sil
            allocate(floatBuffer(size(ch%bed_sil)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_sil

        CASE("bed_cla")
            print *, "Channel flwin: ", ch%bed_cla
            allocate(floatBuffer(size(ch%bed_cla)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_cla

        CASE("bed_gra")
            print *, "Channel bed_gra: ", ch%bed_gra
            allocate(floatBuffer(size(ch%bed_gra)))
            allocate(intBuffer(0))
            floatBuffer = ch%bed_gra

        CASE("bnk_san")
            print *, "Channel bnk_san: ", ch%bnk_san
            allocate(floatBuffer(size(ch%bnk_san)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_san

        CASE("bnk_sil")
            print *, "Channel bnk_sil: ", ch%bnk_sil
            allocate(floatBuffer(size(ch%bnk_sil)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_sil

        CASE("bnk_cla")
            print *, "Channel bnk_cla: ", ch%bnk_cla
            allocate(floatBuffer(size(ch%bnk_cla)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_cla

        CASE("bnk_gra")
            print *, "Channel bnk_gra: ", ch%bnk_gra
            allocate(floatBuffer(size(ch%bnk_gra)))
            allocate(intBuffer(0))
            floatBuffer = ch%bnk_gra

        CASE("depfp")
            print *, "Channel depfp: ", ch%depfp
            allocate(floatBuffer(size(ch%depfp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depfp

        CASE("depprfp")
            print *, "Channel depprfp: ", ch%depprfp
            allocate(floatBuffer(size(ch%depprfp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depprfp

        CASE("depsilfp")
            print *, "Channel depsilfp: ", ch%depsilfp
            allocate(floatBuffer(size(ch%depsilfp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsilfp

        CASE("depclafp")
            print *, "Channel depclafp: ", ch%depclafp
            allocate(floatBuffer(size(ch%depclafp)))
            allocate(intBuffer(0))
            floatBuffer = ch%depclafp

        CASE("depch")
            print *, "Channel depch: ", ch%depch
            allocate(floatBuffer(size(ch%depch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depch

        CASE("depprch")
            print *, "Channel depprch: ", ch%depprch
            allocate(floatBuffer(size(ch%depprch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depprch

        CASE("depsanch")
            print *, "Channel depsanch: ", ch%depsanch
            allocate(floatBuffer(size(ch%depsanch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsanch

        CASE("depsilch")
            print *, "Channel depsilch: ", ch%depsilch
            allocate(floatBuffer(size(ch%depsilch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsilch

        CASE("depclach")
            print *, "Channel depclach: ", ch%depclach
            allocate(floatBuffer(size(ch%depclach)))
            allocate(intBuffer(0))
            floatBuffer = ch%depclach

        CASE("depsagch")
            print *, "Channel depsagch: ", ch%depsagch
            allocate(floatBuffer(size(ch%depsagch)))
            allocate(intBuffer(0))
            floatBuffer = ch%depsagch

        CASE("deplagch")
            print *, "Channel deplagch: ", ch%deplagch
            allocate(floatBuffer(size(ch%deplagch)))
            allocate(intBuffer(0))
            floatBuffer = ch%deplagch

        CASE("depgrach")
            print *, "Channel depgrach: ", ch%depgrach
            allocate(floatBuffer(size(ch%depgrach)))
            allocate(intBuffer(0))
            floatBuffer = ch%depgrach

        CASE("sanst")
            print *, "Channel sanst: ", ch%sanst
            allocate(floatBuffer(size(ch%sanst)))
            allocate(intBuffer(0))
            floatBuffer = ch%sanst

        CASE("silst")
            print *, "Channel silst: ", ch%silst
            allocate(floatBuffer(size(ch%silst)))
            allocate(intBuffer(0))
            floatBuffer = ch%silst

        CASE("clast")
            print *, "Channel clast: ", ch%clast
            allocate(floatBuffer(size(ch%clast)))
            allocate(intBuffer(0))
            floatBuffer = ch%clast

        CASE("sagst")
            print *, "Channel sagst: ", ch%sagst
            allocate(floatBuffer(size(ch%sagst)))
            allocate(intBuffer(0))
            floatBuffer = ch%sagst

        CASE("lagst")
            print *, "Channel lagst: ", ch%lagst
            allocate(floatBuffer(size(ch%lagst)))
            allocate(intBuffer(0))
            floatBuffer = ch%lagst

        CASE("grast")
            print *, "Channel grast: ", ch%grast
            allocate(floatBuffer(size(ch%grast)))
            allocate(intBuffer(0))
            floatBuffer = ch%grast

        CASE("wattemp")
            print *, "Channel wattemp: ", ch%wattemp
            allocate(floatBuffer(size(ch%wattemp)))
            allocate(intBuffer(0))
            floatBuffer = ch%wattemp

        CASE("bactp")
            print *, "Channel bactp: ", ch%bactp
            allocate(floatBuffer(size(ch%bactp)))
            allocate(intBuffer(0))
            floatBuffer = ch%bactp

        CASE("chfloodvol")
            print *, "Channel chfloodvol: ", ch%chfloodvol
            allocate(floatBuffer(size(ch%chfloodvol)))
            allocate(intBuffer(0))
            floatBuffer = ch%chfloodvol

        CASE("bactlp")
            print *, "Channel bactlp: ", ch%bactlp
            allocate(floatBuffer(size(ch%bactlp)))
            allocate(intBuffer(0))
            floatBuffer = ch%bactlp

!-------HRU Variables---------------------------------------------------------------------------------------------------
        case("hru_obj_no")
            print *, "HRU obj_no: ", hru%obj_no
            allocate(intBuffer(size(hru%obj_no)))
            allocate(floatBuffer(0))
            intBuffer = hru%obj_no

        case("hru_area_ha")
            print *, "HRU area_ha: ", hru%area_ha
            allocate(floatBuffer(size(hru%area_ha)))
            allocate(intBuffer(0))
            floatBuffer = hru%area_ha

        case("hru_km")
            print *, "HRU km: ", hru%km
            allocate(floatBuffer(size(hru%km)))
            allocate(intBuffer(0))
            floatBuffer = hru%km

        case("hru_surf_stor")
            !points to res() for surface storage
            print *, "HRU surf_stor: ", hru%surf_stor
            allocate(intBuffer(size(hru%surf_stor)))
            allocate(floatBuffer(0))
            intBuffer = hru%surf_stor

        case("hru_land_use_mgt")
            print *, "HRU land_use_mgt: ", hru%land_use_mgt
            allocate(intBuffer(size(hru%land_use_mgt)))
            allocate(floatBuffer(0))
            intBuffer = hru%land_use_mgt

        !    character(len=16) :: land_use_mgt_c
        case("hru_land_use_mgt_c")
            print *, "HRU land_use_mgt_c: ", hru%land_use_mgt_c
            print *, "CANNOT SEND CHARACTER DATA, SENDING hru%land_use_mgt instead"
            allocate(intBuffer(size(hru%land_use_mgt)))
            allocate(floatBuffer(0))
            intBuffer = hru%land_use_mgt

        case("hru_lum_group")
            print *, "HRU lum_group: ", hru%lum_group
            allocate(intBuffer(size(hru%lum_group)))
            allocate(floatBuffer(0))
            intBuffer = hru%lum_group

        !    character(len=16) :: lum_group_c        !land use group for soft cal and output
        case("hru_lum_group_c")
            print *, "HRU lum_group_c: ", hru%lum_group_c
            print *, "CANNOT SEND CHARACTER DATA, SENDING hru%lum_group instead"
            allocate(intBuffer(size(hru%lum_group)))
            allocate(floatBuffer(0))
            intBuffer = hru%lum_group

        !    character(len=16) :: region
        case("hru_region")
            print *, "HRU region: ", hru%region
            print *, "CANNOT SEND CHARACTER DATA, SENDING 0 instead"
            allocate(intBuffer(1))
            allocate(floatBuffer(0))
            intBuffer = 0


        case("hru_plant_cov")
            print *, "HRU plant_cov: ", hru%plant_cov
            allocate(intBuffer(size(hru%plant_cov)))
            allocate(floatBuffer(0))
            intBuffer = hru%plant_cov

        case("hru_mgt_ops")
            print *, "HRU mgt_ops: ", hru%mgt_ops
            allocate(intBuffer(size(hru%mgt_ops)))
            allocate(floatBuffer(0))
            intBuffer = hru%mgt_ops

        case("hru_tiledrain")
            print *, "HRU tiledrain: ", hru%tiledrain
            allocate(intBuffer(size(hru%tiledrain)))
            allocate(floatBuffer(0))
            intBuffer = hru%tiledrain

        case("hru_septic")
            print *, "HRU septic: ", hru%septic
            allocate(intBuffer(size(hru%septic)))
            allocate(floatBuffer(0))
            intBuffer = hru%septic

        case("hru_fstrip")
            print *, "HRU fstrip: ", hru%fstrip
            allocate(intBuffer(size(hru%fstrip)))
            allocate(floatBuffer(0))
            intBuffer = hru%fstrip

        case("hru_grassww")
            print *, "HRU grassww: ", hru%grassww
            allocate(intBuffer(size(hru%grassww)))
            allocate(floatBuffer(0))
            intBuffer = hru%grassww

        case("hru_bmpuser")
            print *, "HRU bmpuser: ", hru%bmpuser
            allocate(intBuffer(size(hru%bmpuser)))
            allocate(floatBuffer(0))
            intBuffer = hru%bmpuser

        case("hru_crop_reg")
            print *, "HRU crop_reg: ", hru%crop_reg
            allocate(intBuffer(size(hru%crop_reg)))
            allocate(floatBuffer(0))
            intBuffer = hru%crop_reg

        case("hru_cur_op")
            print *, "HRU cur_op: ", hru%cur_op
            allocate(intBuffer(size(hru%cur_op)))
            allocate(floatBuffer(0))
            intBuffer = hru%cur_op

        case("hru_strsa")
            print *, "HRU strsa: ", hru%strsa
            allocate(floatBuffer(size(hru%strsa)))
            allocate(intBuffer(0))
            floatBuffer = hru%strsa

        case("hru_sno_mm")
            !mm H2O        |amount of water in snow on current day
            print *, "HRU sno_mm: ", hru%sno_mm
            allocate(floatBuffer(size(hru%sno_mm)))
            allocate(intBuffer(0))
            floatBuffer = hru%sno_mm

        case("hru_water_fr")
            print *, "HRU water_fr: ", hru%water_fr
            allocate(floatBuffer(size(hru%water_fr)))
            allocate(intBuffer(0))
            floatBuffer = hru%water_fr

        case("hru_water_seep")
            print *, "HRU water_seep: ", hru%water_seep
            allocate(floatBuffer(size(hru%water_seep)))
            allocate(intBuffer(0))
            floatBuffer = hru%water_seep

        case("hru_water_evap")
            print *, "HRU water_evap: ", hru%water_evap
            allocate(floatBuffer(size(hru%water_evap)))
            allocate(intBuffer(0))
            floatBuffer = hru%water_evap

        case("hru_ich_flood")
            print *, "HRU ich_flood: ", hru%ich_flood
            allocate(intBuffer(size(hru%ich_flood)))
            allocate(floatBuffer(0))
            intBuffer = hru%ich_flood

        case("hru_luse%name")
            print *, "HRU luse%name"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"
            do i= 1,size(hru)
                print *, hru(i)%luse%name
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do

        case("hru_luse%cn_lu")
            print *, "HRU cn_luse: "
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%luse%cn_lu
                intBuffer(i) = hru(i)%luse%cn_lu
            end do

        case("hru_luse%cons_prac")
            print *, "HRU luse%cons_prac"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%luse%cons_prac
                intBuffer(i) = hru(i)%luse%cons_prac
            end do

        case("hru_luse%usle_p")
            print *, "HRU luse%usle_p"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%luse%usle_p
                floatBuffer(i) = hru(i)%luse%usle_p
            end do

        case("hru_luse%urb_ro")
            print *, "HRU luse%urb_ro"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%luse%urb_ro
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_luse%urb_lu")
            print *, "HRU luse%urb_lu"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%luse%urb_lu
                intBuffer(i) = hru(i)%luse%urb_lu
            end do

        case("hru_luse%ovn")
            print *, "HRU luse%ovn"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%luse%ovn
                floatBuffer(i) = hru(i)%luse%ovn
            end do

        case("hru_dbs%name")
            print *, "HRU dbs%name"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%name
            end do
            intBuffer = hru%obj_no
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbs%topo")
            print *, "HRU dbs%topo"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%topo
                intBuffer(i) = hru(i)%dbs%topo
            end do

        case("hru_dbs%hyd")
            print *, "HRU dbs%hyd"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%hyd
                intBuffer(i) = hru(i)%dbs%hyd
            end do

        case("hru_dbs%soil")
            print *, "HRU dbs%soil"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%soil
                intBuffer(i) =hru(i)%dbs%soil
            end do

        case("hru_dbs%land_use_mgt")
            print *, "HRU dbs%land_use_mgt"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%land_use_mgt
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do

        case("hru_dbs%soil_plant_in")
            print *, "HRU dbs%soil_plant_init"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%soil_plant_init
                intBuffer(i) = hru(i)%dbs%soil_plant_init
            end do

        case("hru_dbs%surf_stor")
            print *, "HRU dbs%surf_stor"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%surf_stor
                intBuffer(i) =  hru(i)%dbs%surf_stor
            end do

        case("hru_dbs%snow")
            print *, "HRU dbs%snow"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%snow
                intBuffer(i) =hru(i)%dbs%snow
            end do

        case("hru_dbs%field")
            print *, "HRU dbs%hyd"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbs%hyd
                intBuffer(i) = hru(i)%dbs%hyd
            end do

        case("hru_dbsc%name")
            print *, "HRU dbsc%name"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%name
            end do

            intBuffer = hru%obj_no
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_obj_no"


        case("hru_dbsc%topo")
            print *, "HRU dbsc%topo"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%topo
                intBuffer(i) = hru(i)%dbs%topo
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%topo"

        case("hru_dbsc%hyd")
            print *, "HRU dbsc%hyd"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%hyd
                intBuffer(i) = hru(i)%dbs%hyd
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%hyd"

        case("hru_dbsc%soil")
            print *, "HRU dbsc%soil"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%soil
                intBuffer(i) = hru(i)%dbs%soil
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil"

        case("hru_dbsc%land_use_mgt")
            print *, "HRU dbsc%land_use_mgt"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%land_use_mgt
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%land_use_mgt"

        case("hru_dbsc%soil_plant_i")
            print *, "HRU dbsc%soil_plant_init"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%soil_plant_init
                intBuffer(i) = hru(i)%dbs%soil_plant_init
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%soil_plant_init"

        case("hru_dbsc%surf_stor")
            print *, "HRU dbsc%surf_stor"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%surf_stor
                intBuffer(i) = hru(i)%dbs%surf_stor
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%surf_stor"

        case("hru_dbsc%snow")
            print *, "HRU dbsc%snow"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%snow
                intBuffer(i) = hru(i)%dbs%snow
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%snow"

        case("hru_dbsc%field")
            print *, "HRU dbsc%field"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%dbsc%field
                intBuffer(i) = hru(i)%dbs%field
            end do
            print *, "CANNOT RETURN CHARACTER VALUE, SENDING hru_dbs%field"

        case("hru_lumv%usle_p")
            print *, "HRU lumv%usle_p"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%usle_p
                floatBuffer(i) = hru(i)%lumv%usle_p
            end do

        case("hru_lumv%usle_ls")
            print *, "HRU lumv%usle_ls"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%usle_ls
                floatBuffer(i) =hru(i)%lumv%usle_ls
            end do

        case("hru_lumv%usle_mult")
            print *, "HRU lumv%usle_mult"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%usle_mult
                floatBuffer(i) = hru(i)%lumv%usle_mult
            end do

        case("hru_lumv%sdr_dep")
            print *, "HRU lumv%sdr_dep"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%sdr_dep
                floatBuffer(i) = hru(i)%lumv%sdr_dep
            end do

        case("hru_lumv%ldrain")
            print *, "HRU lumv%ldrain"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%ldrain
                intBuffer(i) = hru(i)%lumv%ldrain
            end do

        case("hru_lumv%tile_ttime")
            print *, "HRU lumv%tile_ttime"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%tile_ttime
                floatBuffer(i) = hru(i)%lumv%tile_ttime
            end do

        case("hru_lumv%vfsi")
            print *, "HRU lumv%vfsi"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfsi
                floatBuffer(i) = hru(i)%lumv%vfsi
            end do

        case("hru_lumv%vfsratio")
            print *, "HRU lumv%vfsratio"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfsratio
                floatBuffer(i) = hru(i)%lumv%vfsratio
            end do

        case("hru_lumv%vfscon")
            print *, "HRU lumv%vfscon"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfscon
                floatBuffer(i) = hru(i)%lumv%vfscon
            end do

        case("hru_lumv%vfsch")
            print *, "HRU lumv%vfsch"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%vfsch
                floatBuffer(i) = hru(i)%lumv%vfsch
            end do

        case("hru_lumv%ngrwat")
            print *, "HRU lumv%ngrwat"
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%ngrwat
                intBuffer(i) = hru(i)%lumv%ngrwat
            end do

        case("hru_lumv%grwat_i")
            print *, "HRU lumv%grwat_i"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_i
                floatBuffer(i) = hru(i)%lumv%grwat_i
            end do

        case("hru_lumv%grwat_n")
            print *, "HRU lumv%grwat_n"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_n
                floatBuffer(i) = hru(i)%lumv%grwat_n
            end do

        case("hru_lumv%grwat_spcon")
            print *, "HRU lumv%grwat_spcon"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_spcon
                floatBuffer(i) = hru(i)%lumv%grwat_spcon
            end do

        case("hru_lumv%grwat_d")
            print *, "HRU lumv%grwat_d"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_d
                floatBuffer(i) = hru(i)%lumv%grwat_d
            end do

        case("hru_lumv%grwat_w")
            print *, "HRU lumv%grwat_w"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_w
                floatBuffer(i) = hru(i)%lumv%grwat_w
            end do

        case("hru_lumv%grwat_l")
            print *, "HRU lumv%grwat_l"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_l
                floatBuffer(i) = hru(i)%lumv%grwat_l
            end do

        case("hru_lumv%grwat_s")
            print *, "HRU lumv%grwat_s"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%grwat_s
                floatBuffer(i) = hru(i)%lumv%grwat_s
            end do

        case("hru_lumv%bmp_flag")
            print *, "HRU lumv%bmp_flag"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_flag
                floatBuffer(i) = hru(i)%lumv%bmp_flag
            end do

        case("hru_lumv%bmp_sed")
            print *, "HRU lumv%bmp_sed"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_sed
                floatBuffer(i) = hru(i)%lumv%bmp_sed
            end do

        case("hru_lumv%bmp_pp")
            print *, "HRU lumv%bmp_pp"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_pp
                floatBuffer(i) = hru(i)%lumv%bmp_pp
            end do

        case("hru_lumv%bmp_sp")
            print *, "HRU lumv%bmp_sp"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_sp
                floatBuffer(i) = hru(i)%lumv%bmp_sp
            end do

        case("hru_lumv%bmp_pn")
            print *, "HRU lumv%bmp_pn"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_pn
                floatBuffer(i) = hru(i)%lumv%bmp_pn
            end do

        case("hru_lumv%bmp_sn")
            print *, "HRU lumv%bmp_sn"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_sn
                floatBuffer(i) =hru(i)%lumv%bmp_sn
            end do

        case("hru_lumv%bmp_bac")
            print *, "HRU lumv%bmp_bac"
            allocate(floatBuffer(size(hru)))
            allocate(intBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%lumv%bmp_bac
                floatBuffer(i) = hru(i)%lumv%bmp_bac
            end do

        case("luse")
            print *, "HRU luse: "
            allocate(intBuffer(size(hru)))
            allocate(floatBuffer(0))
            do i= 1,size(hru)
                print *, hru(i)%luse%name
                print *, hru(i)%dbsc%land_use_mgt
                print *, hru(i)%dbs%land_use_mgt
                intBuffer(i) = hru(i)%dbs%land_use_mgt
            end do
        !type (landuse) :: luse

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
            print *, "Sending int buffer: ", intBuffer
            print *, "int buffer size: ", SIZE(intBuffer)
            if(.not.allocated(floatBuffer)) allocate(floatBuffer(0))
        else
            print *, "Sending float buffer: ", floatBuffer
            if(.not.allocated(intBuffer)) allocate(intBuffer(0))
        end if

        if(shapeBuffer == "")then
            shapeBuffer = "-1" // char(0)
        else
            shapeBuffer = shapeBuffer // char(0)
        end if

        call sendr(cliente_obj, intBuffer, floatBuffer, shapeBuffer, size(intBuffer), size(floatBuffer))

        call recibe()

    end subroutine obtener

end module tinamit_module