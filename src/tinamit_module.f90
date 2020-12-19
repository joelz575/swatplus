module tinamit_module

    use landuse_data_module
    use hru_module, ONLY : hru
    use hru_lte_module, ONLY : hlt
    use channel_module, ONLY : ch
    use sd_channel_module, ONLY : sd_ch

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

        if(orden == 'cerrar')then
            call closesock(cliente_obj)
            print *, "The socket was successfully closed"
            dynamic = .false.

        elseif(trim(orden) == 'incr')then
            dias = nPasos
            print *, "Number of Passes: ", nPasos
            !No further action required

        elseif(orden == 'cambiar')then
            call tomar (var, shape, intBuffer, realBuffer)

        elseif(orden == 'leer') then
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
            !if(.not.allocated(sd_ch%props)) allocate(sd_ch%props(shape))
            sd_ch%props = intBuffer

        case("sd_obj_no")
            !if(.not.allocated(sd_ch%obj_no)) allocate(sd_ch%obj_no(shape))
            sd_ch%obj_no = intBuffer

        case("sd_aqu_link")
            !aquifer the channel is linked to
            !if(.not.allocated(sd_ch%aqu_link)) allocate(sd_ch%aqu_link(shape))
            sd_ch%aqu_link = intBuffer

        case("sd_aqu_link_ch")
            !sequential channel number in the aquifer
            !if(.not.allocated(sd_ch%aqu_link_ch)) allocate(sd_ch%aqu_link_ch(shape))
            sd_ch%aqu_link_ch = intBuffer

        case("sd_chw")
            !m          |channel width
            !if(.not.allocated(sd_ch%chw)) allocate(sd_ch%chw(shape))
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

        CASE default
            print *, "Unused variable: ", variable_Name
        end select

        call recibe()
    end subroutine tomar

    subroutine obtener (varNombre)
        character(*) :: varNombre
        character(len = :), allocatable :: senderBuffer, shapeBuffer
        integer, dimension(:), allocatable :: intBuffer
        real, dimension(:), allocatable :: floatBuffer
        character(len = 16) :: temp_shapeBuffer, temp_senderBuffer
        senderBuffer = ""
        shapeBuffer = ""

        print *, "Var nombre in obtener: ", varNombre
        !print *, "size(hru): ", size(hru)
        !print *, "size(hlt): ", size(hlt)
        !print *, "size(ch): ", size(ch)
        !print *, "size(sd_ch)", size(sd_ch)

        if(allocated(intBuffer)) deallocate(intBuffer)
        if(allocated(floatBuffer)) deallocate(floatBuffer)

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

        case("hru_lum_group")
            print *, "HRU lum_group: ", hru%lum_group
            allocate(intBuffer(size(hru%lum_group)))
            allocate(floatBuffer(0))
            intBuffer = hru%lum_group

        !    character(len=16) :: lum_group_c        !land use group for soft cal and output

        !    character(len=16) :: region

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

        !type (topography) :: topo
        !type (field) :: field
        !type (hydrology) :: hyd
        !type (landuse) :: luse
        !type (land_use_mgt_variables) :: lumv
        !type (subsurface_drainage_parameters) :: sdr
        !type (snow_parameters) :: sno
        !type (hru_databases) :: dbs             !database pointers
        !type (hru_databases_char) :: dbsc       !database pointers
        !type (hru_parms_db) :: parms            !calibration parameters

        CASE default
            print *, "Unknown variable: ", varNombre, " checking whether it is a testing variable..."

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
            shapeBuffer = "  "
        end if
        call sendr(cliente_obj, intBuffer, floatBuffer, trim(shapeBuffer), size(intBuffer), size(floatBuffer), len(shapeBuffer))

        call recibe()

    end subroutine obtener

end module tinamit_module