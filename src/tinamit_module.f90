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
            print *, "In evaluar"
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
            print *, "SD props before: ", sd_ch%props
            sd_ch%props = intBuffer
            print *, "SD props after: ", sd_ch%props

        case("sd_obj_no")
            !if(.not.allocated(sd_ch%obj_no)) allocate(sd_ch%obj_no(shape))
            print *, "SD obj_no before: ", sd_ch%obj_no
            sd_ch%obj_no = intBuffer
            print *, "SD obj_no after: ", sd_ch%obj_no

        case("sd_aqu_link")
            !aquifer the channel is linked to
            !if(.not.allocated(sd_ch%aqu_link)) allocate(sd_ch%aqu_link(shape))
            print *, "SD aqu_link before: ", sd_ch%aqu_link
            sd_ch%aqu_link = intBuffer
            print *, "SD aqu_link after: ", sd_ch%aqu_link

        case("sd_aqu_link_ch")
            !sequential channel number in the aquifer
            !if(.not.allocated(sd_ch%aqu_link_ch)) allocate(sd_ch%aqu_link_ch(shape))
            print *, "SD aqu_link_ch before: ", sd_ch%aqu_link_ch
            sd_ch%aqu_link_ch = intBuffer
            print *, "SD aqu_link_ch after: ", sd_ch%aqu_link_ch
        case("sd_chw")
            !m          |channel width
            !if(.not.allocated(sd_ch%chw)) allocate(sd_ch%chw(shape))
            print *, "SD chw before: ", sd_ch%chw
            sd_ch%chw = floatBuffer
            print *, "SD chw after: ", sd_ch%chw

        case("sd_chd")
            !m          |channel depth
            !if(.not.allocated(sd_ch%chd)) allocate(sd_ch%chd(shape))
            print *, "SD chd before: ", sd_ch%chd
            sd_ch%chd = floatBuffer
            print *, "SD chd after: ", sd_ch%chd

        case("sd_chs")
            !m/m        |channel slope
            !if(.not.allocated(sd_ch%chs)) allocate(sd_ch%chs(shape))
            print *, "SD chs before: ", sd_ch%chs
            sd_ch%chs = floatBuffer
            print *, "SD chs after: ", sd_ch%chs

        case("sd_chl")
            !km         |channel length
            !if(.not.allocated(sd_ch%chl)) allocate(sd_ch%chl(shape))
            print *, "SD chl before: ", sd_ch%chl
            sd_ch%chl = floatBuffer
            print *, "SD chl after: ", sd_ch%chl

        case("sd_chn")
            !           |channel Manning's n
            !if(.not.allocated(sd_ch%chn)) allocate(sd_ch%chn(shape))
            print *, "SD chn before: ", sd_ch%chn
            sd_ch%chn = floatBuffer
            print *, "after: ", sd_ch%chn

        case("sd_cov")
            !0-1        |channel cover factor
            !if(.not.allocated(sd_ch%cov)) allocate(sd_ch%cov(shape))
            print *, "SD cov before: ", sd_ch%cov
            sd_ch%cov = floatBuffer
            print *, "after: ", sd_ch%cov

        case("sd_cherod")
            !           |channel erodibility
            !if(.not.allocated(sd_ch%cherod)) allocate(sd_ch%cherod(shape))
            print *, "SD cherod before: ", sd_ch%cherod
            sd_ch%cherod = floatBuffer
            print *, "after: ", sd_ch%cherod

        case("sd_shear_bnk")
            !0-1        |bank shear coefficient - fraction of bottom shear
            !if(.not.allocated(sd_ch%shear_bnk)) allocate(sd_ch%shear_bnk(shape))
            print *, "SD shear_bnk before: ", sd_ch%shear_bnk
            sd_ch%shear_bnk = floatBuffer
            print *, "after: ", sd_ch%shear_bnk

        case("sd_hc_erod")
            !           |headcut erodibility
            !if(.not.allocated(sd_ch%hc_erod)) allocate(sd_ch%hc_erod(shape))
            print *, "SD hc_erod before: ", sd_ch%hc_erod
            sd_ch%hc_erod = floatBuffer
            print *, "after: ", sd_ch%hc_erod

        case("sd_hc_co")
            !m/m        |proportionality coefficient for head cut
            !if(.not.allocated(sd_ch%hc_co)) allocate(sd_ch%hc_co(shape))
            print *, "SD hc_co before: ", sd_ch%hc_co
            sd_ch%hc_co = floatBuffer
            print *, "after: ", sd_ch%hc_co

        case("sd_hc_len")
            !m          |length of head cut
            !if(.not.allocated(sd_ch%hc_len)) allocate(sd_ch%hc_len(shape))
            print *, "SD hc_len before: ", sd_ch%hc_len
            sd_ch%hc_len = floatBuffer
            print *, "after: ", sd_ch%hc_len

        case("sd_hc_hgt")
            !m          |headcut height
            !if(.not.allocated(sd_ch%hc_hgt)) allocate(sd_ch%hc_hgt(shape))
            print *, "SD hc_hgt before: ", sd_ch%hc_hgt
            sd_ch%hc_hgt = floatBuffer
            print *, "after: ", sd_ch%hc_hgt

        case("sd_stor")
            !m3         |water stored in reach at end of the day
            !if(.not.allocated(sd_ch%stor)) allocate(sd_ch%stor(shape))
            print *, "SD stor before: ", sd_ch%stor
            sd_ch%stor = floatBuffer
            print *, "after: ", sd_ch%stor

        case("sd_kd")
            !           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD kd is not yet supported"


        case("sd_aq_mix")
            ! m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
            print *, "SD aq mix is not yet supported"

!-----------Lite HRU Variables------------------------------------------------------------------------------------------
        case("lte_props")
            !if(.not.allocated(hlt%props)) allocate(hlt%props(shape))
            print *, "HLT props before: ", hlt%props
            hlt%props = intBuffer
            print *, "HLT props after: ", hlt%props

        case("lte_obj_no")
            !if(.not.allocated(hlt%obj_no)) allocate(hlt%obj_no(shape))
            print *, "HLT obj_no before: ", hlt%obj_no
            hlt%obj_no = intBuffer
            print *, "HLT obj_no after: ", hlt%obj_no

        case("lte_plant")
            !character(len=16) :: plant
            !              |plant type (as listed in plants.plt)
            print *, "lte_plant is not yet supported for transfer as it is a character value, use iplant instead."
            print *, "lte_plant will then be updated accordinly"

        case("lte_iplant")
            !              |plant number xwalked from hlt_db()%plant and plants.plt
            !if(.not.allocated(hlt%iplant)) allocate(hlt%iplant(shape))
            print *, "HLT iplant before: ", hlt%iplant
            hlt%iplant = intBuffer
            print *, "HLT iplant after: ", hlt%iplant

        case("lte_km2")
            !km^2          |drainage area
            !if(.not.allocated(hlt%km2)) allocate(hlt%km2(shape))
            print *, "HLT km2 before: ", hlt%km2
            hlt%km2 = floatBuffer
            print *, "after: ", hlt%km2

        case("lte_cn2")
            !              |condition II curve number (used in calibration)
            !if(.not.allocated(hlt%cn2)) allocate(hlt%cn2(shape))
            print *, "HLT cn2 before: ", hlt%cn2
            hlt%cn2 = floatBuffer
            print *, "after: ", hlt%cn2

        case("lte_cn3_swf")
            !none          |soil water factor for cn3 (used in calibration)
            !if(.not.allocated(hlt%cn3_swf)) allocate(hlt%cn3_swf(shape))
            print *, "HLT cn3_swf before: ", hlt%cn3_swf
            hlt%cn3_swf = floatBuffer
            print *, "after: ", hlt%cn3_swf

        case("lte_soildep")                                     !              |0 = fc; 1 = saturation (porosity)
            !mm            |soil profile depth
            !if(.not.allocated(hlt%soildep)) allocate(hlt%soildep(shape))
            print *, "HLT soildep before: ", hlt%soildep
            hlt%soildep = floatBuffer
            print *, "after: ", hlt%soildep

        case("lte_etco")
            !              |et coefficient - use with pet and aet (used in calibration)
            !if(.not.allocated(hlt%etco)) allocate(hlt%etco(shape))
            print *, "HLT etco before: ", hlt%etco
            hlt%etco = floatBuffer
            print *, "after: ", hlt%etco

        case("lte_revapc")
            !m/m           |revap from aquifer (used in calibration)
            !if(.not.allocated(hlt%revapc)) allocate(hlt%revapc(shape))
            print *, "HLT revapc before: ", hlt%revapc
            hlt%revapc = floatBuffer
            print *, "after: ", hlt%revapc

        case("lte_perco")
            !              |soil percolation coefficient (used in calibration)
            !if(.not.allocated(hlt%perco)) allocate(hlt%perco(shape))
            print *, "HLT perco before: ", hlt%perco
            hlt%perco = floatBuffer
            print *, "after: ", hlt%perco

        case("lte_tdrain")
            !hr            |design subsurface tile drain time (used in calibration)
            !if(.not.allocated(hlt%tdrain)) allocate(hlt%tdrain(shape))
            print *, "HLT tdrain before: ", hlt%tdrain
            hlt%tdrain = floatBuffer
            print *, "after: ", hlt%tdrain

        case("lte_stress")
            !frac          |plant stress - pest, root restriction, soil quality, nutrient,
            !if(.not.allocated(hlt%stress)) allocate(hlt%stress(shape))
            print *, "HLT stress before: ", hlt%stress
            hlt%stress = floatBuffer
            print *, "after: ", hlt%stress

        case("lte_uslefac")
            !              |USLE slope length factor
            !if(.not.allocated(hlt%uslefac)) allocate(hlt%uslefac(shape))
            print *, "HLT ulsefac before: ", hlt%uslefac
            hlt%uslefac = floatBuffer
            print *, "after: ", hlt%uslefac

        case("lte_wrt1")
            !if(.not.allocated(hlt%wrt1)) allocate(hlt%wrt1(shape))
            print *, "HLT wrt1 before: ", hlt%wrt1
            hlt%wrt1 = floatBuffer
            print *, "after: ", hlt%wrt1

        case("lte_wrt2")
            !if(.not.allocated(hlt%wrt2)) allocate(hlt%wrt2(shape))
            print *, "HLT wrt2 before: ", hlt%wrt2
            hlt%wrt2 = floatBuffer
            print *, "after: ", hlt%wrt2

        case("lte_smx")
            !if(.not.allocated(hlt%smx)) allocate(hlt%smx(shape))
            print *, "HLT smx before: ", hlt%smx
            hlt%smx = floatBuffer
            print *, "after: ", hlt%smx

        case("lte_hk")
            !if(.not.allocated(hlt%hk)) allocate(hlt%hk(shape))
            print *, "HLT hk before: ", hlt%hk
            hlt%hk = floatBuffer
            print *, "after: ", hlt%hk

        case("lte_yls")
            !if(.not.allocated(hlt%yls)) allocate(hlt%yls(shape))
            print *, "HLT yls before: ", hlt%yls
            hlt%yls = floatBuffer
            print *, "after: ", hlt%yls

        case("lte_ylc")
            !if(.not.allocated(hlt%ylc)) allocate(hlt%ylc(shape))
            print *, "HLT ylc before: ", hlt%ylc
            hlt%ylc = floatBuffer
            print *, "after: ", hlt%ylc

        case("lte_awc")
            !mm/mm        |available water capacity of soil
            !if(.not.allocated(hlt%awc)) allocate(hlt%awc(shape))
            print *, "HLT awc before: ", hlt%awc
            hlt%awc = floatBuffer
            print *, "after: ", hlt%awc

        case("lte_g")
            !if(.not.allocated(hlt%g)) allocate(hlt%g(shape))
            print *, "HLT g before: ", hlt%g
            hlt%g = floatBuffer
            print *, "after: ", hlt%g

        case("lte_hufh")
            !if(.not.allocated(hlt%hufh)) allocate(hlt%hufh(shape))
            print *, "HLT hufh before: ", hlt%hufh
            hlt%hufh = floatBuffer
            print *, "after: ", hlt%hufh

        case("lte_phu")
            !if(.not.allocated(hlt%phu)) allocate(hlt%phu(shape))
            print *, "HLT phu before: ", hlt%phu
            hlt%phu = floatBuffer
            print *, "after: ", hlt%phu

        case("lte_por")
            !if(.not.allocated(hlt%por)) allocate(hlt%por(shape))
            print *, "HLT por before: ", hlt%por
            hlt%por = floatBuffer
            print *, "after: ", hlt%por

        case("lte_sc")
            !if(.not.allocated(hlt%sc)) allocate(hlt%sc(shape))
            print *, "HLT sc before: ", hlt%sc
            hlt%sc = floatBuffer
            print *, "after: ", hlt%sc

        case("lte_sw")
            !mm/mm         |initial soil water storage
            !if(.not.allocated(hlt%sw)) allocate(hlt%sw(shape))
            print *, "HLT sw before: ", hlt%sw
            hlt%sw = floatBuffer
            print *, "after: ", hlt%sw

        case("lte_gw")
            !mm            |initial shallow aquifer storage
            !if(.not.allocated(hlt%gw)) allocate(hlt%gw(shape))
            print *, "HLT gw before: ", hlt%gw
            hlt%gw = floatBuffer
            print *, "after: ", hlt%gw

        case("lte_snow")
            !mm            |initial water content of snow
            !if(.not.allocated(hlt%snow)) allocate(hlt%snow(shape))
            print *, "HLT snow before: ", hlt%snow
            hlt%snow = floatBuffer
            print *, "after: ", hlt%snow

        case("lte_gwflow")
            !mm            |initial groundwater flow
            !if(.not.allocated(hlt%gwflow)) allocate(hlt%gwflow(shape))
            print *, "HLT gwflow before: ", hlt%gwflow
            hlt%gwflow = floatBuffer
            print *, "after: ", hlt%gwflow

        case("lte_dm")
            !t/ha          |plant biomass
            !if(.not.allocated(hlt%dm)) allocate(hlt%dm(shape))
            print *, "HLT dm before: ", hlt%dm
            hlt%dm = floatBuffer
            print *, "after: ", hlt%dm

        case("lte_alai")
            !              |leaf area index
            !if(.not.allocated(hlt%alai)) allocate(hlt%alai(shape))
            print *, "HLT alai before: ", hlt%alai
            hlt%alai = floatBuffer
            print *, "after: ", hlt%alai

        case("lte_yield")
            !t/ha          |plant yield
            !if(.not.allocated(hlt%yield)) allocate(hlt%yield(shape))
            print *, "HLT yield before: ", hlt%yield
            hlt%yield = floatBuffer
            print *, "after: ", hlt%yield

        case("lte_npp")
            !t/ha          |net primary productivity
            !if(.not.allocated(hlt%npp)) allocate(hlt%npp(shape))
            print *, "HLT npp before: ", hlt%npp
            hlt%npp = floatBuffer
            print *, "after: ", hlt%npp

        case("lte_lai_mx")
            !              |maximum leaf area index
            !if(.not.allocated(hlt%lai_mx)) allocate(hlt%lai_mx(shape))
            print *, "HLT lai_mx before: ", hlt%lai_mx
            hlt%lai_mx = floatBuffer
            print *, "after: ", hlt%lai_mx

        case("lte_gwdeep")
            !mm            |deep aquifer storage
            !if(.not.allocated(hlt%gwdeep)) allocate(hlt%gwdeep(shape))
            print *, "HLT gwdeep before: ", hlt%gwdeep
            hlt%gwdeep = floatBuffer
            print *, "after: ", hlt%gwdeep

        case("lte_aet")
            !mm            |sum of actual et during growing season (for hi water stress)
            !if(.not.allocated(hlt%aet)) allocate(hlt%aet(shape))
            print *, "HLT aet before: ", hlt%aet
            hlt%aet = floatBuffer
            print *, "after: ", hlt%aet

        case("lte_pet")
            !mm            |sum of potential et during growing season (for hi water stress)
            !if(.not.allocated(hlt%pet)) allocate(hlt%pet(shape))
            print *, "HLT pet before: ", hlt%pet
            hlt%pet = floatBuffer
            print *, "after: ", hlt%pet

        case("lte_start")
            !if(.not.allocated(hlt%start)) allocate(hlt%start(shape))
            print *, "HLT start before: ", hlt%start
            hlt%start = intBuffer
            print *, "HLT start after: ", hlt%start

        case("lte_end")
            !if(.not.allocated(hlt%end)) allocate(hlt%end(shape))
            print *, "HLT end before: ", hlt%end
            hlt%end = intBuffer
            print *, "HLT end after: ", hlt%end

!-------Channel Variables-----------------------------------------------------------------------------------------------
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            do i = 1, size(ch)
                ch(i)%algae = floatBuffer(i)
            end do
            print *, "algae : ", ch(:)%algae

        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            do i = 1, size(ch)
                ch(i)%flwin = floatBuffer(i)
            end do
            print *, "flwin: ", ch(:)%flwin

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
        print *, "size(hru): ", size(hru)
        print *, "size(hlt): ", size(hlt)
        print *, "size(ch): ", size(ch)
        print *, "size(sd_ch)", size(sd_ch)

        if(allocated(intBuffer)) deallocate(intBuffer)
        if(allocated(floatBuffer)) deallocate(floatBuffer)

        select case (trim(varNombre))

!--------Calibration/Initialization-------------------------------------------------------------------------------------

!-----------Landuse Variables-------------------------------------------------------------------------------------------


!-----------Water flow, contaminants, (P o and ao then N, K)------------------------------------------------------------
            !ch(:)%
        CASE("t")
            print *, "current day: ", t
            allocate(intBuffer(1))
            allocate(floatBuffer(0))
            intBuffer = t

!-----------SD-Channel Variables----------------------------------------------------------------------------------------
        case("sd_props")
            print *, "SD channel props: ", sd_ch%props
            allocate(intBuffer(size(sd_ch%props)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%props

        case("sd_obj_no")
            print *, "SD channel obj_no: ", sd_ch%obj_no
            allocate(intBuffer(size(sd_ch%obj_no)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%obj_no

        case("sd_aqu_link")
            !aquifer the channel is linked to
            print *, "SD channel aqu link: ", sd_ch%aqu_link
            allocate(intBuffer(size(sd_ch%aqu_link)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%aqu_link

        case("sd_aqu_link_ch")
            !sequential channel number in the aquifer
            print *, "SD channel props: ", sd_ch%aqu_link_ch
            allocate(intBuffer(size(sd_ch%aqu_link_ch)))
            allocate(floatBuffer(0))
            intBuffer = sd_ch%aqu_link_ch
        case("sd_chw")
            !m          |channel width
            print *, "SD chw: ", sd_ch%chw
            allocate(floatBuffer(size(sd_ch%chw)))
            allocate(intBuffer(0))
            floatBuffer = sd_ch%chw

        case("sd_chd")
            !m          |channel depth
            print *, "SD chd: ", sd_ch%chd
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
            !character(len=16) :: plant
            !              |plant type (as listed in plants.plt)
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
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%li
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("orgn")            !               |organic nitrogen contribution from channel erosion
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%orgn
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("orgp")            !               |organic phosphorus contribution from channel erosion
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%orgp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("si")              !(m/n)          |slope of main channel
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%si
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("wi")              !(m)            |width of main channel at top of bank
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%wi
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("di")              !(m)            |depth of main channel from top of bank to bottom
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%di
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("chlora")          ! mg chl-a/L    |chlorophyll-a concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%chlora
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("pst_conc")        ! mg/(m**3)     |initial pesticide concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%pst_conc
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("dep_chan")        ! m             |average daily water depth in channel
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%dep_chan
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("disolvp")         ! mg P/L        |dissolved P concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%disolvp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("drift")           ! kg            |amount of pesticide drifting onto main channel in subbasin
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%drift
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%flwin
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("flwout")          ! m^3 H2O       |flow out of reach on previous day
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%flwout
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("nitraten")        ! mg N/L        |nitrate concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%nitraten
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("nitriten")        ! mg N/L        |nitrite concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%nitriten
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("organicn")        ! mg N/L        |organic nitrogen concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%organicn
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("organicp")        ! mg P/L        |organic phosphorus concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%organicp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rch_bactlp")      ! # cfu/100ml   |less persistent bacteria stored in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_bactlp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rch_bactp")       ! # cfu/100ml   |persistent bacteria stored in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_bactp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rch_cbod")        ! mg O2/L       |carbonaceous biochemical oxygen demand in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_cbod
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rch_dox")         ! mg O2/L       |dissolved oxygen concentration in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_dox
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rchstor")         ! m^3 H2O       |water stored in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rchstor
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("sedst")           ! metric tons   |amount of sediment stored in reach
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%sedst
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("vel_chan")        ! m/s           |average flow velocity in channel
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%vel_chan
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("bed_san")

        CASE("bed_sil")

        CASE("bed_cla")

        CASE("bed_gra")

        CASE("bnk_san")

        CASE("bnk_sil")

        CASE("bnk_cla")

        CASE("bnk_gra")

        CASE("depfp")

        CASE("depprfp")

        CASE("depsilfp")

        CASE("depclafp")

        CASE("depch")

        CASE("depprch")

        CASE("depsanch")

        CASE("depsilch")

        CASE("depclach")

        CASE("depsagch")

        CASE("deplagch")

        CASE("depgrach")

        CASE("sanst")

        CASE("silst")

        CASE("clast")

        CASE("sagst")

        CASE("lagst")

        CASE("grast")

        CASE("wattemp")

        CASE("bactp")

        CASE("chfloodvol")

        CASE("bactlp")

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

    subroutine StripSpaces(string)
    character(len=*) :: string
    integer :: stringLen
    integer :: last, actual

    stringLen = len (string)
    last = 1
    actual = 1

    do while (actual < stringLen)
        if (string(last:last) == ' ') then
            actual = actual + 1
            string(last:last) = string(actual:actual)
            string(actual:actual) = ' '
        else
            last = last + 1
            if (actual < last) &
                actual = last
        endif
    end do

    end subroutine

end module tinamit_module