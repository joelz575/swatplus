module tinamit_module

    use landuse_data_module
    use hru_module, ONLY : hru
    use hru_lte_module, ONLY : hlt
    use channel_module, ONLY : ch
    use sd_channel_module, ONLY : sd_ch


    save
    integer :: MAX_BUFFER_LEN = 20000
    integer cliente_obj
    character(len = 3) :: hru_mode  ! 'hru' or 'hlt'
    character(len = 3) :: cha_mode  ! 'cha' or 'sdc'
    integer, allocatable, dimension(:) :: entero(:), entero_negativo(:)!*******testing variable*******************************
    real, allocatable, dimension(:) :: decimal(:), decimal_negativo(:)!*******testing variable*******************************
    integer, dimension(5) :: leer_entero = [0,1,2,3,10]
    integer, dimension(10) :: leer_entero_negativo = [-10,-3,-2,-1,0,1,2,3,4,5]
    real, dimension(5) :: leer_decimal = [0.5,1.0,1.5,2.0,2.5]
    real, dimension(10) :: leer_decimal_negativo = [-2.5,-2.0,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,2.0]
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

    subroutine tomar (variable_Name, shape, intBuffer, realBuffer)
        character (*) :: variable_Name
        character(len = :), allocatable :: senderBuffer
        integer :: index, i
        integer :: shape
        integer, dimension(shape) :: intBuffer
        real, dimension(shape) :: realBuffer
        integer :: f = 1


        !integer, allocatable, dimension(:) :: entero, entero_negativo!***testing variable*******************************
        !real, allocatable, dimension(:) :: decimal, decimal_negativo !***testing variable*******************************
        !integer, allocatable,dimension(:,:,:) :: multidim !**************testing variable*******************************

        if (size(hru) > 1) then
            print *, "there are full hru's"
            !do i = 1, size(hru)
                !print *, "hru ", i, " landuse: ", hru(i)%luse
            !end do

        else
            print *, "there are only lite hru's"
            print *, "WARNING: LITE HRU'S DATA TRANSFER IS NOT SUPPORTED BY TINAMIT AT THE MOMENT, PLEASE USE FULL HRU'S"
            !print *, "hlt ", hlt(:)
            !print *, "hlt%cn2 ", hlt(:)%cn2
            !print *, "hlt%lsu ", hlt(:)%lsu
        end if

        if (size(ch) > 0) then
            print *, "There are full channels defined"
            !print *, "ch(:): ", ch(:)

        else
            print *, "there are only lite channels defined"
            print *, "WARNING: SD CHANNEL DATA TRANSFER IS NOT SUPPORTED BY TINAMIT AT THE MOMENT, PLEASE USE FULL CHANNELS"
            !print *, "size(sd_ch): ", size(sd_ch)
            !do f = 1, size(sd_ch)
            !    print *, "sd_ch(", f, "): ", sd_ch(f)%aqu_link_ch
            !end do
        end if

        select case (trim(variable_Name))
            !landuse
            !water flow, contaminants, (P o and ao then N, K)
            !ch(:)%
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            do i = 1, size(ch)
                ch(i)%algae = realBuffer(i)
            end do
            print *, "algae : ", ch(:)%algae

        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            do i = 1, size(ch)
                ch(i)%flwin = realBuffer(i)
            end do
            print *, "flwin: ", ch(:)%flwin

        CASE default
            print *, "Unused variable: ", variable_Name
            !----------Checking for testing variables-----------------------------!
            select case(trim(variable_Name))
                case("entero")
                    if(.not.allocated(entero)) allocate(entero(shape))
                    entero = intBuffer
                case("decimal")
                    if(.not.allocated(decimal)) allocate(decimal(shape))
                    decimal = realBuffer
                case("entero negativo")
                    if(.not.allocated(entero_negativo)) allocate(entero_negativo(shape))
                    entero_negativo = intBuffer
                case("decimal negativo")
                    if(.not.allocated(decimal_negativo)) allocate(decimal_negativo(shape))
                    decimal_negativo = realBuffer
                case("multidim")
                    !allocate(multidim(shape))
                    print *, "Multidimensional Arrays are not yet supported"
                case default
                    print *, variable_Name, " is not a testing variable, this variable is not used by the simulation."
            end select


        end select

        call recibe()
    end subroutine tomar

    subroutine obtener (varNombre)
        character(*) :: varNombre
        logical :: warning = .FALSE.
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
!        CASE("hru_cha_mod")
!            print *, "size(hru): ", size(hru)
!            print *, "size(hlt): ", size(hlt)
!            if (size(hru) > 1) then
!                senderBuffer = senderBuffer // 'hru '
!            else
!                senderBuffer = senderBuffer // 'hlt'
!            end if
!
!            print *, "size(ch): ", size(ch)
!            print *, "size(sd_ch)", size(sd_ch)
!
!            if (size(ch) > 0) then
!                senderBuffer = senderBuffer // 'cha]'
!            else
!                senderBuffer = senderBuffer // 'sdc]'
!            end if

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
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%li
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("orgn")            !               |organic nitrogen contribution from channel erosion
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%orgn
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("orgp")            !               |organic phosphorus contribution from channel erosion
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%orgp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("si")              !(m/n)          |slope of main channel
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%si
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("wi")              !(m)            |width of main channel at top of bank
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%wi
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("di")              !(m)            |depth of main channel from top of bank to bottom
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%di
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("chlora")          ! mg chl-a/L    |chlorophyll-a concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%chlora
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("pst_conc")        ! mg/(m**3)     |initial pesticide concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%pst_conc
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("dep_chan")        ! m             |average daily water depth in channel
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%dep_chan
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("disolvp")         ! mg P/L        |dissolved P concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%disolvp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("drift")           ! kg            |amount of pesticide drifting onto main channel in subbasin
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%drift
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%flwin
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("flwout")          ! m^3 H2O       |flow out of reach on previous day
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%flwout
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("nitraten")        ! mg N/L        |nitrate concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%nitraten
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("nitriten")        ! mg N/L        |nitrite concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%nitriten
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("organicn")        ! mg N/L        |organic nitrogen concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%organicn
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("organicp")        ! mg P/L        |organic phosphorus concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%organicp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rch_bactlp")      ! # cfu/100ml   |less persistent bacteria stored in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_bactlp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rch_bactp")       ! # cfu/100ml   |persistent bacteria stored in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_bactp
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("rch_cbod")        ! mg O2/L       |carbonaceous biochemical oxygen demand in reach
            warning = (size(ch) < 1)
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
            if(allocated(intBuffer)) deallocate(intBuffer)
            if(allocated(floatBuffer)) deallocate(floatBuffer)
            !----------Checking for testing variables-----------------------------!
            select case(trim(varNombre))
                case("entero")
                    print *, 'Testing variable entero detected: ', entero

                    allocate(intBuffer(size(entero)))
                    allocate(floatBuffer(0))
                    intBuffer = entero

                case("decimal")
                    print *, 'Testing variable decimal detected: ', decimal
                    allocate(floatBuffer(size(decimal)))
                    allocate(intBuffer(0))
                    floatBuffer = decimal

                case("entero negativo")
                    print *, 'Testing variable entero_negativo detected: ', entero_negativo
                    allocate(intBuffer(size(entero_negativo)))
                    allocate(floatBuffer(0))
                    intBuffer = entero_negativo

                case("decimal negativo")
                    print *, 'Testing variable decimal_negativo detected: ', decimal_negativo
                    allocate(floatBuffer(size(decimal_negativo)))
                    allocate(intBuffer(0))
                    floatBuffer = decimal_negativo

                case("multidim")
                    print *, "Multidimensional Arrays are not yet supported"

                case("leer entero")
                    print *, 'Testing variable leer_entero detected: ', leer_entero
                    allocate(intBuffer(size(leer_entero)))
                    allocate(floatBuffer(0))
                    intBuffer = leer_entero

                case("leer decimal")
                    print *, 'Testing variable leer_decimal detected: ', leer_decimal
                    allocate(floatBuffer(size(leer_decimal)))
                    allocate(intBuffer(0))
                    floatBuffer = leer_decimal

                case("leer entero negativo")
                    print *, 'Testing variable leer_entero_negativo detected: ', leer_entero_negativo
                    allocate(intBuffer(size(leer_entero_negativo)))
                    allocate(floatBuffer(0))
                    intBuffer = leer_entero_negativo

                case("leer decimal negativo")
                    print *, 'Testing variable leer_decimal_negativo detected: ', leer_decimal_negativo
                    allocate(floatBuffer(size(leer_decimal_negativo)))
                    allocate(intBuffer(0))
                    floatBuffer = leer_decimal_negativo

                case default
                    print *, trim(varNombre), " is not a testing variable and this variable is not used by the simulation."

            end select
        end select

        if(warning)then
            print *, "Variable not used by SWAT+ simulation: ", varNombre

        elseif(.not.(SIZE(intBuffer)==0))then
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