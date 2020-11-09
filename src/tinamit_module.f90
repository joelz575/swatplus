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
    real, allocatable, dimension(:) :: decimal(:), decimal_negativo(:) !*******testing variable*******************************
    integer, allocatable,dimension(:,:,:) :: multidim(:,:,:) !*****************testing variable*******************************
    logical dynamic
    integer :: dias = 1
    integer :: t = 0
contains

    subroutine abre (arg1, arg2, cliente_obj)

        character(len = 32), intent(in) :: arg1, arg2
        character(len = 256) :: host_num
        integer cliente_obj
        integer :: port_num

        host_num = arg2
        write (*, *) "host_num= ", host_num
        READ(arg1, '(I5)') port_num
        print *, "port_Num = ", port_num

        !Opening a socket
        write(*, *) 'Opening Socket now...'
        CAll opensocket(port_num, host_num, cliente_obj)
        print *, "cliente_obj=", cliente_obj
        !print *, "calibration is running...."

        !calling recibe to obtain initial values
        !call recibe(cliente_obj)

    end subroutine

    subroutine recibe (cliente_obj)
        integer cliente_obj
        !character, dimension(:, :), allocatable :: charBuffer
        character(len = 7) :: command
        character(len = 10) :: var = "          "
        character(len = 5) :: tipo_contents
        integer :: tmn_contents, nPasos, i, shape
        character(len = MAX_BUFFER_LEN):: realBufferBuffer, intBufferBuffer
        real, allocatable, dimension(:) :: realBuffer(:)
        integer, allocatable, dimension(:) :: intBuffer(:)

        print *, "About to Recieve..."

        intBufferBuffer = " "
        realBufferBuffer = " "
        tmn_shape = 1
        call receive (cliente_obj, command, var, tipo_contents, nPasos, shape) !charBuffer

        print *, "Cliente Obj: ", cliente_obj
        print *, "Command: ", command
        if (command == "cambiar")then
            print *, "Variable Name: ", var
            print *, "Content Data Type: ", tipo_contents
            print*, "Shape of array: ", shape

            if(tipo_contents=="flt")then
                print *, "about to allocate realBuffer"
                allocate(realBuffer(shape))
                call recvfloat (cliente_obj, realBuffer, shape)

            elseif(tipo_contents=="int".or.tipo_contents=="int64")then
                print *, "about to allocate intBuffer"
                allocate(intBuffer(shape))
                print *, "allocated intBuffer, about to fill it, shape is: ", shape, ", intBuffer is: ", intBuffer, ", intBufferBuffer is: ", trim(intBufferBuffer)
                call recvint (cliente_obj, intBuffer, shape)
                !call jsons2intarray (intBuffer, shape, trim(intBufferBuffer))

            end if

        elseif (command == "leer")then

            print *, "Variable Name: ", var

        end if

        print *, "intBuffer contents: ", intBuffer
        print *, "realBuffer contents: ", realBuffer

        call evaluar(cliente_obj, command, var, tmn_contents, shape, intBuffer, realBuffer, nPasos)

    end subroutine recibe

    subroutine evaluar (cliente_obj, orden, var, tmn_contents, shape1, intBuffer, realBuffer, nPasos)

        character(len = :), allocatable :: senderBuffer
        character(*) :: var, orden
        integer :: cliente_obj, nPasos, t_final, tmn_contents
        integer :: shape1
        integer, dimension(tmn_contents) :: intBuffer
        real, dimension(tmn_contents) :: realBuffer

        print *, "Command: ", orden

        if(orden == 'cerrar')then
            call closesock(cliente_obj)
            print *, "The socket was successfully closed"
            dynamic = .false.

        elseif(trim(orden) == 'incr')then
            dias = nPasos
            print *, "Number of Passes: ", nPasos
            !No further action required

        elseif(trim(orden) == 'cambiar')then
            call tomar (cliente_obj, var, tmn_contents, shape1, intBuffer, realBuffer)

        elseif(trim(orden) == 'leer') then
            call obtener (cliente_obj, var)

        else
            print *, "The command: ", trim(orden), "is not recognized"

        end if
    end subroutine evaluar

    subroutine tomar (cliente_obj, variable_Name, variable_Length, shape1, intBuffer, realBuffer)
        character (*) :: variable_Name
        character(len = :), allocatable :: senderBuffer
        integer :: cliente_obj, index, i, variable_Length
        integer :: shape1
        integer, dimension(variable_Length) :: intBuffer
        real, dimension(variable_Length) :: realBuffer
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
                    allocate(entero(shape1))
                    entero = intBuffer
                case("decimal")
                    allocate(decimal(shape1))
                    decimal = realBuffer
                case("entero negativo")
                    allocate(entero_negativo(shape1))
                    entero_negativo = intBuffer
                case("decimal negativo")
                    allocate(decimal_negativo(shape1))
                    decimal_negativo = realBuffer
                case("multidim")
                    !allocate(multidim(shape))
                    print *, "Multidimensional Arrays are not yet supported"
                case default
                    print *, variable_Name, " is not a testing variable, this variable is not used by the simulation."
            end select


        end select

        call recibe(cliente_obj)
    end subroutine tomar

    subroutine obtener (cliente_obj, varNombre)
        character(*) :: varNombre
        integer :: cliente_obj
        logical :: warning = .FALSE.
        character(len = :), allocatable :: senderBuffer, shapeBuffer
        integer, dimension(:), allocatable :: intBuffer
        real, dimension(:), allocatable :: floatBuffer
        character(len = 16) :: temp_shapeBuffer, temp_senderBuffer
        senderBuffer = ""
        shapeBuffer = ""


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
            write(temp_shapeBuffer, *) shape(t)
            allocate(intBuffer(1))
            call StripSpaces(temp_shapeBuffer)
            shapeBuffer = shapeBuffer // trim(temp_shapeBuffer)
            print *, "Current shape string: ", shapeBuffer
            intBuffer = t
            print *, "Current number of days in simulation: ", intBuffer


        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            warning = (size(ch) < 1)
            allocate(floatBuffer(size(ch)))
            floatBuffer = ch%algae
            print *, "current floatBuffer: ", floatBuffer

        CASE("ammonian")        ! mg N/L        |ammonia concentration in reach
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%ammonian
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
        CASE("bankst")          ! m^3 H2O       |bank storage
            warning = (size(ch) < 1)
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%bankst
                call StripSpaces(temp_senderBuffer)
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ']'
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
            !temporary section for debugging
            intBuffer = Lluvia
            print *, "Unknown variable: ", varNombre, " sending Lluvia data"
        end select

        if(warning)then
            print *, "Variable not used by SWAT+ simulation: ", varNombre

        elseif(.not.(SIZE(intBuffer)==0))then
            print *, "Sending int buffer: ", intBuffer
            print *, "int buffer size: ", SIZE(intBuffer)
            allocate(floatBuffer(0))
        else
            print *, "Sending float buffer: ", floatBuffer
            allocate(intBuffer(0))
        end if
        if(shapeBuffer == "")then
            shapeBuffer = "  "
        end if
        print *, "Int buffer is currently on line 564: ", intBuffer
        call sendr(cliente_obj, intBuffer, floatBuffer, trim(shapeBuffer), size(intBuffer), size(floatBuffer), len(shapeBuffer))

        call recibe(cliente_obj)

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