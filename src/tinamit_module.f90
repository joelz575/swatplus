module tinamit_module

    use landuse_data_module
    use hru_module, ONLY : hru
    use hru_lte_module, ONLY : hlt
    use channel_module, ONLY : ch
    use sd_channel_module, ONLY : sd_ch

    save
    integer cliente_obj
    character(len = 3) :: hru_mode  ! 'hru' or 'hlt'
    character(len = 3) :: cha_mode  ! 'cha' or 'sdc'
    integer :: Lluvia = 348 !temporary variable for debugging
    integer :: Bosques = 67 !temporary variables for debugging
    logical dynamic
    integer :: dias = 1
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
        !consider calling recibe to obtain initial values
    end subroutine

    subroutine recibe (cliente_obj)
        integer cliente_obj
        !character, dimension(:, :), allocatable :: charBuffer
        character(len = 6) :: var, command
        character(len = 3) :: tipo_contents
        integer tmn_contents
        real, allocatable, dimension(:) :: realBuffer(:)
        integer, allocatable, dimension(:) :: intBuffer(:)

        print *, "About to Recieve..."
        print *, "Cliente Obj: ", cliente_obj
        print *, "Variable Name: ", var
        print *, "Content Data Type: ", tipo_contents
        print *, "Size of contents: ", tmn_contents
        print *, "intBuffer contents: ", intBuffer
        print *, "realBuffer contents: ", realBuffer

        call receive (cliente_obj, var, tipo_contents, tmn_contents, intBuffer, realBuffer) !charBuffer
        tmn_contents = INT(tmn_contents)
        allocate(intBuffer(tmn_contents))
        allocate(realBuffer(tmn_contents))
        print *, "Cliente Obj: ", cliente_obj
        print *, "Variable Name: ", var
        print *, "Content Data Type: ", tipo_contents
        print *, "Size of contents: ", tmn_contents
        !print *, "intBuffer contents: ", intBuffer
        !print *, "realBuffer contents: ", realBuffer

        call evaluar(cliente_obj, command, var, tmn_contents, intBuffer, realBuffer)

    end subroutine recibe

    subroutine evaluar (cliente_obj, orden, var, tmn_contents, intBuffer, realBuffer)

        character(len = :), allocatable :: senderBuffer
        character(*) :: var, orden
        integer :: tmn_contents, cliente_obj
        integer, dimension(tmn_contents) :: intBuffer
        real, dimension(tmn_contents) :: realBuffer

        print *, "Command: ", orden

        if(orden == 'CERRAR')then
            call closeSock(cliente_obj)
            stop
            stop
            stop
        end if

        if(orden == 'CORRER')then
            senderBuffer = "running"
            call sendr(cliente_obj, senderBuffer)
        end if

        if(trim(orden) == "TOMAR_")then
            call tomar (cliente_obj, var, tmn_contents, intBuffer, realBuffer)
        end if

        if(orden == 'DAR___') then
            call obtener (cliente_obj, var)
        end if
    end subroutine evaluar

    subroutine tomar (cliente_obj, variable_Name, variable_Length, intBuffer, realBuffer)

        character (*) :: variable_Name
        character(len = :), allocatable :: senderBuffer
        integer :: variable_Length, cliente_obj, index, i
        integer, dimension(variable_Length) :: intBuffer
        real, dimension(variable_Length) :: realBuffer
        integer :: f = 1
        real, allocatable, dimension(:) :: variable(:)

        variable_Name = trim(variable_Name)
        index = 0

        if (size(hru) > 1) then
            do i = 1, size(hru)
                print *, "hru ", i, " landuse: ", hru(i)%luse
            end do

        else
            print *, "hlt ", hlt(:)
            print *, "hlt%cn2 ", hlt(:)%cn2
            print *, "hlt%lsu ", hlt(:)%lsu
        end if

        if (size(ch) > 0) then
            print *, "ch(:): ", ch(:)

        else
            print *, "size(sd_ch): ", size(sd_ch)
            do f = 1, size(sd_ch)
                print *, "sd_ch(", f, "): ", sd_ch(f)%aqu_link_ch

            end do
        end if

        select case (trim(variable_Name))
            !landuse

            !water flow, contaminants, (P o and ao then N, K)
            !ch(:)%
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            do i = 1, size(ch)
                ch(i)%algae = variable(i)

            end do
            print *, "algae: ", ch(:)%algae

        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            do i = 1, size(ch)
                ch(i)%flwin = variable(i)
                print *, "flwin: ", ch(:)%flwin
            end do

        CASE("Lluvia")           ! test variable for debugging
            Lluvia = variable(1)
            print *, "Lluvia: ", Lluvia

        CASE("Bosques")           ! test variable for debugging

            Bosques = variable(1)
            print *, "Bosques: ", Bosques

        CASE default
            print *, "Unknown variable: ", variable_Name

        end select

        senderBuffer = "recvd"
        print *, "About to send recvd"

        call sendr(cliente_obj, senderBuffer)

        call recibe(cliente_obj)
    end subroutine tomar

    subroutine obtener (cliente_obj, varNombre)
        integer :: cliente_obj
        character (*) :: varNombre
        character(len = :), allocatable :: senderBuffer
        character(len = 32) :: temp_senderBuffer

        select case (trim(varNombre))

            !calibration/initialization
        CASE("hru_cha_mod")

            senderBuffer = "["

            print *, "size(hru): ", size(hru)
            print *, "size(hlt): ", size(hlt)

            if (size(hru) > 1) then
                senderBuffer = senderBuffer // 'hru '
            else
                senderBuffer = senderBuffer // 'hlt'
            end if

            print *, "size(ch): ", size(ch)
            print *, "size(sd_ch)", size(sd_ch)

            if (size(ch) > 0) then
                senderBuffer = senderBuffer // 'cha];'
            else
                senderBuffer = senderBuffer // 'sdc];'
            end if
            !landuse variables

            !water flow, contaminants, (P o and ao then N, K)
            !ch(:)%
        CASE("algae")           ! mg alg/L      |algal biomass concentration in reach
            senderBuffer = "["

            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%algae
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // ' ];'
        CASE("ammonian")        ! mg N/L        |ammonia concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%ammonian
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("bankst")          ! m^3 H2O       |bank storage
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%bankst
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("li")              ! km            |initial length of main channel
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%li
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("orgn")            !               |organic nitrogen contribution from channel erosion
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%orgn
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("orgp")            !               |organic phosphorus contribution from channel erosion
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%orgp
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("si")              !(m/n)          |slope of main channel
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%si
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("wi")              !(m)            |width of main channel at top of bank
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%wi
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("di")              !(m)            |depth of main channel from top of bank to bottom
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%di
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("chlora")          ! mg chl-a/L    |chlorophyll-a concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%chlora
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("pst_conc")        ! mg/(m**3)     |initial pesticide concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%pst_conc
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("dep_chan")        ! m             |average daily water depth in channel
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%dep_chan
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("disolvp")         ! mg P/L        |dissolved P concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%disolvp
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("drift")           ! kg            |amount of pesticide drifting onto main channel in subbasin
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%drift
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("flwin")           ! m^3 H2O       |flow into reach on previous day
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%flwin
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("flwout")          ! m^3 H2O       |flow out of reach on previous day
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%flwout
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("nitraten")        ! mg N/L        |nitrate concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%nitraten
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("nitriten")        ! mg N/L        |nitrite concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%nitriten
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("organicn")        ! mg N/L        |organic nitrogen concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%organicn
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("organicp")        ! mg P/L        |organic phosphorus concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%organicp
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("rch_bactlp")      ! # cfu/100ml   |less persistent bacteria stored in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_bactlp
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("rch_bactp")       ! # cfu/100ml   |persistent bacteria stored in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_bactp
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("rch_cbod")        ! mg O2/L       |carbonaceous biochemical oxygen demand in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_cbod
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("rch_dox")         ! mg O2/L       |dissolved oxygen concentration in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rch_dox
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("rchstor")         ! m^3 H2O       |water stored in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%rchstor
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("sedst")           ! metric tons   |amount of sediment stored in reach
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%sedst
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
        CASE("vel_chan")        ! m/s           |average flow velocity in channel
            senderBuffer = "["
            do i = 1, size(ch)
                write(temp_senderBuffer, *) ch(i)%vel_chan
                senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            end do
            senderBuffer = senderBuffer // '];'
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
            senderBuffer = "["
            write(temp_senderBuffer, *) Lluvia
            senderBuffer = senderBuffer // trim(temp_senderBuffer) // ' '
            senderBuffer = senderBuffer // '];'

            print *, "Unknown variable: ", varNombre
        end select
        call sendr(cliente_obj, senderBuffer)
        print *, "Sent sender buffer: "
        call recibe(cliente_obj)
    end subroutine obtener

end module tinamit_module