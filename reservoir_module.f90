      module reservoir_module

      use hydrograph_module
      use climate_parms
      use time_module
      
      !integer :: mres
      real :: resflwi, resflwo, ressedi, ressedo, sedcon, respesti
      real :: reactw, volatpst, setlpst, resuspst, difus, reactb, bury
      real :: solpesto, sorpesto, resev, ressep, respcp, resorgno
      real :: resorgpo, resno3o, resno2o, resnh3o, ressolpo, reschlao
      real :: resorgpc, ressolpc,resornc, resno3c, resno2c, resnh3c
      real :: ressa, tair_mx, tair_mn, tair_av

      type reservoir
        character(len=13) :: name = "default"
        character(len=3) :: typ = "nul"          !res=reservoir; hru=landscape impoundment; fpl=flood plain only (no impound)
        integer :: ob = 0                        !object number if reservoir object; hru number if hru object
        integer :: props = 0                     !points to res_dat
        real :: seci = 0              !m         !seci depth
      end type reservoir          
      type (reservoir), dimension(:),allocatable :: res_ob

      type res_output
          real :: flowi = 0.     !m^3/s                |flow into res
          real :: flowo = 0.     !m^3/s                |flow out of res          
          real :: sedi = 0.      !metric tons          |sed entering res
          real :: sedo = 0.      !metric tons          |sed leaving res            
          real :: sedcon = 0.    !mg/L                 |sed conc in res
          real :: pesti = 0.     !mg pst               |pest entering res
          real :: reactw = 0.    !mg pst               |pest lost from res through reactions
          real :: volatpst = 0.  !mg pst               |pest lost from res through volatilization
          real :: setlpst = 0.   !mg pst               |pest moving from water to sed through settling
          real :: resuspst = 0.  !mg pst               |pest moving from sed to water through resuspension
          real :: difus = 0.     !mg pst               |pest moving from water to sed through diffusion
          real :: reactb = 0.    !mg pst               |pest lost from res sed layer through reactions
          real :: bury = 0.      !mg pst               |pest lost from res sed layer through burial
          real :: pesto = 0.     !mg pst               |pest transported out of res
          real :: pstcon = 0.    !mg pst/m^3           |pest conc in res water
          real :: spstcon = 0.   !mg pst/m^3           |pest conc in res sed layer
          real :: ev = 0.        !m^3 H2O              |evap from res 
          real :: sep = 0.       !m^3 H2O              |seepage from res
          real :: pcp = 0.       !m^3 H2O              |precipitation on res 
          real :: flwim3 = 0.    !m^3 H2O              |water flowing into res 
          real :: flwom3 = 0.    !m^3 H2O              |water flowing out of res 
          real :: orgni = 0.     !kg N                 |org N entering res
          real :: orgno = 0.     !kg N                 |org N leaving res
          real :: orgpi = 0.     !kg P                 |org P entering res
          real :: orgpo = 0.     !kg P                 |org P leaving res
          real :: no3i = 0.      !kg N                 |nitrate N entering res
          real :: no3o = 0.      !kg N                 |nitrate N leaving res
          real :: no2i = 0.      !kg N                 |nitrite entering res
          real :: no2o = 0.      !kg N                 |nitrite leaving res
          real :: nh3i = 0.      !kg N                 |ammonia entering res
          real :: nh3o = 0.      !kg N                 |ammonia leaving res
          real :: solpi = 0.     !kg P                 |mineral P entering res
          real :: solpo = 0.     !kg P                 |mineral P leaving res
          real :: chlai = 0.     !kg chla              |chlorophyll-a entering res 
          real :: chlao = 0.     !kg chla              |chlorophyll-a leaving res 
          real :: orgpc = 0.     !mg P/L               |ave org P conc in res
          real :: solpc = 0.     !mg P/L               |ave sol P conc in res
          real :: orgnc = 0.     !mg N/L               |ave org N in res
          real :: no3c = 0.      !mg N/L               |ave nitrate conc in res
          real :: no2c = 0.      !mg N/L               |ave nitrite conc in res
          real :: nh3c = 0.      !mg N/L               |ave ammonia conc in res
      end type res_output
      
      type (res_output), dimension(:), allocatable, save :: resd
      type (res_output), dimension(:), allocatable, save :: resm
      type (res_output), dimension(:), allocatable, save :: resy
      type (res_output), dimension(:), allocatable, save :: resa
      type (res_output) :: resadd1
      type (res_output) :: resadd2
      type (res_output) :: resadd3
      
!!      type (output_waterbal) :: resmz
      type (res_output) :: resmz
      
      type res_header
          character (len=6) :: yrs =          ' time '
          character (len=6) :: yrc =          ' year '
          character (len=8) :: j =            ' resnum '
          character (len=10) :: flowi =    '    flowi'        !m^3/s                |flow into res
          character (len=10) :: flowo =    '    flowo'        !m^3/s                |flow out of res          
          character (len=10) :: sedi =     '     sedi'        !metric tons          |sed entering res
          character (len=10) :: sedo =     '     sedo'        !metric tons          |sed leaving res            
          character (len=10) :: sedcon =   '   sedcon'        !mg/L                 |sed conc in res
          character (len=10) :: pesti =    '    pesti'        !mg pst               |pest entering res
          character (len=10) :: reactw =   '   reactw'        !mg pst               |pest lost from res through reactions
          character (len=10) :: volatpst = ' volatpst'        !mg pst               |pest lost from res through volatilization
          character (len=10) :: setlpst =  '  setlpst'        !mg pst               |pest moving from water to sed through settling
          character (len=10) :: resuspst = ' resuspst'        !mg pst               |pest moving from sed to water through resuspension
          character (len=10) :: difus =    '    difus'        !mg pst               |pest moving from water to sed through diffusion
          character (len=10) :: reactb =   '   reactb'        !mg pst               |pest lost from res sed layer through reactions
          character (len=10) :: bury  =    '     bury'        !mg pst               |pest lost from res sed layer through burial
          character (len=10) :: pesto =    '    pesto'        !mg pst               |pest transported out of res
          character (len=10) :: pstcon =   '   pstcon'        !mg pst/m^3           |pest conc in res water
          character (len=10) :: spstcon=   '  spstcon'        !mg pst/m^3           |pest conc in res sed layer
          character (len=10) :: ev =       '       ev'        !m^3 H2O              |evap from res 
          character (len=10) :: sep =      '      sep'        !m^3 H2O              |seepage from res
          character (len=10) :: pcp =      '      pcp'        !m^3 H2O              |precipitation on res 
          character (len=10) :: flwim3 =   '   flwim3'        !m^3 H2O              |water flowing into res 
          character (len=10) :: flwom3 =   '   flwom3'        !m^3 H2O              |water flowing out of res 
          character (len=10) :: orgni =    '    orgni'        !kg N                 |org N entering res
          character (len=10) :: orgno =    '    orgno'        !kg N                 |org N leaving res
          character (len=10) :: orgpi =    '    orgpi'        !kg P                 |org P entering res
          character (len=10) :: orgpo =    '    orgpo'        !kg P                 |org P leaving res
          character (len=10) :: no3i =     '     no3i'        !kg N                 |nitrate N entering res
          character (len=10) :: no3o =     '     no3o'        !kg N                 |nitrate N leaving res
          character (len=10) :: no2i =     '     no2i'        !kg N                 |nitrite entering res
          character (len=10) :: no2o =     '     no2o'        !kg N                 |nitrite leaving res
          character (len=10) :: nh3i =     '     nh3i'        !kg N                 |ammonia entering res
          character (len=10) :: nh3o =     '     nh3o'        !kg N                 |ammonia leaving res
          character (len=10) :: solpi =    '    solpi'        !kg P                 |mineral P entering res
          character (len=10) :: solpo =    '    solpo'        !kg P                 |mineral P leaving res
          character (len=10) :: chlai =    '    chali'        !kg chla              |chlorophyll-a entering res 
          character (len=10) :: chlao =    '    chlao'        !kg chla              |chlorophyll-a leaving res 
          character (len=10) :: orgpc =    '    orgpc'        !mg P/L               |ave org P conc in res
          character (len=10) :: solpc =    '    solpc'        !mg P/L               |ave sol P conc in res
          character (len=10) :: orgnc =    '    orgnc'        !mg N/L               |ave org N in res
          character (len=10) :: no3c =     '     no3c'        !mg N/L               |ave nitrate conc in res
          character (len=10) :: no2c =     '     no2c'        !mg N/L               |ave nitrite conc in res
          character (len=10) :: nh3c =     '     nh3c'        !mg N/L               |ave ammonia conc in res
       end type res_header
       type (res_header), dimension(:), allocatable, save :: res_hdr

      type res_header_unit
          character (len=6) :: yrs =       '      '
          character (len=6) :: yrc =       '      '
          character (len=8) :: isd =       '         '
          character (len=10) :: flowi =    '    m^3/s'        !m^3/s                |flow into res
          character (len=10) :: flowo =    '    m^3/s'        !m^3/s                |flow out of res          
          character (len=10) :: sedi =     ' met tons'        !metric tons          |sed entering res
          character (len=10) :: sedo =     ' met tons'        !metric tons          |sed leaving res            
          character (len=10) :: sedcon =   '     mg/L'        !mg/L                 |sed conc in res
          character (len=10) :: pesti =    '   mg pst'        !mg pst               |pest entering res
          character (len=10) :: reactw =   '   mg pst'        !mg pst               |pest lost from res through reactions
          character (len=10) :: volatpst = '   mg pst'        !mg pst               |pest lost from res through volatilization
          character (len=10) :: setlpst =  '   mg pst'        !mg pst               |pest moving from water to sed through settling
          character (len=10) :: resuspst = '   mg pst'        !mg pst               |pest moving from sed to water through resuspension
          character (len=10) :: difus =    '   mg pst'        !mg pst               |pest moving from water to sed through diffusion
          character (len=10) :: reactb =   '   mg pst'        !mg pst               |pest lost from res sed layer through reactions
          character (len=10) :: bury  =    '   mg pst'        !mg pst               |pest lost from res sed layer through burial
          character (len=10) :: pesto =    '   mg pst'        !mg pst               |pest transported out of res
          character (len=10) :: pstcon =   'mg pst/m3'        !mg pst/m^3           |pest conc in res water
          character (len=10) :: spstcon=   'mg pst/m3'        !mg pst/m^3           |pest conc in res sed layer
          character (len=10) :: ev =       '  m^3 H2O'        !m^3 H2O              |evap from res 
          character (len=10) :: sep =      '  m^3 H2O'        !m^3 H2O              |seepage from res
          character (len=10) :: pcp =      '  m^3 H2O'        !m^3 H2O              |precipitation on res 
          character (len=10) :: flwim3 =   '  m^3 H2O'        !m^3 H2O              |water flowing into res 
          character (len=10) :: flwom3 =   '  m^3 H2O'        !m^3 H2O              |water flowing out of res 
          character (len=10) :: orgni =    '     kg N'        !kg N                 |org N entering res
          character (len=10) :: orgno =    '     kg N'        !kg N                 |org N leaving res
          character (len=10) :: orgpi =    '     kg P'        !kg P                 |org P entering res
          character (len=10) :: orgpo =    '     kg P'        !kg P                 |org P leaving res
          character (len=10) :: no3i =     '     kg N'        !kg N                 |nitrate N entering res
          character (len=10) :: no3o =     '     kg N'        !kg N                 |nitrate N leaving res
          character (len=10) :: no2i =     '     kg N'        !kg N                 |nitrite entering res
          character (len=10) :: no2o =     '     kg N'        !kg N                 |nitrite leaving res
          character (len=10) :: nh3i =     '     kg N'        !kg N                 |ammonia entering res
          character (len=10) :: nh3o =     '     kg N'        !kg N                 |ammonia leaving res
          character (len=10) :: solpi =    '     kg p'        !kg P                 |mineral P entering res
          character (len=10) :: solpo =    '     kg P'        !kg P                 |mineral P leaving res
          character (len=10) :: chlai =    '  kg chla'        !kg chla              |chlorophyll-a entering res 
          character (len=10) :: chlao =    '  kg chla'        !kg chla              |chlorophyll-a leaving res 
          character (len=10) :: orgpc =    '   mg P/L'        !mg P/L               |ave org P conc in res
          character (len=10) :: solpc =    '   mg P/L'        !mg P/L               |ave sol P conc in res
          character (len=10) :: orgnc =    '   mg N/L'        !mg N/L               |ave org N in res
          character (len=10) :: no3c =     '   mg N/L'        !mg N/L               |ave nitrate conc in res
          character (len=10) :: no2c =     '   mg N/L'        !mg N/L               |ave nitrite conc in res
          character (len=10) :: nh3c =     '   mg N/L'        !mg N/L               |ave ammonia conc in res
       end type res_header_unit
       type (res_header_unit),dimension(:),allocatable,save::res_hdr_unt
!!           
      interface operator (+)
        module procedure resout_add
      end interface
      
      interface operator (/)
        module procedure resout_div
      end interface
      
      contains
!!    routines for reservoir module
      include 'res_control.f90'
      !include 'res_hourly.f'
      !include 'res_hydsed.f'
      include 'res_nutrient.f90'
      include 'res_pest.f90'
      include 'res_dayinit.f90'
      include 'reservoir_output.f90'

      function resout_add(reso1,reso2) result (reso3)
          type (res_output),  intent (in) :: reso1
          type (res_output),  intent (in) :: reso2
          type (res_output) :: reso3
          reso3%flowi = reso2%flowi + reso1%flowi
          reso3%flowo = reso2%flowo + reso1%flowo
          reso3%sedi = reso2%sedi + reso1%sedi
          reso3%sedo = reso2%sedo + reso1%sedo
          reso3%sedcon = reso2%sedcon + reso1%sedcon
          reso3%pesti = reso2%pesti + reso1%pesti          
          reso3%reactw = reso2%reactw + reso1%reactw        
          reso3%volatpst = reso2%volatpst + reso1%volatpst
          reso3%setlpst = reso2%setlpst + reso1%setlpst
          reso3%resuspst = reso2%resuspst + reso1%resuspst
          reso3%difus = reso2%difus + reso1%difus          
          reso3%reactb = reso2%reactb + reso1%reactb
          reso3%bury = reso2%bury + reso1%bury 
          reso3%pesto = reso2%pesto + reso1%pesto          
          reso3%pstcon = reso2%pstcon + reso1%pstcon
          reso3%spstcon = reso2%spstcon + reso1%spstcon
          reso3%ev = reso2%ev + reso1%ev
          reso3%sep = reso2%sep + reso1%sep          
          reso3%pcp = reso2%pcp + reso1%pcp        
          reso3%flwim3 = reso2%flwim3 + reso1%flwim3          
          reso3%flwom3 = reso2%flwom3 + reso1%flwom3
          reso3%orgni = reso2%orgni + reso1%orgni
          reso3%orgno = reso2%orgno + reso1%orgno  
          reso3%orgpi = reso2%orgpi + reso1%orgpi
          reso3%orgpo = reso2%orgpo + reso1%orgpo  
          reso3%no3i = reso2%no3i + reso1%no3i 
          reso3%no3o = reso2%no3o + reso1%no3o  
          reso3%no2i = reso2%no2i + reso1%no2i
          reso3%no2o = reso2%no2o + reso1%no2o 
          reso3%nh3i = reso2%nh3i + reso1%nh3i
          reso3%nh3o = reso2%nh3o + reso1%nh3o  
          reso3%solpi = reso2%solpi + reso1%solpi
          reso3%solpo = reso2%solpo + reso1%solpo  
          reso3%chlai = reso2%chlai + reso1%chlai 
          reso3%chlao = reso2%chlao + reso1%chlao  
          reso3%orgpc = reso2%orgpc + reso1%orgpc        
          reso3%solpc = reso2%solpc + reso1%solpc          
          reso3%orgnc = reso2%orgnc + reso1%orgnc
          reso3%no3c = reso2%no3c + reso1%no3c          
          reso3%no2c = reso2%no2c + reso1%no2c          
          reso3%nh3c = reso2%nh3c + reso1%nh3c                
      end function
      
       function resout_div (hru1,const) result (hru2)
        type (res_output), intent (in) :: hru1
        real, intent (in) :: const
        type (res_output) :: hru2
        hru2%flowi = hru1%flowi / const
        hru2%flowo = hru1%flowo / const
        hru2%sedi = hru1%sedi / const
        hru2%sedo = hru1%sedo / const
        hru2%sedcon = hru1%sedcon / const
        hru2%pesti = hru1%pesti / const
        hru2%reactw = hru1%reactw / const
        hru2%volatpst = hru1%volatpst / const
        hru2%setlpst = hru1%setlpst / const
        hru2%resuspst = hru1%resuspst / const
        hru2%difus = hru1%difus / const
        hru2%reactb = hru1%reactb / const
        hru2%bury = hru1%bury / const
        hru2%pesto = hru1%pesto / const
        hru2%pstcon = hru1%pstcon / const
        hru2%ev = hru1%ev / const
        hru2%sep = hru1%sep / const
        hru2%pcp = hru1%pcp / const
        hru2%flwim3 = hru1flwim3 / const
        hru2%flwom3= hru1%flwom3 / const
        hru2%orgni = hru1%orgni / const
        hru2%orgno = hru1%orgno / const
        hru2%orgpi = hru1%orgpi / const
        hru2%orgpo = hru1%orgpo / const
        hru2%no3i = hru1%no3i / const
        hru2%no3o = hru1%no3o / const
        hru2%no2i = hru1%no2i / const
        hru2%no2o = hru1%no2o / const
        hru2%nh3i = hru1%nh3i / const
        hru2%nh3o = hru1%nh3o / const
        hru2%solpi = hru1%solpi / const
        hru2%solpo = hru1%solpo / const
        hru2%chlai = hru1%chlai / const
        hru2%chlao = hru1%chlao / const
        hru2%orgpc = hru1%orgpc / const
        hru2%solpc = hru1%solpc / const
        hru2%orgnc = hru1%orgnc / const
        hru2%no3c = hru1%no3c / const
        hru2%no2c = hru1%no2c / const
        hru2%nh3c = hru1%nh3c / const
       end function resout_div
                       
      end module reservoir_module