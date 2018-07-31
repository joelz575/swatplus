      module reservoir_module
    
      real :: ressano,ressilo,resclao,ressago,reslago, resgrao
      real :: ressani, ressili, resclai, ressagi, reslagi,resgrai

      real :: resflwi                 !              |
      real :: resflwo                 !              |  
      real :: ressedi                 !              |
      real :: ressedo                 !              |
      real :: sedcon                  !g/m^3         |sediment concentration
      real :: respesti                !              |
      real :: reactw                  !mg pst        |amount of pesticide in reach that is lost
                                      !              |through reactions
      real :: volatpst                !mg pst        |amount of pesticide lost from reach by
                                      !              |volatilization
      real :: setlpst                 !mg pst        |amount of pesticide moving from water to
                                      !              |sediment due to settling
      real :: resuspst                !mg pst        |amount of pesticide moving from sediment to
                                      !              |reach due to resuspension
      real :: difus                   !mg pst        |diffusion of pesticide from sediment to reach
      real :: reactb                  !mg pst        |amount of pesticide in sediment that is lost
                                      !              |through reactions
                                      !              |up by plant roots in the bank storage zone
      real :: bury                    !mg pst        |loss of pesticide from active sediment layer
                                      !              |by burial
      real :: solpesto                !mg pst/m^3    |soluble pesticide concentration in outflow
                                      !              |on day
      real :: sorpesto                !mg pst/m^3    |sorbed pesticide concentration in outflow
                                      !              |on day 
      real :: resev                   !              |
      real :: ressep                  !              |
      real :: respcp                  !              |
      real :: resorgno                !              |
      real :: resorgpo                !              |
      real :: resno3o                 !              |
      real :: resno2o                 !              | 
      real :: resnh3o                 !              |
      real :: ressolpo                !              |
      real :: reschlao                !              |
      real :: resorgpc                !              |
      real :: ressolpc                !              |
      real :: resno3c                 !              | 
      real :: resno2c                 !              |
      real :: resnh3c                 !              | 
      real :: tair_mx                 !              |
      real :: tair_mn                 !              |
      real :: tair_av                 !              |

      type reservoir
        character(len=13) :: name = "default"
        integer :: ob = 0                           !object number if reservoir object; hru number if hru object
        integer :: props = 0                        !points to res_dat
        real :: psa = 0.                    !ha     |res surface area when res is filled to princ spillway
        real :: pvol = 0.                   !ha-m   |vol of water needed to fill the res to the princ spillway (read in as ha-m and converted to m^3)
        real :: esa = 0.                    !ha     |res surface area when res is filled to emerg spillway 
        real :: evol = 0.                   !ha-m   |vol of water needed to fill the res to the emerg spillway (read in as ha-m and converted to m^3)
        real :: br1 = 0.                    !none   |vol-surface area coefficient for reservoirs (model estimates if zero)
                                            !       |vol-depth coefficient for hru impoundment
        real :: br2 = 0.                    !none   |vol-surface area coefficient for reservoirs (model estimates if zero)
                                            !       |vol-depth coefficient for hru impoundment
        real :: area_ha = 0                 !ha     !reservoir surface area
        real :: seci = 0                    !m      !seci depth
      end type reservoir          
      type (reservoir), dimension(:),allocatable :: res_ob
      
      type wetland
        real :: psa = 0.                    !ha     |res surface area when res is filled to princ spillway
        real :: pvol = 0.                   !ha-m   |vol of water needed to fill the res to the princ spillway (read in as ha-m and converted to m^3)
        real :: esa = 0.                    !ha     |res surface area when res is filled to emerg spillway 
        real :: evol = 0.                   !ha-m   |vol of water needed to fill the res to the emerg spillway (read in as ha-m and converted to m^3)
        real :: area_ha = 0                 !ha     !reservoir surface area
        real :: seci = 0                    !m      !seci depth
      end type wetland          
      type (wetland), dimension(:),allocatable :: wet_ob

      type res_output
          real :: vol = 0.       !ha-m                 |res volume
          real :: area_ha = 0.   !ha                   |res surface area
          real :: flowi = 0.     !ha-m                 |flow into res
          real :: flowo = 0.     !ha-m                 |flow out of res
          real :: ev = 0.        !ha-m                 |evap from res 
          real :: sep = 0.       !ha-m                 |seepage from res
          real :: pcp = 0.       !ha-m                 |precipitation on res 
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
      
      type (res_output), dimension(:), allocatable, save :: res_d
      type (res_output), dimension(:), allocatable, save :: res_m
      type (res_output), dimension(:), allocatable, save :: res_y
      type (res_output), dimension(:), allocatable, save :: res_a
      type (res_output), dimension(:), allocatable, save :: wet_d
      type (res_output), dimension(:), allocatable, save :: wet_m
      type (res_output), dimension(:), allocatable, save :: wet_y
      type (res_output), dimension(:), allocatable, save :: wet_a
      type (res_output) :: bres_d
      type (res_output) :: bres_m
      type (res_output) :: bres_y
      type (res_output) :: bres_a
      type (res_output) :: resadd1
      type (res_output) :: resadd2
      type (res_output) :: resadd3
      
!!      type (output_waterbal) :: resmz
      type (res_output) :: resmz
      
      type res_header
          character (len=5) :: day =      " jday"
          character (len=6) :: mo =       "   mon"
          character (len=6) :: day_mo =   "   day"
          character (len=6) :: yrc =      "    yr"
          character (len=8) :: j =            "  resnum "
          character (len=9) :: id =           "  gis_id "        
          character (len=16) :: name =     " name              " 
          character (len=11) :: vol =      "        vol"       !ha-m                 |flow volume res
          character (len=12) :: area =     "        area"        !ha                   |surface area res
          character (len=10) :: flowi =    "     flowi"        !ha-m                 |flow into res
          character (len=10) :: flowo =    "     flowo"        !ha-m                 |flow out of res
          character (len=10) :: ev =       "        ev"        !ha-m                 |evap from res 
          character (len=10) :: sep =      "       sep"        !ha-m                 |seepage from res
          character (len=10) :: pcp =      "       pcp"        !ha-m                 |precipitation on res 
          character (len=10) :: sedi =     "      sedi"        !metric tons          |sed entering res
          character (len=10) :: sedo =     "      sedo"        !metric tons          |sed leaving res            
          character (len=10) :: sedcon =   "    sedcon"        !mg/L                 |sed conc in res
          character (len=10) :: pesti =    "     pesti"        !mg pst               |pest entering res
          character (len=10) :: reactw =   "    reactw"        !mg pst               |pest lost from res through reactions
          character (len=10) :: volatpst = "  volatpst"        !mg pst               |pest lost from res through volatilization
          character (len=10) :: setlpst =  "   setlpst"        !mg pst               |pest moving from water to sed through settling
          character (len=10) :: resuspst = "  resuspst"        !mg pst               |pest moving from sed to water through resuspension
          character (len=10) :: difus =    "     difus"        !mg pst               |pest moving from water to sed through diffusion
          character (len=10) :: reactb =   "    reactb"        !mg pst               |pest lost from res sed layer through reactions
          character (len=10) :: bury  =    "      bury"        !mg pst               |pest lost from res sed layer through burial
          character (len=10) :: pesto =    "     pesto"        !mg pst               |pest transported out of res
          character (len=10) :: pstcon =   "    pstcon"        !mg pst/m^3           |pest conc in res water
          character (len=10) :: spstcon=   "   spstcon"        !mg pst/m^3           |pest conc in res sed layer 
          character (len=10) :: orgni =    "     orgni"        !kg N                 |org N entering res
          character (len=10) :: orgno =    "     orgno"        !kg N                 |org N leaving res
          character (len=10) :: orgpi =    "     orgpi"        !kg P                 |org P entering res
          character (len=10) :: orgpo =    "     orgpo"        !kg P                 |org P leaving res
          character (len=10) :: no3i =     "      no3i"        !kg N                 |nitrate N entering res
          character (len=10) :: no3o =     "      no3o"        !kg N                 |nitrate N leaving res
          character (len=10) :: no2i =     "      no2i"        !kg N                 |nitrite entering res
          character (len=10) :: no2o =     "      no2o"        !kg N                 |nitrite leaving res
          character (len=10) :: nh3i =     "      nh3i"        !kg N                 |ammonia entering res
          character (len=10) :: nh3o =     "      nh3o"        !kg N                 |ammonia leaving res
          character (len=10) :: solpi =    "     solpi"        !kg P                 |mineral P entering res
          character (len=10) :: solpo =    "     solpo"        !kg P                 |mineral P leaving res
          character (len=10) :: chlai =    "     chali"        !kg chla              |chlorophyll-a entering res 
          character (len=10) :: chlao =    "     chlao"        !kg chla              |chlorophyll-a leaving res 
          character (len=10) :: orgpc =    "     orgpc"        !mg P/L               |ave org P conc in res
          character (len=10) :: solpc =    "     solpc"        !mg P/L               |ave sol P conc in res
          character (len=10) :: orgnc =    "     orgnc"        !mg N/L               |ave org N in res
          character (len=10) :: no3c =     "      no3c"        !mg N/L               |ave nitrate conc in res
          character (len=10) :: no2c =     "      no2c"        !mg N/L               |ave nitrite conc in res
          character (len=10) :: nh3c =     "      nh3c"        !mg N/L               |ave ammonia conc in res
       end type res_header
       type (res_header) :: res_hdr

      type res_header_unit
          character (len=6) :: day =       "      "
          character (len=6) :: mo =        "      "
          character (len=6) :: day_mo =    "      "
          character (len=6) :: yrc =       "      "
          character (len=8) :: isd =       "         "         
          character (len=8) :: id =        "        "         
          character (len=16) :: name =     "                "          
          character (len=12) :: vol =      "       ha_m"       !ha-m                 |volume res
          character (len=11) :: area =     "         ha"         !ha                   |surface area res
          character (len=10) :: flowi =    "      ha_m"        !ha-m                 |flow into res
          character (len=10) :: flowo =    "      ha_m"        !ha-m                 |flow out of res
          character (len=10) :: ev =       "      ha_m"        !ha-m                 |evap from res  
          character (len=10) :: sep =      "      ha_m"        !ha-m                 |seepage from res
          character (len=10) :: pcp =      "      ha_m"        !ha-m                 |precipitation on res 
          character (len=10) :: sedi =     "  met_tons"        !metric tons          |sed entering res
          character (len=10) :: sedo =     "  met_tons"        !metric tons          |sed leaving res            
          character (len=10) :: sedcon =   "      mg/L"        !mg/L                 |sed conc in res
          character (len=10) :: pesti =    "    mg_pst"        !mg pst               |pest entering res
          character (len=10) :: reactw =   "    mg_pst"        !mg pst               |pest lost from res through reactions
          character (len=10) :: volatpst = "    mg_pst"        !mg pst               |pest lost from res through volatilization
          character (len=10) :: setlpst =  "    mg_pst"        !mg pst               |pest moving from water to sed through settling
          character (len=10) :: resuspst = "    mg_pst"        !mg pst               |pest moving from sed to water through resuspension
          character (len=10) :: difus =    "    mg_pst"        !mg pst               |pest moving from water to sed through diffusion
          character (len=10) :: reactb =   "    mg_pst"        !mg pst               |pest lost from res sed layer through reactions
          character (len=10) :: bury  =    "    mg_pst"        !mg pst               |pest lost from res sed layer through burial
          character (len=10) :: pesto =    "    mg_pst"        !mg pst               |pest transported out of res
          character (len=10) :: pstcon =   " mg_pst/m3"        !mg pst/m^3           |pest conc in res water
          character (len=10) :: spstcon=   " mg_pst/m3"        !mg pst/m^3           |pest conc in res sed layer
          character (len=10) :: orgni =    "      kg_N"        !kg N                 |org N entering res
          character (len=10) :: orgno =    "      kg_N"        !kg N                 |org N leaving res
          character (len=10) :: orgpi =    "      kg_P"        !kg P                 |org P entering res
          character (len=10) :: orgpo =    "      kg_P"        !kg P                 |org P leaving res
          character (len=10) :: no3i =     "      kg_N"        !kg N                 |nitrate N entering res
          character (len=10) :: no3o =     "      kg_N"        !kg N                 |nitrate N leaving res
          character (len=10) :: no2i =     "      kg_N"        !kg N                 |nitrite entering res
          character (len=10) :: no2o =     "      kg_N"        !kg N                 |nitrite leaving res
          character (len=10) :: nh3i =     "      kg_N"        !kg N                 |ammonia entering res
          character (len=10) :: nh3o =     "      kg_N"        !kg N                 |ammonia leaving res
          character (len=10) :: solpi =    "      kg_p"        !kg P                 |mineral P entering res
          character (len=10) :: solpo =    "      kg_P"        !kg P                 |mineral P leaving res
          character (len=10) :: chlai =    "   kg_chla"        !kg chla              |chlorophyll-a entering res 
          character (len=10) :: chlao =    "   kg_chla"        !kg chla              |chlorophyll-a leaving res 
          character (len=10) :: orgpc =    "    mg_P/L"        !mg P/L               |ave org P conc in res
          character (len=10) :: solpc =    "    mg_P/L"        !mg P/L               |ave sol P conc in res
          character (len=10) :: orgnc =    "    mg_N/L"        !mg N/L               |ave org N in res
          character (len=10) :: no3c =     "    mg_N/L"        !mg N/L               |ave nitrate conc in res
          character (len=10) :: no2c =     "    mg_N/L"        !mg N/L               |ave nitrite conc in res
          character (len=10) :: nh3c =     "    mg_N/L"        !mg N/L               |ave ammonia conc in res
       end type res_header_unit
       type (res_header_unit) :: res_hdr_unt
!!           
      interface operator (+)
        module procedure resout_add
      end interface
      
      interface operator (/)
        module procedure resout_div
      end interface
      
      contains
!!    routines for reservoir module

      function resout_add(reso1,reso2) result (reso3)
          type (res_output),  intent (in) :: reso1
          type (res_output),  intent (in) :: reso2
          type (res_output) :: reso3
          reso3%vol = reso2%vol + reso1%vol
          reso3%area_ha = reso2%area_ha + reso1%area_ha
          reso3%flowi = reso2%flowi + reso1%flowi
          reso3%flowo = reso2%flowo + reso1%flowo
          reso3%ev = reso2%ev + reso1%ev
          reso3%sep = reso2%sep + reso1%sep          
          reso3%pcp = reso2%pcp + reso1%pcp
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
        hru2%vol = hru1%vol / const
        hru2%area_ha = hru1%area_ha / const
        hru2%flowi = hru1%flowi / const
        hru2%flowo = hru1%flowo / const
        hru2%ev = hru1%ev / const
        hru2%sep = hru1%sep / const
        hru2%pcp = hru1%pcp / const
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