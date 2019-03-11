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
      real :: resorgnc                !              |
      real :: resorgpc                !              |
      real :: ressolpc                !              |
      real :: resno3c                 !              | 
      real :: resno2c                 !              |
      real :: resnh3c                 !              | 
      real :: tair_mx                 !              |
      real :: tair_mn                 !              |
      real :: tair_av                 !              |

      real :: pesti                   !              |
      real :: pesto                   !              |
      real :: pstcon                  !mg pst/m^3    |pest conc in res water
      real :: spstcon                 !mg pst/m^3    |pest conc in res sed layer 
      real :: orgni = 0.              !kg N          |org N entering res
      real :: orgno = 0.              !kg N          |org N leaving res
      real :: orgpi = 0.              !kg P          |org P entering res
      real :: orgpo = 0.              !kg P          |org P leaving res
      real :: no3i = 0.               !kg N          |nitrate N entering res
      real :: no3o = 0.               !kg N          |nitrate N leaving res
      real :: no2i = 0.               !kg N          |nitrite entering res
      real :: no2o = 0.               !kg N          |nitrite leaving res
      real :: nh3i = 0.               !kg N          |ammonia entering res
      real :: nh3o = 0.               !kg N          |ammonia leaving res
      real :: solpi = 0.              !kg P          |mineral P entering res
      real :: solpo = 0.              !kg P          |mineral P leaving res
      real :: chlai = 0.              !kg chla       |chlorophyll-a entering res 
      real :: chlao = 0.              !kg chla       |chlorophyll-a leaving res 
      real :: cbodi = 0.              !kg            |cbod entering res 
      real :: cbodo = 0.              !kg            |cbod leaving res
      real :: orgpc = 0.              !mg P/L        |ave org P conc in res
      real :: solpc = 0.              !mg P/L        |ave sol P conc in res
      real :: orgnc = 0.              !mg N/L        |ave org N in res
      real :: no3c = 0.               !mg N/L        |ave nitrate conc in res
      real :: no2c = 0.               !mg N/L        |ave nitrite conc in res
      real :: nh3c = 0.               !mg N/L        |ave ammonia conc in res
      
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
        real, dimension (:), allocatable :: kd      !           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
        real, dimension (:), allocatable :: aq_mix  ! m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
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
     
      type reservoir_om_processes
        real :: area_ha = 0                 !ha     !reservoir surface area
        real :: evap = 0.                   !mm     |evaporation from res surface area
        real :: seep = 0.                   !mm     |seepage from res bottom
        real :: sed_setl = 0.               !t      |sediment settling
        real :: seci = 0.                   !m      !seci depth
        real :: solp_loss = 0.              !kg     |soluble phosphorus loss
        real :: sedp_loss = 0.              !kg     |sediment attached phosphorus loss
        real :: orgn_loss = 0.              !kg     |organic nitrogen loss
        real :: no3_loss = 0.               !kg     |nitrate loss
        real :: nh3_loss = 0.               !kg     |ammonium nitrogen loss
        real :: no2_loss = 0.               !kg     |nitrite loss
      end type reservoir_om_processes
      type (reservoir_om_processes), dimension(:),allocatable :: res_om_d
      type (reservoir_om_processes), dimension(:),allocatable :: res_om_m
      type (reservoir_om_processes), dimension(:),allocatable :: res_om_y
      type (reservoir_om_processes), dimension(:),allocatable :: res_om_a
      type (reservoir_om_processes), dimension(:),allocatable :: wet_om_d
      type (reservoir_om_processes), dimension(:),allocatable :: wet_om_m
      type (reservoir_om_processes), dimension(:),allocatable :: wet_om_y
      type (reservoir_om_processes), dimension(:),allocatable :: wet_om_a
      type (reservoir_om_processes) :: res_omz
            
      type reservoir_pest_processes
        real :: react = 0.              ! kg       !pesticide lost through reactions in water layer
        real :: volat = 0.              ! kg       !pesticide lost through volatilization
        real :: settle = 0.             ! kg       !pesticide settling to benthic layer
        real :: resus = 0.              ! kg       !pesticide resuspended into lake water
        real :: difus = 0.              ! kg       !pesticide diffusing from benthic sediment to water
        real :: react_ben = 0.          ! kg       !pesticide lost from benthic by reactions
        real :: bury = 0.               ! Kg       |pesticide lost from benthic by burial
      end type reservoir_pest_processes
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_d
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_m
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_y
      type (reservoir_pest_processes), dimension(:),allocatable :: res_pest_a
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_d
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_m
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_y
      type (reservoir_pest_processes), dimension(:),allocatable :: wet_pest_a
      
      type res_header
          ! first part of header for res_in
          character (len=5) :: day    =   " jday"
          character (len=6) :: mo     =   "   mon"
          character (len=6) :: day_mo =   "   day"
          character (len=6) :: yrc    =   "    yr"
          character (len=8) :: j      =   "  resnum "
          character (len=9) :: id     =   "  gis_id "        
          character (len=16) :: name  =   " name               " 
          character (len=13) :: flo   =   "        flo"     !! ha-m         |volume of water
          character (len=12) :: sed   =   "       sed"      !! metric tons  |sediment 
          character (len=10) :: orgn  =   "    orgn"        !! kg N         |organic N
          character (len=10) :: sedp  =   "    sedp"        !! kg P         |organic P
          character (len=10) :: no3   =   "     no3"        !! kg N         |NO3-N
          character (len=10) :: solp  =   "    solp"        !! kg P         |mineral (soluble P)
          character (len=10) :: chla  =   "    chla"        !! kg           |chlorophyll-a
          character (len=10) :: nh3   =   "     nh3"        !! kg N         |NH3
          character (len=10) :: no2   =   "     no2"        !! kg N         |NO2
          character (len=10) :: cbod  =   "    cbod"        !! kg           |carbonaceous biological oxygen demand
          character (len=10) :: dox   =   "     dox"        !! kg           |dissolved oxygen
          character (len=10) :: san   =   "     san"        !! tons         |detached sand
          character (len=10) :: sil   =   "     sil"        !! tons         |detached silt
          character (len=10) :: cla   =   "     cla"        !! tons         |detached clay
          character (len=10) :: sag   =   "     sag"        !! tons         |detached small ag
          character (len=10) :: lag   =   "     lag"        !! tons         |detached large ag
          character (len=10) :: grv   =   "     grv"        !! tons         |gravel
          character (len=10) :: temp  =   "    temp"        !! deg c        |temperature
          end type res_header
       type (res_header) :: res_hdr
      
      type res_header1
          !! this one for res_out
          character (len=8) :: flo    = "     flo"          !! ha-m         |volume of water
          character (len=10) :: sed   = "       sed"        !! metric tons  |sediment 
          character (len=10) :: orgn  = "      orgn"        !! kg N         |organic N
          character (len=10) :: sedp  = "      sedp"        !! kg P         |organic P
          character (len=10) :: no3   = "       no3"        !! kg N         |NO3-N
          character (len=10) :: solp  = "      solp"        !! kg P         |mineral (soluble P)
          character (len=10) :: chla  = "      chla"        !! kg           |chlorophyll-a
          character (len=10) :: nh3   = "       nh3"        !! kg N         |NH3
          character (len=10) :: no2   = "       no2"        !! kg N         |NO2
          character (len=10) :: cbod  = "      cbod"        !! kg           |carbonaceous biological oxygen demand
          character (len=10) :: dox   = "       dox"        !! kg           |dissolved oxygen
          character (len=10) :: san   = "       san"        !! tons         |detached sand
          character (len=10) :: sil   = "       sil"        !! tons         |detached silt
          character (len=10) :: cla   = "       cla"        !! tons         |detached clay
          character (len=10) :: sag   = "       sag"        !! tons         |detached small ag
          character (len=10) :: lag   = "       lag"        !! tons         |detached large ag
          character (len=10) :: grv   = "       grv"        !! tons         |gravel
          character (len=10) :: temp  = "      temp"        !! deg c        |temperature
          end type res_header1
       type (res_header1) :: res_hdr1
       
       type reservoir_hdr
           !! last part of header for res_om
        character (len=10) :: area_ha    = "   area_ha"
        character (len=10) :: evap       = "      evap"             !mm     |evaporation from res surface area
        character (len=10) :: seep       = "      seep"             !mm     |seepage from res bottom
        character (len=10) :: sed_setl   = " sed_setlp"             !t      |sediment settling
        character (len=10) :: seci       = "      seci"             !m      !seci depth
        character (len=10) :: solp_loss  = " solp_loss"             !kg     |soluble phosphorus loss
        character (len=10) :: sedp_loss  = " sedp_loss"             !kg     |sediment attached phosphorus loss
        character (len=10) :: orgn_loss  = " orgn_loss"             !kg     |organic nitrogen loss
        character (len=10) :: no3_loss   = "  no3_loss"             !kg     |nitrate loss
        character (len=10) :: nh3_loss   = "  nh3_loss"             !kg     |ammonium nitrogen loss
        character (len=10) :: no2_loss   = "  no2_loss"             !kg     |nitrite loss
      end type reservoir_hdr
      type (reservoir_hdr) :: res_hdr2
      
      type res_headerbsn
          ! this one used for the reservoir_???.bsn.txt files
          character (len=8) :: flo    = "     flo"            !! ha-m         |volume of water
          character (len=12) :: sed   = "         sed"        !! metric tons  |sediment 
          character (len=12) :: orgn  = "        orgn"        !! kg N         |organic N
          character (len=12) :: sedp  = "        sedp"        !! kg P         |organic P
          character (len=12) :: no3   = "         no3"        !! kg N         |NO3-N
          character (len=12) :: solp  = "        solp"        !! kg P         |mineral (soluble P)
          character (len=12) :: chla  = "        chla"        !! kg           |chlorophyll-a
          character (len=12) :: nh3   = "         nh3"        !! kg N         |NH3
          character (len=12) :: no2   = "         no2"        !! kg N         |NO2
          character (len=12) :: cbod  = "        cbod"        !! kg           |carbonaceous biological oxygen demand
          character (len=12) :: dox   = "         dox"        !! kg           |dissolved oxygen
          character (len=12) :: san   = "         san"        !! tons         |detached sand
          character (len=12) :: sil   = "         sil"        !! tons         |detached silt
          character (len=12) :: cla   = "         cla"        !! tons         |detached clay
          character (len=12) :: sag   = "         sag"        !! tons         |detached small ag
          character (len=12) :: lag   = "         lag"        !! tons         |detached large ag
          character (len=12) :: grv   = "         grv"        !! tons         |gravel
          character (len=12) :: temp  = "        temp"        !! deg c        |temperature
          end type res_headerbsn
       type (res_headerbsn) :: res_hdrbsn
       interface operator (+)
        module procedure resom_add
      end interface

      interface operator (/)
        module procedure resom_div_const
      end interface   
      
      contains
      
     !! routines for reservoir module
      function resom_add (resom1, resom2) result (resom3)
        type (reservoir_om_processes), intent (in) :: resom1
        type (reservoir_om_processes), intent (in) :: resom2
        type (reservoir_om_processes) :: resom3
        resom3%area_ha = resom1%area_ha + resom2%area_ha
        resom3%evap = resom1%evap + resom2%evap        
        resom3%seep = resom1%seep + resom2%seep   
        resom3%sed_setl = resom1%sed_setl + resom2%sed_setl
        resom3%seci = resom1%seci + resom2%seci
        resom3%solp_loss = resom1%solp_loss + resom2%solp_loss
        resom3%sedp_loss = resom1%sedp_loss + resom2%sedp_loss
        resom3%orgn_loss = resom1%orgn_loss + resom2%orgn_loss
        resom3%no3_loss = resom1%no3_loss + resom2%no3_loss
        resom3%nh3_loss = resom1%nh3_loss + resom2%nh3_loss
        resom3%no2_loss = resom1%no2_loss + resom2%no2_loss
      end function resom_add
      
      function resom_div_const (resom1,const) result (resom2)
        type (reservoir_om_processes), intent (in) :: resom1
        real, intent (in) :: const
        type (reservoir_om_processes) :: resom2
        resom2%area_ha = resom1%area_ha / const
        resom2%evap = resom1%evap / const
        resom2%seep = resom1%seep / const
        resom2%sed_setl = resom1%sed_setl / const
        resom2%seci = resom1%seci / const
        resom2%solp_loss = resom1%solp_loss / const
        resom2%sedp_loss = resom1%sedp_loss / const
        resom2%orgn_loss = resom1%orgn_loss / const
        resom2%no3_loss = resom1%no3_loss / const
        resom2%nh3_loss = resom1%nh3_loss / const
        resom2%no2_loss = resom1%no2_loss / const
      end function resom_div_const
      
      end module reservoir_module