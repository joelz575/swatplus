      module output_landscape_module
      
      type output_waterbal
        real :: precip = 0.           !mm H2O        |prec falling on they HRU during timestep
        real :: snofall = 0.          !mm H2O        |amt of prec falling as snow, sleet or freezing rain during timestep
        real :: snomlt = 0.           !mm H2O        |amt of snow or ice melting during timestep
        real :: surq_gen = 0.         !mm H2O        |amt of surf runoff to main channel
        real :: latq = 0.             !mm H2O        |amt of lat flow contrib to main channel during HRU during mon
        real :: wateryld = 0.         !mm H2O        |water yld (tot amt of water entering main channel) from HRU during mon
        real :: perc = 0.             !mm H2O        |amt of water perc out of the soil profile and into the vadose zone in HRU during mon
        real :: et = 0.               !mm H2O        |actual ET in HRU during mon
        real :: tloss = 0.            !mm H2O        |amt of trans losses from trib channels in HRU for mon
        real :: eplant = 0.           !mm H2O        |actual amt of transpiration that occurs during mon in HRU
        real :: esoil = 0.            !mm H2O        |actual amt of evap (from soil) that occurs during mon in HRU
        real :: surq_cont = 0.        !mm H2O        |amt of surf runoff gen during mon in HRU
        real :: cn = 0.               !none          |CN values during mon in HRU
        real :: sw = 0.               !mm H2O        |sum of daily soil water values used to calc the curve number
        real :: pet = 0.              !mm H2O        |pot et on current day in HRU
        real :: qtile = 0.            !mm H2O        |drainage tile flow contrib to main channel from HRU in mon
        real :: irr = 0.              !mm H2O        |amount of water applied to HRU 
      end type output_waterbal
       
      type (output_waterbal), dimension (:), allocatable :: hwb_d
      type (output_waterbal), dimension (:), allocatable :: hwb_m
      type (output_waterbal), dimension (:), allocatable :: hwb_y
      type (output_waterbal), dimension (:), allocatable :: hwb_a
      type (output_waterbal) :: hwbz
      
      type (output_waterbal), dimension (:), allocatable :: sdwb_d
      type (output_waterbal), dimension (:), allocatable :: sdwb_m
      type (output_waterbal), dimension (:), allocatable :: sdwb_y
      type (output_waterbal), dimension (:), allocatable :: sdwb_a
      
      type (output_waterbal), dimension (:), allocatable :: swb_d
      type (output_waterbal), dimension (:), allocatable :: swb_m
      type (output_waterbal), dimension (:), allocatable :: swb_y
      type (output_waterbal), dimension (:), allocatable :: swb_a
      
      type (output_waterbal) :: bwb_d
      type (output_waterbal) :: bwb_m
      type (output_waterbal) :: bwb_y
      type (output_waterbal) :: bwb_a
      
      type output_waterbal_header
        character (len=6) :: yrs =        ' time '
        character (len=6) :: yrc =        ' year '
        character (len=8) :: isd =        '   unit '
        character (len=12) :: precip =   '  prec_mm   '
        character (len=12) :: snofall =  '  snow_mm   '
        character (len=12) :: snomlt =   'snomlt_mm   '        
        character (len=12) :: surq_gen = 'sq_gen_mm   '      
        character (len=12) :: latq =     '  latq_mm   ' 
        character (len=12) :: wateryld = 'wtryld_mm   '
        character (len=12) :: perc =     '  perc_mm   '   
        character (len=12) :: et =       '    et_mm   '
        character (len=12) :: tloss =    ' tloss_mm   '
        character (len=12) :: eplant =   'eplant_mm   '
        character (len=12) :: esoil =    ' esoil_mm   '
        character (len=12) :: surq_cont ='sq_cont_mm  '
        character (len=12) :: cn =       '       cn   '
        character (len=12) :: sw =       '    sw_mm   '
        character (len=12) :: pet     =  '   pet_mm   '
        character (len=12) :: qtile =    ' qtile_mm   '
        character (len=12) :: irr =      '   irr_mm   '
      end type output_waterbal_header
      
      type (output_waterbal_header) :: wb_hdr
      
      type output_nutbal
        real :: cfertn = 0.               !kg N/ha       |total amount of nitrogen applied to soil
!!                                                         during continuous fertilizer operation 
        real :: cfertp = 0.               !kg P/ha       |total amount of phosphorus applied to soil
!!                                                         during continuous fertilizer operation 
        real :: grazn = 0.                !kg N/ha       |amt of nit added to soil in grazing on the day in HRU
        real :: grazp = 0.                !kg P/ha       |amt of phos added to soil in grazing on the day in HRU
        real :: auton = 0.                !kg N/ha       |amt of nit applied in auto-fert applic
        real :: autop = 0.                !kg P/ha       |amt of phos applied in auto-fert applic
        real :: rmp1tl = 0.               !kg P/ha       |amt of phos moving from the labile min pool to the active min pool
        !                                                    in the soil profile on the current day in the HRU
        real :: roctl = 0.                !kg P/ha       |amt of phos moving from the active min pool to the stable min pool
        !                                                    in the soil profile on the current day in the HRU
        real :: fertn = 0.                !kg N/ha       |tot amt of nit applied to soil in HRU on day
        real :: fertp = 0.                !kg P/ha       |tot amt of phos applied to soil in HRU on day
        real :: fixn = 0.                 !kg N/ha       |amt of nit added to plant biomass via fixation on the day in HRU
        real :: wdntl = 0.                !kg N/ha       |amt of nit lost from nitrate pool by denit in soil profile
        !                                                    on current day in HRU
        real :: hmntl = 0.                !kg N/ha       |amt of nit moving from active org to nit pool in soil profile
        !                                                    on current day in HRU 
        real :: rwntl = 0.                !kg N/ha       |amt of nit moving from active org to stable org pool in soil
        !                                                    profile on current day in HRU
        real :: hmptl = 0.                !kg P/ha       |amt of phos moving from the org to labile pool in soil profile
        !                                                    on current day in HRU
        real :: rmn2tl = 0.               !kg N/ha       |amt of nit moving from the fresh org (residue) to the nitrate(80%)
        !                                                    and active org(20%) pools in soil profile on current day in HRU
        real :: rmptl = 0.                !kg P/ha       |amt of phos moving from the fresh org (residue) to the labile(80%)
        !                                                    and org(20%) pools in soil profile on current day in HRU
        real :: no3pcp = 0.               !kg N/ha       |nitrate added to the soil in rainfall
      end type output_nutbal

      type (output_nutbal), dimension (:), allocatable :: hnb_d
      type (output_nutbal), dimension (:), allocatable :: hnb_m
      type (output_nutbal), dimension (:), allocatable :: hnb_y
      type (output_nutbal), dimension (:), allocatable :: hnb_a
      type (output_nutbal) :: hnbz
      
      type (output_nutbal), dimension (:), allocatable :: sdnb_d
      type (output_nutbal), dimension (:), allocatable :: sdnb_m
      type (output_nutbal), dimension (:), allocatable :: sdnb_y
      type (output_nutbal), dimension (:), allocatable :: sdnb_a
      
      type (output_nutbal), dimension (:), allocatable :: snb_d
      type (output_nutbal), dimension (:), allocatable :: snb_m
      type (output_nutbal), dimension (:), allocatable :: snb_y
      type (output_nutbal), dimension (:), allocatable :: snb_a
      
      type (output_nutbal) :: bnb_d
      type (output_nutbal) :: bnb_m
      type (output_nutbal) :: bnb_y
      type (output_nutbal) :: bnb_a
      
      type output_nutbal_header
         character (len=6) :: yrs =        ' time '
         character (len=6) :: yrc =        ' year '
         character (len=8) :: isd =        '   unit '
         character(len=12) :: cfertn =     'cfrtn_kgha  '
         character(len=12) :: cfertp =     'cfrtp_kgha  ' 
         character(len=12) :: grazn =      'grzn_kgha   '
         character(len=12) :: grazp =      'grzp_kgha   '     
         character(len=12) :: auton =      'autn_kgha   '    
         character(len=12) :: autop =      'autp_kgha   '      
         character(len=12) :: rmp1tl =     'rm1t_kgha   '     
         character(len=12) :: roctl =      'rctl_kgha   '
         character(len=12) :: fertn =      'frtn_kgha   '       
         character(len=12) :: fertp =      'frtp_kgha   '       
         character(len=12) :: fixn =       'fixn_kgha   '       
         character(len=12) :: wdntl =      'wntl_kgha   '
         character(len=12) :: hmntl =      'hntl_kgha   '
         character(len=12) :: rwntl =      'rbtl_kgha   '
         character(len=12) :: hmptl =      'hmpt_kgha   '
         character(len=12) :: rmn2tl =     'rm2t_kgha   '      
         character(len=12) :: rmptl =      'rmpt_kgha   '      
         character(len=12) :: no3pcp =     'no3p_kgha   ' 
      end type output_nutbal_header
          
      type (output_nutbal_header) :: nb_hdr
      
      type output_losses
        real :: sedyld = 0.               !metric tons    | daily soil loss caused by water erosion
        real :: sedorgn = 0.              !kg N/ha        | amt of org nit in surf runoff in HRU for the day
        real :: sedorgp = 0.              !kg P/ha        | amt of org phos in surf runoff in HRU for the day
        real :: surqno3 = 0.              !kg N/ha        | amt of NO3-N in surf runoff in HRU for the day
        real :: latno3 = 0.               !kg N/ha        | amt of NO3-N in lat flow in HRU for the day
        real :: surqsolp = 0.             !kg P/ha        | amt of soluble phos in surf runoff in HRU for the day
        real :: usle = 0.                 !metric tons/ha | daily soil loss predicted with USLE equation
        real :: bactp = 0.                ! 
        real :: bactlp = 0.               ! 
        real :: sedmin = 0.               !
        real :: tileno3 = 0.              !kg N/ha        | NO3 in tile flow
      end type output_losses
      
      type (output_losses), dimension (:), allocatable :: hls_d
      type (output_losses), dimension (:), allocatable :: hls_m
      type (output_losses), dimension (:), allocatable :: hls_y
      type (output_losses), dimension (:), allocatable :: hls_a
      type (output_losses) :: hlsz
      
      type (output_losses), dimension (:), allocatable :: sdls_d
      type (output_losses), dimension (:), allocatable :: sdls_m
      type (output_losses), dimension (:), allocatable :: sdls_y
      type (output_losses), dimension (:), allocatable :: sdls_a
      
      type (output_losses), dimension (:), allocatable :: sls_d
      type (output_losses), dimension (:), allocatable :: sls_m
      type (output_losses), dimension (:), allocatable :: sls_y
      type (output_losses), dimension (:), allocatable :: sls_a
      
      type (output_losses) :: bls_d
      type (output_losses) :: bls_m
      type (output_losses) :: bls_y
      type (output_losses) :: bls_a
      
      type output_losses_header
        character (len=6) :: yrs =        ' time '
        character (len=6) :: yrc =        ' year '
        character (len=8) :: isd =        '   unit '
        character (len=12) :: sedyld =    'sedy_tha    '
        character (len=12)  :: sedorgn =  'sedn_kgha   '
        character (len=12)  :: sedorgp =  'sedp_kgha   '
        character (len=12)  :: surqno3 =  ' sq3_kgha   '
        character (len=12)  :: latno3 =   'lat3_kgha   '            
        character (len=12)  :: surqsolp = 'sqsp_kgha   '
        character (len=12)  :: usle =     'usle_tons   '  
        character (len=12)  :: bactp =    'bacp_tons   '
        character (len=12)  :: bactlp =   ' blp_tons   '     
        character (len=12)  :: sedmin =   '    sedmn   '
        character (len=12)  :: tileno3 =  '  tileno3   '
      end type output_losses_header
      
      type (output_losses_header) :: ls_hdr
   
      type output_plantweather
        real :: lai = 0.                   !m**2/m**2     |leaf area index
        real :: bioms = 0.                 !kg/ha         |land cover/crop biomass 
        real :: yield = 0.                 !kg/ha         |yield (dry weight) by crop type
        real :: residue = 0.               !kga/ha        |initial residue cover
        real :: sol_tmp = 0.               !deg C         |daily average temperature of soil layer
        real :: strsw = 0.                 !0-1           |water (drought) stress
        real :: strsa = 0.                 !0-1           |water (aeration) stress
        real :: strstmp = 0.               !0-1           |temperature stress      
        real :: strsn = 0.                 !0-1           |nitrogen stress
        real :: strsp = 0.                 !0-1           |phosphorus stress
        real :: nplnt = 0.                 !kg N/ha       |plant uptake of nit in HRU for the day
        real :: percn = 0.                 !kg N/ha       |NO3-N leached from soil profile
        real :: pplnt = 0.                 !kg P/ha       |plant uptake of phos in HRU for the day
        real :: tmx = 0.                   !deg C         |maximum temperature for the day in HRU
        real :: tmn = 0.                   !deg C         |minimum temperature for the day in HRU
        real :: tmpav = 0.                 !deg C         |average air temperature on current day in HRU
        real :: solrad = 0.                !MJ/m^2        |solar radiation for the day in HRU
        real :: phubase0 = 0.              !              |base zero potential heat units
      end type output_plantweather
      
      type (output_plantweather), dimension (:), allocatable :: hpw_d
      type (output_plantweather), dimension (:), allocatable :: hpw_m
      type (output_plantweather), dimension (:), allocatable :: hpw_y
      type (output_plantweather), dimension (:), allocatable :: hpw_a
      type (output_plantweather) :: hpwz
      
      type(output_plantweather), dimension (:), allocatable :: sdpw_d
      type(output_plantweather), dimension (:), allocatable :: sdpw_m
      type(output_plantweather), dimension (:), allocatable :: sdpw_y
      type(output_plantweather), dimension (:), allocatable :: sdpw_a
      
      type (output_plantweather), dimension (:), allocatable :: spw_d
      type (output_plantweather), dimension (:), allocatable :: spw_m
      type (output_plantweather), dimension (:), allocatable :: spw_y
      type (output_plantweather), dimension (:), allocatable :: spw_a
      
      type (output_plantweather) :: bpw_d
      type (output_plantweather) :: bpw_m
      type (output_plantweather) :: bpw_y
      type (output_plantweather) :: bpw_a
      
      type output_plantweather_header
        character (len=6) :: yrs =        ' time '
        character (len=6) :: yrc =        ' year '
        character (len=8) :: isd =        '   unit '
        character (len=12) :: lai =       '      lai   '
        character (len=12) :: bioms =     '    bioms   '
        character (len=12) :: yield =     '    yield   '
        character (len=12) :: residue =   '  residue   '
        character (len=12) :: sol_tmp =   '   soltmp   '
        character (len=12) :: strsw =     '    strsw   '
        character (len=12) :: strsa =     '    strsa   '
        character (len=12) :: strstmp =   '  strstmp   '
        character (len=12) :: strsn =     '    strsn   '
        character (len=12) :: strsp =     '    strsp   '
        character (len=12) :: nplnt =     'nplt_kgha   '
        character (len=12) :: percn =     'prcn_kgha   '
        character (len=12) :: pplnt =     'plnt_kgha   '
        character (len=12) :: tmx =       ' tmx_degc   '
        character (len=12) :: tmn =       ' tmn_degc   '
        character (len=12) :: tmpav =     'tave_degc   '
        character (len=12) :: solrad =    'sr_mj/m^2   '
        character (len=12) :: phubase0  = 'phubase     '
      end type output_plantweather_header
      
      type (output_plantweather_header) :: pw_hdr
      
      
      interface operator (+)
        module procedure hruout_waterbal_add
      end interface
             
      interface operator (+)
        module procedure hruout_nutbal_add
      end interface
           
      interface operator (+)
        module procedure hruout_losses_add
      end interface
            
      interface operator (+)
        module procedure hruout_plantweather_add
      end interface
        
      interface operator (/)
        module procedure hruout_waterbal_div
      end interface
      
      interface operator (//)
        module procedure hruout_waterbal_ave
      end interface
        
      interface operator (/)
        module procedure hruout_nutbal_div
      end interface
        
      interface operator (/)
        module procedure hruout_losses_div
      end interface
        
      interface operator (/)
        module procedure hruout_plantweather_div
        end interface
          
      interface operator (//)
        module procedure hruout_plantweather_ave
        end interface
  
      contains

      !include 'output_landscape_init'
      
      function hruout_waterbal_add (hru1, hru2) result (hru3)
        type (output_waterbal), intent (in) :: hru1
        type (output_waterbal), intent (in) :: hru2
        type (output_waterbal) :: hru3
        hru3%precip = hru1%precip + hru2%precip
        hru3%snofall = hru1%snofall + hru2%snofall
        hru3%snomlt = hru1%snomlt + hru2%snomlt
        hru3%surq_gen = hru1%surq_gen + hru2%surq_gen
        hru3%latq = hru1%latq + hru2%latq
        hru3%wateryld = hru1%wateryld + hru2%wateryld
        hru3%perc = hru1%perc + hru2%perc
        hru3%et = hru1%et + hru2%et
        hru3%tloss = hru1%tloss + hru2%tloss
        hru3%eplant = hru1%eplant + hru2%eplant
        hru3%esoil = hru1%esoil + hru2%esoil
        hru3%surq_cont = hru1%surq_cont + hru2%surq_cont
        hru3%cn = hru1%cn + hru2%cn
        hru3%sw = hru1%sw + hru2%sw
        hru3%pet = hru1%pet + hru2%pet
        hru3%qtile = hru1%qtile + hru2%qtile
        hru3%irr = hru1%irr + hru2%irr       
      end function hruout_waterbal_add
      
      function hruout_nutbal_add (hru1, hru2) result (hru3)
        type (output_nutbal), intent (in) :: hru1
        type (output_nutbal), intent (in) :: hru2
        type (output_nutbal) :: hru3
        hru3%cfertn = hru1%cfertn + hru2%cfertn
        hru3%cfertp = hru1%cfertp + hru2%cfertp
        hru3%grazn = hru1%grazn + hru2%grazn
        hru3%grazp = hru1%grazp + hru2%grazp
        hru3%auton = hru1%auton + hru2%auton
        hru3%autop = hru1%autop + hru2%autop
        hru3%rmp1tl = hru1%rmp1tl + hru2%rmp1tl
        hru3%roctl = hru1%roctl + hru2%roctl
        hru3%fertn = hru1%fertn + hru2%fertn
        hru3%fertp = hru1%fertp + hru2%fertp
        hru3%fixn = hru1%fixn + hru2%fixn
        hru3%wdntl = hru1%wdntl + hru2%wdntl
        hru3%hmntl = hru1%hmntl + hru2%hmntl
        hru3%rwntl = hru1%rwntl + hru2%rwntl
        hru3%hmptl = hru1%hmptl + hru2%hmptl
        hru3%rmn2tl = hru1%rmn2tl + hru2%rmn2tl
        hru3%rmptl = hru1%rmptl + hru2%rmptl
        hru3%no3pcp = hru1%no3pcp + hru2%no3pcp
      end function hruout_nutbal_add

      function hruout_losses_add (hru1, hru2) result (hru3)
        type (output_losses), intent (in) :: hru1
        type (output_losses), intent (in) :: hru2
        type (output_losses) :: hru3
        hru3%sedyld = hru1%sedyld + hru2%sedyld
        hru3%sedorgn = hru1%sedorgn + hru2%sedorgn
        hru3%sedorgp = hru1%sedorgp + hru2%sedorgp
        hru3%surqno3 = hru1%surqno3 + hru2%surqno3
        hru3%latno3 = hru1%latno3 + hru2%latno3
        hru3%surqsolp = hru1%surqsolp + hru2%surqsolp
        hru3%usle = hru1%usle + hru2%usle
        hru3%bactp = hru1%bactp + hru2%bactp
        hru3%bactlp = hru1%bactlp + hru2%bactlp
        hru3%sedmin = hru1%sedmin + hru2%sedmin
        hru3%tileno3 = hru1%tileno3 + hru2%tileno3
      end function hruout_losses_add
      
      function hruout_plantweather_add (hru1, hru2) result (hru3)
        type (output_plantweather), intent (in) :: hru1
        type (output_plantweather), intent (in) :: hru2
        type (output_plantweather) :: hru3
        hru3%lai = hru1%lai + hru2%lai
        hru3%bioms = hru1%bioms + hru2%bioms
        hru3%yield = hru1%yield + hru2%yield
        hru3%residue = hru1%residue + hru2%residue
        hru3%sol_tmp = hru1%sol_tmp + hru2%sol_tmp
        hru3%strsw = hru1%strsw + hru2%strsw
        hru3%strstmp = hru1%strstmp + hru2%strstmp
        hru3%strsn = hru1%strsn + hru2%strsn
        hru3%strsp = hru1%strsp + hru2%strsp
        hru3%nplnt = hru1%nplnt + hru2%nplnt
        hru3%percn = hru1%percn + hru2%percn
        hru3%tmx = hru1%tmx + hru2%tmx
        hru3%tmn = hru1%tmn + hru2%tmn
        hru3%tmpav = hru1%tmpav + hru2%tmpav
        hru3%solrad = hru1%solrad + hru2%solrad
        hru3%phubase0 = hru1%phubase0 + hru2%phubase0
      end function hruout_plantweather_add

      function hruout_waterbal_div (hru1,const) result (hru2)
        type (output_waterbal), intent (in) :: hru1
        real, intent (in) :: const
        type (output_waterbal) :: hru2
        hru2%precip = hru1%precip / const
        hru2%snofall = hru1%snofall / const
        hru2%snomlt= hru1%snomlt / const
        hru2%surq_gen = hru1%surq_gen / const
        hru2%latq = hru1%latq / const
        hru2%wateryld = hru1%wateryld / const
        hru2%perc = hru1%perc / const
        hru2%et = hru1%et / const
        hru2%tloss = hru1%tloss / const
        hru2%eplant = hru1%eplant / const
        hru2%esoil = hru1%esoil / const
        hru2%surq_cont = hru1%surq_cont / const 
        hru2%cn = hru1%cn / const 
        hru2%sw = hru1%sw / const
        hru2%pet = hru1%pet / const 
        hru2%qtile = hru1%qtile / const 
        hru2%irr = hru1%irr / const 
      end function hruout_waterbal_div
      
      function hruout_waterbal_ave (hru1,const) result (hru2)
        type (output_waterbal), intent (in) :: hru1
        real, intent (in) :: const
        type (output_waterbal) :: hru2   
        hru2%cn = hru1%cn / const 
        hru2%sw = hru1%sw / const
        hru2%pet = hru1%pet / const
      end function hruout_waterbal_ave

      function hruout_nutbal_div (hru1,const) result (hru2)
        type (output_nutbal), intent (in) :: hru1
        real, intent (in) :: const
        type (output_nutbal) :: hru2
        hru2%cfertn = hru1%cfertn / const
        hru2%cfertp = hru1%cfertp / const
        hru2%grazn = hru1%grazn / const
        hru2%grazp = hru1%grazp / const
        hru2%auton = hru1%auton / const        
        hru2%autop = hru1%autop / const
        hru2%rmp1tl = hru1%rmp1tl / const
        hru2%roctl = hru1%roctl / const
        hru2%fertn = hru1%fertn / const
        hru2%fertp = hru1%fertp / const
        hru2%fixn = hru1%fixn / const
        hru2%wdntl = hru1%wdntl / const
        hru2%hmntl = hru1%hmntl / const
        hru2%rwntl = hru1%rwntl / const
        hru2%hmptl = hru1%hmptl / const
        hru2%rmn2tl = hru1%rmn2tl / const
        hru2%rmptl = hru1%rmptl / const
        hru2%no3pcp = hru1%no3pcp / const
      end function hruout_nutbal_div
      
      function hruout_losses_div (hru1,const) result (hru2)
        type (output_losses), intent (in) :: hru1
        real, intent (in) :: const
        type (output_losses) :: hru2
        hru2%sedyld = hru1%sedyld / const
        hru2%sedorgn = hru1%sedorgn/ const
        hru2%sedorgp = hru1%sedorgp / const
        hru2%surqno3 = hru1%surqno3 / const
        hru2%latno3 = hru1%latno3 / const
        hru2%surqsolp = hru1%surqsolp / const
        hru2%usle = hru1%usle / const        
        hru2%bactp = hru1%bactp / const
        hru2%bactlp = hru1%bactlp / const
        hru2%sedmin = hru1%sedmin / const
        hru2%tileno3 = hru1%tileno3 / const
      end function hruout_losses_div
      
      function hruout_plantweather_div (hru1,const) result (hru2)
        type (output_plantweather), intent (in) :: hru1
        real, intent (in) :: const
        type (output_plantweather) :: hru2
        hru2%lai = hru1%lai / const
        hru2%bioms = hru1%bioms / const
        hru2%yield = hru1%yield / const
        hru2%residue = hru1%residue / const
        hru2%sol_tmp = hru1%sol_tmp / const
        hru2%strsw = hru1%strsw / const
        hru2%strstmp = hru1%strstmp / const
        hru2%strsn = hru1%strsn / const
        hru2%strsp = hru1%strsp / const
        hru2%nplnt = hru1%nplnt / const
        hru2%percn = hru1%percn / const
        hru2%pplnt = hru1%pplnt / const
        hru2%percn = hru1%percn / const
        hru2%tmx = hru1%tmx / const
        hru2%tmn = hru1%tmn / const
        hru2%tmpav = hru1%tmpav / const
        hru2%solrad = hru1%solrad / const
        hru2%phubase0 = hru1%phubase0 / const
      end function hruout_plantweather_div
                  
      function hruout_plantweather_ave (hru1,const) result (hru2)
        type (output_plantweather), intent (in) :: hru1
        real, intent (in) :: const
        type (output_plantweather) :: hru2
        hru2%lai = hru1%lai / const
        hru2%bioms = hru1%bioms / const
        hru2%yield = hru1%yield
        hru2%residue = hru1%residue / const
        hru2%sol_tmp = hru1%sol_tmp / const
        hru2%strsw = hru1%strsw / const
        hru2%strstmp = hru1%strstmp / const
        hru2%strsn = hru1%strsn / const
        hru2%strsp = hru1%strsp / const
        hru2%nplnt = hru1%nplnt / const
        hru2%percn = hru1%percn / const
        hru2%pplnt = hru1%pplnt / const
        hru2%percn = hru1%percn / const
        hru2%tmx = hru1%tmx / const
        hru2%tmn = hru1%tmn / const
        hru2%tmpav = hru1%tmpav / const
        hru2%solrad = hru1%solrad / const
        hru2%phubase0 = hru1%phubase0 / const
      end function hruout_plantweather_ave
                            
      end module output_landscape_module