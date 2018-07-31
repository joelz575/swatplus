      module hru_module
    
      implicit none
      
      integer :: isep                !          |
      integer :: ith                 !          |
      integer :: ilu                 !          | 
      integer :: ulu                 !          |
      integer :: ipot                !          |
      integer :: iwgen               !          |
      character (len=1) :: timest    !          |
     
      type uptake_parameters
       real :: water_dis = 10.        !               |the uptake distribution for water is hardwired
       real :: water_norm             !none           |water uptake normalization parameter 
       real :: n_norm                 !none           |nitrogen uptake normalization parameter 
       real :: p_norm                 !none           |phosphorus uptake normalization parameter
      end type uptake_parameters
      type (uptake_parameters)  :: uptake
      
      type pesticide
        character(len=10) :: name
        integer :: num_db
        real :: enr = 0.         !! |none         |pesticide enrichment ratio
        real :: zdb = 0.         !! |mm           |division term from net pesticide equation
      end type pesticide

      type irrigation_sources
        integer :: flag = 0   !0= don't irrigate, 1=irrigate
        integer, dimension(:), allocatable :: chan
        integer, dimension(:), allocatable :: res
        integer, dimension(:), allocatable :: pond
        integer, dimension(:), allocatable :: shal
        integer, dimension(:), allocatable :: deep
      end type irrigation_sources
      
      type topography
           character(len=13) :: name
           real :: elev = 0.         !!               |m             |elevation of HRU
           real :: slope = 0.        !!	hru_slp(:)  |m/m           |average slope steepness in HRU
           real :: slope_len = 0.    !! slsubbsn(:)   |m             |average slope length for erosion
           real :: dr_den = 0.       !!               |km/km2        |drainage density
           real :: lat_len = 0.      !! slsoil(:)     |m             |slope length for lateral subsurface flow
           real :: dis_stream = 0.   !! dis_stream(:) | m            |average distance to stream
           real :: dep_co = 1.       !!               |              |deposition coefficient
           integer :: field_db = 0   !!               |              |pointer to field.fld
           integer :: channel_db=0   !!               |              |pointer to channel.dat
      end type topography
      
      type field
           character(len=13) :: name = "rep field"
           real :: length = 0.2  !!            |km            |field length for wind erosion
           real :: wid = 0.2  !!               |km            |field width for wind erosion
           real :: ang = 60.  !!               |deg           |field angle for wind erosion
      end type field
      
      type hydrology
           character(len=13) :: name
           real :: lat_ttime = 0.  !! lat_ttime(:)  |none          |Exponential of the lateral flow travel time
           real :: lat_sed = 0.    !! lat_sed(:)    |g/L           |sediment concentration in lateral flow
           real :: canmx = 0.      !! canmx(:)      |mm H2O        |maximum canopy storage
           real :: esco = 0.       !! esco(:)       |none          |soil evaporation compensation factor
           real :: epco = 0.       !! epco(:)       |none          |plant water uptake compensation factor (0-1)
           real :: erorgn = 0.     !! erorgn(:)     |none          |organic N enrichment ratio, if left blank
                                   !!                              |the model will calculate for every event
           real :: erorgp = 0.     !! erorgp(:)     |none          |organic P enrichment ratio, if left blank
                                   !!                              |the model will calculate for every event
           real :: cn3_swf = 0.    !! evpot(:)      |none          |pothole evaporation coefficient
           real :: biomix = 0.     !! biomix(:)     |none          |biological mixing efficiency.
                                   !!                              |Mixing of soil due to activity of earthworms
                                   !!                              |and other soil biota. Mixing is performed at
                                   !!                              |the end of every calendar year.
           real :: dep_imp = 0.    !! dep_imp(:)    |mm            |depth to impervious layer
           real :: dep_imp_init = 0.    !! dep_imp(:)    |mm       |initial depth to impervious layer- for calibration
           real :: lat_orgn = 0.
           real :: lat_orgp = 0.
           real :: harg_pet  = .0023  
           real :: cncoef = 0.3    !!               |              |plant ET curve number coefficient
           real :: perco = 1.      !!               |              |percolation coefficient-adjusts soil mositure
                                   !!                              | for perc to occur (1.0 = fc)
      end type hydrology
      
      type landuse
          character(len=15) :: name
          integer :: cn_lu = 0
          integer :: cons_prac = 0
          real :: usle_p = 0.           !! none     | USLE equation support practice (P) factor daily
          character (len=16) :: urb_ro  !! none     | urban runoff model
                                        !!          | "usgs_reg", simulate using USGS regression eqs
                                        !!          | "buildup_washoff", simulate using build up/wash off alg 
          integer ::  urb_lu = 0        !! none     | urban land type identification number
          real :: ovn = 0.05            !! none     | Manning's "n" value for overland flow
      end type landuse
      type (landuse), dimension (:), allocatable :: luse

      type hru_databases
        character(len=13) :: name = ""
        integer :: topo = 1
        integer :: hyd = 1
        integer :: soil = 1
        integer :: land_use_mgt = 1
        integer :: soil_nutr_init = 1
        integer :: surf_stor = 0
        integer :: snow = 1
        integer :: field = 0
      end type hru_databases
      
      type hru_databases_char
        character(len=16) :: name = ""
        character(len=16) :: topo = ""
        character(len=16) :: hyd = ""
        character(len=16) :: soil = ""
        character(len=16) :: land_use_mgt = ""
        character(len=16) :: soil_nutr_init = ""
        character(len=16) :: surf_stor = ""
        character(len=16) :: snow = ""
        character(len=16) :: field = ""
      end type hru_databases_char
        
      type hru_parms_db
        real :: co2 = 350.
      end type hru_parms_db
      
      type hydrologic_response_unit_db
        character(len=13) :: name = "default"
        type (hru_databases) :: dbs
        type (hru_databases_char) :: dbsc
        type (hru_parms_db) :: parms
      end type hydrologic_response_unit_db
      type (hydrologic_response_unit_db), dimension(:),allocatable :: hru_db
      
      type land_use_mgt_variables
        real :: usle_p = 0.                 !! |none          |USLE equation comservation practice (P) factor
        real :: usle_ls = 0.                !! |none          |USLE equation length slope (LS) factor
        real :: usle_mult = 0.              !! |none          |product of USLE K,P,LS,exp(rock)
        real :: sdr_dep = 0.                !! |
        integer :: ldrain= 0.               !! |none          |soil layer where drainage tile is located
        real :: tile_ttime = 0.             !! |none          |Exponential of the tile flow travel time
        real :: vfsi = 0.                   !! |none          |initial SCS curve number II value
        real :: vfsratio = 0.               !! |none          |contouring USLE P factor
        real :: vfscon = 0.                 !! |none          |fraction of the total runoff from the entire field
        real :: vfsch = 0;                  !! |none          |fraction of flow entering the most concentrated 10% of the VFS.
                                            !!                     which is fully channelized
        integer :: ngrwat = 0
        real :: grwat_i = 0.                !! |none          |On/off Flag for waterway simulation
        real :: grwat_n = 0.                !! |none          |Mannings's n for grassed waterway
        real :: grwat_spcon = 0.            !! |none          |sediment transport coefficant defined by user
        real :: grwat_d = 0.                !! |m             |depth of Grassed waterway
        real :: grwat_w = 0.                !! |none          |Width of grass waterway
        real :: grwat_l = 0.                !! |km            |length of Grass Waterway
        real :: grwat_s = 0.                !! |m/m           |slope of grass waterway
        real :: bmp_flag = 0.  
        real :: bmp_sed = 0.                !! |%             | Sediment removal by BMP 
        real :: bmp_pp = 0.                 !! |%             | Particulate P removal by BMP
        real :: bmp_sp = 0.                 !! |%             | Soluble P removal by BMP
        real :: bmp_pn = 0.                 !! |%             | Particulate N removal by BMP 
        real :: bmp_sn = 0.                 !! |%             | Soluble N removal by BMP  
        real :: bmp_bac = 0.                !! |%             | Bacteria removal by BMP
      end type land_use_mgt_variables
     
      type hydrologic_response_unit
        character(len=13) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        integer :: surf_stor               !!points to res() for surface storage
        type (hru_databases) :: dbs       !!database pointers
        type (hru_databases_char) :: dbsc !!database pointers
        type (hru_parms_db) :: parms      !!calibration parameters
        integer :: land_use_mgt
        character(len=16) :: land_use_mgt_c
        integer :: lum_group
        character(len=16) :: lum_group_c        !!land use group for soft cal and output
        character(len=16) :: region
        integer :: plant_cov
        integer :: mgt_ops
        integer :: tiledrain = 0
        integer :: septic = 0
        integer :: fstrip = 0
        integer :: grassww = 0
        integer :: bmpuser = 0
        !! impunded water - points to res()
        integer :: res
        type (pesticide), dimension(:), allocatable :: pst  !pest names simulated in the hru

        !! other data
        type (topography) :: topo
        type (field) :: field
        type (hydrology) :: hyd
        type (landuse) :: luse
        type (land_use_mgt_variables) :: lumv
        integer :: irrsrc
        real :: water_fr
        real :: water_seep
        integer :: ich_flood
      end type hydrologic_response_unit
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru_init

      type pothole_dynamic
          real :: seep = 0.
          real :: vol = 0.            !! m**3 H2O     |current vol of water stored in the depression/impounded area
          real :: evap = 0.
          real :: sedin = 0.
          real :: solp = 0.           !! kg N         |amount of soluble p in pothole water body
          real :: solpi = 0.
          real :: orgp = 0.           !! kg N         |amount of organic P in pothole water body
          real :: orgpi = 0.
          real :: orgn = 0.           !! kg N         |amount of organic N in pothole water body
          real :: orgni = 0.
          real :: mps = 0.            !! kg N         |amount of stable mineral pool P in pothole water body
          real :: mpsi = 0.
          real :: mpa = 0.            !! kg N         |amount of active mineral pool P in pothole water body
          real :: mpai = 0.
          real :: no3i = 0.
          real :: sa = 0.             !! ha           |surface area of impounded water body
          real :: volx = 0.
          real :: flwi = 0.           !! m^3 H2O      |water entering pothole on day
          real :: sedi = 0.           !! metric tons  |sediment entering pothole on day
          real :: tile = 0.           !! m3/d         |average daily outflow to main channel from tile flow if drainage tiles are installed
                                                        !! in pothole (needed only if current HRU is  IPOT)
          real :: sed = 0.            !! metric tons  | amount of sediment in pothole water body
          real :: no3 = 0.            !! kg N         | amount of nitrate in pothole water body
          real :: san = 0.
          real :: sil = 0.
          real :: cla = 0.
          real :: lag = 0.
          real :: sag = 0.
      end type pothole_dynamic
      type (pothole_dynamic), dimension (:), allocatable :: pot

      
      real :: precipday         !! mm   |daily precip for the hru
      real :: precip_eff        !! mm   |daily effective precip for runoff calculations = precipday + ls_overq + snomlt - canstor
                                !!      |precip_eff = precipday + ls_overq - snofall + snomlt - canstor
      real :: qday              !! mm   |surface runoff that reaches main channel during day in HRU                               
                                
!!    change per JGA 8/31/2011 gsm for output.mgt 
      real :: yield,  pst_kg
      
!!    new/modified arrays for plant competition
      integer :: ipl, isol

      real :: strsa_av,strsn_av,strsp_av,strstmp_av
      real :: rto_no3,rto_solp,uno3d_tot,uapd_tot,sum_no3
      real :: sum_solp
      real, dimension (:), allocatable :: epmax,cvm_com,blai_com
      real, dimension (:), allocatable :: rsdco_plcom, translt
      real, dimension (:), allocatable :: strsw_av,uno3d,uapd
      real, dimension (:), allocatable :: par,htfac,un2,up2
      integer, dimension (:), allocatable :: iseptic
     
!! septic variables for output.std
      real :: peakr, sw_excess, albday
      real :: wt_shall
      real :: sq_rto
      real :: tloss, snomlt, snofall, fixn, qtile
      real :: latlyr                 !!mm            |lateral flow in soil layer for the day
      real :: inflpcp                !!mm            |amount of precipitation that infiltrates
      real :: fertn, sepday, bioday
      real :: sepcrk, sepcrktot, fertno3, fertnh3, fertorgn, fertsolp
      real :: fertorgp
      real :: fertp, grazn, grazp, sdti
      real :: voltot                 !!mm            |total volumne of cracks expressed as depth per area unit
      real :: volcrmin               !!mm            |minimum crack volume allowed in any soil layer
      real :: canev, usle, rcn
      real :: bactrop, bactsedp
      real :: enratio
      real :: da_ha, vpd
      real :: bactrolp, bactsedlp, pet_day, ep_day
      real :: snoev, sno3up
      real :: es_day, ls_overq, latqrunon
      real :: sbactrop, sbactrolp, sbactsedp, sbactsedlp, ep_max
      real :: sbactlchlp
      real :: bsprev
      real :: usle_ei, no3pcp, rcharea
      real :: snocov1, snocov2, lyrtile

      real :: autop, auton, etday, hmntl, rwntl, hmptl, rmn2tl
      real :: rmptl, wdntl, rmp1tl, roctl, gwseep
      real :: wdlprch
      integer :: myr
      integer :: nhru,  mo, nrch
      integer :: ihru             !!none          |HRU number
      integer :: npmx, curyr
      integer :: nd_30
      integer :: iscen
      integer :: mpst, mlyr
      integer, dimension(100) :: ida_lup, iyr_lup
      integer :: no_up
!  routing 5/3/2010 gsm per jga    
! date
      character(len=8) :: date

!! septic change added iseptic 1/28/09 gsm
      integer :: isep_ly
      real, dimension (:), allocatable :: percp    
      real, dimension (:), allocatable :: qstemm
!! septic changes added 1/28/09 gsm
      real, dimension (:), allocatable :: bio_bod, biom,rbiom
      real, dimension (:), allocatable :: fcoli, bz_perc, plqm
!! Septic system by Jaehak Jeong
      integer, dimension (:), allocatable :: i_sep
      integer, dimension (:), allocatable :: sep_tsincefail
      
 !!   change per JGA 9/8/2011 gsm for output.mgt 
      real, dimension (:), allocatable :: sol_sumno3, sol_sumsolp
      
! output files 
!!  added for binary files 3/25/09 gsm
      real, dimension (:,:), allocatable :: wpstaao

!     Sediment parameters added by Balaji for the new routines

      real, dimension (:), allocatable :: sanyld,silyld,clayld,sagyld
      real, dimension (:), allocatable :: lagyld,grayld
      integer, dimension (:), allocatable :: itb
      real, dimension (:), allocatable :: qird
      
!!!!!! drains
      real, dimension (:), allocatable :: wnan
      real, dimension (:,:), allocatable :: uh

      real, dimension (:), allocatable :: irramt
      real, dimension (:), allocatable :: phusw
      integer, dimension (:), allocatable :: nop
      integer, dimension (:), allocatable :: yr_skip, isweep
      real :: sweepeff,frt_kg, pst_dep

      real, dimension (:), allocatable :: ranrns_hru
      integer, dimension (:), allocatable :: itill

      real, dimension (:), allocatable :: tc_gwat
      real, dimension (:), allocatable :: wfsh
      real, dimension (:), allocatable :: fsred
      real, dimension (:), allocatable :: sed_con, orgn_con, orgp_con
      real, dimension (:), allocatable :: soln_con, solp_con
      real, dimension (:), allocatable :: filterw
      real, dimension (:), allocatable :: flowfr
      real, dimension (:), allocatable :: flowmin
      real, dimension (:), allocatable :: divmax, cn2
      real, dimension (:), allocatable :: sol_cov
      real, dimension (:), allocatable :: driftco
      real, dimension (:), allocatable :: smx,sci
      real, dimension (:), allocatable :: bactpq
      real, dimension (:), allocatable :: cnday
      real, dimension (:), allocatable :: bactlpq
      real, dimension (:), allocatable :: bactps,bactlps,tmpav
      real, dimension (:), allocatable :: sno_hru,sno_init,hru_ra
      real, dimension (:), allocatable :: tmx,tmn
      real, dimension (:), allocatable :: tconc,hru_rmx
      real, dimension (:), allocatable :: usle_cfac,usle_eifac
      real, dimension (:), allocatable :: aird,t_ov
      real, dimension (:), allocatable :: u10,rhd
      real, dimension (:), allocatable :: canstor,ovrlnd
      real, dimension (:), allocatable :: irr_mx, auto_wstr
      real, dimension (:), allocatable :: irr_asq, irr_eff
      real, dimension (:), allocatable :: irrsq
      real, dimension (:), allocatable :: bio_eat         !!(kg/ha)/day     |dry weight of biomass removed by grazing daily
      real, dimension (:), allocatable :: bio_trmp        !!(kg/ha)/day     |dry weight of biomass removed by trampling daily
      integer, dimension (:), allocatable :: irr_noa
      integer, dimension (:), allocatable :: irr_sc,irr_no
      integer, dimension (:), allocatable :: imp_trig, irr_sca
      integer, dimension (:), allocatable :: wstrs_id
!    Drainmod tile equations  08/2006 
	  real, dimension (:), allocatable :: cumei,cumeira
	  real, dimension (:), allocatable :: cumrt, cumrai
      real, dimension (:), allocatable :: sstmaxd
	  real, dimension (:), allocatable :: stmaxd
!    Drainmod tile equations  08/2006
      real, dimension (:), allocatable :: bio_min         !!kg/ha           |minimum plant biomass for grazing
      real, dimension (:), allocatable :: surqsolp
      real, dimension (:), allocatable :: cklsp
      real, dimension (:), allocatable :: trapeff
      real, dimension (:), allocatable :: pplnt,snotmp
      real, dimension (:), allocatable :: brt

      real, dimension (:), allocatable :: twash,doxq
      real, dimension (:), allocatable :: percn
      real, dimension (:), allocatable :: cbodu,chl_a,qdr
      real, dimension (:), allocatable :: latno3,latq,nplnt
      real, dimension (:), allocatable :: tileq, tileno3
      real, dimension (:), allocatable :: sedminpa,sedminps,sedorgn
      real, dimension (:), allocatable :: sedorgp,sedyld,sepbtm
      real, dimension (:), allocatable :: surfq,surqno3
      real, dimension (:), allocatable :: hru_dafr
      real, dimension (:), allocatable :: phubase
      real, dimension (:), allocatable :: lai_yrmx,dormhr
      real, dimension (:), allocatable :: wtab,wtab_mn,wtab_mx
      real, dimension (:), allocatable :: manure_kg
      real, dimension (:,:), allocatable :: rfqeo_30d,eo_30d
      real, dimension (:,:), allocatable :: wrt
      real, dimension (:,:), allocatable :: bss,surf_bs  
      real, dimension (:,:,:), allocatable :: pst_lag
      integer, dimension (:), allocatable :: swtrg,hrupest
      !! burn
      integer, dimension (:), allocatable :: grz_days
      integer, dimension (:), allocatable :: irrno
      integer, dimension (:), allocatable :: igrz,ndeat
      integer, dimension (:), allocatable :: irrsc
      integer, dimension (:), allocatable :: irr_flag
      integer, dimension (:), allocatable :: manure_id

!!     gsm added for sdr (drainage) 7/24/08
      integer, dimension (:,:), allocatable :: mgt_ops
      integer, dimension (:), allocatable :: npno

      real, dimension (:,:), allocatable :: hhqday
! additional reach variables , added by Ann van Griensven
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
!Additional buffer and filter strip variables Mike White

	real, dimension (:), allocatable :: ubnrunoff,ubntss
	real, dimension (:,:), allocatable :: ovrlnd_dt,hhsurfq	
	real, dimension (:,:,:), allocatable :: hhsurf_bs

!! subdaily erosion modeling by Jaehak Jeong
	real, dimension(:,:), allocatable:: hhsedy
	real, dimension(:), allocatable:: init_abstrc
     
      real :: bio_init, lai_init, cnop, harveff, frac_harvk

      integer, dimension(:), allocatable :: tillage_switch
      real, dimension(:), allocatable :: tillage_depth
      integer, dimension(:), allocatable :: tillage_days
      real, dimension(:), allocatable :: tillage_factor

      end module hru_module