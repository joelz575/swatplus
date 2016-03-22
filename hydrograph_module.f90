      module hydrograph_module
      
      use time_module
      use basin_module
      
      integer :: mhyd, mcmd,inum2,jrch,jrchq,mrte,ihout,iwst,isdch
      integer :: icmd, ich, mobj_out, isd_chsur
      integer, dimension (:), allocatable :: rcv_sum, dfn_sum, elem_cnt
      real, dimension (:), allocatable :: hyd_km2
      real, dimension(:,:,:), allocatable:: rchhr

      type hyd_output
        !H !character (len=13) :: name
        real :: temp = 0.              !! deg c        |temperature
        real :: flo = 0.               !! m^3          |volume of water
        real :: sed = 0.               !! metric tons  |sediment
        real :: orgn = 0.              !! kg N         |organic N
        real :: sedp = 0.              !! kg P         |organic P
        real :: no3 = 0.               !! kg N         |NO3-N
        real :: solp = 0.              !! kg P         |mineral (soluble P)
        real :: psol = 0.              !! mg pst       |pesticide in solution
        real :: psor = 0.              !! mg pst       |pestitice sorbed to sediment
        real :: chla = 0.              !! kg           |chlorophyll-a
        real :: nh3 = 0.               !! kg N         |NH3
        real :: no2 = 0.               !! kg N         |NO2
        real :: cbod = 0.              !! kg           |carbonaceous biological oxygen demand
        real :: dox = 0.               !! kg           |dissolved oxygen
        real :: bacp = 0.              !! # cfu/100ml  |persistent bacteria
        real :: baclp = 0.             !! # cfu/100ml  |less persistent bacteria
        real :: met1 = 0.              !! kg           |conservative metal #1
        real :: met2 = 0.              !! kg           |conservative metal #2
        real :: met3 = 0.              !! kg           |conservative metal #3
        real :: san = 0.               !! tons         |detached sand
        real :: sil = 0.               !! tons         |detached silt
        real :: cla = 0.               !! tons         |detached clay
        real :: sag = 0.               !! tons         |detached small ag
        real :: lag = 0.               !! tons         |detached large ag
        real :: grv = 0.               !! tons         |gravel
      end type hyd_output
      
      type (hyd_output), dimension(:),allocatable :: hd
      type (hyd_output), dimension(:),allocatable :: hm
      type (hyd_output), dimension(:),allocatable :: ha
      type (hyd_output), dimension(:),allocatable :: haa
      type (hyd_output) :: hz
      type (hyd_output), dimension(:),allocatable :: hcnst
      type (hyd_output), dimension(:),allocatable :: hhr
      type (hyd_output) :: ht1, ht2, ht3, ht4, ht5, delrto
      
      type (hyd_output), dimension(:),allocatable :: res
  
      type hyd_header
        !H !character (len=16) :: name  =     'obj_name        ' 
        character (len=8) :: yrs =        '    time'
        character (len=8) :: yrc =        '    year'
        !H !character (len=8) :: icmd =       '   icmd'
        character (len=8) :: otype =      '    type'
        !H !character (len=8) :: oprops =     '  props' 
        !H !character (len=8) :: io =         ' in/out' 
        !H !character (len=8) :: iotyp =      ' obj_typ'
        character (len=8) :: iotypno =    '  typ_no'
        !H !character (len=8) :: hydio =      ' hyd_typ'
        !H !character (len=8) :: objno =      ' obj_no'
        !H !character (len=13) :: name2 =  '   hyd_name'
        character (len=18) :: temp =   '         temp_degc'        !! deg c        |temperature
        character (len=18) :: flo =    '           flo_m^3'        !! m^3          |volume of water
        character (len=18) :: sed =    '         sed_mtons'        !! metric tons  |sediment
        character (len=18) :: orgn =   '          orgn_kgN'        !! kg N         |organic N
        character (len=18) :: sedp =   '          sedp_kgP'        !! kg P         |organic P
        character (len=18) :: no3 =    '           no3_kgN'        !! kg N         |NO3-N
        character (len=18) :: solp =   '          solp_kgP'        !! kg P         |mineral (soluble P)
        character (len=18) :: psol =   '        psol_mgpst'        !! mg pst       |pesticide in solution
        character (len=18) :: psor =   '        psor_mgpst'        !! mg pst       |pestitice sorbed to sediment
        character (len=18) :: chla =   '           chla_kg'        !! kg           |chlorophyll-a
        character (len=18) :: nh3 =    '           nh3_kgN'        !! kg N         |NH3
        character (len=18) :: no2 =    '           no2_kgN'        !! kg N         |NO2
        character (len=18) :: cbod =   '           cbod_kg'        !! kg           |carbonaceous biological oxygen demand
        character (len=18) :: dox =    '            dox_kg'        !! kg           |dissolved oxygen
        character (len=18) :: bacp =   '   bacp_#cfu/100ml'        !! # cfu/100ml  |persistent bacteria
        character (len=18) :: baclp =  '  baclp_#cfu/100ml'        !! # cfu/100ml  |less persistent bacteria
        character (len=18) :: met1 =   '           met1_kg'        !! kg           |conservative metal #1
        character (len=18) :: met2 =   '           met2_kg'        !! kg           |conservative metal #2
        character (len=18) :: met3 =   '           met3_kg'        !! kg           |conservative metal #3
        character (len=18) :: san =    '          san_tons'        !! tons         |detached sand
        character (len=18) :: sil =    '          sil_tons'        !! tons         |detached silt
        character (len=18) :: cla =    '          cla_tons'        !! tons         |detached clay
        character (len=18) :: sag =    '          sag_tons'        !! tons         |detached small ag
        character (len=18) :: lag =    '          lag_tons'        !! tons         |detached large ag
        character (len=18) :: grv =    '          grv_tons'        !! tons         |gravel
      end type hyd_header
      type (hyd_header) :: hyd_hdr
      type object_output
        character (len=3) :: name
        character (len=3) :: obtyp     !! object type: hru,hlt,hs,rxc,dr,out,sdc
        integer :: obtypno             !! object type number: 1=hru, 2=hru_lte, 3=channel
        character (len=3) :: hydtyp    !! hydrograph type: tot,rhg,sur,lat,til
        integer :: objno               !! object number
        integer :: hydno               !! code computes from hydtyp
        character (len=13) :: filename !! file with hydrograph output from the object
        integer :: unitno = 5009       !! filename unit number
      end type object_output
      type (object_output), dimension (:), allocatable :: ob_out
      
      type timestep
        type (hyd_output), dimension(:),allocatable :: hh
      end type timestep
      type (timestep), dimension(:),allocatable, save :: ts

      type object_connectivity
        character(len=16) :: name = "default"
        character(len=8) :: typ = " "       !object type - ie hru, hru_lte, sub, chan, res, recall
        real :: lat              !latitude (degrees)
        real :: long             !longitude (degrees) 
        real :: area_ha = 80.    !area - ha
        integer :: props = 1     !properties number from data base (ie hru.dat, sub.dat) - change props to data
        integer :: wst = 1       !weather station number
        integer :: constit       !constituent data pointer to pesticides, pathogens, metals, salts
        integer :: props2        !overbank connectivity pointer to landscape units - change props2 to overbank
        integer :: ruleset       !ruleset pointer for flow fraction of hydrograph
  
        integer :: num = 1       !spatial object number- ie hru number corresponding to sequential command number
                                 !this is the first column in hru_dat (doesn't have to be sequential)
   
        integer :: fired = 0     !0=not fired; 1=fired off as a command
        integer :: cmd_next = 0  !next command (object) number
        integer :: cmd_prev = 0  !previous command (object) number
        integer :: cmd_order = 0 !1=headwater,2=2nd order,etc
        integer :: src_tot = 0   !total number of outgoing (source) objects
        integer :: rcv_tot = 0   !total number of incoming (receiving) hydrographs
        integer :: rcvob_tot = 0 !total number of incoming (receiving) objects
        integer :: dfn_tot = 0   !total number of defining objects (ie hru's within a subbasin)
        integer :: subs_tot      !number of subbasins that contain this object
        integer :: elem          !subbasins element number for this object
        integer :: flood_ch_lnk = 0  !channel the landscape unit is linked to
        integer :: flood_ch_elem = 0 !landscape unit number - 1 is nearest to stream
        integer :: wr_ob = 0         !1=element in a water rights object; 0=not an element
        character (len=3), dimension(:), allocatable :: obtyp_out    !outflow object type (ie 1=hru, 2=sd_hru, 3=sub, 4=chan, etc)
        integer, dimension(:), allocatable :: obtypno_out  !outflow object type name
        integer, dimension(:), allocatable :: obj_out      !outflow object
        character (len=3), dimension(:), allocatable :: htyp_out     !outflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        integer, dimension(:), allocatable :: ihtyp_out     !outflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        real, dimension(:), allocatable :: frac_out        !fraction of hydrograph
        character(len=8), dimension(:), allocatable :: obtyp_in     !inflow object type (ie 1=hru, 2=sd_hru, 3=sub, 4=chan, etc)
        integer, dimension(:), allocatable :: obtypno_in   !outflow object type number
        integer, dimension(:), allocatable :: obj_in
        integer, dimension(:), allocatable :: htyp_in
        real, dimension(:), allocatable :: frac_in
        type (hyd_output) :: hin                           !inflow hydrograph for surface runon - sum of all inflow hyds
        type (hyd_output) :: hin_s                         !inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
        type (hyd_output), dimension(:),allocatable :: hd  !generated hydrograph (ie 1=tot, 2= recharge, 3=surf, etc)
        type (hyd_output), dimension(:,:),allocatable :: ts  !subdaily hydrographs
        type (hyd_output), dimension(:),allocatable :: tsin  !inflow subdaily hydrograph
        integer :: day_cur = 1                               !current hydrograph day in ts
        integer :: day_max                                   !maximum number of days to store the hydrograph
        real :: peakrate                                   !peak flow rate during time step - m3/s
        
        type (hyd_output), dimension(:),allocatable :: hin_m
        type (hyd_output), dimension(:),allocatable :: hin_y
        type (hyd_output), dimension(:),allocatable :: hin_a
        type (hyd_output), dimension(:),allocatable :: hout_m
        type (hyd_output), dimension(:),allocatable :: hout_y
        type (hyd_output), dimension(:),allocatable :: hout_a
        type (hyd_output) :: hdep_m
        type (hyd_output) :: hdep_y
        type (hyd_output) :: hdep_a
        integer, dimension(:), allocatable :: obj_subs      !subbasins object number that contain this object
      end type object_connectivity
      type (object_connectivity), dimension(:),allocatable, save :: ob
      
     type water_right_elements
        character(len=16) :: name
        integer :: obj = 1              !object number
        character (len=3) :: obtyp      !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer :: right                !0-100
        integer :: obtypno = 0          !2-number of hru_lte's or 1st hru_lte command
        integer,  dimension(:), allocatable :: wro   !water rights object the element is in
      end type water_right_elements
      
      type water_rights_object
        character(len=16) :: name
        integer :: num_src
        character(len=16), dimension(:), allocatable :: typ_src
        integer, dimension(:), allocatable :: num_elem
        type (water_right_elements), dimension(:), allocatable :: wro_elem
        real, dimension(:), allocatable :: demand
      end type water_rights_object
      
      !water rights elements (objects) within the water rights object
      type water_rights_elements
        character (len=16) :: name
        character (len=16) :: ob_typ            !object type - hru, channel, reservoir, etc
        character (len=16) :: ob_num            !object number
        character (len=16) :: rights_typ        !ie. jr, sr
        real :: rights                          !ie. irr demand, minimum flow, flow fraction, etc)
      end type water_rights_elements
      
      !water rights objects
      type water_rights_data
        character (len=16) :: name
        integer :: num = 0                      !number of objects
        integer :: constit                      !points to constituent data
        character (len=16) :: cond              !points to ruleset to allocate water within the water rights object
        type (water_rights_elements), dimension (:), allocatable :: elem    !irrigation water
      end type water_rights_data
      type (water_rights_data),dimension(:),allocatable:: wr_ob
      
      !water allocation
      type water_allocation
        integer :: typ = 0                                                  !water rights object = 1; hru unlimited source = 0
        real, dimension (:), allocatable :: demand                        !m^3    |water demand for each element
        !hyd_output units are in mm and mg/L
        type (hyd_output), dimension (:), allocatable :: hd                 !irrigation water
      end type water_allocation
      type (water_allocation),dimension(:),allocatable:: wat_allo
      
      !recall hydrograph inputs
      type recall_hydrograph_inputs
         character (len=16) :: name
         integer :: num = 0                    !number of elements
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename         !filename
         !hyd_output units are in cms and mg/L
         type (hyd_output), dimension (:,:), allocatable :: hd     !export coefficients
      end type recall_hydrograph_inputs
      type (recall_hydrograph_inputs),dimension(:),allocatable:: recall
      
      type spatial_objects
        integer :: objs = 0      !number of objects or 1st object command
        integer :: hru = 0       !1-number of hru's or 1st hru command
        integer :: hru_lte = 0   !2-number of hru_lte's or 1st hru_lte command
        integer :: sub = 0       !3-number of sub's or 1st sub command
        integer :: modflow = 0   !4-number of modparm's or 1st modparm command
        integer :: aqu = 0       !5-number of aquifer's or 1st aquifer command
        integer :: chan = 0      !6-number of chan's or 1st chan command
        integer :: res = 0       !7-number of res's or 1st res command
        integer :: recall = 0    !8-number of recdays's or 1st recday command
        integer :: exco = 0      !11-number of exco's or 1st export coeff command
        integer :: dr = 0        !12-number of dr's or 1st del ratio command
        integer :: canal = 0     !13-number of canal's or 1st canal command
        integer :: pump = 0      !14-number of pump's or 1st pump command
        integer :: outlet = 0    !15-number of outlet's or 1st outlet command
        integer :: chandeg = 0   !16-number of swat-deg channel's or 1st swat-deg channel command
        integer :: aqu2d = 0     !17-number of 2D aquifer's or 1st 2D aquifer command
        integer :: herd = 0      !18-number of herds
        integer :: wro = 0       !19-number of water rights
      end type spatial_objects
      type (spatial_objects) :: sp_ob
      type (spatial_objects) :: sp_ob1
      
      type subbasin_data
        integer :: num_tot
        integer, dimension (:), allocatable :: num             !points to subbasin element (sub_elem)
      end type subbasin_data
      type (subbasin_data),dimension(:), allocatable:: sub_d
      
      type subbasin_elements
        character(len=16) :: name
        integer :: obj = 1              !object number
        character (len=3) :: obtyp      !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer :: obtypno = 0          !2-number of hru_lte's or 1st hru_lte command
        character (len=3) :: htyp       !hydrograph type (1=total, 2=surface, etc)
        integer :: htypno               !outflow hyd type (ie 1=tot, 2= recharge, 3=surf, etc)
        real :: frac = 0                !fraction of element in sub (expansion factor)
        integer :: idr = 0               !points to dr's in delratio.dat
        type (hyd_output), dimension (:), allocatable :: dr    !calculated del ratios for element
        integer,  dimension(:), allocatable :: sub   !subbasin the element is in
      end type subbasin_elements
      type (subbasin_elements), dimension(:), allocatable :: sub_elem
      
      integer,  dimension(:), allocatable :: ielem_sub   !sequential counter for subbasin the hru is in
            
      !channel-surface element linkage for overbank flooding
      type channel_surface_elements
        character(len=16) :: name
        integer :: num = 0                                      !number of elements
        character (len=3), dimension(:), allocatable :: obtyp   !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer, dimension(:), allocatable :: obtypno           !2-number of hru_lte's or 1st hru_lte command
        real, dimension(:), allocatable :: wid                  !maxflood plain width for each element
        real, dimension(:), allocatable :: dep                  !max flood depth for each element
        real, dimension(:), allocatable :: flood_volmx          !max flood volume for each landscape unit
        type (hyd_output), dimension (:), allocatable :: hd     !flood water for each element
      end type channel_surface_elements
      type (channel_surface_elements),dimension(:),allocatable :: ch_sur
      
      !channel-aquifer linkage for 2D groundwater flow model
      type channel_aquifer_elements
        character(len=16) :: name
        integer :: num = 0                               !number of elements
        integer, dimension(:), allocatable :: aqu_no     !aquifer number
      end type channel_aquifer_elements
      type (channel_aquifer_elements),dimension(:),allocatable :: ch_aqu
      
      !export coefficient is hyd_output type but not part of an object 
      type (hyd_output), dimension(:), allocatable :: dr          !delivery ratio for objects- chan, res, lu
      type (hyd_output), dimension(:), allocatable :: sub_dr      !delivery ratio for subbasin elements
      
      !delevery ratio is hyd_output type but not part of an object 
      type (hyd_output), dimension(:), allocatable :: exco        !export coefficient

      interface operator (+)
        module procedure hydout_add
      end interface
            
      interface operator (**)
        module procedure hydout_mult
        end interface 
      
      interface operator (.add.)
        module procedure hydout_add_const
      end interface 

      interface operator (*)
        module procedure hydout_mult_const
      end interface 

      interface operator (/)
        module procedure hydout_div_const
      end interface   
             
      interface operator (//)
        module procedure hydout_div_conv
      end interface   
             
      contains
      include 'hyd_connect_out.f90'
      include 'hydin_output.f90'
      include 'hydout_output.f90'
      include 'obj_output.f90'
      include 'object_output_read.f90'
      include 'hyddep_output.f90'
            
      !! function to convert concentration to mass
      subroutine hyd_convert_mass (hyd1)
        type (hyd_output), intent (inout) :: hyd1
        ! m3/s to m3
        hyd1%flo = hyd1%flo * 86400.
        ! t = ppm * m3 / 1000000.
        hyd1%sed = hyd1%sed * hyd1%flo / 1000000.
        ! kg = ppm * m3 / 1000.
        hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.
        hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
        hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
        hyd1%solp = hyd1%solp * hyd1%flo / 1000.
        hyd1%psol = hyd1%psol * hyd1%flo / 1000.
        hyd1%psor = hyd1%psor * hyd1%flo / 1000.
        hyd1%chla = hyd1%chla * hyd1%flo / 1000.
        hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
        hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
        hyd1%cbod = hyd1%cbod * hyd1%flo / 1000.
        hyd1%dox = hyd1%dox * hyd1%flo / 1000.
        hyd1%bacp = hyd1%bacp * hyd1%flo / 1000.
        hyd1%baclp = hyd1%baclp * hyd1%flo / 1000.
        hyd1%met1 = hyd1%met1 * hyd1%flo / 1000.
        hyd1%met2 = hyd1%met2 * hyd1%flo / 1000.
        hyd1%met3 = hyd1%met3 * hyd1%flo / 1000.
        hyd1%san = hyd1%san * hyd1%flo / 1000000.
        hyd1%sil = hyd1%sil * hyd1%flo / 1000000.
        hyd1%cla = hyd1%cla * hyd1%flo / 1000000.
        hyd1%sag = hyd1%sag * hyd1%flo / 1000000.
        hyd1%lag = hyd1%lag * hyd1%flo / 1000000.
        hyd1%grv = hyd1%grv * hyd1%flo / 1000000.
      end subroutine hyd_convert_mass
                     
      !! function to convert mass to concentration
      subroutine hyd_convert_conc (hyd1)
        type (hyd_output), intent (inout) :: hyd1
        hyd1%flo = hyd1%flo
        ! ppm = 1000000. * t / m3
        hyd1%sed = 1000000. * hyd1%sed / hyd1%flo
        ! ppm = 1000. * kg / m3
        hyd1%orgn = 1000. * hyd1%orgn / hyd1%flo
        hyd1%sedp = 1000. * hyd1%sedp / hyd1%flo
        hyd1%no3 = 1000. * hyd1%no3 / hyd1%flo
        hyd1%solp = 1000. * hyd1%solp / hyd1%flo
        hyd1%psol = 1000. * hyd1%psol / hyd1%flo
        hyd1%psor = 1000. * hyd1%psor / hyd1%flo
        hyd1%chla = 1000. * hyd1%chla / hyd1%flo
        hyd1%nh3 = 1000. * hyd1%nh3 / hyd1%flo
        hyd1%no2 = 1000. * hyd1%no2 / hyd1%flo
        hyd1%cbod = 1000. * hyd1%cbod / hyd1%flo
        hyd1%dox = 1000. * hyd1%dox / hyd1%flo
        hyd1%bacp = 1000. * hyd1%bacp / hyd1%flo
        hyd1%baclp = 1000. * hyd1%baclp / hyd1%flo
        hyd1%met1 = 1000. * hyd1%met1 / hyd1%flo
        hyd1%met2 = 1000. * hyd1%met2 / hyd1%flo
        hyd1%met3 = 1000. * hyd1%met3 / hyd1%flo
        hyd1%san = 1000000. * hyd1%san / hyd1%flo
        hyd1%sil = 1000000. * hyd1%sil / hyd1%flo
        hyd1%cla = 1000000. * hyd1%cla / hyd1%flo
        hyd1%sag = 1000000. * hyd1%sag / hyd1%flo
        hyd1%lag = 1000000. * hyd1%lag / hyd1%flo
        hyd1%grv = 1000000. * hyd1%grv / hyd1%flo
      end subroutine hyd_convert_conc
         
      !! routines for hydrograph module
      function hydout_add (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo + hyd2%flo
        hyd3%sed = hyd1%sed + hyd2%sed        
        hyd3%orgn = hyd1%orgn + hyd2%orgn        
        hyd3%sedp = hyd1%sedp + hyd2%sedp   
        hyd3%no3 = hyd1%no3 + hyd2%no3
        hyd3%solp = hyd1%solp + hyd2%solp
        hyd3%psol = hyd1%psol + hyd2%psol
        hyd3%psor = hyd1%psor + hyd2%psor
        hyd3%chla = hyd1%chla + hyd2%chla
        hyd3%nh3 = hyd1%nh3 + hyd2%nh3
        hyd3%no2 = hyd1%no2 + hyd2%no2
        hyd3%cbod = hyd1%cbod + hyd2%cbod
        hyd3%dox = hyd1%dox + hyd2%dox
        hyd3%bacp = hyd1%bacp + hyd2%bacp
        hyd3%baclp = hyd1%baclp + hyd2%baclp
        hyd3%met1 = hyd1%met1 + hyd2%met1
        hyd3%met2 = hyd1%met2 + hyd2%met2
        hyd3%met3 = hyd1%met3 + hyd2%met3
        hyd3%san = hyd1%san + hyd2%san
        hyd3%sil = hyd1%sil + hyd2%sil
        hyd3%cla = hyd1%cla + hyd2%cla
        hyd3%sag = hyd1%sag + hyd2%sag
        hyd3%lag = hyd1%lag + hyd2%lag
        hyd3%grv = hyd1%grv + hyd2%grv
      end function hydout_add
            
      !! routines for hydrograph module
      function hydout_mult (hyd1, hyd2) result (hyd3)
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output), intent (in) :: hyd2
        type (hyd_output) :: hyd3
        hyd3%flo = hyd1%flo * hyd2%flo
        hyd3%sed = hyd1%sed * hyd2%sed        
        hyd3%orgn = hyd1%orgn * hyd2%orgn        
        hyd3%sedp = hyd1%sedp * hyd2%sedp   
        hyd3%no3 = hyd1%no3 * hyd2%no3
        hyd3%solp = hyd1%solp * hyd2%solp
        hyd3%psol = hyd1%psol * hyd2%psol
        hyd3%psor = hyd1%psor * hyd2%psor
        hyd3%chla = hyd1%chla * hyd2%chla
        hyd3%nh3 = hyd1%nh3 * hyd2%nh3
        hyd3%no2 = hyd1%no2 * hyd2%no2
        hyd3%cbod = hyd1%cbod * hyd2%cbod
        hyd3%dox = hyd1%dox * hyd2%dox
        hyd3%bacp = hyd1%bacp * hyd2%bacp
        hyd3%baclp = hyd1%baclp * hyd2%baclp
        hyd3%met1 = hyd1%met1 * hyd2%met1
        hyd3%met2 = hyd1%met2 * hyd2%met2
        hyd3%met3 = hyd1%met3 * hyd2%met3
        hyd3%san = hyd1%san * hyd2%san
        hyd3%sil = hyd1%sil * hyd2%sil
        hyd3%cla = hyd1%cla * hyd2%cla
        hyd3%sag = hyd1%sag * hyd2%sag
        hyd3%lag = hyd1%lag * hyd2%lag
        hyd3%grv = hyd1%grv * hyd2%grv
      end function hydout_mult
            
      !! routines for hydrograph module
      function hydout_add_const (const, hyd1) result (hyd2)
        real, intent (in) :: const
        type (hyd_output), intent (in) :: hyd1
        type (hyd_output) :: hyd2
        hyd2%flo = const + hyd1%flo 
        hyd2%sed = const + hyd1%sed       
        hyd2%orgn = const + hyd1%orgn       
        hyd2%sedp = const + hyd1%sedp 
        hyd2%no3 = const + hyd1%no3
        hyd2%solp = const + hyd1%solp
        hyd2%psol = const + hyd1%psol
        hyd2%psor = const + hyd1%psor
        hyd2%chla = const + hyd1%chla
        hyd2%nh3 = const + hyd1%nh3
        hyd2%no2 = const + hyd1%no2
        hyd2%cbod = const + hyd1%cbod
        hyd2%dox = const + hyd1%dox
        hyd2%bacp = const + hyd1%bacp
        hyd2%baclp = const + hyd1%baclp
        hyd2%met1 = const + hyd1%met1
        hyd2%met2 = const + hyd1%met2
        hyd2%met3 = const + hyd1%met3
        hyd2%san = const + hyd1%san
        hyd2%sil = const + hyd1%sil
        hyd2%cla = const + hyd1%cla
        hyd2%sag = const + hyd1%sag
        hyd2%lag = const + hyd1%lag
        hyd2%grv = const + hyd1%grv
      end function hydout_add_const
      
      function hydout_mult_const (const, hyd1) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const
        type (hyd_output) :: hyd2
        hyd2%flo = const * hyd1%flo 
        hyd2%sed = const * hyd1%sed       
        hyd2%orgn = const * hyd1%orgn       
        hyd2%sedp = const * hyd1%sedp 
        hyd2%no3 = const * hyd1%no3
        hyd2%solp = const * hyd1%solp
        hyd2%psol = const * hyd1%psol
        hyd2%psor = const * hyd1%psor
        hyd2%chla = const * hyd1%chla
        hyd2%nh3 = const * hyd1%nh3
        hyd2%no2 = const * hyd1%no2
        hyd2%cbod = const * hyd1%cbod
        hyd2%dox = const * hyd1%dox
        hyd2%bacp = const * hyd1%bacp
        hyd2%baclp = const * hyd1%baclp
        hyd2%met1 = const * hyd1%met1
        hyd2%met2 = const * hyd1%met2
        hyd2%met3 = const * hyd1%met3
        hyd2%san = const * hyd1%san
        hyd2%sil = const * hyd1%sil
        hyd2%cla = const * hyd1%cla
        hyd2%sag = const * hyd1%sag
        hyd2%lag = const * hyd1%lag
        hyd2%grv = const * hyd1%grv
      end function hydout_mult_const
      
      function hydout_div_const (hyd1,const) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const
        type (hyd_output) :: hyd2
        hyd2%flo = hyd1%flo / const
        hyd2%sed = hyd1%sed / const
        hyd2%orgn = hyd1%orgn / const
        hyd2%sedp = hyd1%sedp / const
        hyd2%no3 = hyd1%no3 / const
        hyd2%solp = hyd1%solp / const
        hyd2%psol = hyd1%psol / const
        hyd2%psor = hyd1%psor / const
        hyd2%chla = hyd1%chla / const
        hyd2%nh3 = hyd1%nh3 / const
        hyd2%no2 = hyd1%no2 / const
        hyd2%cbod = hyd1%cbod / const
        hyd2%dox = hyd1%dox / const
        hyd2%bacp = hyd1%bacp / const
        hyd2%baclp = hyd1%baclp / const
        hyd2%met1 = hyd1%met1 / const
        hyd2%met2 = hyd1%met2 / const
        hyd2%met3 = hyd1%met3 / const
        hyd2%san = hyd1%san / const
        hyd2%sil = hyd1%sil / const
        hyd2%cla = hyd1%cla / const
        hyd2%sag = hyd1%sag / const
        hyd2%lag = hyd1%lag / const
        hyd2%grv = hyd1%grv / const
      end function hydout_div_const
            
      !function to convert m^3-> mm and kg(or t)->kg(or t)/ha
      function hydout_div_conv (hyd1,const) result (hyd2)
        type (hyd_output), intent (in) :: hyd1
        real, intent (in) :: const  !ha
        type (hyd_output) :: hyd2
        hyd2%flo = hyd1%flo / (10. * const)
        hyd2%sed = hyd1%sed / const
        hyd2%orgn = hyd1%orgn / const
        hyd2%sedp = hyd1%sedp / const
        hyd2%no3 = hyd1%no3 / const
        hyd2%solp = hyd1%solp / const
        hyd2%psol = hyd1%psol / const
        hyd2%psor = hyd1%psor / const
        hyd2%chla = hyd1%chla / const
        hyd2%nh3 = hyd1%nh3 / const
        hyd2%no2 = hyd1%no2 / const
        hyd2%cbod = hyd1%cbod / const
        hyd2%dox = hyd1%dox / const
        hyd2%bacp = hyd1%bacp / const
        hyd2%baclp = hyd1%baclp / const
        hyd2%met1 = hyd1%met1 / const
        hyd2%met2 = hyd1%met2 / const
        hyd2%met3 = hyd1%met3 / const
        hyd2%san = hyd1%san / const
        hyd2%sil = hyd1%sil / const
        hyd2%cla = hyd1%cla / const
        hyd2%sag = hyd1%sag / const
        hyd2%lag = hyd1%lag / const
        hyd2%grv = hyd1%grv / const
      end function hydout_div_conv
      
      !function to set dr to a constant
      function dr_constant (dr1, const)
        type (hyd_output) :: dr1
        real, intent (in) :: const
        dr1%flo = const
        dr1%sed = const
        dr1%orgn = const
        dr1%sedp = const
        dr1%no3 = const
        dr1%solp = const
        dr1%psol = const
        dr1%psor = const
        dr1%chla = const
        dr1%nh3 = const
        dr1%no2 = const
        dr1%cbod = const
        dr1%dox = const
        dr1%bacp = const
        dr1%baclp = const
        dr1%met1 = const
        dr1%met2 = const
        dr1%met3 = const
        dr1%san = const
        dr1%sil = const
        dr1%cla = const
        dr1%sag = const
        dr1%lag = const
        dr1%grv = const
      end function dr_constant
      
      end module hydrograph_module