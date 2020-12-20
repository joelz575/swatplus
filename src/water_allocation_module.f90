      module water_allocation_module
    
      implicit none
            
      !water source objects
      type water_source_objects
        character (len=3) :: ob_typ             !reservoir(res), aquifer(aqu), unlimited groundwater source(gwu)
        integer :: obtyp_num                    !number of the object type
        real :: frac                            !fraction of water taken from object
      end type water_source_objects

      !water demand objects
      type water_demand_objects
        integer :: num                          !demand object number
        character (len=4) :: ob_typ             !hru or municipal (hru or muni)
        integer :: obtyp_num                    !number of the object type
        character (len=25) :: dmd_typ           !ave_daily for municipal and irrigation type for hru
        real :: amount                          !ha-m per day for muni and mm for hru
        integer :: irr_typ                      !irrigation number from irrop_db
        !integer :: num_src                     !number of sources - 1 or 2
        type (water_source_objects), dimension(2) :: src         !sources for each demand object
      end type water_demand_objects
      
      !water allocation
      type water_allocation
        character (len=25) :: name              !name of the water allocation object
        character (len=25) :: rule_typ          !rule type to allocate water
        real :: res_lim                         !frac - lower limit to take water from reservoir - frac*principal volume
        character (len=1) :: comp               !compensate from second source if first is below limit (y/n)
        integer :: dmd_obs                      !number of demand objects
        type (water_demand_objects), dimension(:), allocatable :: dmd        !dimension by demand objects
      end type water_allocation
      type (water_allocation), dimension(:), allocatable :: wallo            !dimension by water allocation objects
      
      !source output
      type source_output
        real :: demand = 0.                     !ha-m       !demand
        real :: withdr = 0.                     !ha-m       |amoount withdrawn from the source
        real :: unmet  = 0.                     !ha-m       |unmet demand
      end type source_output
      type (source_output) :: walloz
      
      !water allocation output
      type water_allocation_output
        real :: dmd_tot = 0.                    !ha-m       !demand
        type (source_output), dimension(2) :: src
      end type water_allocation_output
      type (water_allocation_output), dimension(:), allocatable :: wallod_out     !dimension by demand objects
      type (water_allocation_output), dimension(:), allocatable :: wallom_out     !dimension by demand objects
      type (water_allocation_output), dimension(:), allocatable :: walloy_out     !dimension by demand objects
      type (water_allocation_output), dimension(:), allocatable :: walloa_out     !dimension by demand objects
      
      type wallo_header            
		character(len=6) :: day      =	 "  jday"       
		character(len=6) :: mo       =	 "	 mon"
		character(len=6) :: day_mo   =	 " day "
		character(len=6) :: yrc      =	 " yr  "        
		character(len=8) :: idmd	 =	 " unit   "      
		character(len=8) :: dmd_typ  =   "dmd_typ "
		character(len=8) :: dmd_num =	 " dmd_num	"     
		character(len=10) :: src1_typ =	 " src1_typ	" 
        character(len=8) :: src1_num =	 "src1_num	"        
        character(len=15) :: dmd1  =     "	 demand	     "      !! ha-m     |demand - muni or irrigation       
        character(len=15) :: s1out  =   "src1_withdraw  "       !! ha-m     |withdrawal from source 1
        character (len=12) :: s1un =    "  src1_unmet"          !! ha-m     |unmet from source 1        
	    character(len=10) :: src2_typ =	 "  src2_typ" 
        character(len=10)  :: src2_num = " src2_num "        
        character(len=15) :: dmd2  =    " demand        "       !! ha-m     |demand - muni or irrigation        
        character (len=15) :: s2out =   " src2_withdraw "       !! ha-m     |withdrawal from source 2
        character (len=15) :: s2un  =   "   src2_unmet  "       !! ha_m     |unmet from source 2
      end type wallo_header
      type (wallo_header) :: wallo_hdr

      type wallo_header_units         
		character (len=8) :: day	  =  "	      "
		character (len=8) :: mo       =  "	      "
		character (len=8) :: day_mo   =  "	      "       
		character (len=8) :: yrc      =  "	      "       
		character (len=8) :: idmd	  =  "	      "      
		character (len=8) :: dmd_typ  =  "	      "
		character (len=8) :: dmd_num  =	 "	      "        
		character (len=8) :: src1_typ =  "	      "        
		character (len=8) :: src1_num =  "        "      
        character (len=12) :: dmd1 =     " ha_m       "        !! ha-m    |demand - muni or irrigation
        character (len=12) :: s1out =	 "   ha_m     "        !! ha-m    |withdrawal from source 1       
        character (len=12) :: s1un =     "        ha_m"        !! ha-m    |unmet from source 1
        character (len=8) :: src2_typ = "        "       
		character (len=8) :: src2_num = "        "        
        character (len=15) :: dmd2 =  "           ha_m "        !! ha-m    |demand - muni or irrigation
        character (len=15) :: s2out = "           ha_m "        !! ha-m    |withdrawal from source 2
        character (len=15) :: s2un =  "           ha_m "        !! ha-m    |unmet from source 2
      end type wallo_header_units
      type (wallo_header_units) :: wallo_hdr_units 
      
      interface operator (+)
        module procedure wallout_add
      end interface

      interface operator (/)
        module procedure wallo_div_const
      end interface   

      contains

      !! routines for hydrograph module
      function wallout_add (wallo1, wallo2) result (wallo3)
        type (source_output), intent (in) :: wallo1
        type (source_output), intent (in) :: wallo2
        type (source_output) :: wallo3
        wallo3%demand = wallo1%demand + wallo2%demand
        wallo3%withdr = wallo1%withdr + wallo2%withdr
        wallo3%unmet = wallo1%unmet + wallo2%unmet
      end function wallout_add

      function wallo_div_const (wallo1, const) result (wallo2)
        type (source_output), intent (in) :: wallo1
        real, intent (in) :: const
        type (source_output) :: wallo2
        wallo2%demand = wallo1%demand / const
        wallo2%withdr = wallo1%withdr / const
        wallo2%unmet = wallo1%unmet / const
      end function wallo_div_const

      end module water_allocation_module
