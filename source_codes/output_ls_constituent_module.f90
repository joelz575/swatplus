      module output_ls_constituent_module
    
      implicit none 
      
      type pesticide_balance
        character(len=10) :: name
        integer :: num_db
        real :: plant = 0.          !! |kg/ha       |pesticide on plant foliage
        real :: soil = 0.           !! |kg/ha       |pesticide enrichment ratio
        real :: sed = 0.            !! |kg/ha       |pesticide loading from HRU sorbed onto sediment
        real :: surq = 0.           !! |kg/ha       |amount of pesticide type lost in surface runoff on current day in HRU
        real :: latq = 0.           !! |kg/ha       |amount of pesticide in lateral flow in HRU for the day
        real :: perc = 0.           !! |kg/ha       |amount of pesticide leached past bottom of soil
        real :: apply = 0.          !! |kg/ha       |amount of pesticed applied
        real :: decay = 0.          !! |kg/ha       |amount of pesticide decayed
        real :: wash = 0.           !! |kg/ha       |amount of pesticide washed off from plant to soil
      end type pesticide_balance
      type (pesticide_balance) :: pestbz

      type object_pesticide_balance
        type (pesticide_balance), dimension (:), allocatable :: pest
      end type object_pesticide_balance

      type (object_pesticide_balance), dimension (:), allocatable :: hpest_bal
      type (object_pesticide_balance), dimension (:), allocatable :: hpestb_m
      type (object_pesticide_balance), dimension (:), allocatable :: hpestb_y
      type (object_pesticide_balance), dimension (:), allocatable :: hpestb_a
      
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_d
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_m
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_y
      type (object_pesticide_balance), dimension (:), allocatable :: rupestb_a
      
      type (object_pesticide_balance) :: bpestb_d
      type (object_pesticide_balance) :: bpestb_m
      type (object_pesticide_balance) :: bpestb_y
      type (object_pesticide_balance) :: bpestb_a
      
      type output_pestbal_header
        character (len=5) :: day =      " jday"
        character (len=6) :: mo =       "   mon"
        character (len=6) :: day_mo =   "   day"
        character (len=6) :: yrc =      "    yr"
        character (len=8) :: isd =          "    unit"
        character (len=8) :: id =           "  gis_id"        
        character (len=16) :: name =        "  name          "        
        character (len=14) :: plant =       "  plant_kg/h"
        character (len=12) :: soil =        "  soil_kg/h"
        character (len=12) :: sed =         "   sed_kg/h"        
        character (len=12) :: surq =        "  surq_kg/h"      
        character (len=12) :: latq =        "  latq_kg/h" 
        character (len=12) :: perc =        "  perc_kg/h"   
        character (len=12) :: apply =       " apply_kg/h"
        character (len=12) :: decay =       " decay_kg/h"
      end type output_pestbal_header      
      type (output_pestbal_header) :: pestb_hdr
      
      interface operator (+)
        module procedure hruout_pestbal_add
      end interface

      interface operator (/)
        module procedure hruout_pestbal_div
      end interface
      
      interface operator (//)
        module procedure hruout_pestbal_ave
      end interface
        
        
      contains

      function hruout_pestbal_add (hru1, hru2) result (hru3)
        type (pesticide_balance), intent (in) :: hru1
        type (pesticide_balance), intent (in) :: hru2
        type (pesticide_balance) :: hru3
        hru3%plant = hru1%plant + hru2%plant
        hru3%soil = hru1%soil + hru2%soil
        hru3%sed = hru1%sed + hru2%sed
        hru3%surq = hru1%surq + hru2%surq
        hru3%latq = hru1%latq + hru2%latq
        hru3%perc = hru1%perc + hru2%perc
        hru3%apply = hru1%apply + hru2%apply
        hru3%decay = hru1%decay + hru2%decay
      end function hruout_pestbal_add

      function hruout_pestbal_div (hru1,const) result (hru2)
        type (pesticide_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (pesticide_balance) :: hru2
        hru2%plant = hru1%plant / const
        hru2%soil = hru1%soil / const
        hru2%sed= hru1%sed / const
        hru2%surq = hru1%surq / const
        hru2%latq = hru1%latq / const
        hru2%perc = hru1%perc / const
        hru2%apply = hru1%apply / const
        hru2%decay = hru1%decay / const
      end function hruout_pestbal_div
      
      function hruout_pestbal_ave (hru1,const) result (hru2)
        type (pesticide_balance), intent (in) :: hru1
        real, intent (in) :: const
        type (pesticide_balance) :: hru2   
        hru2%plant = hru1%plant / const 
        hru2%soil = hru1%soil / const
      end function hruout_pestbal_ave
                            
      end module output_ls_constituent_module