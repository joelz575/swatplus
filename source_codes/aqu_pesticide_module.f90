      module aqu_pesticide_module

      implicit none
              
      type aqu_pesticide_processes
        real :: tot_in = 0.             ! kg        !total pesticide into aquifer
        real :: sol_out = 0.            ! kg        !soluble pesticide out of aquifer
        real :: sor_out = 0.            ! kg        !sorbed pesticide out of aquifer
        real :: react = 0.              ! kg        !pesticide lost through reactions in water layer
        real :: stor = 0.               ! kg        !pesticide in aquifer at the end of the day 
      end type aqu_pesticide_processes
      
      type aqu_pesticide_output
        type (aqu_pesticide_processes), dimension (:), allocatable :: pest         !pesticide hydrographs
      end type aqu_pesticide_output
      type (aqu_pesticide_processes) :: aqu_pestbz
           
      type (aqu_pesticide_output), dimension(:), allocatable, save :: aqupst_d
      type (aqu_pesticide_output), dimension(:), allocatable, save :: aqupst_m
      type (aqu_pesticide_output), dimension(:), allocatable, save :: aqupst_y
      type (aqu_pesticide_output), dimension(:), allocatable, save :: aqupst_a
      type (aqu_pesticide_output) :: baqupst_d
      type (aqu_pesticide_output) :: baqupst_m
      type (aqu_pesticide_output) :: baqupst_y
      type (aqu_pesticide_output) :: baqupst_a
      type (aqu_pesticide_output) :: aqupst, aqupstz
                 
      type aqu_pesticide_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=8) :: id =         " gis_id "           
          character (len=16) :: name =      " name"
          character (len=16) :: pest =      " pesticide"
          character(len=13) :: tot_in =     "tot_in_kg "            ! (mg)
          character(len=13) :: sol_out =    "sol_out_kg "           ! (mg)
          character(len=14) :: sor_out =    "sor_out_kg "           ! (mg)
          character(len=13) :: react =      "react_mkg"        	    ! (mg)
          character(len=10) :: stor  =      "stor_kg "        		! (mg)
      end type aqu_pesticide_header
      type (aqu_pesticide_header) :: aqupest_hdr
     
      interface operator (+)
        module procedure aqupest_add
      end interface
      
      interface operator (/)
        module procedure aqupest_div
      end interface
        
      interface operator (//)
        module procedure aqupest_ave
      end interface 
             
      contains
!! routines for swatdeg_hru module

      function aqupest_add(aqu1, aqu2) result (aqu3)
        type (aqu_pesticide_processes),  intent (in) :: aqu1
        type (aqu_pesticide_processes),  intent (in) :: aqu2
        type (aqu_pesticide_processes) :: aqu3
        aqu3%tot_in = aqu1%tot_in + aqu2%tot_in
        aqu3%sol_out = aqu1%sol_out + aqu2%sol_out
        aqu3%sor_out = aqu1%sor_out + aqu2%sor_out
        aqu3%react = aqu1%react + aqu2%react
        aqu3%stor = aqu1%stor + aqu2%stor
      end function aqupest_add
      
      function aqupest_div (aqu1, const) result (aqu2)
        type (aqu_pesticide_processes), intent (in) :: aqu1
        real, intent (in) :: const
        type (aqu_pesticide_processes) :: aqu2
          aqu2%tot_in = aqu1%tot_in / const
          aqu2%sol_out = aqu1%sol_out / const
          aqu2%sor_out = aqu1%sor_out / const
          aqu2%react = aqu1%react / const
          aqu2%stor = aqu1%stor
      end function aqupest_div
      
      function aqupest_ave (aqu1, const) result (aqu2)
        type (aqu_pesticide_processes), intent (in) :: aqu1
        real, intent (in) :: const
        type (aqu_pesticide_processes) :: aqu2
          aqu2%tot_in = aqu1%tot_in
          aqu2%sol_out = aqu1%sol_out
          aqu2%sor_out = aqu1%sor_out
          aqu2%react = aqu1%react
          aqu2%stor = aqu1%stor / const
      end function aqupest_ave
      
      end module aqu_pesticide_module