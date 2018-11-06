      module ch_pathogen_module
    
      implicit none
      
      type ch_pathogen_output
        real :: sol_in = 0.             ! (m^3/s)      !ave flow rate
        real :: sol_out = 0.            ! (m^3/s)      |peak runoff rate
        real :: sor_in = 0.             ! (tons)       !total sed in
        real :: sor_out = 0.            ! (tons)       !total sed out
        real :: dre_off = 0.            ! (tons)       !wash load
        real :: regrow = 0.             ! (tons)       !bed load
        real :: water = 0.              !
        real :: benthic = 0.            !          !
      end type ch_pathogen_output
      
      type (ch_pathogen_output), dimension(:), allocatable, save :: chpth_d
      type (ch_pathogen_output), dimension(:), allocatable, save :: chpth_m
      type (ch_pathogen_output), dimension(:), allocatable, save :: chpth_y
      type (ch_pathogen_output), dimension(:), allocatable, save :: chpth_a
      type (ch_pathogen_output) :: bchpth_d
      type (ch_pathogen_output) :: bchpth_m
      type (ch_pathogen_output) :: bchpth_y
      type (ch_pathogen_output) :: bchpth_a
      type (ch_pathogen_output) :: chpthz
                             
      type ch_pathogen_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=8) :: id =         " gis_id "           
          character (len=16) :: name =      " name              "        
          character(len=16) :: sol_in =     " solpestin_#cfu/100ml"         ! # cfu/100ml
          character(len=15) :: sol_out =    " solpestout_#cfu/100ml"        ! # cfu/100ml
          character(len=15) :: sor_in =     " sorpestin_#cfu/100ml"         ! # cfu/100ml
          character(len=15) :: sor_out=     " sorpestout_#cfu/100ml"        ! # cfu/100ml
          character(len=15) :: dre_off =    "  dre_off_#cfu/100ml "         ! # cfu/100ml
          character(len=15) :: regrow =     "    regrow_#cfu/100ml"         ! # cfu/100ml
          character(len=15) :: water =      " water_stor_#cfu/100ml"        ! # cfu/100ml
          character(len=15) :: benthic =    "   benthic_#cfu/100ml"         ! # cfu/100ml
      end type ch_pathogen_header
      type (ch_pathogen_header) :: chpath_hdr
     
      interface operator (+)
        module procedure chpath_add
      end interface
      
      interface operator (/)
        module procedure chpath_div
      end interface
        
      interface operator (*)
        module procedure chpath_mult
      end interface 
             
      contains
!! routines for swatdeg_hru module

      function chpath_add(cho1,cho2) result (cho3)
      type (ch_pathogen_output),  intent (in) :: cho1
      type (ch_pathogen_output),  intent (in) :: cho2
      type (ch_pathogen_output) :: cho3
       cho3%sol_in = cho1%sol_in + cho2%sol_in
       cho3%sol_out = cho1%sol_out + cho2%sol_out
       cho3%sor_in = cho1%sor_in + cho2%sor_in
       cho3%sor_out = cho1%sor_out + cho2%sor_out
       cho3%dre_off = cho1%dre_off + cho2%dre_off
       cho3%regrow = cho1%regrow + cho2%regrow
       cho3%water = cho1%water + cho2%water
       cho3%benthic = cho1%benthic + cho2%benthic
 
      end function chpath_add
      
      function chpath_div (ch1,const) result (ch2)
        type (ch_pathogen_output), intent (in) :: ch1
        real, intent (in) :: const
        type (ch_pathogen_output) :: ch2
        ch2%sol_in = ch1%sol_in / const
        ch2%sol_out = ch1%sol_out / const
        ch2%sor_in = ch1%sor_in / const
        ch2%sor_out = ch1%sor_out / const
        ch2%dre_off = ch1%dre_off / const
        ch2%regrow = ch1%regrow / const
        ch2%water = ch1%water / const
        ch2%benthic = ch1%benthic / const
      end function chpath_div
      
      function chpath_mult (const, chn1) result (chn2)
        type (ch_pathogen_output), intent (in) :: chn1
        real, intent (in) :: const
        type (ch_pathogen_output) :: chn2
        chn2%sol_in = const * chn1%sol_in
        chn2%sol_out = const * chn1%sol_out
        chn2%sor_in = const * chn1%sor_in
        chn2%sor_out = const * chn1%sor_out
        chn2%dre_off = const * chn1%dre_off
        chn2%regrow = const * chn1%regrow
        chn2%water = const * chn1%water
        chn2%benthic = const * chn1%benthic
      end function chpath_mult
      
      end module ch_pathogen_module