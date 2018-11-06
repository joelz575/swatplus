      module ch_pesticide_module
    
      use constituent_mass_module, only : cs_db
      
      implicit none
              
      type ch_pesticide_processes
        real :: sol_in = 0.             ! (m^3/s)      !ave flow rate
        real :: sol_out = 0.            ! (m^3/s)      |peak runoff rate
        real :: sor_in = 0.             ! (tons)       !total sed in
        real :: sor_out = 0.            ! (tons)       !total sed out
        real :: react = 0.              ! (tons)       !wash load
        real :: volat = 0.              ! (tons)       !bed load
        real :: settle = 0.             ! (tons)       !deposition
        real :: resus = 0.              ! (tons)       !bottom erosion
        real :: difus = 0.              ! (tons)       !bank erosion
        real :: react_bot = 0.          ! (tons)       !headcut erosion
        real :: bury = 0.               ! 
        real :: water = 0.              !
        real :: benthic = 0.            !
      end type ch_pesticide_processes
      
      type ch_pesticide_output
        type (ch_pesticide_processes), dimension (:), allocatable :: pest         !pesticide hydrographs
      end type ch_pesticide_output
           
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_d
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_m
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_y
      type (ch_pesticide_output), dimension(:), allocatable, save :: chpst_a
      type (ch_pesticide_output) :: bchpst_d
      type (ch_pesticide_output) :: bchpst_m
      type (ch_pesticide_output) :: bchpst_y
      type (ch_pesticide_output) :: bchpst_a
      type (ch_pesticide_output) :: chpst, chpstz
                 
      type ch_pesticide_header
          character (len=6) :: day =        "  jday"
          character (len=6) :: mo =         "   mon"
          character (len=6) :: day_mo =     "   day"
          character (len=6) :: yrc =        "    yr"
          character (len=8) :: isd =        "   unit "
          character (len=8) :: id =         " gis_id "           
          character (len=16) :: name =      " name              "        
          character(len=16) :: sol_in =     "   solpestin_kg"        ! (kg)
          character(len=15) :: sol_out =    "  solpestout_kg"        ! (kg)
          character(len=15) :: sor_in =     "   sorpestin_kg"        ! (kg)
          character(len=15) :: sor_out=     "  sorpestout_kg"        ! (kg)
          character(len=15) :: react =      "   react_h2o_kg"        ! (kg)
          character(len=15) :: volat =      "       volat_kg"        ! (kg)
          character(len=15) :: settle =     "      settle_kg"        ! (kg)
          character(len=15) :: resus =      "   resuspend_kg"        ! (kg)
          character(len=15) :: difus =      "     diffuse_kg"        ! (kg)
          character(len=15) :: react_bot =  " react_benth_kg"        ! (kg)
          character(len=15) :: bury =       "  bury_benth_kg"        ! (kg)
          character(len=15) :: water =      "  water_stor_kg"        ! (kg)
          character(len=15) :: benthic =    "     benthic_kg"        ! (kg)
      end type ch_pesticide_header
      type (ch_pesticide_header) :: chpest_hdr
     
      interface operator (+)
        module procedure chpest_add
      end interface
      
      interface operator (/)
        module procedure chpest_div
      end interface
        
      interface operator (*)
        module procedure chpest_mult
      end interface 
             
      contains
!! routines for swatdeg_hru module

      function chpest_add(cho1,cho2) result (cho3)
      type (ch_pesticide_output),  intent (in) :: cho1
      type (ch_pesticide_output),  intent (in) :: cho2
      type (ch_pesticide_output) :: cho3
      integer :: ipest
      allocate (cho3%pest(cs_db%num_pests))
             
      do ipest = 1, cs_db%num_pests
        cho3%pest(ipest)%sol_in = cho1%pest(ipest)%sol_in + cho2%pest(ipest)%sol_in
        cho3%pest(ipest)%sol_out = cho1%pest(ipest)%sol_out + cho2%pest(ipest)%sol_out
        cho3%pest(ipest)%sor_in = cho1%pest(ipest)%sor_in + cho2%pest(ipest)%sor_in
        cho3%pest(ipest)%sor_out = cho1%pest(ipest)%sor_out + cho2%pest(ipest)%sor_out
        cho3%pest(ipest)%react = cho1%pest(ipest)%react + cho2%pest(ipest)%react
        cho3%pest(ipest)%volat = cho1%pest(ipest)%volat + cho2%pest(ipest)%volat
        cho3%pest(ipest)%settle = cho1%pest(ipest)%settle + cho2%pest(ipest)%settle
        cho3%pest(ipest)%resus = cho1%pest(ipest)%resus + cho2%pest(ipest)%resus
        cho3%pest(ipest)%difus = cho1%pest(ipest)%difus + cho2%pest(ipest)%difus
        cho3%pest(ipest)%react_bot = cho1%pest(ipest)%react_bot + cho2%pest(ipest)%react_bot
        cho3%pest(ipest)%bury = cho1%pest(ipest)%bury + cho2%pest(ipest)%bury
        cho3%pest(ipest)%water = cho1%pest(ipest)%water + cho2%pest(ipest)%water
        cho3%pest(ipest)%benthic = cho1%pest(ipest)%benthic + cho2%pest(ipest)%benthic
      end do
      end function chpest_add
      
      function chpest_div (ch1,const) result (ch2)
        type (ch_pesticide_output), intent (in) :: ch1
        real, intent (in) :: const
        integer :: ipest
        type (ch_pesticide_output) :: ch2
        allocate (ch2%pest(cs_db%num_pests))
        
        do ipest = 1, cs_db%num_pests
          ch2%pest(ipest)%sol_in = ch1%pest(ipest)%sol_in / const
          ch2%pest(ipest)%sol_out = ch1%pest(ipest)%sol_out / const
          ch2%pest(ipest)%sor_in = ch1%pest(ipest)%sor_in / const
          ch2%pest(ipest)%sor_out = ch1%pest(ipest)%sor_out / const
          ch2%pest(ipest)%react = ch1%pest(ipest)%react / const
          ch2%pest(ipest)%volat = ch1%pest(ipest)%volat / const
          ch2%pest(ipest)%settle = ch1%pest(ipest)%settle / const
          ch2%pest(ipest)%resus = ch1%pest(ipest)%resus / const
          ch2%pest(ipest)%difus = ch1%pest(ipest)%difus / const
          ch2%pest(ipest)%react_bot = ch1%pest(ipest)%react_bot / const
          ch2%pest(ipest)%bury = ch1%pest(ipest)%bury / const
          ch2%pest(ipest)%water = ch1%pest(ipest)%water / const
          ch2%pest(ipest)%benthic = ch1%pest(ipest)%benthic / const
        end do
      end function chpest_div
      
      function chpest_mult (const, chn1) result (chn2)
        type (ch_pesticide_output), intent (in) :: chn1
        integer :: ipest
        real, intent (in) :: const
        type (ch_pesticide_output) :: chn2
        allocate (chn2%pest(cs_db%num_pests))
               
        do ipest = 1, cs_db%num_pests
          chn2%pest(ipest)%sol_in = const * chn1%pest(ipest)%sol_in
          chn2%pest(ipest)%sol_out = const * chn1%pest(ipest)%sol_out
          chn2%pest(ipest)%sor_in = const * chn1%pest(ipest)%sor_in
          chn2%pest(ipest)%sor_out = const * chn1%pest(ipest)%sor_out
          chn2%pest(ipest)%react = const * chn1%pest(ipest)%react
          chn2%pest(ipest)%volat = const * chn1%pest(ipest)%volat
          chn2%pest(ipest)%settle = const * chn1%pest(ipest)%settle
          chn2%pest(ipest)%resus = const * chn1%pest(ipest)%resus
          chn2%pest(ipest)%difus = const * chn1%pest(ipest)%difus
          chn2%pest(ipest)%react_bot = const * chn1%pest(ipest)%react_bot 
          chn2%pest(ipest)%bury = const * chn1%pest(ipest)%bury
          chn2%pest(ipest)%water = const * chn1%pest(ipest)%water
          chn2%pest(ipest)%benthic = const * chn1%pest(ipest)%benthic
        end do
      end function chpest_mult
      
      end module ch_pesticide_module