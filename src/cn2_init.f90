      subroutine cn2_init

      use hru_module, only : cn2, hru
      use soil_module
      use maximum_data_module
      use landuse_data_module
      use hydrograph_module, only : sp_ob
      
      implicit none
 
      integer :: ihru                !none       |counter 
      integer :: icn                 !none       |counter 
      integer :: isol                !none       |counter 
      integer :: ilum                !none       |counter 
      
      !!assign topography and hyd parameters
      do ihru = 1, sp_ob%hru
          
        ilum = hru(ihru)%land_use_mgt
        isol = hru(ihru)%dbs%soil
        !! set initial curve number parameters
        icn = lum_str(ilum)%cn_lu
        select case (sol(isol)%s%hydgrp)
        case ("A")
          cn2(ihru) = cn(icn)%cn(1)
        case ("B")
          cn2(ihru) = cn(icn)%cn(2)
        case ("C")
          cn2(ihru) = cn(icn)%cn(3)
        case ("D")
          cn2(ihru) = cn(icn)%cn(4)
        end select
        
        call curno(cn2(ihru), ihru)
      end do
      
      return
      end subroutine cn2_init
