      subroutine pathogen_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i             |none          |number of specific reservoir or HRU
!!    ndays(:)      |julian date   |julian date for last day of preceding 
!!                                 |month (where the array location is the 
!!                                 |number of the month) The dates are for
!!                                 |leap years
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: soil_chem, soil_phys, rteinit, h2omgt_init, hydro_init,

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, hru_db, ihru, ipl, isol, mlyr, mpst,   &
         sol_cov, wfsh, sol_plt_ini
      use soil_module
      use plant_module
      use pathogen_data_module
      use channel_module
      use basin_module
      use conditional_module
      use organic_mineral_mass_module
      use hydrograph_module, only : sp_ob, icmd
      use constituent_mass_module
      use output_ls_pathogen_module
      
      implicit none

      integer :: eof                   !          |end of file
      character (len=80) :: titldum    !          |title of file
      integer :: mpath                 !          |
      integer :: ly                    !none      |counter
      integer :: ipath                  !none      |counter
      integer :: ipath_db               !          |
      integer :: isp_ini

      do ihru = 1, sp_ob%hru  
        !! allocate pathogens
        mpath = cs_db%num_paths
        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
        if (mpath > 0) then
          !! allocate pathogens associated with soil
          do ly = 1, soil(ihru)%nly
            allocate (soil(ihru)%ly(ly)%bacsol(mpath))
            allocate (soil(ihru)%ly(ly)%bacsor(mpath))
          end do
          !! allocate pathogens associated with plant
          allocate (pcom(ihru)%path(mpath))
          
          do ipath = 1, mpath
            if (ly == 1) then
              soil(ihru)%ly(1)%bacsol(ipath) = pathi_db(ipath_db)%pathi(ipath)%soil
              soil(ihru)%ly(1)%bacsor(ipath) = pathi_db(ipath_db)%pathi(ipath)%soil
            else
              soil(ihru)%ly(1)%bacsol(ipath) = 0.
              soil(ihru)%ly(1)%bacsor(ipath) = 0.
            end if
             
            hpath_bal(ihru)%path(ipath)%plant = pathi_db(ipath_db)%pathi(ipath)%plt
          end do
        end if
      end do

      return
      end subroutine pathogen_init