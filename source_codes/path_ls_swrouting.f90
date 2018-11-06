      subroutine path_ls_swrouting
    
      use pathogen_data_module
      use constituent_mass_module
      use output_ls_pathogen_module
      use hru_module, only : hru, sol_plt_ini, ihru
      use soil_module
      
      implicit none

      real :: j             !none          |hru number
      integer :: ipath      !none          |pathogen counter
      integer :: ipath_db   !none          |pathogen number from data file
      integer :: isp_ini    !none          |soil-plant initialization number from data file

      j = ihru
         
      do ipath = 1, cs_db%num_paths
        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
        !! compute pathogen incorporated into the soil
        hpath_bal(j)%path(ipath)%perc1 = soil(j)%ly(1)%bacsol(ipath) * soil(j)%ly(1)%prk / ((soil(j)%phys(1)%conv_wt / 1000.) *          & 
                                          path_db(ipath_db)%perco)
        hpath_bal(j)%path(ipath)%perc1 = Min(hpath_bal(j)%path(ipath)%perc1, soil(j)%ly(1)%bacsol(ipath))
        hpath_bal(j)%path(ipath)%perc1 = Max(hpath_bal(j)%path(ipath)%perc1, 0.)
        soil(j)%ly(1)%bacsol(ipath) = soil(j)%ly(1)%bacsol(ipath) - hpath_bal(j)%path(ipath)%perc1
      end do

      return
      end subroutine path_ls_swrouting