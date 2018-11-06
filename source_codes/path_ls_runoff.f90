      subroutine path_ls_runoff
    
      use pathogen_data_module
      use constituent_mass_module
      use output_ls_pathogen_module
      use hru_module, only : hru, sol_plt_ini, ihru, sedyld, qday, enratio
      use soil_module
      
      implicit none

      integer :: j          !none          |hru number
      integer :: ipath      !none          |pathogen counter
      integer :: ipath_db   !none          |pathogen number from data file
      integer :: isp_ini    !none          |soil-plant initialization number from data file
      real :: cpath         !              |concentration of pathogen in soil

      j = ihru
         
      do ipath = 1, cs_db%num_paths
        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
        
        !! compute soluble bacteria in the surface runoff
        hpath_bal(j)%path(ipath)%surq = soil(j)%ly(1)%bacsol(ipath) * qday /                        &
                 (soil(j)%phys(1)%bd * soil(j)%phys(1)%d * path_db(ipath_db)%kd)
        hpath_bal(j)%path(ipath)%surq = Min(hpath_bal(j)%path(ipath)%surq, soil(j)%ly(1)%bacsol(ipath))
        hpath_bal(j)%path(ipath)%surq = Max(soil(j)%ly(1)%bacsol(ipath), 0.)
        soil(j)%ly(1)%bacsol(ipath) = soil(j)%ly(1)%bacsol(ipath) - hpath_bal(j)%path(ipath)%surq

        !! compute bacteria transported with sediment
        if (enratio > 0.) then 
          cpath = soil(j)%ly(1)%bacsor(ipath) * enratio / soil(j)%phys(1)%conv_wt
          hpath_bal(j)%path(ipath)%sed = .0001 * cpath * sedyld(j) / (hru(j)%area_ha + 1.e-6)
          hpath_bal(j)%path(ipath)%sed = Min(hpath_bal(j)%path(ipath)%sed, soil(j)%ly(1)%bacsor(ipath))
          soil(j)%ly(1)%bacsor(ipath) = soil(j)%ly(1)%bacsor(ipath) - hpath_bal(j)%path(ipath)%sed
        end if
      end do
      
      return
      end subroutine path_ls_runoff