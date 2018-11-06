      subroutine path_ls_process
    
      use pathogen_data_module
      use constituent_mass_module
      use output_ls_pathogen_module
      use hru_module, only : hru, sol_plt_ini, ihru, precipday, tmpav
      use soil_module
      use plant_module
      
      implicit none

      integer :: j          !none          |hru number
      integer :: ipath      !none          |pathogen counter
      integer :: ipath_db   !none          |pathogen number from data file
      integer :: isp_ini    !none          |soil-plant initialization number from data file
      real :: pl_ini        !              |
      real :: sol_ini       !              |
      real :: sor_ini       !              |
      real :: pl_die_gro
      real :: sol_die_gro
      real :: sor_die_gro
      real :: bacdiegrosol_out
      real :: bacdiegrosor_out
      real :: bacdiegroplt_out
      real :: theta 

      j = ihru
         
      do ipath = 1, cs_db%num_paths
        isp_ini = hru(ihru)%dbs%soil_plant_init
        ipath_db = sol_plt_ini(isp_ini)%path
      
        !! compute pathogen wash off
        if (precipday >= 2.54) then
          hpath_bal(j)%path(ipath)%wash = path_db(ipath_db)%washoff * pcom(j)%path(ipath)
          if (hpath_bal(j)%path(ipath)%wash > pcom(j)%path(ipath)) hpath_bal(j)%path(ipath)%wash = pcom(j)%path(ipath)
          soil(j)%ly(1)%bacsol(ipath) = soil(j)%ly(1)%bacsol(ipath) + hpath_bal(j)%path(ipath)%wash
          pcom(j)%path(ipath) = pcom(j)%path(ipath) - hpath_bal(j)%path(ipath)%wash
        end if

        !! compute pathogen die-off and re-growth on foilage
        pl_ini = pcom(j)%path(ipath)
        pl_die_gro = path_db(ipath_db)%do_plnt - path_db(ipath_db)%gr_plnt
        pcom(j)%path(ipath) = pcom(j)%path(ipath) * Exp(-Theta(pl_die_gro, path_db(ipath_db)%t_adj, tmpav(j))) -     &
                                              path_db(ipath_db)%conc_min
        pcom(j)%path(ipath) = Max(0., pcom(j)%path(ipath))
        if (pcom(j)%path(ipath) < path_db(ipath_db)%conc_min) pcom(j)%path(ipath) = path_db(ipath_db)%conc_min

        !! compute pathogen die-off and re-growth in surface soil layer
        sol_ini = soil(j)%ly(1)%bacsol(ipath)
        sol_die_gro = path_db(ipath_db)%do_soln - path_db(ipath_db)%gr_soln
        soil(j)%ly(1)%bacsol(ipath) = soil(j)%ly(1)%bacsol(ipath) * Exp(-Theta(sol_die_gro, path_db(ipath_db)%t_adj, tmpav(j)))   &
                                              - path_db(ipath_db)%conc_min
        soil(j)%ly(1)%bacsol(ipath) = Max(0., soil(j)%ly(1)%bacsol(ipath))
        if (soil(j)%ly(1)%bacsol(ipath) < path_db(ipath_db)%conc_min) soil(j)%ly(1)%bacsol(ipath) = path_db(ipath_db)%conc_min

        sor_ini = soil(j)%ly(1)%bacsor(ipath)
        sor_die_gro = path_db(ipath_db)%do_sorb - path_db(ipath_db)%gr_sorb
        soil(j)%ly(1)%bacsor(ipath) = soil(j)%ly(1)%bacsor(ipath) * Exp(-Theta(sor_die_gro, path_db(ipath_db)%t_adj, tmpav(j)))   &
                                             - path_db(ipath_db)%conc_min
        soil(j)%ly(1)%bacsor(ipath) = Max(0., soil(j)%ly(1)%bacsor(ipath))
        if (soil(j)%ly(1)%bacsor(ipath) < path_db(ipath_db)%conc_min) soil(j)%ly(1)%bacsor(ipath) = path_db(ipath_db)%conc_min

        bacdiegrosol_out = pl_ini - soil(j)%ly(1)%bacsol(ipath)
        bacdiegrosor_out = sol_ini - soil(j)%ly(1)%bacsor(ipath)
        bacdiegroplt_out = sor_ini - pcom(j)%path(ipath)
        !! net die_off - negative is regrowth
        hpath_bal(j)%path(ipath)%die_off = bacdiegrosol_out + bacdiegrosor_out + bacdiegroplt_out
        
      end do

      return
      end subroutine path_ls_process