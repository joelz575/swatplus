import numpy as np

datos = {
    'sd_props': np.array([0, 1]),
    'sd_obj_no': np.array([0, 1]),
    'sd_aqu_link': np.array([0, 1]),       #aquifer the channel is linked to
    'sd_aqu_link_ch' : np.array([0, 1]),   #sequential channel number in the aquifer
    'sd_chw' : np.array([3.0, 9.37300014]),       #m          |channel width
    'sd_chd': np.array([0.5, 4.79999995]),        #m          |channel depth
    'sd_chs': np.array([9.99999978E-03, 3.30999999E-02]),        #m/m        |channel slope
    'sd_chl': np.array([0.100000001, 1.229000002]),        #km         |channel length
    'sd_chn': np.array([1.79366203E-43, 4.00000007E-02]),        #           |channel Manning's n
    'sd_cov': np.array([0.0, 4.00000007E-02]),        #0-1        |channel cover factor
    'sd_cherod': np.array([float('nan'), 1.200000003]),     #           |channel erodibility
    'sd_shear_bnk': np.array([float('nan'), 0]),  #0-1        |bank shear coefficient - fraction of bottom shear
    'sd_hc_erod': np.array([1.40129846E-45, 0.5]),    #           |headcut erodibility
    'sd_hc_co': np.array([0.0, 8.47151327]),      #m/m        |proportionality coefficient for head cut
    'sd_hc_len': np.array([0.0, 1.0]),     #m          |length of head cut
    'sd_hc_hgt': np.array([6.86636248E-44, 2.0]),     #m          |headcut height
    'sd_stor': np.array([0.0, 1.0]),       #m3         |water stored in reach at end of the day
    #'sd_kd' : np.array([1.0, 1.0]),        #           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'sd_aq_mix' : np.array([1.0, 1.0])     # m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'name': ,
    'lte_props': np.array([2]),
    'lte_obj_no': np.array([2]),
            #character(len=16) :: lsu             #              |landscape unit - character
            #character(len=16) :: region          #              |region - character
            #character(len=16) :: plant           #              |plant type (as listed in plants.plt)
    'lte_iplant': np.array([15]),                  #              |plant number xwalked from hlt_db()%plant and plants.plt
    'lte_km2': np.array([1.0572]),                     #km^2          |drainage area
    'lte_cn2': np.array([75.]),                     #              |condition II curve number (used in calibration)
    'lte_cn3_swf': np.array([.5]),                #none          |soil water factor for cn3 (used in calibration)
    #              |0 = fc; 1 = saturation (porosity)
    'lte_soildep': np.array([1530.]),                #mm            |soil profile depth
    'lte_etco': np.array([2.]),                    #              |et coefficient - use with pet and aet (used in calibration)
    'lte_revapc': np.array([0.5]),                  #m/m           |revap from aquifer (used in calibration)
    'lte_perco': np.array([1.5]),                  #              |soil percolation coefficient (used in calibration)
    'lte_tdrain': np.array([1.0]),                  #hr            |design subsurface tile drain time (used in calibration)
    'lte_stress': np.array([1.5]),                  #frac          |plant stress - pest, root restriction, soil quality, nutrient,
    #              |(non water, temp) (used in calibration)
    'lte_uslefac': np.array([0.295745185]),                 #              |USLE slope length factor
    'lte_wrt1': np.array([4.94383001]),
    'lte_wrt2': np.array([3.64162780E-03]),
    'lte_smx': np.array([110.272125]),
    'lte_hk': np.array([161.504364]),
    'lte_yls': np.array([0.622464037]),
    'lte_ylc': np.array([0.952661312]),
    'lte_awc': np.array([193.539993]),                    #mm/mm        |available water capacity of soil
    'lte_g': np.array([0.5]),
    'lte_hufh': np.array([0.6]),
    'lte_phu': np.array([2798.91772]),
    'lte_por': np.array([542.000000]),
    'lte_sc': np.array([3.29999995]),
    'lte_sw': np.array([92.7699966]),                      #mm/mm         |initial soil water storage
    'lte_gw': np.array([0.5]),                      #mm            |initial shallow aquifer storage
    'lte_snow': np.array([0.5]),                    #mm            |initial water content of snow
    'lte_gwflow': np.array([0.5]),                  #mm            |initial groundwater flow
    #character(len=1) :: gro = "n"        #              |y=plant growing; n=not growing;
    'lte_dm': np.array([0.5]),                     #t/ha          |plant biomass
    'lte_alai': np.array([0.250000006]),                    #              |leaf area index
    'lte_yield': np.array([0.3]),                   #t/ha          |plant yield
    'lte_npp': np.array([0.5]),                     #t/ha          |net primary productivity
    'lte_lai_mx': np.array([0.5]),                 #              |maximum leaf area index
    'lte_gwdeep': np.array([0.5]),                 #mm            |deep aquifer storage
    'lte_aet': np.array([0.5]),                     #mm            |sum of actual et during growing season (for hi water stress)
    'lte_pet': np.array([0.5]),                     #mm            |sum of potential et during growing season (for hi water stress)
    'lte_start': np.array([1]),
    'lte_end': np.array([2]),
}

leer_datos = {
    'sd_props': np.array([0, 0]),
    'sd_obj_no': np.array([0, 0]),
    'sd_aqu_link': np.array([0, 0]),       #aquifer the channel is linked to
    'sd_aqu_link_ch' : np.array([0, 0]),   #sequential channel number in the aquifer
    'sd_chw' : np.array([3.0, 8.37300014]),       #m          |channel width
    'sd_chd': np.array([0.5, 3.79999995]),        #m          |channel depth
    'sd_chs': np.array([9.99999978E-03, 2.30999999E-02]),        #m/m        |channel slope
    'sd_chl': np.array([0.100000001, 0.229000002]),        #km         |channel length
    'sd_chn': np.array([1.79366203E-43, 5.00000007E-02]),        #           |channel Manning's n
    'sd_cov': np.array([0.0, 5.00000007E-02]),        #0-1        |channel cover factor
    'sd_cherod': np.array([float('nan'), 0.200000003]),     #           |channel erodibility
    'sd_shear_bnk': np.array([float('nan'), float('nan')]),  #0-1        |bank shear coefficient - fraction of bottom shear
    'sd_hc_erod': np.array([1.40129846E-45, 1.40129846E-45]),    #           |headcut erodibility
    'sd_hc_co': np.array([0.0, 7.47151327]),      #m/m        |proportionality coefficient for head cut
    'sd_hc_len': np.array([0.0, 0.0]),     #m          |length of head cut
    'sd_hc_hgt': np.array([6.86636248E-44, 1.0]),     #m          |headcut height
    'sd_stor': np.array([0.0, 0.0]),       #m3         |water stored in reach at end of the day
    #'sd_kd' : np.array([1.0, 1.0]),        #           |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'sd_aq_mix' : np.array([1.0, 1.0])     # m/day     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'name': ,
    'lte_props': np.array([1]),
    'lte_obj_no': np.array([1]),
    #character(len=16) :: lsu             #              |landscape unit - character
    #character(len=16) :: region          #              |region - character
    #character(len=16) :: plant           #              |plant type (as listed in plants.plt)
    'lte_iplant': np.array([14]),                  #              |plant number xwalked from hlt_db()%plant and plants.plt
    'lte_km2': np.array([0.0572]),                     #km^2          |drainage area
    'lte_cn2': np.array([85.]),                     #              |condition II curve number (used in calibration)
    'lte_cn3_swf': np.array([0.0]),                #none          |soil water factor for cn3 (used in calibration)
    #              |0 = fc; 1 = saturation (porosity)
    'lte_soildep': np.array([1330.]),                #mm            |soil profile depth
    'lte_etco': np.array([1.]),                    #              |et coefficient - use with pet and aet (used in calibration)
    'lte_revapc': np.array([0.0]),                  #m/m           |revap from aquifer (used in calibration)
    'lte_perco': np.array([1]),                  #              |soil percolation coefficient (used in calibration)
    'lte_tdrain': np.array([0.0]),                  #hr            |design subsurface tile drain time (used in calibration)
    'lte_stress': np.array([1]),                  #frac          |plant stress - pest, root restriction, soil quality, nutrient,
    #              |(non water, temp) (used in calibration)
    'lte_uslefac': np.array([0.195745185]),                 #              |USLE slope length factor
    'lte_wrt1': np.array([3.94383001]),
    'lte_wrt2': np.array([2.64162780E-03]),
    'lte_smx': np.array([109.272125]),
    'lte_hk': np.array([151.504364]),
    'lte_yls': np.array([0.522464037]),
    'lte_ylc': np.array([0.852661312]),
    'lte_awc': np.array([183.539993]),                    #mm/mm        |available water capacity of soil
    'lte_g': np.array([0.0]),
    'lte_hufh': np.array([0.0]),
    'lte_phu': np.array([2698.91772]),
    'lte_por': np.array([532.000000]),
    'lte_sc': np.array([2.29999995]),
    'lte_sw': np.array([91.7699966]),                      #mm/mm         |initial soil water storage
    'lte_gw': np.array([0.0]),                      #mm            |initial shallow aquifer storage
    'lte_snow': np.array([0.0]),                    #mm            |initial water content of snow
    'lte_gwflow': np.array([0.0]),                  #mm            |initial groundwater flow
    #character(len=1) :: gro = "n"        #              |y=plant growing; n=not growing;
    'lte_dm': np.array([0.0]),                     #t/ha          |plant biomass
    'lte_alai': np.array([0.150000006]),                    #              |leaf area index
    'lte_yield': np.array([0.0]),                   #t/ha          |plant yield
    'lte_npp': np.array([0.0]),                     #t/ha          |net primary productivity
    'lte_lai_mx': np.array([0.0]),                 #              |maximum leaf area index
    'lte_gwdeep': np.array([0.0]),                 #mm            |deep aquifer storage
    'lte_aet': np.array([0.0]),                     #mm            |sum of actual et during growing season (for hi water stress)
    'lte_pet': np.array([0.0]),                     #mm            |sum of potential et during growing season (for hi water stress)
    'lte_start': np.array([0]),
    'lte_end': np.array([0]),
    #"hru_obj_no": np.array([0]),
    #'hru_area_ha': np.array([0.0]),
    #'hru_km': np.array([0.0]),
    #"hru_surf_stor": np.array([0]),                    #points to res() for surface storage
    #"hru_land_use_mgt": np.array([0]),
    #"hru_lum_group": np.array([0]),
    #"hru_plant_cov": np.array([0]),
    #"hru_mgt_ops": np.array([0]),
    #"hru_tiledrain": np.array([0]),
    #"hru_septic": np.array([0]),
    #"hru_fstrip": np.array([0]),
    #"hru_grassww": np.array([0]),
    #"hru_bmpuser": np.array([0]),
    #"hru_crop_reg": np.array([0]),
    #"hru_cur_op": np.array([1]),
    #'hru_sno_mm': np.array([0.0]),                          #mm H2O        |amount of water in snow on current day
    #'hru_water_fr': np.array([0.0]),
    #'hru_water_seep': np.array([0.0]),
    #'hru_water_evap': np.array([0.0]),
    #"hru_ich_flood": np.array([0]),

    # other data
    #type(topography):: topo
    #type(field):: field
    #type(hydrology):: hyd
    #type(landuse):: luse
    #type(land_use_mgt_variables):: lumv
    #type(subsurface_drainage_parameters):: sdr
    #type(snow_parameters):: sno
    #type(hru_databases):: dbs  # database pointers
    #type(hru_databases_char):: dbsc  # database pointers
    #type(hru_parms_db):: parms  # calibration parameters
}
