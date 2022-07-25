import numpy as np

model_vars = {
    "07120002_Iroquois_IL": "",
    "ceap_connectivity test": ['sd_chd','hru_luse%ovn'],
    "Chris_George_tx": "",
    "Little_River_Tifton": "",
    "saturated_buffer": ["sd_hc_co"],
    "Texas_large_gully": ['lte_km2'],
    "tropic_dataset": "",
    "TxtInOut_CoonCreek_aqu": "",
    "Usa_Basin_model": ["luse", "hru_land_use_mgt", "hru_lumv%usle_mult", "sd_chs"]
}


sample_data = {
    'sd_props': np.array([0, 1]),
    'sd_obj_no': np.array([0, 1]),
    #'sd_aqu_link': np.array([0, 1]),    #aquifer the channel is linked to
    #'sd_aqu_link_ch' : np.array([0, 1]),   #sequential channel number in the aquifer
    'sd_chw' : np.array([3., 4., 4., 4., 4., 4.]),    #m   ,    |channel width
    'sd_chd': np.array([0.4, 0.5, 0.5, 0.5, 0.5, 0.5]),     #m   ,    |channel depth
    'sd_chs': np.array([0.02, 0.01525, 0.02704, 0.01885, 0.03607, 0.02579, 0.0254, 0.01646, 0.02123, 0.00777, 0.01283, 0.00784]),     #m/m     |channel slope
    'sd_chl': np.array([0.1, 0.5, 0.5, 0.5, 0.5, 0.5]),     #km   ,   |channel length
    'sd_chn': np.array([1.79366203E-43, 4.00000007E-02]),     #   ,     |channel Manning's n
    'sd_cov': np.array([0.0, 4.00000007E-02]),     #0-1     |channel cover factor
    'sd_cherod': np.array([float('nan'), 1.200000003]),     #   ,     |channel erodibility
    'sd_shear_bnk': np.array([float('nan'), 0]),  #0-1     |bank shear coefficient - fraction of bottom shear
    'sd_hc_erod': np.array([1.40129846E-45, 0.5]),    #   ,     |headcut erodibility
    'sd_hc_co': np.array([0.0, 6.87572]),   #m/m     |proportionality coefficient for head cut
    'sd_hc_len': np.array([0.0, 1.0]),     #m   ,    |length of head cut
    'sd_hc_hgt': np.array([6.86636248E-44, 2.0]),     #m   ,    |headcut height
    'sd_stor': np.array([0.0, 1.0]),    #m3   ,   |water stored in reach at end of the day
    #'sd_kd' : np.array([1.0, 1.0]),     #   ,     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'sd_aq_mix' : np.array([1.0, 1.0]),     # m/day,     |aquatic mixing velocity (diffusion/dispersion)-using mol_wt
    #'name': ,
    'lte_props': np.array([2]),
    'lte_obj_no': np.array([2]),
    #character(len=16) :: lsu       #        |landscape unit - character
    #character(len=16) :: region   ,    #        |region - character
    #character(len=16) :: plant   ,     #        |plant type (as listed in plants.plt)
    'lte_iplant': np.array([15]),         #        |plant number xwalked from hlt_db()%plant and plants.plt
    'lte_km2': np.array([1.0672]),         #km^2   ,    |drainage area
    'lte_cn2': np.array([75.]),         #        |condition II curve number (used in calibration)
    'lte_cn3_swf': np.array([.5]),    #none   ,    |soil water factor for cn3 (used in calibration)
    #        |0 = fc; 1 = saturation (porosity)
    'lte_soildep': np.array([1530.]),    #mm      |soil profile depth
    'lte_etco': np.array([2.]), #        |et coefficient - use with pet and aet (used in calibration)
    'lte_revapc': np.array([0.5]),         #m/m   ,     |revap from aquifer (used in calibration)
    'lte_perco': np.array([1.5]),         #        |soil percolation coefficient (used in calibration)
    'lte_tdrain': np.array([1.0]),         #hr      |design subsurface tile drain time (used in calibration)
    'lte_stress': np.array([1.5]),         #frac   ,    |plant stress - pest, root restriction, soil quality, nutrient,
    #        |(non water, temp) (used in calibration)
    'lte_uslefac': np.array([0.295745185]),#        |USLE slope length factor
    'lte_wrt1': np.array([4.94383001]),
    'lte_wrt2': np.array([3.64162780E-03]),
    'lte_smx': np.array([110.272125]),
    'lte_hk': np.array([161.504364]),
    'lte_yls': np.array([0.622464037]),
    'lte_ylc': np.array([0.952661312]),
    'lte_awc': np.array([193.539993]), #mm/mm     |available water capacity of soil
    'lte_g': np.array([0.5]),
    'lte_hufh': np.array([0.6]),
    'lte_phu': np.array([2798.91772]),
    'lte_por': np.array([542.000000]),
    'lte_sc': np.array([3.29999995]),
    'lte_sw': np.array([92.7699966]),      #mm/mm   ,   |initial soil water storage
    'lte_gw': np.array([0.5]),   #mm      |initial shallow aquifer storage
    'lte_snow': np.array([0.5]), #mm      |initial water content of snow
    'lte_gwflow': np.array([0.5]),         #mm      |initial groundwater flow
    #character(len=1) :: gro = "n"     #        |y=plant growing; n=not growing;
    'lte_dm': np.array([0.5]),         #t/ha   ,    |plant biomass
    'lte_alai': np.array([0.250000006]), #        |leaf area index
    'lte_yield': np.array([0.3]),       #t/ha   ,    |plant yield
    'lte_npp': np.array([0.5]),         #t/ha   ,    |net primary productivity
    'lte_lai_mx': np.array([0.5]),      #        |maximum leaf area index
    'lte_gwdeep': np.array([0.5]),      #mm      |deep aquifer storage
    'lte_aet': np.array([0.5]),         #mm      |sum of actual et during growing season (for hi water stress)
    'lte_pet': np.array([0.5]),         #mm      |sum of potential et during growing season (for hi water stress)
    'lte_start': np.array([1]),
    'lte_end': np.array([2]),
    "hru_obj_no": np.array([0,1,2,3,4]),
    "hru_land_use_mgt": np.array([2, 1, 2, 2, 3, 3,12,12,11,11, 6, 7, 8,10, 1, 9,14,13, 1, 2, 3,11,10, 7,12, 8, 6, 1, 9,13, 1,12, 6,11, 8,10, 7, 2, 3, 9,13, 1,14, 1,12, 2, 9, 6, 8, 7,11, 3,10, 1,13, 1,12, 2, 8,10,11, 6, 9, 3, 7, 1,13, 1, 2,11, 3,12, 7, 9,10, 8, 6, 1,13, 2,11, 6, 8, 9, 3, 7,12,10, 1, 1,13, 1, 3, 2,11,12, 8, 7,10, 6, 1, 9,13,14,12, 6, 8,10, 1, 3,11, 7, 2, 1, 9,11, 1, 7,12,10, 8, 9, 3, 2, 6,13,11,12, 7, 1, 8, 2, 3,10, 6, 9]),
    "hru_land_use_mgt_c": np.array([0, 1, 3, 3, 3]),
    "hru_lum_group": np.array([119, 119, 119, 119, 119]),
    "hru_lum_group_c": np.array([119, 119, 119, 119, 119]), # this variable holds no data
    "hru_region": np.array([0]),# this variable does not print character data and is only important for calibration it seems
    "hru_plant_cov": np.array([-96, 1, 4, 4, 4]),
    "hru_mgt_ops": np.array([-1, 1, 3, 3, 3]),
    "hru_tiledrain": np.array([0, 1, 1, 1, 1]),
    "hru_septic": np.array([0, 0, 0, 0, 0]),
    "hru_fstrip": np.array([0, 0, 0, 0, 0]),
    "hru_grassww": np.array([0, 0, 0, 0, 0]),
    "hru_bmpuser": np.array([0, 0, 0, 0, 0]),
    "hru_crop_reg": np.array([0, 0, 0, 0, 0]),
    "hru_cur_op": np.array([1, 0, 0, 0, 0]),
    "hru_strsa": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_ich_flood": np.array([21982, 21982, 21982, 21982, 21982]),
    "hru_luse%name": np.array([1, 3, 1, 1, 538976288]), #cannot read what the values are supposed to be
    "hru_luse%cn_lu": np.array([7, 37, 7, 7, -1]),
    "hru_luse%cons_prac": np.array([1, 1, 1, 1, 1]),
    "hru_luse%usle_p": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_luse%urb_ro": np.array([1, 3, 1, 1, 538976288]),
    "hru_luse%urb_lu": np.array([0, 0, 0, 0, 0]),
    "hru_luse%ovn": np.array([0.02, 0.01, 0.01, 0.01]),
    "hru_dbs%name": np.array([1409366192, 1, 2, 3, 4]),
    "hru_dbs%topo": np.array([1, 1, 1, 1, 538976288]),
    "hru_dbs%hyd": np.array([1, 1, 1, 1, 538976288]),
    "hru_dbs%soil": np.array([1, 1, 1, 1, 538976288]),
    "hru_dbs%land_use_mgt": np.array([1, 3, 1, 3, 538976288]),
    "hru_dbs%soil_plant_in": np.array([2, 1, 2, 2, 0]),
    "hru_dbs%surf_stor": np.array([0, 1, 0, 0, 1]),
    "hru_dbs%snow": np.array([1, 1, 1, 1, 0]),
    "hru_dbs%field": np.array([1, 1, 1, 1, 538976288]),
    "hru_dbsc%name": np.array([-135231312, 1, 2, 3, 4]), #char value is 4x hru0010104 1x illegible
    "hru_dbsc%topo": np.array([1, 1, 1, 1, 538976288]), #char value is 4x hru00101 1x nothing
    "hru_dbsc%hyd": np.array([1, 1, 1, 1, 538976288]), #char value is 4x hru0010104 1x illegible
    "hru_dbsc%soil": np.array([1, 1, 1, 1, 538976288]), #char values is 4x IN025 and 1x nothing
    "hru_dbsc%land_use_mgt": np.array([1, 3, 1, 1, 538976288]), #char values [corn_soyb_notill, sat_buffer, corn_soyb_notill, sat_buffer, ____]
    "hru_dbsc%soil_plant_i": np.array([2, 1, 2, 2, 0]),
    "hru_dbsc%surf_stor": np.array([0, 1, 0, 0, 1]), #char values [null, surf_stor, null, surf_stor, {illegible}]
    "hru_dbsc%snow": np.array([1, 1, 1, 1, 0]), #char values 4x snow001 1xillegible
    "hru_dbsc%field": np.array([0, 0, 0, 0, 0]), #char values 4x null 1x illegible
    "hru_lumv%usle_p": np.array([1.0, 1.0, 1.0, 1.0, 0.0]),
    "hru_lumv%usle_ls": np.array([0.61949, 0.61949, 0.61949, 0.61949, 0.0]),
    "hru_lumv%usle_mult": np.array([
        5.94911,   1.81651,   3.62293,   1.04751,   2.69064,   2.00721,   0.20671,
        5.00352,   2.65543,   0.20671,   2.2519,    2.54716,   2.33478,   2.6422,
        2.96705,   4.05498,   2.23648,   1.83822,   6.34246,   3.40399,   1.80816,
        2.33152,   2.50629,   2.63538,   2.23098,   1.74789,   2.28369,   2.27112,
        3.92724,   2.25336,  18.91446,  18.06911,   7.57528,   4.84975,   5.27156,
        3.71761,   3.86148,   8.8481,   15.2227,    4.65371,   2.08378,   3.07099,
        1.72056,  13.762,     5.68686,   4.24774,   2.66015,   3.87236,   1.97469,
        1.80791,   3.4695,    3.43267,   1.75161,   2.36331,   2.11279,  15.90663,
        24.25159,   5.52052,   7.66916,   7.95479,   9.60458,  10.54403,   5.76376,
        8.40028,   5.80444,   3.07245,   0.20609,  11.62282,   4.98392,   5.8704,
        5.67137,   4.86416,   7.60532,   4.62798,   1.86841,   6.09251,   3.17276,
        12.5826,    2.36668,   6.72253,   3.90654,   3.67035,   3.11736,   4.24272,
        4.62637,   2.74677,   4.00466,   1.88782,   4.43035,   2.9113,    4.23904,
        1.59623,   2.08225,   1.78525,   2.88897,   5.41522,   1.94942,   2.48077,
        2.24172,   2.76186,   2.65416,   1.30273,   2.37029,   0.20609,   3.09836,
        2.54423,   3.38029,  31.19085,   2.8553,    6.78521,   9.71964,   5.27538,
        35.72303, 101.05276,  10.53396,   2.16589,   9.34291,   1.62873,   2.24782,
        2.81671,   1.79573,   6.33117,   1.56814,   1.76021,   7.65208,   1.34621,
        2.17068,   3.09497,   3.56385,   3.4822,    1.57893,  18.31804,   2.0204,
        2.05598,   4.12083,   1.28232]),
    "hru_lumv%sdr_dep": np.array([1000.0, 1000.0, 1000.0, 1000.0, 0.05]),
    "hru_lumv%ldrain": np.array([4, 4, 4, 4, 0]),
    "hru_lumv%tile_ttime": np.array([0.22120, 0.22120, 0.22120, 0.22120, 0.0]),
    "hru_lumv%vfsi": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%vfsratio": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%vfscon": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%vfsch": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%ngrwat": np.array([0, 0, 0, 0, 0]),
    "hru_lumv%grwat_i": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%grwat_n": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%grwat_spcon": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%grwat_d": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%grwat_w": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%grwat_l": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%grwat_s": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%bmp_flag": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%bmp_sed": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%bmp_pp": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%bmp_sp": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%bmp_pn": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%bmp_sn": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    "hru_lumv%bmp_bac": np.array([0.0, 0.0, 0.0, 0.0, 0.0]),
    #"hru_obj_no": np.array([-2098603856, 1, 2, 3, 4]), #not good for testing; changes every run
    "hru_area_ha": np.array([0, 40, 40, 40, 40]),
    "hru_km": np.array([0, 0.4, 0.4, 0.4, 0.4]),
    "hru_surf_stor": np.array([1,1,1,1,1]),
   #"hru_sno_mm": np.array([0.05924, 0, 0, 0, 0]),,   #not good for testing; changes every run -- mm H2O     |amount of water in snow on current day
    "hru_water_fr": np.array([0, 0, 0, 0, 0]),
    "hru_water_seep": np.array([0, 0, 0, 0, 0]),
    "hru_water_evap": np.array([0, 0, 0, 0, 0]),
  #"hru_ich_flood": np.array([22053, 22093, 21856, 22041, 21870]), #not good for testing; changes every run
    "cn_luse": np.array([7, 37, 7, 37, -1]),
    "luse": np.array([5, 4, 3, 3, 3, 8,13,10,9,6, 7, 7, 8,10, 1, 9,14,13, 1, 2, 3,11,10, 7,12, 8, 6, 1, 9,13, 1,12, 6,11, 8,10, 7, 2, 3, 9,13, 1,14, 1,12, 2, 9, 6, 8, 7,11, 3,10, 1,13, 1,12, 2, 8,10,11, 6, 9, 3, 7, 1,13, 1, 2,11, 3,12, 7, 9,10, 8, 6, 1,13, 2,11, 6, 8, 9, 3, 7,12,10, 1, 1,13, 1, 3, 2,11,12, 8, 7,10, 6, 1, 9,13,14,12, 6, 8,10, 1, 3,11, 7, 2, 1, 9,11, 1, 7,12,10, 8, 9, 3, 2, 6,13,11,12, 7, 1, 8, 2, 3,10, 6, 9]),
    "algae": np.array([]),     # mg alg/L   |algal biomass concentration in reach
    "ammonian": np.array([]),     # mg N/L     |ammonia concentration in reach
    "bankst": np.array([]),    # m^3 H2O    |bank storage
    "li": np.array([]),        # km      |initial length of main channel
    "orgn": np.array([]),      #      ,   |organic nitrogen contribution from channel erosion
    "orgp": np.array([]),      #      ,   |organic phosphorus contribution from channel erosion
    "si": np.array([]),        #(m/n)   ,    |slope of main channel
    "wi": np.array([]),        #(m)      |width of main channel at top of bank
    "di": np.array([]),        #(m)      |depth of main channel from top of bank to bottom
    "chlora": np.array([]),    # mg chl-a/L,    |chlorophyll-a concentration in reach
    "pst_conc": np.array([]),     # mg/(m**3),     |initial pesticide concentration in reach
    "dep_chan": np.array([]),     # m       |average daily water depth in channel
    "disolvp": np.array([]),   # mg P/L     |dissolved P concentration in reach
    "drift": np.array([]),     # kg      |amount of pesticide drifting onto main channel in subbasin
    "flwin": np.array([]),     # m^3 H2O    |flow into reach on previous day
    "flwout": np.array([]),    # m^3 H2O    |flow out of reach on previous day
    "nitraten": np.array([]),     # mg N/L     |nitrate concentration in reach
    "nitriten": np.array([]),     # mg N/L     |nitrite concentration in reach
    "organicn": np.array([]),     # mg N/L     |organic nitrogen concentration in reach
    "organicp": np.array([]),     # mg P/L     |organic phosphorus concentration in reach
    "rch_bactlp": np.array([]),   # cfu/100ml,   |less persistent bacteria stored in reach
    "rch_bactp": np.array([]),    # cfu/100ml,   |persistent bacteria stored in reach
    "rch_cbod": np.array([]),     # mg O2/L    |carbonaceous biochemical oxygen demand in reach
    "rch_dox": np.array([]),   # mg O2/L    |dissolved oxygen concentration in reach
    "rchstor": np.array([]),   # m^3 H2O    |water stored in reach
    "sedst": np.array([]),     # metric tons,   |amount of sediment stored in reach
    "vel_chan": np.array([]),     # m/s   ,     |average flow velocity in channel
    "bed_san": np.array([]),
    "bed_sil": np.array([]),
    "bed_cla": np.array([]),
    "bed_gra": np.array([]),
    "bnk_san": np.array([]),
    "bnk_sil": np.array([]),
    "bnk_cla": np.array([]),
    "bnk_gra": np.array([]),
    "depfp": np.array([]),
    "depprfp": np.array([]),
    "depsilfp": np.array([]),
    "depclafp": np.array([]),
    "depch": np.array([]),
    "depprch": np.array([]),
    "depsanch": np.array([]),
    "depsilch": np.array([]),
    "depclach": np.array([]),
    "depsagch": np.array([]),
    "deplagch": np.array([]),
    "depgrach": np.array([]),
    "sanst": np.array([]),
    "silst": np.array([]),
    "clast": np.array([]),
    "sagst": np.array([]),
    "lagst": np.array([]),
    "grast": np.array([]),
    "wattemp": np.array([]),
    "bactp": np.array([]),
    "chfloodvol": np.array([]),
    "bactlp": np.array([]),
}

