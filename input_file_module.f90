      module input_file_module

!! file.cio input file   
      type input_sim
!! simulation
        character(len=25) :: time = "time.sim"
        character(len=25) :: prt = "print.prt"
        character(len=25) :: object_prt = "object.prt"
        character(len=25) :: object_cnt = "object.cnt"
      end type input_sim
      type (input_sim) :: in_sim
	
      type input_cli
!! climate
       character(len=25) :: weat_sta = "weather-sta.cli"
       character(len=25) :: weat_wgn = "weather-wgn.cli"
       character(len=25) :: wind_dir = "wind-dir.cli"
       character(len=25) :: pcp_cli = "pcp.cli"
       character(len=25) :: tmp_cli = "tmp.cli"
       character(len=25) :: slr_cli = "slr.cli"
       character(len=25) :: hmd_cli = "hmd.cli"
       character(len=25) :: wnd_cli = "wnd.cli"
      end type input_cli
      type (input_cli) :: in_cli

      type input_con
!! connect
       character(len=25) :: hru_con = "hru.con"
       character(len=25) :: hruez_con = "hru-lte.con"
       character(len=25) :: sub_con = "subbasin.con"
       character(len=25) :: modflow_con = "modflow.con"
       character(len=25) :: aqu_con = "aquifer.con"
       character(len=25) :: aqu2d_con = "aquifer2d.con"
       character(len=25) :: chan_con = "channel.con"
       character(len=25) :: res_con = "reservoir.con"
       character(len=25) :: rec_con = "recall.con"
       character(len=25) :: exco_con = "exco.con"
       character(len=25) :: delr_con = "delratio.con"
       character(len=25) :: out_con = "outlet.con"
       character(len=25) :: chandeg_con = "chandeg.con"
      end type input_con
      type (input_con) :: in_con

      type input_cha 
!! channel  
       character(len=25) :: init = "initial.cha"
       character(len=25) :: dat =  "channel.cha"
       character(len=25) :: hyd =  "hydrology.cha"
       character(len=25) :: sed =  "sediment.cha"
       character(len=25) :: nut =  "nutrients.cha"
       character(len=25) :: pest = "pesticide.cha"
       character(len=25) :: chan_ez = "channel-lte.cha"
      end type input_cha
      type (input_cha) :: in_cha

      type input_res
!! reservoir
       character(len=25) :: init_res = "initial.res"
       character(len=25) :: res =      "reservoir.res"
       character(len=25) :: hyd_res =  "hydrology.res"
       character(len=25) :: nut_res =  "nutrients.res"
       character(len=25) :: pest_res = "pesticide.res"
       character(len=25) :: sed_res =  "sediment.res"
       character(len=25) :: weir_res = "weir.res"
      end type input_res
      type (input_res) :: in_res

      type input_sub
!! subbasin
       character(len=25) :: def_sub = "define.sub"
       character(len=25) :: ele_sub = "element.sub"
       character(len=25) :: sub = "subbasin.sub"
       character(len=25) :: sub_del = "subbasin.del"
      end type input_sub
      type (input_sub) :: in_sub

      type input_hru
!! HRU
       character(len=25) :: hru_data = "hru-data.hru"
       character(len=25) :: hru_ez   = "hru-lte.hru"
      end type input_hru
      type (input_hru) :: in_hru

      type input_delr
!! delivery ratio
       character(len=25) :: del_ratio = "delratio.del"
      end type input_delr
      type (input_delr) :: in_delr

!! aquifer 
      type input_aqu
       character(len=25) :: aqu = "aquifer.aqu"
      end type input_aqu
      type (input_aqu) :: in_aqu
      
!! herd
      type input_herd
        character(len=25) :: animal = "animal.hrd"
        character(len=25) :: herd   = "herd.hrd"
        character(len=25) :: ranch  = "ranch.hrd"
      end type input_herd
      type (input_herd) :: in_herd
      
!! water-rights
      type input_water_rights
        character(len=25) :: define = "define.wro"
        character(len=25) :: element = "element.wro"
        character(len=25) :: water_rights = "water_rights.wro"
      end type input_water_rights
      type (input_water_rights) :: in_watrts
      
!! link
      type input_link
       character(len=25) :: chan_surf = "chan-surf.lin"
       character(len=25) :: chan_aqu = "chan-aqu.lin"
      end type input_link
      type (input_link) :: in_link

      type input_basin
       character(len=25) :: codes_bas = "codes.bsn"
       character(len=25) :: parms_bas = "parameters.bsn"
      end type input_basin
      type (input_basin) :: in_basin

      type input_hydrology
       character(len=25) :: hydrol_hyd = "hydrology.hyd"
       character(len=25) :: topogr_hyd = "topography.hru"
       character(len=25) :: toposub_hyd = "topography.sub"
       character(len=25) :: field_fld  = "field.fld"
      end type input_hydrology
      type (input_hydrology) :: in_hyd
  
      type input_exco
       character(len=25) :: exco = "exco.exc"
      end type input_exco
      type (input_exco) :: in_exco

      type input_bacteria
       character(len=25) :: init_bac = "initial.bac"
       character(len=25) :: bacteria = "bacteria.bac"
      end type input_bacteria
      type (input_bacteria) :: in_bac

      type input_structural
       character(len=25) :: septic_str = "septic.str"
       character(len=25) :: bmpuser_str = "bmpuser.str"
       character(len=25) :: contour_str = "contour.str"
       character(len=25) :: fstrip_str = "filterstrip.str"
       character(len=25) :: fire_str = "fire.str"
       character(len=25) :: grassww_str = "grassedww.str"
       character(len=25) :: plparms_str = "plantparms.str"
       character(len=25) :: residue_str = "residue.str"
       character(len=25) :: stcrop_str = "stripcrop.str"
       character(len=25) :: terrace_str = "terrace.str"
       character(len=25) :: tiledrain_str = "tiledrain.str"
       character(len=25) :: initial_str = "initial.str"
      end type input_structural
      type (input_structural) :: in_str

      type input_parameter_databases
       character(len=25) :: plants_plt = "plants.plt"
       character(len=25) :: fert_frt = "fertilizer.frt"
       character(len=25) :: till_til = "tillage.til"
       character(len=25) :: pest_pst = "pesticide.pst"
       character(len=25) :: urban_urb = "urban.urb"
       character(len=25) :: septic_sep = "septic.sep"
       character(len=25) :: snow = "snow.sno"
       character(len=25) :: atmodb = "atmo.atm"
      end type input_parameter_databases
      type (input_parameter_databases) :: in_parmdb

      type input_ops
       character(len=25) :: autofert_ops = "autofert.ops"
       character(len=25) :: autoirr_ops = "autoirr.ops"
       character(len=25) :: contfert_ops = "contfert.ops"
       character(len=25) :: contpest_ops = "contpest.ops"
       character(len=25) :: fert_ops = "fert.ops"
       character(len=25) :: graze_ops = "graze.ops"
       character(len=25) :: harv_ops = "harv.ops"
       character(len=25) :: irr_ops = "irr.ops"
       character(len=25) :: pest_ops = "pest.ops"
       character(len=25) :: sweep_ops = "sweep.ops"
      end type input_ops
      type (input_ops) :: in_ops

      type input_sch
       character(len=25) :: management_sch = "management.sch"
       character(len=25) :: structural_sch = "structural.sch"
      end type input_sch
      type (input_sch) :: in_sch

      type input_lum
       character(len=25) :: cntable_lum = "cntable.lum"
       character(len=25) :: landuse_lum = "landuse.lum"
      end type input_lum
      type (input_lum) :: in_lum

      type input_chg
       character(len=25) :: codes_cal = "codes.cal"
       character(len=25) :: cal_parms = "cal_parms.cal"
       character(len=25) :: cal_upd = "calibration.cal"
       character(len=25) :: ls_parms_cal = "ls_parms.cal"
       character(len=25) :: ls_regions_cal = "ls_regions.cal"
       character(len=25) :: ch_orders_cal = "ch_orders.cal"
       character(len=25) :: ch_parms_cal = "ch_parms.cal"
      end type input_chg
      type (input_chg) :: in_chg

      type input_init
       character(len=25) :: initial_pst = "initial.pst"
       character(len=25) :: initial_plt = "initial.plt"
       end type input_init
      type (input_init) :: in_init

      type input_soils
       character(len=25) :: soils_sol = "soils.sol"
       character(len=25) :: nut_sol = "nutrients.sol"
      end type input_soils
      type (input_soils) :: in_sol

      type input_condition
       character(len=25) :: cond_ctl = "d_table.dtl"
      end type input_condition
      type (input_condition) :: in_cond
      
      type input_constituents
        character(len=25) :: cs_db = "constituents.cs"
        character(len=25) :: pestcom_db = "pest.cs"
        character(len=25) :: pathcom_db = "path.cs"
        character(len=25) :: hmetcom_db = "hmet.cs"
        character(len=25) :: saltcom_db = "salt.cs"
      end type input_constituents
      type (input_constituents) :: in_const
      
     type input_regions
        character(len=25) :: ele_lcu = "ls_catunit.ele"
        character(len=25) :: def_lcu = "ls_catunit.def"
        character(len=25) :: def_lcu_reg = "ls_reg.def"
        character(len=25) :: cal_lcu = "ls_cal.reg"
        character(len=25) :: ele_cha = "ch_catunit.ele"
        character(len=25) :: def_cha = "ch_catunit.def"
        character(len=25) :: def_cha_reg = "ch_reg.def"
        character(len=25) :: ele_aqu = "aqu_catunit.ele"
        character(len=25) :: def_aqu = "aqu_catunit.def"
        character(len=25) :: def_aqu_reg = "aqu_reg.def"
        character(len=25) :: ele_res = "res_catunit.ele"
        character(len=25) :: def_res = "res_catunit.def"
        character(len=25) :: def_res_reg = "res_reg.def"
        character(len=25) :: ele_psc = "rec_catunit.ele"
        character(len=25) :: def_psc = "rec_catunit.def"
        character(len=25) :: def_psc_reg = "rec_reg.def"
      end type input_regions
      type (input_regions) :: in_regs
      
      contains

      include 'readcio_read.f90'

      end module input_file_module 