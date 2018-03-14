      subroutine current_par_value (ielem, ly, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine finds the current paramter value based on 
!!    user defined change

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    val_cur     |variable      |current parameter value
!!                               |the standard temperature (20 degrees C)
!!    chg         |data type     |contains information on variable change
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chg_par     |variable      |new parameter value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use basin_module
      use channel_data_module 
      use reservoir_data_module
      use hru_module, only : hru, isol, soil, sol, cn2
      use channel_module
      use reservoir_module
      use aquifer_module
      use hru_lte_module
      use organic_mineral_mass_module
      use hydrograph_module

      character(len=16), intent (in) :: chg_parm, chg_typ
      real, intent (in) :: chg_val, absmin, absmax
      integer, intent (in) :: ielem, num_db, ly

      select case (chg_parm)
          
      case ("cn2")
        cn2(ielem) = chg_par (cn2(ielem), ielem, chg_typ, chg_val, absmin, absmax, num_db)
        call curno (cn2(ielem), ielem)

      !! HRU  
      case ("biomix") 
        hru(ielem)%hyd%biomix = chg_par (hru(ielem)%hyd%biomix,           &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("cn3_swf") 
        hru(ielem)%hyd%cn3_swf = chg_par (hru(ielem)%hyd%cn3_swf,         &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("usle_p")
        hru(ielem)%lumv%usle_p = chg_par (hru(ielem)%lumv%usle_p,         &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("ovn")
        hru(ielem)%luse%ovn = chg_par (hru(ielem)%luse%ovn,               &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("elev")
        hru(ielem)%topo%elev = chg_par (hru(ielem)%topo%elev,             &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("slope")
        hru(ielem)%topo%slope = chg_par (hru(ielem)%topo%slope,           & 
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("slope_len")
        hru(ielem)%topo%slope_len = chg_par(hru(ielem)%topo%slope_len,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("lat_ttime")
        hru(ielem)%hyd%lat_ttime = chg_par(hru(ielem)%hyd%lat_ttime,      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
      case ("lat_sed")
        hru(ielem)%hyd%lat_sed = chg_par (hru(ielem)%hyd%lat_sed,         & 
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("lat_len")
        hru(ielem)%topo%lat_len = chg_par (hru(ielem)%topo%lat_len,     &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("canmx")
        hru(ielem)%hyd%canmx = chg_par (hru(ielem)%hyd%canmx,           & 
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("esco")
        hru(ielem)%hyd%esco = chg_par (hru(ielem)%hyd%esco,             & 
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("epco")
        hru(ielem)%hyd%epco = chg_par (hru(ielem)%hyd%epco,             & 
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("erorgn")
        hru(ielem)%hyd%erorgn = chg_par (hru(ielem)%hyd%erorgn,         & 
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("erorgp")
        hru(ielem)%hyd%erorgp = chg_par (hru(ielem)%hyd%erorgp,         &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("dis_stream")
        hru(ielem)%topo%dis_stream=chg_par(hru(ielem)%topo%dis_stream,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("dep_imp")
        nly = soil(ielem)%nly
        dep_below_soil = hru(ielem)%hyd%dep_imp + soil(ielem)%phys(nly)%d
        dep_below_soil = chg_par (dep_below_soil,                       &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        hru(ielem)%hyd%dep_imp = dep_below_soil + sol(ielem)%phys(nly)%d
        
      case ("lat_orgn")
        hru(ielem)%hyd%lat_orgn = chg_par (hru(ielem)%hyd%lat_orgn,     &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
      
      case ("lat_orgp")
        hru(ielem)%hyd%lat_orgp = chg_par (hru(ielem)%hyd%lat_orgp,     & 
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("field_len")
        hru(ielem)%field%length = chg_par(hru(ielem)%field%length,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("field_wid")
        hru(ielem)%field%wid = chg_par(hru(ielem)%field%wid,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("field_ang")
        hru(ielem)%field%ang = chg_par(hru(ielem)%field%ang,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
               
      !! SOL  
      case ("anion_excl")
        sol(isol)%s%anion_excl = chg_par(sol(isol)%s%anion_excl,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("crk")
         sol(isol)%s%crk = chg_par(sol(isol)%s%crk,                     &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("z")
          soil(ielem)%phys(ly)%d = chg_par(soil(ielem)%phys(ly)%d,      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("bd")
          soil(ielem)%phys(ly)%bd = chg_par(soil(ielem)%phys(ly)%bd,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("awc")
          soil(ielem)%phys(ly)%awc = chg_par(soil(ielem)%phys(ly)%awc,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("k")
          soil(ielem)%phys(ly)%k = chg_par(soil(ielem)%phys(ly)%k,      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("cbn")
          soil1(ielem)%tot(ly)%c = chg_par(soil1(ielem)%tot(ly)%c,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("clay")
          soil(ielem)%phys(ly)%clay = chg_par(soil(ielem)%phys(ly)%clay, &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("silt")
          soil(ielem)%phys(ly)%silt = chg_par(soil(ielem)%phys(ly)%silt, &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("sand")
          soil(ielem)%phys(ly)%sand = chg_par(soil(ielem)%phys(ly)%sand, &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("rock")
          soil(ielem)%phys(ly)%rock = chg_par(soil(ielem)%phys(ly)%rock, &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

      case ("alb")
          soil(ielem)%ly(ly)%alb = chg_par(soil(ielem)%ly(ly)%alb,       &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

      case ("usle_k")
          soil(ielem)%ly(ly)%usle_k = chg_par(soil(ielem)%ly(ly)%usle_k, &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

      case ("ec")
          soil(ielem)%ly(ly)%ec = chg_par(soil(ielem)%ly(ly)%ec,         &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("cal")
          soil(ielem)%ly(ly)%cal = chg_par(soil(ielem)%ly(ly)%cal,       &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
      
      case ("ph")
           soil(ielem)%ly(ly)%ph = chg_par(soil(ielem)%ly(ly)%ph,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       
       !! BSN
      case ("surlag")
        bsn_prm%surlag = chg_par(bsn_prm%surlag,                         &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("adj_pkr")
        bsn_prm%adj_pkr = chg_par(bsn_prm%adj_pkr,                      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("prf")
        bsn_prm%prf = chg_par(bsn_prm%prf,                              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("spcon")
        bsn_prm%spcon = chg_par(bsn_prm%spcon,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("spexp")
        bsn_prm%spexp = chg_par(bsn_prm%spexp,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("evrch")
        bsn_prm%evrch = chg_par(bsn_prm%evrch,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("evlai")
        bsn_prm%evlai = chg_par(bsn_prm%evlai,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("ffcb")
        bsn_prm%ffcb = chg_par(bsn_prm%ffcb,                            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("cmn")
        bsn_prm%cmn = chg_par(bsn_prm%cmn,                              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("nperco")
        bsn_prm%nperco = chg_par(bsn_prm%nperco,                        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("pperco")
        bsn_prm%pperco = chg_par(bsn_prm%pperco,                        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("phoskd")
        bsn_prm%phoskd = chg_par(bsn_prm%phoskd,                        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("psp")
        bsn_prm%psp = chg_par(bsn_prm%psp,                              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("rsdco")
        bsn_prm%rsdco = chg_par(bsn_prm%rsdco,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("percop")
        bsn_prm%percop = chg_par(bsn_prm%percop,                        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("msk_co1")
        bsn_prm%msk_co1= chg_par(bsn_prm%msk_co1,                       &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("msk_co2")
        bsn_prm%msk_co2 = chg_par(bsn_prm%msk_co2,                      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("msk_x")
        bsn_prm%msk_x = chg_par(bsn_prm%msk_x,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("cncoef")
        bsn_prm%cncoef = chg_par(bsn_prm%cncoef,                        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("trnsrch")
        bsn_prm%trnsrch = chg_par(bsn_prm%trnsrch,                      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("cdn")
        bsn_prm%cdn = chg_par(bsn_prm%cdn,                              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
      case ("tb_adj")
        bsn_prm%tb_adj = chg_par(bsn_prm%tb_adj,                        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("sdnco")
        bsn_prm%sdnco = chg_par(bsn_prm%sdnco,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("n_updis")
        bsn_prm%n_updis = chg_par(bsn_prm%n_updis,                      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("p_updis")
        bsn_prm%p_updis = chg_par(bsn_prm%p_updis,                      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("dorm_hr")
        bsn_prm%dorm_hr = chg_par(bsn_prm%dorm_hr,                      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("smxco")
        bsn_prm%smxco = chg_par(bsn_prm%smxco,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
      case ("fixco")
        bsn_prm%fixco = chg_par(bsn_prm%fixco,                          &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

!!     SWQ
      case ("rs1")
          ch_nut(ielem)%rs1 = chg_par(ch_nut(ielem)%rs1,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
         
       case ("rs2")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("rs3")
          ch_nut(ielem)%rs3 = chg_par(ch_nut(ielem)%rs3,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
  
       case ("rs4")
          ch_nut(ielem)%rs4 = chg_par(ch_nut(ielem)%rs4,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("rs5")
          ch_nut(ielem)%rs5 = chg_par(ch_nut(ielem)%rs5,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("rs6")
          ch_nut(ielem)%rs6 = chg_par(ch_nut(ielem)%rs6,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
       case ("rs7")
          ch_nut(ielem)%rs7 = chg_par(ch_nut(ielem)%rs7,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("rk1")
          ch_nut(ielem)%rk1 = chg_par(ch_nut(ielem)%rk1,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("rk2")
          ch_nut(ielem)%rk2 = chg_par(ch_nut(ielem)%rk2,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("rk3")
          ch_nut(ielem)%rk3 = chg_par(ch_nut(ielem)%rk3,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
       case ("rk4")
          ch_nut(ielem)%rk4 = chg_par(ch_nut(ielem)%rk4,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
       case ("rk5")
          ch_nut(ielem)%rs2 = chg_par(ch_nut(ielem)%rs2,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
       case ("rk6")
          ch_nut(ielem)%rk6 = chg_par(ch_nut(ielem)%rk6,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("bc1")
          ch_nut(ielem)%bc1 = chg_par(ch_nut(ielem)%bc1,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("bc2")
          ch_nut(ielem)%bc2 = chg_par(ch_nut(ielem)%bc2,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
       case ("bc3")
          ch_nut(ielem)%bc3 = chg_par(ch_nut(ielem)%bc3,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("bc4")
          ch_nut(ielem)%bc4 = chg_par(ch_nut(ielem)%bc4,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("sedpst_conc")
          ch_pst(ielem)%sedpst_conc = chg_par(ch_pst(ielem)%sedpst_conc, &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("pst_rea_ch")
          ch_pst(ielem)%pst_rea = chg_par(ch_pst(ielem)%pst_rea,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("pst_vol_ch")
          ch_pst(ielem)%pst_vol = chg_par(ch_pst(ielem)%pst_vol,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("pst_koc_ch")
          ch_pst(ielem)%pst_koc = chg_par(ch_pst(ielem)%pst_koc,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("pst_stl_ch")
          ch_pst(ielem)%pst_stl = chg_par(ch_pst(ielem)%pst_stl,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("pst_rsp_ch")
          ch_pst(ielem)%pst_rsp = chg_par(ch_pst(ielem)%pst_rsp,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("pst_mix_ch")
          ch_pst(ielem)%pst_mix = chg_par(ch_pst(ielem)%pst_mix,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("sedpst_rea")
          ch_pst(ielem)%sedpst_rea = chg_par(ch_pst(ielem)%sedpst_rea,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("sedpst_bry")
          ch_pst(ielem)%sedpst_bry = chg_par(ch_pst(ielem)%sedpst_bry,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("sedpst_act")
          ch_pst(ielem)%sedpst_act = chg_par(ch_pst(ielem)%sedpst_act,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("rch_dox")
          ch(ielem)%rch_dox = chg_par(ch(ielem)%rch_dox,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("rch_cbod")
          ch(ielem)%rch_cbod = chg_par(ch(ielem)%rch_cbod,              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("algae")
          ch(ielem)%algae = chg_par(ch(ielem)%algae,                    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("organicn")
          ch(ielem)%organicn = chg_par(ch(ielem)%organicn,              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("ammonian")
          ch(ielem)%ammonian = chg_par(ch(ielem)%ammonian,              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("nitriten")
          ch(ielem)%nitriten = chg_par(ch(ielem)%nitriten,              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
        case ("organicp")
          ch(ielem)%organicp = chg_par(ch(ielem)%organicp,              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
        case ("disolvp")
          ch(ielem)%disolvp = chg_par(ch(ielem)%disolvp,                &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
!!      RTE
         case ("w")
            ch_hyd(ielem)%w = chg_par(ch_hyd(ielem)%w,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
       
         case ("d")
            ch_hyd(ielem)%d = chg_par(ch_hyd(ielem)%d,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("s")
            ch_hyd(ielem)%s = chg_par(ch_hyd(ielem)%s,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("l")
            ch_hyd(ielem)%l = chg_par(ch_hyd(ielem)%l,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("n")
            ch_hyd(ielem)%n = chg_par(ch_hyd(ielem)%n,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("k_ch")
            ch_hyd(ielem)%k = chg_par(ch_hyd(ielem)%k,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("cov1")
            ch_sed(ielem)%cov1 = chg_par(ch_sed(ielem)%cov1,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("cov2")
            ch_sed(ielem)%cov2 = chg_par(ch_sed(ielem)%cov2,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("wdr")
            ch_hyd(ielem)%wdr = chg_par(ch_hyd(ielem)%wdr,              &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("alpha_bnk")
            ch_hyd(ielem)%alpha_bnk = chg_par(ch_hyd(ielem)%alpha_bnk,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("onco")
            ch_nut(ielem)%onco = chg_par(ch_nut(ielem)%onco,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("opco")
            ch_nut(ielem)%opco = chg_par(ch_nut(ielem)%opco,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("side")
            ch_hyd(ielem)%side = chg_par(ch_hyd(ielem)%side,            &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("bnk_bd")
            ch_sed(ielem)%bnk_bd = chg_par(ch_sed(ielem)%bnk_bd,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("bed_bd")
            ch_sed(ielem)%bed_bd = chg_par(ch_sed(ielem)%bed_bd,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("bnk_kd")
            ch_sed(ielem)%bnk_kd = chg_par(ch_sed(ielem)%bnk_kd,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("bed_kd")
            ch_sed(ielem)%bed_kd = chg_par(ch_sed(ielem)%bed_kd,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("bnk_d50")
            ch_sed(ielem)%bnk_d50 = chg_par(ch_sed(ielem)%bnk_d50,      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("bed_d50")
            ch_sed(ielem)%bed_d50 = chg_par(ch_sed(ielem)%bed_d50,      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("tc_bnk")
            ch_sed(ielem)%tc_bnk = chg_par(ch_sed(ielem)%tc_bnk,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("tc_bed")
            ch_sed(ielem)%tc_bed = chg_par(ch_sed(ielem)%tc_bed,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("erod(1)")    !! January only    
            ch_sed(ielem)%erod(1) = chg_par(ch_sed(ielem)%erod(1),      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)       
 
      !!RES
         case ("esa")
           res_ob(ielem)%esa = chg_par(res_ob(ielem)%esa,             &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("evol")
           res_ob(ielem)%evol = chg_par(res_ob(ielem)%evol,           &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db) 
        
         case ("psa")
           res_ob(ielem)%psa = chg_par(res_ob(ielem)%psa,             &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("pvol")
           res_ob(ielem)%pvol = chg_par(res_ob(ielem)%pvol,           &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("nsed")
           res_sed(ielem)%nsed = chg_par(res_sed(ielem)%nsed,           &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("k_res")
           res_hyd(ielem)%k = chg_par(res_hyd(ielem)%k,                 &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

         case ("evrsv")
            res_hyd(ielem)%evrsv = chg_par(res_hyd(ielem)%evrsv,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("vol")
            res(ielem)%flo = chg_par(res(ielem)%flo,                    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("sed")
            res(ielem)%sed = chg_par(res(ielem)%sed,                    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

         case ("orgp")
            res(ielem)%sedp = chg_par(res(ielem)%sedp,                  &
                          ielem, chg_typ, chg_val, absmin, absmax, num_db)
       
         case ("orgn")
            res(ielem)%orgn = chg_par(res(ielem)%orgn,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("solp")
            res(ielem)%solp = chg_par(res(ielem)%solp,                  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("no3")
            res(ielem)%no3 = chg_par(res(ielem)%no3,                    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("nh3")
            res(ielem)%nh3 = chg_par(res(ielem)%nh3,                    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("no2")
            res(ielem)%no2 = chg_par(res(ielem)%no2,                    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("psetlr1")
            res_nut(ielem)%psetlr1 = chg_par(res_nut(ielem)%psetlr1,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("psetlr2")
            res_nut(ielem)%psetlr2 = chg_par(res_nut(ielem)%psetlr2,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("nsetlr1")
            res_nut(ielem)%nsetlr1 = chg_par(res_nut(ielem)%nsetlr1,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("nsetlr2")
            res_nut(ielem)%nsetlr2 = chg_par(res_nut(ielem)%nsetlr2,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("chlar")
            res_nut(ielem)%chlar = chg_par(res_nut(ielem)%chlar,        &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("seccir")
            res_nut(ielem)%seccir = chg_par(res_nut(ielem)%seccir,      &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("pst_conc")
            res_pst(ielem)%pst_conc = chg_par(res_pst(ielem)%pst_conc,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
                       
         case ("pst_rea_res")
            res_pst(ielem)%pst_rea = chg_par(res_pst(ielem)%pst_rea,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
                       
         case ("pst_vol_res")
            res_pst(ielem)%pst_vol = chg_par(res_pst(ielem)%pst_vol,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("pst_koc_res")
            res_pst(ielem)%pst_koc = chg_par(res_pst(ielem)%pst_koc,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("pst_stl_res")
            res_pst(ielem)%pst_stl = chg_par(res_pst(ielem)%pst_stl,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("pst_rsp_res")
            res_pst(ielem)%pst_rsp = chg_par(res_pst(ielem)%pst_rsp,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("pst_mix_res")
            res_pst(ielem)%pst_mix = chg_par(res_pst(ielem)%pst_mix,    &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
                                
         case ("spst_conc")
            res_pst(ielem)%spst_conc = chg_par(res_pst(ielem)%spst_conc, &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
                
         case ("spst_bry")
            res_pst(ielem)%spst_bry = chg_par(res_pst(ielem)%spst_bry,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("spst_act")
            res_pst(ielem)%spst_act = chg_par(res_pst(ielem)%spst_act,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

         case ("alpha")
            alpha = chg_par(res_pst(ielem)%spst_act,  ielem, chg_typ, chg_val, absmin, absmax, num_db)
            aqu_prm(ielem)%alpha_e = Exp(-alpha)

         case ("delay")
            delay = chg_par(res_pst(ielem)%spst_act,  ielem, chg_typ, chg_val, absmin, absmax, num_db)
            aqu_prm(ielem)%delay_e = Exp(-1./(delay + 1.e-6))
            
         case ("flo_min")
            aqu_st(ielem)%flo_min = chg_par(res_pst(ielem)%spst_act,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)

         case ("revap_co")
            aqu_st(ielem)%revap_co = chg_par(res_pst(ielem)%spst_act,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("revap_min")
            aqu_st(ielem)%revap_min = chg_par(res_pst(ielem)%spst_act,  &
                         ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
      !!LTE
         case ("cn2_lte")
            hlt_db(ielem)%cn2 = chg_par (hlt_db(ielem)%cn2, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("awc_lte")
            c_val = chg_val * hlt(ielem)%soildep
            abmax = absmax * hlt(ielem)%soildep
            hlt(ielem)%awc = chg_par (hlt(ielem)%awc, ielem, chg_typ, c_val, absmin, abmax, num_db)
            
         case ("etco_lte")
            hlt_db(ielem)%etco = chg_par (hlt_db(ielem)%etco, ielem, chg_typ, chg_val, absmin, absmax, num_db)
                       
         case ("tc_lte")
            hlt_db(ielem)%tc = chg_par (hlt_db(ielem)%tc, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("soildep_lte")
            hlt_db(ielem)%soildep = chg_par (hlt_db(ielem)%soildep, ielem, chg_typ, chg_val, absmin, absmax, num_db)  
        
         case ("slope_lte")
            hlt_db(ielem)%slope = chg_par (hlt_db(ielem)%slope, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("slopelen_lte")
            hlt_db(ielem)%slopelen = chg_par (hlt_db(ielem)%slopelen, ielem, chg_typ, chg_val, absmin, absmax, num_db)
        
         case ("sy_lte")
            hlt_db(ielem)%sy = chg_par (hlt_db(ielem)%sy, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("abf_lte")
            hlt_db(ielem)%abf = chg_par (hlt_db(ielem)%abf, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("revapc_lte")
            hlt_db(ielem)%revapc = chg_par (hlt_db(ielem)%revapc, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("percc_lte")
            hlt_db(ielem)%percc = chg_par (hlt_db(ielem)%percc, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("sw_lte")
            hlt_db(ielem)%sw = chg_par (hlt_db(ielem)%sw, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("gw_lte")
            hlt_db(ielem)%gw = chg_par (hlt_db(ielem)%gw, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("gwflow_lte")
            hlt_db(ielem)%gwflow = chg_par (hlt_db(ielem)%gwflow, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
         case ("gwdeep_lte")
            hlt_db(ielem)%gwdeep = chg_par (hlt_db(ielem)%gwdeep, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
        case ("snow_lte")
            hlt_db(ielem)%snow = chg_par (hlt_db(ielem)%snow, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
        case ("uslek_lte")
            hlt_db(ielem)%uslek = chg_par (hlt_db(ielem)%uslek, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
        case ("uslec_lte")
            hlt_db(ielem)%uslec = chg_par (hlt_db(ielem)%uslec, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
        case ("uslep_lte")
            hlt_db(ielem)%uslep = chg_par (hlt_db(ielem)%uslep, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
        case ("uslels_lte")
            hlt_db(ielem)%uslels = chg_par (hlt_db(ielem)%uslels, ielem, chg_typ, chg_val, absmin, absmax, num_db)
            
        case ("t_opt")
            hlt_db(ielem)%uslels = chg_par (hlt_db(ielem)%uslels, ielem, chg_typ, chg_val, absmin, absmax, num_db)
        end select

      return
      end subroutine current_par_value