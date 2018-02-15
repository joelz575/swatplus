      subroutine topohyd_init
    
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      use parm, only : hru, hru_db, sno_hru, ihru
      use jrw_datalib_module

      integer :: eof

      !!assign topography and hyd paramters
      do ihru = 1, sp_ob%hru
        iob = sp_ob1%hru + ihru - 1
        itopo_db = hru(ihru)%dbs%topo
        ihyd_db = hru(ihru)%dbs%hyd
        itopohd_db = hru(ihru)%dbs%topo
        ihyd_db = hru(ihru)%dbs%hyd
        isol = hru(ihru)%dbs%soil
        ifield_db = hru(ihru)%dbs%field
        hru(ihru)%topo%name = topo_db(itopo_db)%name
        hru(ihru)%topo%elev = ob(iob)%elev
        hru(ihru)%topo%slope = topo_db(itopohd_db)%slope
        hru(ihru)%topo%slope_len = topo_db(itopohd_db)%slope_len
        hru(ihru)%hyd%name = hyd_db(ihyd_db)%name
        hru(ihru)%hyd%lat_ttime = hyd_db(ihyd_db)%lat_ttime
        hru(ihru)%hyd%lat_sed = hyd_db(ihyd_db)%lat_sed / 1000. !mg/l => g/l ; mm * km2 * g/l = t
        hru(ihru)%topo%lat_len = topo_db(itopohd_db)%lat_len
        hru(ihru)%hyd%canmx = hyd_db(ihyd_db)%canmx
        hru(ihru)%hyd%esco = hyd_db(ihyd_db)%esco
        hru(ihru)%hyd%epco = hyd_db(ihyd_db)%epco
        hru(ihru)%hyd%erorgn = hyd_db(ihyd_db)%erorgn
        hru(ihru)%hyd%erorgp = hyd_db(ihyd_db)%erorgp
        hru(ihru)%hyd%cn3_swf = hyd_db(ihyd_db)%cn3_swf
        hru(ihru)%hyd%perco = hyd_db(ihyd_db)%perco
        hru(ihru)%topo%dis_stream = topo_db(itopohd_db)%dis_stream
        hru(ihru)%hyd%biomix = hyd_db(ihyd_db)%biomix
        nly = soildb(isol)%s%nly
        hru(ihru)%hyd%dep_imp = hyd_db(ihyd_db)%dep_imp + soildb(isol)%ly(nly)%z
        hru(ihru)%hyd%dep_imp_init = hyd_db(ihyd_db)%dep_imp
        hru(ihru)%hyd%lat_orgn = hyd_db(ihyd_db)%lat_orgn
        hru(ihru)%hyd%lat_orgp = hyd_db(ihyd_db)%lat_orgp
        ! set field data
        hru(ihru)%field%length = field_db(ifield_db)%length
        hru(ihru)%field%wid = field_db(ifield_db)%wid
        hru(ihru)%field%ang = field_db(ifield_db)%ang
        hru(ihru)%topo%dep_co = topo_db(itopohd_db)%dep_co
        ! set initial snow cover
        isno = hru_db(i)%dbs%snow 
        sno_hru(ihru) = snodb(isno)%init_mm
      end do
      
      return
      end subroutine topohyd_init