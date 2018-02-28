      subroutine hru_read

      use jrw_datalib_module, only : db_mx, lum, solt_db, topo_db, hyd_db, soildb, wet_dat_c, snodb, field_db
      use input_file_module
      use hru_module, only : hru, hru_db, soil, pcom, ihru, isolt 
      use hydrograph_module, only : ob, sp_ob, sp_ob1, wet
      use organic_mineral_mass_module
      use carbon_module
      
      character (len=500) :: header
      character (len=80) :: titldum
      integer :: eof, imax
      
      eof = 0
      imax = 0
      
      call allocate_parms

      inquire (file=in_hru%hru_data, exist=i_exist)
      if (i_exist == 0 .or. in_hru%hru_data == 'null') then
        allocate (hru_db(0:0))
        allocate (hru(0:0))
        allocate (soil(0:0))
        allocate (soil1(0:0))
        allocate (soil1_init(0:0))
        allocate (cbn_loss(0:0))
        allocate (pcom(0:0))
        allocate (rsd1(0:0))
      else 
      do
        open (113,file=in_hru%hru_data)
        read (113,*,iostat=eof) titldum
        if (eof < 0) exit
        read (113,*,iostat=eof) header
        if (eof < 0) exit
         do while (eof == 0)
            read (113,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
          end do
          
        allocate (hru_db(0:imax))
        allocate (hru(0:imax))
        allocate (soil(0:imax))
        allocate (soil1(0:imax))
        allocate (soil1_init(0:imax))
        allocate (cbn_loss(0:imax))
        allocate (pcom(0:imax))
        allocate (wet(0:imax))
        allocate (rsd1(0:imax))
        
        rewind (113)
        read (113,*) titldum
        read (113,*) header

      do ihru = 1, sp_ob%hru
        read (113,*) i
        backspace (113)
        read (113,*,iostat=eof) k, hru_db(i)%dbsc

        hru(ihru)%obj_no = sp_ob1%hru + ihru - 1
        hru(ihru)%area_ha = ob(hru(ihru)%obj_no)%area_ha
        hru(ihru)%km = ob(hru(ihru)%obj_no)%area_ha / 100.
        hru(ihru)%land_use_mgt_c = hru_db(ihru)%dbsc%land_use_mgt
        if (eof < 0) exit

          do ilum = 1, db_mx%landuse
            if (hru_db(i)%dbsc%land_use_mgt == lum(ilum)%name) then
               hru_db(i)%dbs%land_use_mgt = ilum
            exit
            end if
           ! write (*,*) 'LUM not found during crosswalk.  Please ensure it is included in landuse.lum'
           ! write (*,*) 'For LUM not found, the model will default to #1 in the file'
           ! write (*,*) 'Enter to continue/CNTRL break to cancel simulation'
           ! stop
          end do
          do isolt = 1, db_mx%soiltest
            if (hru_db(i)%dbsc%soil_nutr_init == solt_db(isolt)%name) then
               hru_db(i)%dbs%soil_nutr_init = isolt
            exit
            end if
          end do
          do ith = 1, db_mx%topo
            if (hru_db(i)%dbsc%topo == topo_db(ith)%name) then
               hru_db(i)%dbs%topo = ith
            exit
            end if
          end do
         do ithyd = 1, db_mx%hyd
            if (hru_db(i)%dbsc%hyd == hyd_db(ithyd)%name) then
               hru_db(i)%dbs%hyd = ithyd
            exit
            end if
         end do
         do isol = 1, db_mx%soil
            if (hru_db(i)%dbsc%soil == soildb(isol)%s%snam) then
               hru_db(i)%dbs%soil = isol
            exit
            end if
         end do
         do isstor = 1, db_mx%wet_dat
            if (hru_db(i)%dbsc%surf_stor == wet_dat_c(isstor)%name) then
               hru_db(i)%dbs%surf_stor = isstor
            exit
            end if
         end do
         do isno = 1, db_mx%sno
            if (hru_db(i)%dbsc%snow == snodb(isno)%name) then
               hru_db(i)%dbs%snow = isno
            exit
            end if
         end do
         do ifld = 1, db_mx%field
             if (hru_db(i)%dbsc%field == field_db(ifld)%name) then
               hru_db(i)%dbs%field = ifld
            exit
            end if
         end do

      end do
      exit
      enddo
      endif
      
      close (113)
     
       return
       end subroutine hru_read     