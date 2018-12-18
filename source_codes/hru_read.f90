      subroutine hru_read

      use maximum_data_module
      use reservoir_data_module
      use reservoir_module
      use landuse_data_module
      use hydrology_data_module
      use topography_data_module
      use soil_data_module
      use input_file_module
      use hru_module, only : hru, hru_db, ihru, sol_plt_ini
      use soil_module
      use plant_module
      use hydrograph_module !, only : ob, sp_ob, sp_ob1, wet
      use organic_mineral_mass_module
      use carbon_module
      use constituent_mass_module
      
      implicit none
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: i_exist              !none       |check to determine if file exists
      integer :: i                    !           |
      integer :: max                  !           |
      integer :: k                    !           |
      integer :: ilum                 !none       |counter
      integer :: ith                  !none       |counter 
      integer :: ithyd                !none       |counter
      integer :: isol                 !none       |counter
      integer :: isolt                !none       |counter
      integer :: isstor               !none       |counter
      integer :: isno                 !none       |counter
      integer :: ifld                 !none       |counter
      integer :: isp_ini              !none       |counter
      integer :: ics                  !none       |counter
      
      eof = 0
      imax = 0
      
      call allocate_parms

      inquire (file=in_hru%hru_data, exist=i_exist)
      if (i_exist == 0 .or. in_hru%hru_data == "null") then
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
        allocate (wet_ob(imax))
        allocate (wet_om_d(imax))
        allocate (wet_om_m(imax))
        allocate (wet_om_y(imax))
        allocate (wet_om_a(imax))
        allocate (wet_in_d(imax))
        allocate (wet_in_m(imax))
        allocate (wet_in_y(imax))
        allocate (wet_in_a(imax))
        allocate (wet_out_d(imax))
        allocate (wet_out_m(imax))
        allocate (wet_out_y(imax))
        allocate (wet_out_a(imax))
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
          end do
          
          ! initialize nutrients and constituents in soil and plants
          do isp_ini = 1, db_mx%sol_plt_ini
            if (hru_db(i)%dbsc%soil_plant_init == sol_plt_ini(isp_ini)%name) then
              hru_db(i)%dbs%soil_plant_init = isp_ini
              ! initial soil nutrients (soil test)
              do ics = 1, db_mx%soiltest
                if (sol_plt_ini(isp_ini)%nutc == solt_db(ics)%name) then
                  sol_plt_ini(isp_ini)%nut = ics
                  exit
                end if
              end do
              ! initial pesticides
              do ics = 1, db_mx%pest_ini
                if (sol_plt_ini(isp_ini)%pestc == pesti_db(ics)%name) then
                  sol_plt_ini(isp_ini)%pest = ics
                  exit
                end if
              end do
              ! initial pathogens
              do ics = 1, db_mx%path_ini
                if (sol_plt_ini(isp_ini)%pathc == pathi_db(ics)%name) then
                  sol_plt_ini(isp_ini)%path = ics
                  exit
                end if
              end do
              ! initial heavy metals
              do ics = 1, db_mx%hmet_ini
                if (sol_plt_ini(isp_ini)%hmetc == hmeti_db(ics)%name) then
                  sol_plt_ini(isp_ini)%hmet = ics
                  exit
                end if
              end do
              ! initial salts
              do ics = 1, db_mx%salt_ini
                if (sol_plt_ini(isp_ini)%saltc == salti_db(ics)%name) then
                  sol_plt_ini(isp_ini)%salt = ics
                  exit
                end if
              end do
              
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