      subroutine hru_lum_init
    
      use hru_module, only : hru
      use plant_module, only : pcom
      use landuse_data_module, only : lum, lum_str, lum_grp
      use hydrograph_module, only : ob, sp_ob
      use climate_module, only : wst
      
      implicit none

      integer :: ihru               !none       |hru number 
      integer :: iob                !           |spatial object number
      integer :: ilu                !none       |land use number 
      integer :: ilug               !none       |counter 
      integer :: isched             !           |management schedule number
      integer :: iwst               !           |weather station number
      integer :: iwgn               !           |weather generator number

      do ihru = 1, sp_ob%hru
      
        !!assign land use pointers for the hru
        hru(ihru)%land_use_mgt = hru(ihru)%dbs%land_use_mgt
        ilu = hru(ihru)%dbs%land_use_mgt
        pcom(ihru)%name = lum(ilu)%plant_cov
        hru(ihru)%plant_cov = lum_str(ilu)%plant_cov
        hru(ihru)%lum_group_c = lum(ilu)%cal_group
        do ilug = 1, lum_grp%num
          if (hru(ihru)%lum_group_c == lum_grp%name(ilu)) then
            hru(ihru)%lum_group =  ilug
          end if
        end do
        iob = hru(ihru)%obj_no
        iwst = ob(iob)%wst
        iwgn = wst(iwst)%wco%wgn
        isched = lum_str(ilu)%mgt_ops
        hru(ihru)%mgt_ops = lum_str(ilu)%mgt_ops
        hru(ihru)%tiledrain = lum_str(ilu)%tiledrain
        hru(ihru)%septic = lum_str(ilu)%septic
        hru(ihru)%fstrip = lum_str(ilu)%fstrip
        hru(ihru)%grassww = lum_str(ilu)%grassww
        hru(ihru)%bmpuser = lum_str(ilu)%bmpuser
        hru(ihru)%luse%cn_lu = lum_str(ilu)%cn_lu
        hru(ihru)%luse%cons_prac = lum_str(ilu)%cons_prac
      end do

      return
      end subroutine hru_lum_init
