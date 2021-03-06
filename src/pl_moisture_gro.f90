      subroutine pl_moisture_gro

      use climate_module
      use hru_module, only : ihru, iwgen
      use plant_module
      use plant_data_module
      
      implicit none 

      integer :: j             !none    |hru number
      integer :: idp           !none    |plant number in plant parameter database
      integer :: ipl           !none    |plant number within community
      real :: p_pet_rto        !ratio   |precip/pet - 30 day sum of each

      j = ihru

      do ipl = 1, pcom(j)%npl
        if (pcom(j)%plcur(ipl)%monsoon_init == 1) then
          idp = pcomdb(j)%pl(ipl)%db_num
          p_pet_rto = wgn_pms(iwgen)%precip_sum / wgn_pms(iwgen)%pet_sum
          if (p_pet_rto > pldb(idp)%frsw_gro) then
            pcom(j)%plcur(ipl)%gro = "y"
            pcom(j)%plcur(ipl)%phuacc = 0. 
            pcom(j)%plcur(ipl)%idorm = "n"
            pcom(j)%plcur(ipl)%monsoon_init = 0
          end if
        endif
      end do

      return
      end subroutine pl_moisture_gro
