      subroutine res_allo
      
      use reservoir_module
      use reservoir_data_module
      use hydrograph_module
      use constituent_mass_module
      
      implicit none     

      integer :: ires           !             |
      integer :: mres           !             |
      
      mres = sp_ob%res
      allocate (res(0:mres))
      allocate (res_ob(0:mres))
      allocate (res_in_d(mres))
      allocate (res_in_m(mres))
      allocate (res_in_y(mres))
      allocate (res_in_a(mres))
      allocate (res_out_d(mres))
      allocate (res_out_m(mres))
      allocate (res_out_y(mres))
      allocate (res_out_a(mres))
      allocate (res_om_d(mres))
      allocate (res_om_m(mres))
      allocate (res_om_y(mres))
      allocate (res_om_a(mres))
      allocate (res_water(mres))
      allocate (res_benthic(mres))
      
      if (cs_db%num_tot > 0) then
        do ires = 1, sp_ob%res
          allocate (res_water(ires)%pest(cs_db%num_pests))
          allocate (res_benthic(ires)%pest(cs_db%num_pests))
          allocate (res_water(ires)%path(cs_db%num_paths))
          allocate (res_benthic(ires)%path(cs_db%num_paths))
          allocate (res_water(ires)%hmet(cs_db%num_metals))
          allocate (res_benthic(ires)%hmet(cs_db%num_metals))
          allocate (res_water(ires)%salt(cs_db%num_salts))
          allocate (res_benthic(ires)%salt(cs_db%num_salts))
        end do
      end if
      
      !allocate (wet_om_d(mres))
      !allocate (wet_om_m(mres))
      !allocate (wet_om_y(mres))
      !allocate (wet_om_a(mres))
      
      return
      end subroutine res_allo