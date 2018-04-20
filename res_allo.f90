      subroutine res_allo
      
      use reservoir_module
      use hydrograph_module
      
      implicit none     

      integer :: mres           !             |
      
      mres = sp_ob%res
      allocate (res(0:mres))
      allocate (res_ob(0:mres))
      allocate (res_d(mres))
      allocate (res_m(mres))
      allocate (res_y(mres))
      allocate (res_a(mres))

      return
      end subroutine res_allo