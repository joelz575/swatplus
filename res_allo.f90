      subroutine res_allo (mres)
      
      use reservoir_module

      allocate (res(0:mres))
      allocate (res_ob(0:mres))
      allocate (res_d(mres))
      allocate (res_m(mres))
      allocate (res_y(mres))
      allocate (res_a(mres))

      return
      end subroutine res_allo