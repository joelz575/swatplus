      subroutine res_allo (mres)
      
      use reservoir_module

      allocate (res(0:mres))
      allocate (res_ob(0:mres))
      allocate (resd(mres))
      allocate (resm(mres))
      allocate (resy(mres))
      allocate (resa(mres))

      return
      end subroutine res_allo