      subroutine res_objects

      use reservoir_module
      use hydrograph_module
      
      use parm
    
      !! set reservoir object numbers for reservoir objects
      iob1 = sp_ob1%res
      iob2 = sp_ob1%res + sp_ob%res - 1
      ires = 0
      do i = iob1, iob2
        ires = ires + 1
        res_ob(ires)%ob = i
        res_ob(ires)%typ = "res"
        res_ob(ires)%props = ob(i)%props
      end do
        
      !! set reservoir object numbers for hru's with surface storage
      do ihru = 1, mhru
        if (hru(ihru)%dbs%surf_stor > 0) then
          ires = ires + 1
          hru(ihru)%res = ires
          res_ob(ires)%ob = ihru
          res_ob(ires)%typ = "hru"
          res_ob(ires)%props = hru(ihru)%dbs%surf_stor
        else
          iob = sp_ob1%hru + ihru - 1
          if (ob(iob)%flood_ch_lnk > 0) then
            ires = ires + 1
            hru(ihru)%res = ires
            res_ob(ires)%ob = ihru
            res_ob(ires)%typ = "fpl"
            res_ob(ires)%props = 0
          end if
        end if
      end do
      return
      end subroutine res_objects