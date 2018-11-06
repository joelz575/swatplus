      subroutine constit_hyd_frac

      use constituent_mass_module

      do ipest = 1, cs_db%num_pests
        hcs1%pest(ipest)%sol =  frac * hcs2%pest(ipest)%sol
        hcs1%pest(ipest)%sor =  frac * hcs2%pest(ipest)%sor
      end do
      
      do ipath = 1, cs_db%num_paths
        hcs1%path(ipath)%sol =  frac * hcs2%path(ipath)%sol
        hcs1%path(ipath)%sor =  frac * hcs2%path(ipath)%sor
      end do
      
      do ihmet = 1, cs_db%num_metals
        hcs1%hmet(ihmet)%sol =  frac * hcs2%hmet(ihmet)%sol
        hcs1%hmet(ihmet)%sor =  frac * hcs2%hmet(ihmet)%sor
      end do
      
      do isalt = 1, cs_db%num_salts
        hcs1%salt(isalt)%sol =  frac * hcs2%salt(isalt)%sol
        hcs1%salt(isalt)%sor =  frac * hcs2%salt(isalt)%sor
      end do
      
      return
      end subroutine constit_hyd_frac