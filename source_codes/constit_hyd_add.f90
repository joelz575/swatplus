      subroutine constit_hyd_add
 
      use constituent_mass_module

      do ipest = 1, cs_db%num_pests
        hcs2%pest(ipest)%sol =  hcs2%pest(ipest)%sol + hcs1%pest(ipest)%sol
        hcs2%pest(ipest)%sor =  hcs2%pest(ipest)%sor + hcs1%pest(ipest)%sol
      end do
      
      do ipath = 1, cs_db%num_paths
        hcs2%path(ipath)%sol =  hcs2%path(ipath)%sol + hcs1%path(ipath)%sol
        hcs2%path(ipath)%sor =  hcs2%path(ipath)%sor + hcs1%path(ipath)%sol
      end do
      
      do ihmet = 1, cs_db%num_metals
        hcs2%hmet(ihmet)%sol =  hcs2%hmet(ihmet)%sol + hcs1%hmet(ihmet)%sol
        hcs2%hmet(ihmet)%sor =  hcs2%hmet(ihmet)%sor + hcs1%hmet(ihmet)%sol
      end do
      
      do isalt = 1, cs_db%num_salts
        hcs2%salt(isalt)%sol =  hcs2%salt(isalt)%sol + hcs1%salt(isalt)%sol
        hcs2%salt(isalt)%sor =  hcs2%salt(isalt)%sor + hcs1%salt(isalt)%sol
      end do
      
      return
      end subroutine constit_hyd_add