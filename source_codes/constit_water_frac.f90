      subroutine constit_water_frac (iob, ihyd, frac)

      use constituent_mass_module
      
      integer, intent (in)  :: ihyd
      integer, intent (in)  :: iob
      real, intent (in) :: frac
     
      do ipest = 1, cs_db%num_pests
        ch_water(jrch)%pest(ipest)%sol =  frac * obcs(iob)%hd(ihyd)%pest(ipest)%sol
        hcs1%pest(ipest)%sor =  frac * obcs(iob)%hd(ihyd)%pest(ipest)%sor
      end do
      
      do ipath = 1, cs_db%num_paths
        hcs1%path(ipath)%sol =  frac * obcs(iob)%hd(ihyd)%path(ipath)%sol
        hcs1%path(ipath)%sor =  frac * obcs(iob)%hd(ihyd)%path(ipath)%sor
      end do
      
      do ihmet = 1, cs_db%num_metals
        hcs1%hmet(ihmet)%sol =  frac * obcs(iob)%hd(ihyd)%hmet(ihmet)%sol
        hcs1%hmet(ihmet)%sor =  frac * obcs(iob)%hd(ihyd)%hmet(ihmet)%sor
      end do
      
      do isalt = 1, cs_db%num_salts
        hcs1%salt(isalt)%sol =  frac * obcs(iob)%hd(ihyd)%salt(isalt)%sol
        hcs1%salt(isalt)%sor =  frac * obcs(iob)%hd(ihyd)%salt(isalt)%sor
      end do
      
      return
      end subroutine constit_water_frac