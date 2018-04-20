      subroutine constit_hyd_mult (iob, idr)
 
      use constituent_mass_module
      use dr_module
      
      integer, intent (in)  :: idr
      integer, intent (in)  :: iob
      
      idr_pest = dr_pest_num(idr)
      do ipest = 1, cs_db%num_pests
        obcs(iob)%hd(1)%pest(ipest)%sol =  obcs(iob)%hin%pest(ipest)%sol * dr_pest(idr_pest)%pest(ipest)%sol
        obcs(iob)%hd(1)%pest(ipest)%sor =  obcs(iob)%hin%pest(ipest)%sor * dr_pest(idr_pest)%pest(ipest)%sol
      end do
      
      idr_path = dr_path_num(idr)
      do ipath = 1, cs_db%num_paths
        obcs(iob)%hd(1)%path(ipath)%sol =  obcs(iob)%hin%path(ipath)%sol * dr_path(idr_path)%path(ipath)%sol
        obcs(iob)%hd(1)%path(ipath)%sor =  obcs(iob)%hin%path(ipath)%sor * dr_path(idr_path)%path(ipath)%sol
      end do
      
      idr_hmet = dr_hmet_num(idr)
      do ihmet = 1, cs_db%num_metals
        obcs(iob)%hd(1)%hmet(ihmet)%sol =  obcs(iob)%hin%hmet(ihmet)%sol * dr_hmet(idr_hmet)%hmet(ihmet)%sol
        obcs(iob)%hd(1)%hmet(ihmet)%sor =  obcs(iob)%hin%hmet(ihmet)%sor * dr_hmet(idr_hmet)%hmet(ihmet)%sol
      end do
      
      idr_salt = dr_salt_num(idr)
      do isalt = 1, cs_db%num_salts
        obcs(iob)%hd(1)%salt(isalt)%sol =  obcs(iob)%hin%salt(isalt)%sol * dr_salt(idr_salt)%salt(isalt)%sol
        obcs(iob)%hd(1)%salt(isalt)%sor =  obcs(iob)%hin%salt(isalt)%sor * dr_salt(idr_salt)%salt(isalt)%sol
      end do
      
      return
      end subroutine constit_hyd_mult