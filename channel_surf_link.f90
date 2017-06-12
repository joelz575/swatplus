      subroutine channel_surf_link (isdc, ics)
                 
      use hydrograph_module
      use channel_module
      use ru_module
      use jrw_datalib_module
      
      character (len=3) :: iobtyp
      integer :: isdc, ics

      tot_ha = 0
      
      do ics = 1, db_mx%ch_surf
        
        do ii = 1, ch_sur(ics)%num
          iobtyp = ch_sur(ics)%obtyp(ii)     !object type
          select case (iobtyp)
          case ("hru")   !hru
            ob(i)%obj_out(ii) = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            ob(iob)%flood_ch_lnk = ics   !pointer back to channel-hru link
            ob(iob)%flood_ch_elem = ii   !pointer to landscape element - 1 nearest to channel
            
            ihru = ch_sur(ics)%obtypno(ii)
            tot_ha = tot_ha + ob(iob)%area_ha
            
          case ("hlt")   !hru_lte
            ob(i)%obj_out(ii) = sp_ob1%hru_lte + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            ob(iob)%flood_ch_lnk = ics   !pointer back to channel-hru link
            ob(iob)%flood_ch_elem = ii   !pointer to landscape element - 1 nearest to channel
            
            ihru = ch_sur(ics)%obtypno(ii)
            tot_ha = tot_ha + ob(iob)%area_ha
            
          case ("sub")   !subbasin
            ob(i)%obj_out(ii) = sp_ob1%sub + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            isub = ch_sur(ics)%obtypno(ii)

            !set flood plain link and landscape element (1==closest to river)
            do ihru = 1, ru_def(isub)%num_tot
              iob = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
              ob(iob)%flood_ch_lnk = ics   !pointer back to channel-sub link
              ob(iob)%flood_ch_elem = ii   !pointer to landscape element - 1 nearest to channel
            end do
            
          case ("cha")   !channel
            !
          case ("sdc")   !swat-deg channel
            !
          end select
        end do

        !! loop again to get fractions
        do ii = 1, ch_sur(ics)%num
          iobtyp = ch_sur(ics)%obtyp(ii)     !object type
          select case (iobtyp)
          case ("hru")   !hru
            ob(i)%obj_out(ii) = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            ob(iob)%flood_frac = ob(iob)%area_ha / tot_ha

          case ("hlt")   !hru_lte
            ob(i)%obj_out(ii) = sp_ob1%hru_lte + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            ob(iob)%flood_frac = ob(iob)%area_ha / tot_ha

          case ("sub")   !subbasin
            ob(i)%obj_out(ii) = sp_ob1%sub + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            isub = ch_sur(ics)%obtypno(ii)
            ith = ru(isub)%dbs%toposub_db
            ifld = ru(isub)%dbs%field_db

            !set flood plain link and landscape element (1==closest to river)
            do iele = 1, ru_def(isub)%num_tot
              iob = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
              ob(iob)%flood_frac = (ru_elem(iele)%frac * ob(iob)%area_ha) / tot_ha
            end do
            
          case ("cha")   !channel
            !
          case ("sdc")   !swat-deg channel
            !
          end select
        end do
        
      end do    ! ics = 1, db_mx%ch_surf
        
        return

      end subroutine channel_surf_link