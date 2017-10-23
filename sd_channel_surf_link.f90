      subroutine sd_channel_surf_link (isdc, ics)
                 
      use hydrograph_module
      use sd_channel_module
      use ru_module
      use parm, only : hru, ihru 
      
      character (len=3) :: iobtyp
      integer :: isdc, ics, imp, mres
      
        imp = mres
        ii = 0
        ch_sur(ics)%dep(ii) = sd_chd(isdc)%chd
        ch_sur(ics)%wid(ii) = sd_chd(isdc)%chw
        ch_sur(ics)%flood_volmx(ii) = sd_chd(isdc)%chw *                  & 
                             sd_chd(isdc)%chd * sd_chd(isdc)%chl * 1000.
        do ii = 1, ch_sur(ics)%num
          iobtyp = ch_sur(ics)%obtyp(ii)     !object type
          select case (iobtyp)
          case ("hru")   !hru
            ob(i)%obj_out(ii) = sp_ob1%hru + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            ob(iob)%flood_ch_lnk = ics   !pointer back to channel-hru link
            ob(iob)%flood_ch_elem = ii   !pointer to landscape element - 1 nearest to channel
            
            ihru = ch_sur(ics)%obtypno(ii)

            !set depth, width, flood volume max
            ch_sur(ics)%dep(ii) = ch_sur(ics)%dep(ii-1) +                 &
                              hru(ihru)%field%wid * hru(ihru)%topo%slope
            ch_sur(ics)%wid(ii) = ch_sur(ics)%wid(ii-1) +                 &
                                                2. * hru(ihru)%field%wid
            ch_sur(ics)%flood_volmx(ii)= ch_sur(ics)%flood_volmx(ii-1) +  &
              (ch_sur(ics)%wid(ii-1) * (ch_sur(ics)%dep(ii) -             & 
              ch_sur(ics)%dep(ii-1)) + (2. * ch_sur(ics)%wid(ii) ** 2 *   &
              hru(ihru)%topo%slope)) * sd_chd(ics)%chl * 1000.
          case ("hlt")   !hru_lte
            !
          case ("sub")   !subbasin
            ob(i)%obj_out(ii) = sp_ob1%sub + ob(i)%obtypno_out(ii) - 1
            iob = ob(i)%obj_out(ii)
            
            isub = ch_sur(ics)%obtypno(ii)

            ith = ru(isub)%dbs%toposub_db
            ifld = ru(isub)%dbs%field_db
            !set depth, width, flood volume max
            ch_sur(ics)%dep(ii) = ch_sur(ics)%dep(ii-1) + field_db(ifld)%wid * topo_db(ith)%slope
            ch_sur(ics)%wid(ii) = ch_sur(ics)%wid(ii-1) + 2. * field_db(ifld)%length
            ch_sur(ics)%flood_volmx(ii)= ch_sur(ics)%flood_volmx(ii-1) +        &
                    (ch_sur(ics)%wid(ii-1) * (ch_sur(ics)%dep(ii) -             & 
                    ch_sur(ics)%dep(ii-1)) + (2. * ch_sur(ics)%wid(ii) ** 2 *   &
                    topo_db(ith)%slope)) * sd_chd(ics)%chl * 1000.
            
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
        
        mres = imp   
        return

      end subroutine sd_channel_surf_link