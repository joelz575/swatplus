      subroutine update_init

      use jrw_datalib_module, only : db_mx, cal_upd, cal_parms 
      use hru_lte_module
      use parm, only : soil, hru
      use time_module
      use climate_parms, only : pcp, tmp
      
      character(len=16) :: chg_parm, chg_typ
      character(len=1) :: cond_met
      integer :: lyr = 1
      
      do ichg_par = 1, db_mx%cal_upd
        do ispu = 1, cal_upd(ichg_par)%num_elem
          ielem = cal_upd(ichg_par)%num(ispu)
          chg_parm = cal_upd(ichg_par)%name
          chg_typ = cal_upd(ichg_par)%chg_typ
          chg_val = cal_upd(ichg_par)%val
          absmin = cal_parms(cal_upd(ichg_par)%num_db)%absmin
          absmax = cal_parms(cal_upd(ichg_par)%num_db)%absmax
          num_db = cal_upd(ichg_par)%num_db
          
          !check to see if conditions are met
          cond_met = 'y'
          do ic = 1, cal_upd(ichg_par)%conds
            select case (cal_upd(ichg_par)%cond(ic)%var)
            case ("hsg")
              if (cal_upd(ichg_par)%cond(ic)%targc /= soil(ielem)%hydgrp) then
                cond_met = 'n'
              end if
            case ("texture")
              if (cal_upd(ichg_par)%cond(ic)%targc /= soil(ielem)%texture) then
                cond_met = 'n'
              end if
            case ("plant")      !for hru-lte
              if (cal_upd(ichg_par)%cond(ic)%targc /= hlt(ielem)%plant) then 
                cond_met = 'n'
                exit
              end if
            case ("landuse")    !for hru
              if (cal_upd(ichg_par)%cond(ic)%targc /= hru(ielem)%land_use_mgt_c) then 
                cond_met = 'n'
                exit
              end if
            case ("region")     !for hru    
              if (cal_upd(ichg_par)%cond(ic)%targc /= hru(ielem)%region) then 
                cond_met = 'n'
                exit
              end if
            case ("region_lte")     !for hru    
              if (cal_upd(ichg_par)%cond(ic)%targc /= hru(ielem)%region) then 
                cond_met = 'n'
                exit
              end if
            end select
          end do

          if (cond_met == 'y') then
            if (cal_parms(num_db)%ob_typ /= 'sol' .and. cal_parms(num_db)%ob_typ /= 'cli') then
              call current_par_value (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
            end if
            select case (cal_parms(num_db)%ob_typ)
            case ("sol")
              !! check layers for soil variables
              if (cal_upd(ichg_par)%lyr1 <= 0) cal_upd(ichg_par)%lyr1 = 1
              if (cal_upd(ichg_par)%lyr2 <= 0) cal_upd(ichg_par)%lyr2 = soil(ielem)%nly
              cal_upd(ichg_par)%lyr2 = Min (cal_upd(ichg_par)%lyr2, soil(ielem)%nly)
              do lyr = cal_upd(ichg_par)%lyr1, cal_upd(ichg_par)%lyr2
                call current_par_value (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
              end do
            case ("cli")
            !! check dates for climate variable
              select case (cal_upd(ichg_par)%name)
              case ("precip")
                do ielem = 1, cal_upd(ichg_par)%num_elem
                  ipg = cal_upd(ichg_par)%num(ielem)
                  do iyear = cal_upd(ichg_par)%year1, cal_upd(ichg_par)%year2
                    iyr = iyear - time%yrc + 1
                    do iday = cal_upd(ichg_par)%day1, cal_upd(ichg_par)%day2
                      val_cur = pcp(ipg)%ts(iday,iyr)
                      pcp(ipg)%ts(iday,iyr) = chg_par (val_cur, ielem, chg_typ, chg_val, absmin, absmax, num_db)
                    end do
                  end do
                end do
              case ("temp")
                do ielem = 1, cal_upd(ichg_par)%num_elem
                  ipg = cal_upd(ichg_par)%num(ielem)
                  do iyear = cal_upd(ichg_par)%year1, cal_upd(ichg_par)%year2
                    iyr = iyear - time%yrc + 1
                    do iday = cal_upd(ichg_par)%day1, cal_upd(ichg_par)%day2
                      val_cur = tmp(ig)%ts(iday,iyr)
                      tmp(ig)%ts(iday,iyr) = chg_par (val_cur, ielem, chg_typ, chg_val, absmin, absmax, num_db)
                    end do
                  end do
                end do
              end select
            end select
          end if
        end do
      end do
      
      return
      end subroutine update_init