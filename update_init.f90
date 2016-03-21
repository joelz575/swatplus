      subroutine update_init

      use jrw_datalib_module
      use hru_module
      
      character(len=16):: chg_parm, chg_typ

      do ichg_par = 1, db_mx%cal_upd
        do ispu = 1, cal_upd(ichg_par)%num_tot
          ielem = cal_upd(ichg_par)%num(ispu)
          chg_parm = cal_upd(ichg_par)%name
          chg_typ = cal_upd(ichg_par)%chg_typ
          chg_val = cal_upd(ichg_par)%val
          absmin = cal_parms(cal_upd(ichg_par)%num_db)%absmin
          absmax = cal_parms(cal_upd(ichg_par)%num_db)%absmax
          num_db = cal_upd(ichg_par)%num_db
          select case (cal_parms(cal_upd(ichg_par)%num_db)%ob_typ)
          case ('hru')
            mx_elem = sp_ob%hru
          case ('hru_lte')
            mx_elem = sp_ob%hru_lte
          case ('sub')
            mx_elem = sp_ob%sub
          case ('aqu')
            mx_elem = sp_ob%aqu
          case ('chan')
            mx_elem = sp_ob%chan
          case ('res')
            mx_elem = sp_ob%res
          case ('chandeg')
            mx_elem = sp_ob%chandeg
          end select
          if (ielem <= mx_elem) then
            call current_par_value (ielem, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
          end if
        end do
      end do
      return
      end subroutine update_init