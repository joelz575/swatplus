      subroutine read_mgtops(isched)
      
      use jrw_datalib_module
      use parm
      use time_module
      
      integer :: iyear, day, mon

      iyear = 1
      do iop = 1, sched(isched)%num_ops             !! operation loop
        read (107,*)   sched(isched)%mgt_ops(iop)%op,                    & !! operation character
                       sched(isched)%mgt_ops(iop)%mon,                   &
                       sched(isched)%mgt_ops(iop)%day,                   &
                       sched(isched)%mgt_ops(iop)%husc,                  &
                       sched(isched)%mgt_ops(iop)%cond,                  &
                       sched(isched)%mgt_ops(iop)%op_char,               & !! operation type character
                       sched(isched)%mgt_ops(iop)%op_plant,              & !! plant character 
                       sched(isched)%mgt_ops(iop)%op3                      !! override
        
          day = sched(isched)%mgt_ops(iop)%day
          mon = sched(isched)%mgt_ops(iop)%mon
          sched(isched)%mgt_ops(iop)%jday = Jdt(ndays,day,mon)
          sched(isched)%mgt_ops(iop)%year = iyear
          if (sched(isched)%mgt_ops(iop)%op == "skip") iyear = iyear + 1
 
      select case(sched(isched)%mgt_ops(iop)%op)
          
        case ("pcom")
          do idb = 1, db_mx%plantcom
              if (sched(isched)%mgt_ops(iop)%op_char == pcom_xw(idb)%name) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
        case ("plnt")
          do idb = 1, db_mx%plantparm
              if (sched(isched)%mgt_ops(iop)%op_char == plnt_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
        case ("harv")
          do idb = 1, db_mx%harvop_db
              if (sched(isched)%mgt_ops(iop)%op_char == harv_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
  

        case ("hvkl")
          do idb = 1, db_mx%harvop_db
              if (sched(isched)%mgt_ops(iop)%op_char == harv_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
        case ("till")
          do idb = 1, db_mx%tillparm
              if (sched(isched)%mgt_ops(iop)%op_char == till_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
        case ("irrm")
          sched(isched)%irr = 1
          do idb = 1, db_mx%irrop_db
              if (sched(isched)%mgt_ops(iop)%op_char == irrm_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do

          case ("fert")
          do idb = 1, db_mx%fertparm
              if (sched(isched)%mgt_ops(iop)%op_char == fertdb(idb)%fertnm) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do

          case ("pest")
          do idb = 1, db_mx%pstop_db
              if (sched(isched)%mgt_ops(iop)%op_char == pestop_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do

          case ("graz")
          do idb = 1, db_mx%grazeop_db
              if (sched(isched)%mgt_ops(iop)%op_char == graz_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
 
          case ("burn")
          do idb = 1, db_mx%fireop_db
              if (sched(isched)%mgt_ops(iop)%op_char == burn_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do
          
          case ("swep")
          do idb = 1, db_mx%sweepop_db
              if (sched(isched)%mgt_ops(iop)%op_char == swep_xw(idb)) then
                  sched(isched)%mgt_ops(iop)%op1 = idb
                  exit
              endif
          end do      
       end select          
      end do                                  !! operation loop
    
      return
      end