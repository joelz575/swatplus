      subroutine hrudb_init
    
      use hydrograph_module, only : sp_ob, sp_ob1, ob
      use hru_module, only : hru, hru_db

      integer :: eof

      !!Section i
      !!assign database pointers for the hru
      imp = 0
      do ihru = 1, sp_ob%hru
        iob = sp_ob1%hru + ihru - 1
        ihru_db = ob(iob)%props    !points to hru.dat
        hru(ihru)%dbs = hru_db(ihru_db)%dbs
        hru(ihru)%dbsc = hru_db(ihru_db)%dbsc
        hru(ihru)%parms = hru_db(ihru_db)%parms
      end do
      !! use the same res object for resrvoirs and landscape storage
      !! allocate res and other types later in res_init

      return
      end subroutine hrudb_init