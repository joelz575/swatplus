      subroutine pesticide_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    npmx          |none          |total number of pesticides modeled in
!!                                 |in watershed plus 1
!!    nope(:)       |none          |sequence number of pesticide in NPNO(:)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i             |none          |number of specific reservoir or HRU
!!    ndays(:)      |julian date   |julian date for last day of preceding 
!!                                 |month (where the array location is the 
!!                                 |number of the month) The dates are for
!!                                 |leap years
!!    npmx          |none          |total number of pesticides modeled in
!!                                 |watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: soil_chem, soil_phys, rteinit, h2omgt_init, hydro_init,

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, only : soil, hru, pesti_db, sol_cov
      use constituent_mass_module
      use hydrograph_module, only : sp_ob, icmd
        
        !! allocate pesticides
        do ihru = 1, sp_ob%hru
          npmx = obcs(icmd)%num_pests
          if (npmx > 0) then
            allocate (hru(ihru)%pst(mpst))
            do ly = 1, soil(ihru)%nly
              allocate (soil(ihru)%ly(ly)%kp(npmx))
              allocate (soil(ihru)%ly(ly)%pst(npmx))
            end do
          end if

        npmx = obcs(icmd)%num_pests
        do ipest = 1, npmx
         hru(ihru)%pst(ipest)%num_db = pesti_db(ipest_db)%pesti(ipest)%num_db
         hru(ihru)%pst(ipest)%plt = pesti_db(ipest_db)%pesti(ipest)%plt
         soil(ihru)%ly(1)%pst(ipest) = pesti_db(ipest_db)%pesti(ipest)%soil
         hru(ihru)%pst(ipest)%enr = pesti_db(ipest_db)%pesti(ipest)%enr
        end do
        
        !!  topohyd defaults
        !hru(ihru)%topo%lat_len = 50.
        sol_cov(ihru) = soil(ihru)%ly(1)%rsd
        
      end do    !hru loop
                                   
      return
      end subroutine pesticide_init