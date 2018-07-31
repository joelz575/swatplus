      subroutine pesticide_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: soil_chem, soil_phys, rteinit, h2omgt_init, hydro_init,

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

        use hru_module, only : hru, sol_cov
        use soil_module
        use constituent_mass_module
        use output_ls_constituent_module
        use hydrograph_module, only : sp_ob, icmd
      
        implicit none 
        
        integer :: ihru            !none          !counter       
        integer :: npmx            !none          |total number of pesticides modeled in
                                   !              |in watershed plus 1
        integer :: mpst            !none          |max number of pesticides used in wshed       
        integer :: ly              !none          |counter
        integer :: ipest           !none          |counter
        integer :: ipest_db        !              | 
        
        !! allocate pesticides
        do ihru = 1, sp_ob%hru
          npmx = cs_db%num_pests
          if (npmx > 0) then
            allocate (hru(ihru)%pst(mpst))
            do ly = 1, soil(ihru)%nly
              allocate (soil(ihru)%ly(ly)%kp(npmx))
              allocate (soil(ihru)%ly(ly)%pst(npmx))
            end do
          end if

        npmx = cs_db%num_pests
        do ipest = 1, npmx
         hpest_bal(ihru)%pest(ipest)%plant = pesti_db(ipest_db)%pesti(ipest)%plt
         soil(ihru)%ly(1)%pst(ipest) = pesti_db(ipest_db)%pesti(ipest)%soil
        end do
        
        !!  topohyd defaults
        !hru(ihru)%topo%lat_len = 50.
        sol_cov(ihru) = soil(ihru)%ly(1)%rsd
        
      end do    !hru loop
                                   
      return
      end subroutine pesticide_init