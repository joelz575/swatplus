      subroutine bacteria_init

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

      use hru_module, only : hru, hru_db, ihru, ipl, isol, mlyr, mpst, npmx, &
         sol_cov, wfsh
      use soil_module
      use plant_module
      use bacteria_module
      use channel_module
      use basin_module
      use conditional_module
      use organic_mineral_mass_module
      use hydrograph_module, only : sp_ob, icmd
      use constituent_mass_module
      
      implicit none

      integer :: eof                   !          |end of file
      character (len=80) :: titldum    !          |title of file
      integer :: mbac                  !          |
      integer :: ly                    !none      |counter
      integer :: ibac                  !none      |counter
      integer :: ibac_db               !          |
      integer :: j                     !          |
      
     
      do ihru = 1, sp_ob%hru

      !! read initial bacteria data
      !! compute sequential basin number for routing
      !!this needs to be cleaned up and look like pesticides!!
      !mbac_db = bsn%num_bac
      !do mbac = 1, mbac_db
      !  do ibac = 1, bsn%num_bac
      !    if (bsn%bac(ibac) == bact(mbac)%bac(ibac)%num_db) then
      !      bact(mbac)%bac(ibac)%num_bsn = ibac
      !    end if
      !  end do
      !end do          
    
            !do ihru = 1, mhru
              !!Convert QSTE from volume to depth unit, mm
              !isep = hru(ihru)%dbs%septic
              ! if (sep(isep)%area > 1.e-6) then
              ! qstemm(ihru) = sepdb(sep(isep)%typ)%qs * sep(isep)%cap /
    !&	                        sep(isep)%area * 1000.
              ! end if
           ! end do
        
        !! allocate bacteria
        icmd = hru(ihru)%obj_no
        mbac = cs_db%num_paths
        if (mbac > 0) then
          !! allocate bacteria associated with
          do ly = 1, soil(ihru)%nly
            allocate (soil(ihru)%ly(ly)%bacsol(mbac))
            allocate (soil(ihru)%ly(ly)%bacsor(mbac))
          end do
          do ibac = 1, mbac
            if (ly == 1) then
              soil(ihru)%ly(1)%bacsol(ibac) = bact(ibac_db)%bac(ibac)%sol
              soil(ihru)%ly(1)%bacsor(ibac) = bact(ibac_db)%bac(ibac)%sor
            else
              soil(ihru)%ly(1)%bacsol(ibac) = 0.
              soil(ihru)%ly(1)%bacsor(ibac) = 0.
            end if
          end do   
          !! allocate bacteria associated with plant
          mbac = cs_db%num_paths
          if (mbac > 0) then
            do ipl = 1, pcom(j)%npl
              allocate (pcom(ihru)%plg(ipl)%bac(mbac))
            end do
          end if
        end if
        end do

      return
      end subroutine bacteria_init