      subroutine hru_soil_assign
    
      use constituent_mass_module
      use hru_module, only : hru, pesti_db, ihru, ipl, isol, mpst, npmx
      use soil_module
      use plant_module
      use bacteria_module
      use hydrograph_module
      
      implicit none
      
      integer :: mbac               !none          |ending of loop  
      integer :: ly                 !none          |counter
      integer :: ibac               !none          |counter
      integer :: mbac_db            !              |
      integer :: mpl                !none          |ending of loop
      integer :: ibacdb             !              |
      integer :: ipest              !none          |counter
      integer :: ipest_db           !              |

      !! set hru soils to appropriate database soils
      do ihru = 1, sp_ob%hru

        !! allocate bacteria
        mbac = cs_db%num_paths
        if (mbac > 0) then
          do ly = 1, sol(isol)%s%nly
            allocate (soil(ihru)%ly(ly)%bacsol(mbac))
            allocate (soil(ihru)%ly(ly)%bacsor(mbac))
          end do

        do ibac = 1, mbac
          if (ly == 1) then
            soil(ihru)%ly(1)%bacsol(ibac) = bact(mbac_db)%bac(ibac)%sol
            soil(ihru)%ly(1)%bacsor(ibac) = bact(mbac_db)%bac(ibac)%sor
          else
            soil(ihru)%ly(1)%bacsol(ibac) = 0.
            soil(ihru)%ly(1)%bacsor(ibac) = 0.
          end if
        end do
        pcom(ihru)%path(ibac) = bact(ibacdb)%bac(ibac)%plt
        end if
      
        !! allocate pesticides
        npmx = cs_db%num_pests
        allocate (hru(ihru)%pst(mpst))
        npmx = cs_db%num_pests
        if (npmx > 0) then
          do ly = 1, sol(isol)%s%nly
            allocate (soil(ihru)%ly(ly)%kp(npmx))
            allocate (soil(ihru)%ly(ly)%pst(npmx))
          end do
        end if
        
        npmx = cs_db%num_pests
        do ipest = 1, npmx
          pcom(ihru)%pest(ibac) = pesti_db(ipest_db)%pesti(ipest)%plt
          soil(ihru)%ly(1)%pst(ipest) = pesti_db(ipest_db)%pesti(ipest)%soil
        end do

      end do   !hru loop
      
      return
      end subroutine hru_soil_assign