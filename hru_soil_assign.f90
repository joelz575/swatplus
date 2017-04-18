      subroutine hru_soil_assign
    
      use constituent_mass_module

      !! set hru soils to appropriate database soils
      do ihru = 1, mhru

        !! allocate bacteria
        mbac = obcs(icmd)%num_paths
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
        do ipl = 1, mpl
          pcom(ihru)%plg(ipl)%bac(ibac) = bact(ibacdb)%bac(ibac)%plt
        end do
        end if
      
        !! allocate pesticides
        npmx = obcs(icmd)%num_pests
        allocate (hru(ihru)%pst(mpst))
        npmx = obcs(icmd)%num_pests
        if (npmx > 0) then
          do ly = 1, sol(isol)%s%nly
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

      end do   !hru loop
      
      return
      end subroutine hru_soil_assign