      subroutine layersplit(dep_new)

      use parm, only : soil, ihru, isep_ly, ly1
      use organic_mineral_mass_module
      
      integer nly, nly1, lyn
      real :: dif
	  real, intent(in):: dep_new
      
	  nly = soil(j)%nly

      allocate (ly1(nly))
      do ly = 1, nly
        ly1(ly) = soil(ihru)%ly(ly)
      end do
      
      do ly = 2, nly 
        dif = Abs(dep_new - soil(ihru)%phys(ly)%d)
        !! if values are within 10 mm of one another, reset boundary
        if (dif < 10.) then
          soil(ihru)%phys(ly)%d = dep_new
          exit
        end if

        !! set a soil layer at dep_new and adjust all lower layers                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (ihru)%ly)
        deallocate (soil(ihru)%phys)
        !!!deallocate (soil(ihru)%nut)    !! nbs
        deallocate (soil(ihru)%cbn)    !! nbs
        deallocate (soil(ihru)%ly)     !! nbs
        nly1 = soil(ihru)%nly + 1                                                                                                         
        allocate (soil(ihru)%ly(nly1))
        allocate (soil(ihru)%phys(nly1))
        allocate (soil1(ihru)%tot(nly1))   !! nbs ?
        !!!allocate (soil(ihru)%nut(nly1))    !! remove after nuts are gone
        allocate (soil(ihru)%cbn(nly1))    !! nbs
        if (soil(ihru)%phys(ly)%d > dep_new) then                                                                                                     
          isep_ly = ly
          soil(ihru)%phys(ly)%d = dep_new
          do lyn = ly, nly
            soil(ihru)%ly(lyn+1) = ly1(lyn)
          end do
        end if
      end do
      
      deallocate (ly1)
	  return
      end        