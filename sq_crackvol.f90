      subroutine sq_crackvol
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine computes total crack volume for the soil profile and 
!!    modifies surface runoff to account for crack flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    volcrmin    |mm            |minimum crack volume allowed in any soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    crlag       |none          |lag factor for day
!!    crlagdry    |none          |lag in crack development when soil is dry
!!    crlagwet    |none          |lag in crack development when soil is wet
!!    j           |none          |HRU number
!!    l           |none          |counter
!!    volcrnew    |mm            |crack volume for soil layer based on new 
!!                               |moisture conditions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

 
      integer :: l, j
      real :: volcrnew, crlag, crlagdry = .99, crlagwet = 0.

      j = 0
      j = ihru
      voltot = 0.

      !! calculate volume of cracks in soil
      do l = 1, soil(j)%nly
        volcrnew = 0.
        crlag = 0.
        volcrnew = soil(j)%phys(l)%crdep * (soil(j)%phys(l)%fc -      &
                   soil(j)%phys(l)%st) / (soil(j)%phys(l)%fc)
        if (soil(j)%sw < .90 * soil(j)%sumfc) then
          if (volcrnew > soil(j)%ly(l)%volcr) then
            crlag = crlagdry
          else
            crlag = crlagwet
          end if
        else
          crlag = crlagwet
        end if
        soil(j)%ly(l)%volcr = crlag*soil(j)%ly(l)%volcr + (1. - crlag) *   &
                  volcrnew
        if (soil(j)%ly(l)%volcr < 0.) soil(j)%ly(l)%volcr = 0.
        voltot = voltot + soil(j)%ly(l)%volcr + volcrmin
      end do

      return
      end subroutine sq_crackvol