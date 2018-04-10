      subroutine pst_washp

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of pesticide washed off the plant
!!    foliage and onto the soil

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    ihru          |none          |HRU number
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    pst_wof(:)    |none          |fraction of pesticide on foliage which
!!                                 |is washed-off by a rainfall event
!!    sol_pst(:,:,1)|kg/ha         |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha         |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from pest.dat
!!    xx          |kg/ha         |amount of pesticide washed off foliage
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use pesticide_data_module
      use hru_module, only : hru, soil, hrupest, ihru, npmx
      use constituent_mass_module
      
      integer :: j, k, kk
      real :: xx

      j = ihru

      if (hrupest(j) == 0) return
 
      npmx = cs_db%num_pests
      do k = 1, npmx
        kk = hru(j)%pst(k)%num_db
        if (hru(j)%pst(k)%plt >= 0.0001) then
          if (kk > 0) then
            xx = 0.
            xx = pestdb(kk)%pst_wof * hru(j)%pst(k)%plt
            if (xx > hru(j)%pst(k)%plt) xx = hru(j)%pst(k)%plt
            soil(j)%ly(1)%pst(k) = soil(j)%ly(1)%pst(k) + xx
            hru(j)%pst(k)%plt = hru(j)%pst(k)%plt - xx
          end if
        end if
      end do

      return
      end subroutine pst_washp