      subroutine pst_pesty
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates pesticide transported with suspended sediment 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    hrupest(:)    |none         |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru          |none         |HRU number
!!    npmx          |none         |number of different pesticides used in
!!                                |the simulation
!!    npno(:)       |none         |array of unique pesticides used in watershed
!!    pst_enr(:,:)  |none         |pesticide enrichment ratio
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer in HRU
!!    zdb(:,:)      |mm           |division term from net pesticide equation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pst_sed(:,:)  |kg/ha        |pesticide loading from HRU sorbed onto
!!                                |sediment
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, hrupest, sedyld, ihru, enratio, npmx
      use soil_module
      use constituent_mass_module
      
      implicit none 

      real :: conc        !              |concentration of pesticide in soil
      real :: er          !none          |enrichment ratio for pesticides
      integer :: j        !none          |HRU number
      integer :: k        !none          |counter
      integer :: kk       !none          |pesticide number from database
      real :: xx          !kg/ha         |amount of pesticide in soil
      integer :: icmd     !              | 
      
      j = 0
      j = ihru

      if (hrupest(j) == 0) return
          
      npmx = cs_db%num_pests
      do k = 1, npmx
        kk = hru(j)%pst(k)%num_db
        if (kk > 0) then
          xx = 0.
          
          if (xx >= .0001) then
            conc = 100. * soil(j)%ly(1)%kp(k) * xx / (hru(j)%pst(k)%zdb + 1.e-10)
            if (hru(j)%pst(k)%enr > 0.) then
              er = hru(j)%pst(k)%enr
            else
              er = enratio
            end if

            hru(j)%pst(k)%sed=.001* sedyld(j) *conc * er/hru(j)%area_ha
            if (hru(j)%pst(k)%sed < 0.) hru(j)%pst(k)%sed = 0.
            if (hru(j)%pst(k)%sed > xx) hru(j)%pst(k)%sed = xx
            soil(j)%ly(1)%pst(k) = xx -hru(j)%pst(k)%sed
           
          end if
        end if
      end do

      return
      end subroutine pst_pesty