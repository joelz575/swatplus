      subroutine pst_decay
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates degradation of pesticide in the soil and on 
!!    the plants

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    decay_f(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide on foliage
!!    decay_s(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide in soil
!!    hru_dafr(:)   |none          |fraction of watershed area in HRU
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    ihru          |none          |HRU number
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,:)|kg/ha         |pesticide in soil layer
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,:)|kg/ha         |pesticide in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use pesticide_data_module
      use hru_module, only : hru, hrupest, ihru, npmx
      use constituent_mass_module
      use soil_module
      
      implicit none 
      
      integer :: j               !none     |counter
      integer :: k               !         |
      integer :: kk              !         |  
      integer :: l               !none     |counter 
      real :: x1                 !kg/ha    |amount of pesticide present at beginning of 
                                 !         |day
      real :: xx                 !kg/ha    |amount of pesticide present at end of day
      integer :: icmd            !         |  
   
      j = 0
      j = ihru

      if (hrupest(j) == 0) return

      npmx = cs_db%num_pests
      do k = 1, npmx
        kk = hru(j)%pst(k)%num_db
        if (kk > 0) then

          !! calculate degradation in soil
          do l = 1, soil(j)%nly
            x1 = 0.
            x1 = soil(j)%ly(l)%pst(k)
            
            if (x1 >= 0.0001) then
              xx = 0.
              xx = x1 * pstcp(kk)%decay_s
              soil(j)%ly(l)%pst(k) = xx
              
            end if
          end do

          !! calculate degradation off plant foliage
          x1 = 0.
          x1 = hru(j)%pst(k)%plt
          if (x1 >= 0.0001) then
            xx = 0.
            xx = x1 * pstcp(kk)%decay_f
            hru(j)%pst(k)%plt = xx
          end if
        end if
      end do

      return
      end subroutine pst_decay