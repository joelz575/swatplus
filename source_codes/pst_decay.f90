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
!!    ihru          |none          |HRU number
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
      use hru_module, only : hru, ihru
      use constituent_mass_module
      use soil_module
      use plant_module
      use output_ls_pesticide_module
      
      implicit none 
      
      integer :: j               !none     |hru number
      integer :: k               !none     |seqential pesticide number being simulated
      integer :: ipest_db        !none     |pesticide number from pesticide data base
      integer :: l               !none     |layer number 
      real :: pest_init          !kg/ha    |amount of pesticide present at beginning of day
      real :: pest_end           !kg/ha    |amount of pesticide present at end of day
      real :: pest_decay         !kg/ha    |amount of pesticide decay during day

      j = ihru

      if (cs_db%num_pests == 0) return

      do k = 1, cs_db%num_pests
        ipest_db = cs_db%pest_num(k)
        if (ipest_db > 0) then
          pest_decay = 0.
          !! calculate degradation in soil
          do l = 1, soil(j)%nly
            pest_init = soil(j)%ly(l)%pst(k)
            if (pest_init >= 0.0001) then
              pest_end = pest_init * pstcp(ipest_db)%decay_s
              soil(j)%ly(l)%pst(k) = pest_end
              pest_decay = pest_decay + (pest_init - pest_end)
            end if
          end do

          !! calculate degradation off plant foliage
          pest_init = pcom(j)%pest(k)
          if (pest_init >= 0.0001) then
            pest_end = pest_init * pstcp(ipest_db)%decay_f
            pcom(j)%pest(k) = pest_end
            pest_decay = pest_decay + (pest_init - pest_end)
          end if
        end if
      end do

      
      
      return
      end subroutine pst_decay