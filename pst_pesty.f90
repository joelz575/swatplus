      subroutine pst_pesty(iwave)
      
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
!!    iwave         |none         |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    npmx          |none         |number of different pesticides used in
!!                                |the simulation
!!    npno(:)       |none         |array of unique pesticides used in watershed
!!    pst_enr(:,:)  |none         |pesticide enrichment ratio
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer in HRU
!!    sub_pst(:,:)  |kg/ha        |amount of pesticide in layer in subbasin
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
!!    conc        |              |concentration of pesticide in soil
!!    er          |none          |enrichment ratio for pesticides
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from database
!!    xx          |kg/ha         |amount of pesticide in soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      integer, intent (in) :: iwave
      integer :: j, k, kk
      real :: xx, conc, er

      j = 0
      j = ihru

      if (hrupest(j) == 0) return
          
      npmx = obcs(icmd)%num_pests
      do k = 1, npmx
        kk = hru(j)%pst(k)%num_db
        if (kk > 0) then
          xx = 0.
          if (iwave <= 0) then
            xx = soil(j)%ly(1)%pst(k)
          else
            xx = sub_pst(kk,iwave)
          end if

          if (xx >= .0001) then
            conc = 0.
            er = 0.
!!            conc = 100. * sol_kp(k,j,1) * xx / (zdb(k,j)+1.e-10)
            conc = 100. * soil(j)%ly(1)%kp(k) * xx /                     &  
                                       (hru(j)%pst(k)%zdb + 1.e-10)
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