      subroutine pst_pestw
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this suroutine writes summary information on pesticide fate in watershed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    nhru        |none          |number of HRUs in watershed
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,:)|kg/ha       |pesticide in soil layer
!!    wpstaao(:,1)|mg pst/ha     |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed 
!!                               |(in solution) - average annual
!!    wpstaao(:,2)|mg pst/ha     |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed
!!                               |(sorbed to sediment) -average annual
!!    wpstaao(:,3)|kg pst/ha     |amount of pesticide type leached from soil
!!                               |profile in watershed - average annual
!!    wpstaao(:,4)|kg pst/ha     |amount of pesticide type in lateral flow
!!                               |contribution to stream in watershed -
!!                               |average annual
!!
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wpstaao(:,3)|mg pst/ha     |amount of pesticide type leached from soil
!!                               |profile in watershed on day
!!    wpstaao(:,4)|mg pst/ha     |amount of pesticide type in lateral flow
!!                               |contribution to stream in watershed on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Maxval

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, hru_dafr, hrupest, ihru, iscen, nhru, npmx, wpstaao
      use soil_module
      use constituent_mass_module
      
      implicit none 

      integer :: l
      integer :: j         !none          |HRU number
      integer :: k         !none          |counter
      integer :: kk        !none          |pesticide number from pest.dat
      real :: pfp          !mg/ha         |pesticide on plants at end of simulation
      real :: pfg          !mg/ha         |pesticide in soil at end of simulation
      integer :: icmd      !              | 
          
      j = ihru
      npmx = cs_db%num_pests
      do k = 1, npmx
        kk = hru(j)%pst(k)%num_db

        !! change units from kg/ha to mg/ha
        wpstaao(k,3) = wpstaao(k,3) * 1.e6
        wpstaao(k,4) = wpstaao(k,4) * 1.e6

        !!calculate pesticide levels on plants/in soil at end of simulation
        pfp = 0.
        pfg = 0.
        do j = 1, nhru
          if (hrupest(j) == 1) then
            pfp = pfp + hru(j)%pst(k)%plt * hru_dafr(j)
            do l = 1, soil(j)%nly
              pfg = pfg +  soil(j)%ly(l)%pst(k) * hru_dafr(j)
             
            end do
          end if
        end do
        !! change units from kg/ha to mg/ha
        pfp = pfp * 1.e6
        pfg = pfg * 1.e6
!        if (iscen == 1) then
        write (26,5100) pfp, pfg
 !       else if (isproj == 1) then
 !       write (19,5100) pfp, pfg
 !       endif
      end do

      return
 5000 format (/,"AVERAGE ANNUAL PESTICIDE SUMMARY DATA, PESTICIDE #",i3,  &
          ": ",a16,/,t20,"APPLIED = ",f15.4," mg/ha",/,t20,"DECAYED = ",  &
          f15.4," mg/ha",/,t20,                                           &
          "IN SURFACE RUNOFF ENTERING STREAM (DISSOLVED) = ",f15.4,       &
          " mg/ha",/,t20,"IN SURFACE RUNOFF ENTERING STREAM (SORBED) = "  &
          ,f15.4," mg/ha",/,t20, "LEACHED OUT OF SOIL PROFILE = ",f15.4,  &
          " mg/ha",/,t20,"IN LATERAL FLOW ENTERING STREAM = ",f15.4,      &
          " mg/ha")
 5100 format (/,t20,"FINAL AMOUNT OF PESTICIDE ON PLANT = ",f15.4,        &
          " mg/ha",/,t20,"FINAL AMOUNT OF PESTICIDE IN GROUND = ",f15.4,  &
          " mg/ha")
 5200 format (//,"PESTICIDE RISK BY ROUTING REACH",/,"REACH",t12,         &
          "4 DAY",t30,"21 DAY",t49,"60 DAY",t69,"90 DAY",/,t6,            &
          "DAY YEAR CON(PPB)",t24,"DAY YEAR CON(PPB)",t42,                &
          "DAY YEAR CON(PPB)",t60,"DAY YEAR CON(PPB)",t80,"% FLOW")       
 5300 format (i4,4(2i4,e10.3),f8.2)
      end subroutine pst_pestw