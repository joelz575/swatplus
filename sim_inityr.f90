      subroutine sim_inityr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables at the beginning of the year

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year in simulation (sequence)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anano3(:)   |kg N/ha       |total amount of NO3 applied during the year
!!                               |in auto-fertilization
!!    lai_yrmx(:) |none          |maximum leaf area index for the year in the 
!!                               |HRU
!!    ncf(:)      |none          |sequence number of continuous fertilization 
!!                               |operation within the year
!!    nfert(:)    |none          |sequence number of fertilizer application 
!!                               |within the year
!!    ngr(:)      |none          |sequence number of grazing operation 
!!                               |within the year
!!    nirr(:)     |none          |sequence number of irrigation application 
!!                               |within the year
!!    phubase(:)  |heat units    |base zero total heat units (used when no land
!!                               |cover is growing
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use time_module
      use basin_module

      !! initialize variables/arrays at beginning of every year
      nfert = 1
      nirr = 1
      ngr = 1
      ncf = 1
      lai_yrmx = 0.
      anano3 = 0.
      tauton = 0.
      tautop = 0.
      tcfrtn = 0.
      tcfrtp = 0.
      tfertn = 0.
      tfertp = 0.
      tgrazn = 0.
      tgrazp = 0.

      return
      end