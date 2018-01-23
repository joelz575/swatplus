      subroutine res_dayinit

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the
!!    channel routing command loop

      use basin_module
      use parm, only : resclao, resgrao, reslago, ressago, ressano
      use reservoir_module

!! initialize daily variables
      bury = 0.
      difus = 0.
      reactb = 0.
      reactw = 0.
      reschlao = 0.
      resev = 0.
      resflwi = 0.
      resflwo = 0.
      respcp = 0.
      resnh3o = 0.
      resno2o = 0.
      resno3o = 0.
      resorgno = 0.
      resorgpo = 0.
      respesti = 0.
      ressedc = 0.
      ressedi = 0.
      ressedo = 0.
      ressano = 0.
      ressilo = 0.
      resclao = 0.
      ressago = 0.
      reslago = 0.
	resgrao = 0.
      ressep = 0.
      ressolpo = 0.
      resuspst = 0.
      setlpst = 0.
      solpesti = 0.
      solpesto = 0.
      sorpesti = 0. 
      sorpesto = 0.
      volatpst = 0.

      return
      end subroutine res_dayinit