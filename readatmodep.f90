      subroutine readatmodep
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the atmospheric deposition values
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use basin_module
      
      character (len=80) :: titldum
      integer :: eof 
  
      eof = 0

!!    Atmosperic deposition filename 
      if (atmofile /= '             ') then
        open (127,file=atmofile)
          do iii = 1, 5
            read (127,*) titldum
          end do
          read (127,*) matmodep, momax
          allocate (atmodep(matmodep))
          
       if (bsn_cc%atmo == 1) then
          do iadep = 1, matmodep
            read (127,*,iostat=eof) atmodep(iadep)%nh4_rf,               &
                                       atmodep(iadep)%no3_rf,            &
                                       atmodep(iadep)%nh4_dry,           &
                                       atmodep(iadep)%no3_dry
            if (eof < 0) exit
          end do
       else if (bsn_cc%atmo == 2) then
            read (127,1001,iostat=eof) mo_atmo1, iyr_atmo1
1001        format (2i6)
            do iadep = 1, matmodep
              allocate (atmodep(iadep)%no3_rfmo(momax))
              allocate (atmodep(iadep)%nh4_rfmo(momax))
              allocate (atmodep(iadep)%no3_drymo(momax))
              allocate (atmodep(iadep)%nh4_drymo(momax))
              read (127,*) (atmodep(iadep)%nh4_rfmo(imo), imo = 1,momax)
              read (127,*) (atmodep(iadep)%no3_rfmo(imo), imo = 1,momax)
              read (127,*) (atmodep(iadep)%nh4_drymo(imo),imo = 1,momax)
              read (127,*) (atmodep(iadep)%no3_drymo(imo),imo = 1,momax)
            end do
       endif
       else
        !!no filename present - set to defaults
	  allocate (atmodep(1))
      endif

      return
      end