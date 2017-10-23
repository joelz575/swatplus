      module recall_hourly
      
      use hydrograph_module
      use time_module
      use parm, only : idapa, ifirsta, inum1, iypa
      
      contains
      
!!
      subroutine  read_rechour
      
      character (len=13) :: hour_in      
      integer :: ifirsta, idapa, iypa
      allocate (hhr(1))

      inum1 = ihout
      ifirsta = 1
      
      hour_in = ""
      read (102,5100) hour_in
      call caps(hour_in)
      open (200+inum1,file=hour_in,recl=350)
      do ii = 1, 6
        read (200+inum1,5200) titldum
      end do

      !! different start date than the SWAT run
      if (ifirsta == 1) then
        do 
          read (200+inum1,*) idapa, iypa
	    if(idapa == time%idaf .and. iypa == time%yrc) exit
        end do
        ifirsta = 0
      endif
      
 5100 format (10x,2a13)
 5200 format (a80)
      return
      end subroutine read_rechour
      
      
      subroutine rechour
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network for 
!!    routing through the watershed where the records are summarized on a
!!    hourly basis

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1       |none          |reach number
!!    ihout       |none          |hydrograph storage location number
!!    inum1       |none          |file number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idap        |julian date   |julian date of record
!!    ihr         |hour (0-23)   |hour of record
!!    ii          |none          |counter
!!    iyp         |year          |year of record

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      
      integer :: ii

!! initialize variables
      idap = 0
      iyp = 0
      hd(ihout) = hz
      do ii = 1, time%step   ! subhourly time step, jaehak jeong
        ob(icmd)%ts(1,ii) = hz
      end do

      do ii = 1, time%step   ! subhourly time step, jaehak jeong
        read (200+inum1,*) idap, iyp, ihr, hhr(1)%flo, hhr(1)%sed,      &
          hhr(1)%orgn, hhr(1)%sedp, hhr(1)%no3, hhr(1)%nh3, hhr(1)%no2, &
          hhr(1)%solp, hhr(1)%cbod, hhr(1)%dox, hhr(1)%chla,hhr(1)%psol,&
          hhr(1)%psor, hhr(1)%bacp, hhr(1)%baclp, hhr(1)%met1,          &
          hhr(1)%met2, hhr(1)%met3

        ob(icmd)%ts(1,ii) = hhr(1)

        hd(ihout)%flo = hd(ihout)%flo + hhr(1)%flo
        hd(ihout)%sed = hd(ihout)%sed + hhr(1)%sed
        hd(ihout)%orgn = hd(ihout)%orgn + hhr(1)%orgn
        hd(ihout)%sedp = hd(ihout)%sedp + hhr(1)%sedp
        hd(ihout)%no3 = hd(ihout)%no3 + hhr(1)%no3
        hd(ihout)%solp = hd(ihout)%solp + hhr(1)%solp
        hd(ihout)%psol = hd(ihout)%psol + hhr(1)%psol
        hd(ihout)%psor = hd(ihout)%psor + hhr(1)%psor
        hd(ihout)%chla = hd(ihout)%chla + hhr(1)%chla
        hd(ihout)%nh3 = hd(ihout)%nh3 + hhr(1)%nh3
        hd(ihout)%no2 = hd(ihout)%no2 + hhr(1)%no2
        hd(ihout)%cbod = hd(ihout)%cbod + hhr(1)%cbod
        hd(ihout)%dox = hd(ihout)%dox + hhr(1)%dox
        hd(ihout)%bacp = hd(ihout)%bacp + hhr(1)%bacp
        hd(ihout)%baclp = hd(ihout)%baclp + hhr(1)%baclp
        hd(ihout)%met1 = hd(ihout)%met1 + hhr(1)%met1
        hd(ihout)%met3 = hd(ihout)%met2 + hhr(1)%met2
        hd(ihout)%met3 = hd(ihout)%met3 + hhr(1)%met3

        !! Assumed equal distribution of sediment
        hd(ihout)%san = 0.                            ! sand
        hd(ihout)%sil = hd(ihout)%sil + hhr(1)%sed    ! silt
        hd(ihout)%cla = 0.                            ! cla
        hd(ihout)%sag = 0.                            ! sag
        hd(ihout)%lag = 0.                            ! lag
        hd(ihout)%grv = 0.                            ! gravel

      end do

      return
      end subroutine rechour

      
      end module recall_hourly