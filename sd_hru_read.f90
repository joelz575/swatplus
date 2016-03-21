      subroutine sd_hru_read
      
      use jrw_datalib_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, grow_start, grow_end
      
      eof = 0
      imax = 0
      msd_h = 0
      
      ! soil textures
      awct(1) = .056
      awct(2) = .116
      awct(3) = .115
      awct(4) = .114
      awct(5) = .186
      awct(6) = .254
      awct(7) = .141
      awct(8) = .138
      awct(9) = .113
      awct(10) = .130
      awct(11) = .132
      awct(12) = .113  !.123
      
      port(1) = .40
      port(2) = .40
      port(3) = .40
      port(4) = .40
      port(5) = .43
      port(6) = .47
      port(7) = .43
      port(8) = .40
      port(9) = .40
      port(10) = .40
      port(11) = .48
      port(12) = .47
      
      scon(1) = 105.
      scon(2) = 60. 
      scon(3) = .26 
      scon(4) = 13.2
      scon(5) = 6.8 
      scon(6) = 5.6 
      scon(7) = 4.3 
      scon(8) = 2.3 
      scon(9) = 1.5 
      scon(10) = 1.2 
      scon(11) = 0.9 
      scon(12) = 1.5 

      a1 = .2 
      a2 = .8 
      
      inquire (file=in_hru%hru_ez, exist=i_exist)
      if (i_exist == 0 .or. in_hru%hru_ez == 'null') then
        allocate (sd_db(0:0))
!        allocate (sd(sp_ob%hru_lte))
      else
      do
        open (1,file=in_hru%hru_ez)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
!        read (1,*,iostat=eof) msd_h
!        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
         do while (eof >= 0)
            read (1,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            msd_h = msd_h + 1
         end do
         
        !assumes data for each hru -> ok since there is only one file
        allocate (sd_db(0:imax))
        allocate (sd(sp_ob%hru_lte))
        rewind (1)
        read (1,*) titldum
        read (1,*) header

      do isd_h = 1, msd_h
        read (1,*,iostat = eof) i
        backspace (1)
        read (1,*,iostat=eof) k, sd_db(i)
        if (eof < 0) exit
      end do

      do i = 1, sp_ob%hru_lte
         icmd = sp_ob1%hru_lte + i - 1
         idb = ob(icmd)%props
         sd(i)%sw = sd_db(i)%sw * awct(sd_db(idb)%itext) *               &
                                       sd_db(idb)%soildep !* 1000.
         sd(i)%awc = awct(sd_db(idb)%itext) * sd_db(idb)%soildep !* 1000.
         sd(i)%por = port(sd_db(idb)%itext) * sd_db(idb)%soildep !* 1000.
         sd(i)%sc = scon(sd_db(idb)%itext) 
         sd_db(idb)%abf = EXP(-sd_db(idb)%abf) 
         qn1 = sd_db(idb)%cn2 - (20. * (100. - sd_db(idb)%cn2)) /        &
            (100.-sd_db(idb)%cn2 + EXP(2.533-.063*(100.-sd_db(idb)%cn2)))
         qn3 = sd_db(idb)%cn2 * EXP(.00673*(100.-sd_db(idb)%cn2)) 
         sd(i)%s1 = 254. * (100. / qn1 - 1.) 
         sd(i)%s3 = 254. * (100. / qn3 - 1.) 
         xi = 30. * mo - 15. 
         xx = sd_db(idb)%xlat / 57.3 
         sd(i)%yls = SIN(xx) 
         sd(i)%ylc = COS(xx) 
         sd(i)%phu = 2000. 
         sd(i)%dm = 0. 
         sd(i)%alai = .15 
         sd(i)%g = 0. 
                  
         !crosswalk plant with plants.plt
         do ipl = 1, db_mx%plantparm
            if (sd_db(idb)%plant == plnt_xw(ipl)) then
              sd(i)%iplant = ipl
              exit
            endif
          end do
         
         !compute heat unit from growing season and weather generator
         iwst = ob(icmd)%wst
         iwgn = wst(iwst)%wco%wgn
         if (sd_db(idb)%igrow2 > sd_db(idb)%igrow1) then
           grow_start = sd_db(idb)%igrow1
           grow_end = sd_db(idb)%igrow2
         else
           grow_start = sd_db(idb)%igrow2
           grow_end = sd_db(idb)%igrow1
         end if
         mo = 1
         imo = 2
         phutot = 0.
         do iday = 1, 365
           if (iday > ndays(imo)) then
             imo = imo + 1
             mo = mo + 1
           end if
           if (iday > grow_start .and. iday < grow_end) then
             tave = (wgn(iwgn)%tmpmx(mo) + wgn(iwgn)%tmpmn(mo)) / 2.
             iplt = sd(i)%iplant
             phuday = tave - pldb(iplt)%t_base
             if (phuday > 0.) then
               phutot = phutot + phuday
             end if
           end if
         end do
         ! change from growing season to time to maturity
         sd(i)%phu = .9 * phutot
         sd(i)%phu = amax1(700., sd(i)%phu)

         ! compute musle factors
         ! calculate USLE slope length factor
         xm = 0.
         sin_sl = 0.
         xm = .6 * (1. - EXP(-35.835 * sd_db(idb)%slope))
         sin_sl = SIN(Atan(sd_db(idb)%slope))
         sd_db(idb)%uslels = (sd_db(idb)%slopelen/22.128)**xm *          &
                        (65.41 * sin_sl * sin_sl + 4.56 * sin_sl + .065)
!     !      calculate composite usle value
         sd(i)%uslefac = sd_db(idb)%uslek * sd_db(idb)%uslep *           &
           sd_db(idb)%uslels * sd_db(idb)%uslec * 11.8
         
        ! compute time of concentration using Kirpich equation
        IF (sd_db(idb)%tc < 1.e-6) THEN
         ch_len = sd_db(idb)%dakm2
         ch_sl = sd_db(idb)%slope
         sd_db(idb)%tc = .0078 * (ch_len * 3210.) ** .77 * sd_sl        &   
                                                        ** (-.385)
        END IF
        sd_db(idb)%tc = sd_db(idb)%tc * 60.     !!min to seconds
      end do

      !! dimension swatdeg output variables
      msd_h = sp_ob%hru_lte
      allocate (sdwb_d(msd_h))
      allocate (sdwb_m(msd_h))
      allocate (sdwb_y(msd_h))
      allocate (sdwb_a(msd_h))
      allocate (sdnb_d(msd_h))
      allocate (sdnb_m(msd_h))
      allocate (sdnb_y(msd_h))
      allocate (sdnb_a(msd_h))
      allocate (sdls_d(msd_h))
      allocate (sdls_m(msd_h))
      allocate (sdls_y(msd_h))
      allocate (sdls_a(msd_h))
      allocate (sdpw_d(msd_h))
      allocate (sdpw_m(msd_h))
      allocate (sdpw_y(msd_h))
      allocate (sdpw_a(msd_h))
       
      !allocate(qday(nyrs*366),qfdc(nyrs*366)) !Jaehak Jeong for fdc
      !allocate(pr(nyrs*366))
      !allocate(sd_qday(5*366),sd_qfdc(5*366)) !Jaehak Jeong for fdc
      !allocate(pr(5*366))
      
      exit
      end do
      endif
      
      close (1)
        
      return
      end subroutine sd_hru_read   