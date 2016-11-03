      subroutine sd_channel_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax, iisd
      real :: kh
      
      mlte = 0
      eof = 0
      imax = 0
      maxint = 10
      allocate (timeint(maxint))
      allocate (hyd_rad(maxint))
      allocate (sd_ch(0:sp_ob%chandeg))
      allocate (chsd_d(0:sp_ob%chandeg))
      allocate (chsd_m(0:sp_ob%chandeg))
      allocate (chsd_y(0:sp_ob%chandeg))
      allocate (chsd_a(0:sp_ob%chandeg))
      
      inquire (file=in_cha%chan_ez, exist=i_exist)
      if (i_exist == 0 .or. in_cha%chan_ez == 'null') then
        allocate (sd_chd(0:0))
      else
      do
        open (1,file=in_cha%chan_ez)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (1,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
            mlte = mlte + 1
          end do       
           
        allocate (sd_chd(0:imax))
        
        rewind (1)
        read (1,*) titldum
        read (1,*) header
        do idb = 1, mlte
        read (1,*,iostat = eof) i
          backspace (1)
          read (1,*,iostat=eof) k, sd_chd(idb)
          if (eof < 0) exit
        end do
            
        do i = 1, sp_ob%chandeg
         icmd = sp_ob1%chandeg + i - 1
         idb = ob(icmd)%props
          
          sd_ch(i)%order = sd_chd(idb)%order
          sd_ch(i)%chw = sd_chd(idb)%chw
          sd_ch(i)%chd = sd_chd(idb)%chd
          sd_ch(i)%chs = sd_chd(idb)%chs
          sd_ch(i)%chl = sd_chd(idb)%chl
          sd_ch(i)%cherod = sd_chd(idb)%cherod
          sd_ch(i)%cov = sd_chd(idb)%cov
          sd_ch(i)%shear_bnk = sd_chd(idb)%shear_bnk
          sd_ch(i)%hc_len = sd_chd(idb)%hc_ini
          sd_ch(i)%hc_hgt = sd_chd(idb)%hc_hgt
          
          !! compute headcut parameters
          kh = sd_chd(idb)%hc_kh
          if (kh > 1.e-6) then
            sd_ch(i)%hc_co = .37 * (17.83 + 16.56 * kh - 15. * sd_chd(idb)%hc_cov)
            sd_ch(i)%hc_co = amax1 (0., sd_ch(i)%hc_co)
          else
            sd_ch(i)%hc_co = 0.
          end if

!!      compute travel time coefficients
          aa = 1.
          b = 0.
          d = 0.
          chside = sd_chd(idb)%chss
          fps = 4.
          b = sd_ch(i)%chw - 2. * sd_ch(i)%chd * chside

!!      check IF bottom width (b) is < 0
          IF (b <= 0.) THEN
            b = .5 * sd_ch(i)%chw
            b = Max(0., b)
            chside = (sd_ch(i)%chw - b) / (2. * sd_ch(i)%chd)
          END IF
          sd_ch(i)%phi(6) = b
          sd_ch(i)%phi(7) = sd_ch(i)%chd

!!      compute flow and travel time at bankfull depth
        p = b + 2. * sd_ch(i)%chd * Sqrt(chside * chside + 1.)
        a = b * sd_ch(i)%chd + chside * sd_ch(i)%chd * sd_ch(i)%chd
        rh = a / p
        sd_ch(i)%phi(1) = a
        sd_ch(i)%phi(5) = Qman(a, rh, sd_chd(idb)%chn, sd_ch(i)%chs)
        sd_ch(i)%phi(8) = Qman(aa, rh, sd_chd(idb)%chn, sd_ch(i)%chs)
        sd_ch(i)%phi(9) = sd_ch(i)%phi(8) * 5. / 3.
        sd_ch(i)%phi(10) = sd_ch(i)%chl / sd_ch(i)%phi(9) / 3.6
        tt2 = sd_ch(i)%chl * a / sd_ch(i)%phi(5)
  
!!      compute flow and travel time at 1.2 bankfull depth
        d = 0.
        rh = 0.
        qq1 = 0.
        tt1 = 0.
        d = 1.2 * sd_ch(i)%chd
        a = a + (sd_ch(i)%chw * sd_ch(i)%chd + fps * (sd_ch(i)%chd -     &   
          sd_ch(i)%chd) ** 2)
        p = p + 4. * sd_ch(i)%chw + (0.4 * sd_ch(i)%chd *                &            
          Sqrt(fps * fps + 1.))
        rh = a / p
        qq1 = Qman(a, rh, sd_chd(idb)%chn, sd_ch(i)%chs)
        tt1 = sd_ch(i)%chl * a / qq1

!!      compute flow and travel time at 0.1 bankfull depth
        d = 0.1 * sd_ch(i)%chd
        p = b + 2. * sd_ch(i)%chd * Sqrt(chside * chside + 1.)
        a = b * sd_ch(i)%chd + chside * sd_ch(i)%chd * sd_ch(i)%chd
        rh = a / p
        qq1 = Qman(a, rh, sd_chd(idb)%chn, sd_ch(i)%chs)
        tt1 = sd_ch(i)%chl * a / qq1
        sd_ch(i)%phi(11) = Qman(aa, rh, sd_chd(idb)%chn, sd_ch(i)%chs)
        sd_ch(i)%phi(12) = sd_ch(i)%phi(11) * 5. / 3.
        sd_ch(i)%phi(13) = sd_ch(i)%chl / sd_ch(i)%phi(12) / 3.6 
        end do
        exit
      enddo
      endif

      close (1)
      return
      end subroutine sd_channel_read