      subroutine pestparm_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use pesticide_data_module
         
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: pestdbase
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_parmdb%pest, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%pest == 'null') then
        allocate (pestdb(0:0))
        allocate (pstcp(0:0))
      else
      do
        open (106,file=in_parmdb%pest)
        read (106,*,iostat=eof) titldum
        if (eof < 0) exit
        read (106,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (106,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (pestdb(0:imax))
        allocate (pstcp(0:imax))

        rewind (106)
        read (106,*) titldum
        read (106,*) header 
        
        do ip = 1, imax
           read (106,*,iostat=eof) pestdb(ip)
           if (eof < 0) exit
      
          !! calculations: the first-order rate law for the decay of pesticides
          !! is dP/dt = -kP where P is the amount of pesticide, 
          !! t is the time and k is the rate constant for degradation. To calculate
          !! the amount of pesticide at any time after application the equation
          !! P(t) = P_o*Exp(-kt) is used where P_o is the original amount of 
          !! pesticide. k can be calculate with the equation k = 0.693/hlife.
          !! decay_f or decay_s = Exp(-k)
          if (pestdb(ip)%hlife_f > 0.) then
            pstcp(ip)%decay_f = Exp(-.693 / pestdb(ip)%hlife_f)
          else
            pstcp(ip)%decay_f = 0.
          endif
          if (pestdb(ip)%hlife_s > 0.) then
            pstcp(ip)%decay_s = Exp(-.693 / pestdb(ip)%hlife_s)
          else
            pstcp(ip)%decay_s = 0.
          endif
   
          !! set values for pesticide routed through main channel network
          !if (ip == bsn_cc%rtpest) then
          !  pest_sol = pestdb(ip)%pst_wof * 1000.
          !end if

        end do
        exit
      enddo
      endif
      
      db_mx%pestparm = imax

      close (106)
      return
      end subroutine pestparm_read