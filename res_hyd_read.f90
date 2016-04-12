      subroutine res_hyd_read
      
      use basin_module
      use input_file_module

      character (len=80) :: titldum, header
      integer :: eof, mon, i, imax
      real :: lnvol
      
      eof = 0
      imax = 0
      
      inquire (file=in_res%init_res, exist=i_exist)
      if (i_exist == 0 .or. in_res%init_res == 'null') then
        allocate (res_init(0:0))
      else   
      do
       open (105,file=in_res%init_res)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof >= 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
        end do
      
      allocate (res_init(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
           
       do ires = 1, imax
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, res_init(ires)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif
      
      imax = 0
      inquire (file=in_res%res, exist=i_exist)
      if (i_exist == 0 .or. in_res%res == 'null') then
        allocate (res_dat(0:0))
      else   
      do
       open (105,file=in_res%res)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof >= 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
        end do
      
      allocate (res_dat(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
      
       do ires = 1, imax
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, res_dat(ires)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif
        
      imax = 0
      inquire (file=in_res%hyd_res, exist=i_exist)
      if (i_exist == 0 .or. in_res%hyd_res == 'null') then
        allocate (res_hyd(0:0))
      else   
      do
       open (105,file=in_res%hyd_res)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof >= 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = amax1(imax,i)
        end do
      
      allocate (res_hyd(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
      
       do ires = 1, imax
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, res_hyd(ires)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif
  
      !! set default values
      do i = 1, imax
        if (res_hyd(i)%pvol + res_hyd(i)%evol> 0.) then
          if(res_hyd(i)%pvol <= 0) res_hyd(i)%pvol = 0.9*res_hyd(i)%evol
        else
          if (res_hyd(i)%pvol <= 0) res_hyd(i)%pvol = 60000.0
        end if
        if (res_hyd(i)%evol <= 0.0) res_hyd(i)%evol=1.11*res_hyd(i)%pvol
        if (res_hyd(i)%psa <= 0.0) res_hyd(i)%psa =0.08* res_hyd(i)%pvol
        if (res_hyd(i)%esa <= 0.0) res_hyd(i)%esa = 1.5 * res_hyd(i)%psa
        if (res_hyd(i)%evrsv <= 0.) res_hyd(i)%evrsv = 0.6
       
        !! convert units
        !res_hyd(i)%evol = res_hyd(i)%evol * 10000.          !! 10**4 m**3 => m**3
        !res_hyd(i)%pvol = res_hyd(i)%pvol * 10000.          !! 10**4 m**3 => m**3
        
        !! calculate shape parameters for surface area equation
        resdif = res_hyd(i)%evol - res_hyd(i)%pvol
        if ((res_hyd(i)%esa - res_hyd(i)%psa) > 0. .and. resdif>0.) then
          lnvol = Log10(res_hyd(i)%evol) - Log10(res_hyd(i)%pvol)
          if (lnvol > 1.e-4) then
            res_hyd(i)%br2 = (Log10(res_hyd(i)%esa) - Log10(res_hyd(i)%psa)) / lnvol
          else  
            res_hyd(i)%br2 = (Log10(res_hyd(i)%esa) - Log10(res_hyd(i)%psa)) / 0.001
          end if
          if (res_hyd(i)%br2 > 0.9) then
            res_hyd(i)%br2 = 0.9
            res_hyd(i)%br1 = (res_hyd(i)%psa / res_hyd(i)%pvol) ** 0.9
          else
            res_hyd(i)%br1 = (res_hyd(i)%esa / res_hyd(i)%evol) ** res_hyd(i)%br2
          end if  
        else
          res_hyd(i)%br2 = 0.9
          res_hyd(i)%br1 = (res_hyd(i)%psa / res_hyd(i)%pvol) ** 0.9
        end if

      end do

      return
      end subroutine res_hyd_read