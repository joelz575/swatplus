      subroutine wet_hyd_read
      
      use basin_module
      use input_file_module
      use jrw_datalib_module
      use output_landscape_module

      character (len=80) :: titldum, header
      integer :: eof, mon, i, imax

      eof = 0
      imax = 0

      inquire (file=in_res%hyd_wet, exist=i_exist)
      if (i_exist == 0 .or. in_res%hyd_wet == 'null') then
        allocate (wet_hyd(0:0))
      else   
      do
       open (105,file=in_res%hyd_wet)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
        
      db_mx%wet_hyd = imax
      
      allocate (wet_hyd(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
      
       do ires = 1, imax
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, wet_hyd(ires)
         if (eof < 0) exit

        if (wet_hyd(ires)%pvol + wet_hyd(ires)%evol > 0.) then
          if(wet_hyd(ires)%pvol <= 0) wet_hyd(ires)%pvol = 0.9 * wet_hyd(ires)%evol
        else
          if (wet_hyd(ires)%pvol <= 0) wet_hyd(ires)%pvol = 60000.0
        end if
        if (wet_hyd(ires)%evol <= 0.0) wet_hyd(ires)%evol = 1.11 * wet_hyd(ires)%pvol
        if (wet_hyd(ires)%psa <= 0.0) wet_hyd(ires)%psa = 0.08 * wet_hyd(ires)%pvol
        if (wet_hyd(ires)%esa <= 0.0) wet_hyd(ires)%esa = 1.5 * wet_hyd(ires)%psa
        if (wet_hyd(ires)%evrsv <= 0.) wet_hyd(ires)%evrsv = 0.6

       end do
       close (105)
      exit
      enddo
      endif
  
      return
      end subroutine wet_hyd_read