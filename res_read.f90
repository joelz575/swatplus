      subroutine res_read
      
      use basin_module
      use input_file_module
      use jrw_datalib_module
      use conditional_module

      character (len=80) :: titldum, header
      integer :: eof, mon, i, imax
      real :: lnvol
      
      eof = 0
      imax = 0
            
      !read reservoir.res
      imax = 0
      inquire (file=in_res%res, exist=i_exist)
      if (i_exist == 0 .or. in_res%res == 'null') then
        allocate (res_dat_c(0:0))
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
          imax = Max(imax,i)
        end do
        
      db_mx%res_dat = imax
       
      allocate (res_dat_c(0:imax))
      allocate (res_dat(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
      
       do ires = 1, db_mx%res_dat
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, res_dat_c(ires)
         if (eof < 0) exit
         
         do iinit = 1, db_mx%res_init
           if (res_init(iinit)%name == res_dat_c(ires)%init) then
             res_dat(ires)%init = iinit
             exit
           end if
         end do
       
         do ihyd = 1, db_mx%res_hyd
           if (res_hyd(ihyd)%name == res_dat_c(ires)%hyd) then
             res_dat(ires)%hyd = ihyd
             exit
           end if
         end do
       
          do irel = 1, db_mx%d_tbl
            if (d_tbl(irel)%name == res_dat_c(ires)%release) then
             res_dat(ires)%release = irel
             exit
            end if
          end do      
 
         do ised = 1, db_mx%res_sed
           if (res_sed(ised)%name == res_dat_c(ires)%sed) then
             res_dat(ires)%sed = ised
             exit
           end if
         end do      

         do inut = 1, db_mx%res_nut
           if (res_nut(inut)%name == res_dat_c(ires)%nut) then
             res_dat(ires)%nut = inut
             exit
           end if
         end do   
 
         do ipst = 1, db_mx%res_pst
           if (res_pst(ipst)%name == res_dat_c(ires)%pst) then
             res_dat(ires)%pst = ipst
             exit
           end if
         end do  

       end do
       
      db_mx%res = imax
       
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
        if ((res_hyd(i)%esa - res_hyd(i)%psa) > 0. .and. resdif > 0.) then
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
      end subroutine res_read