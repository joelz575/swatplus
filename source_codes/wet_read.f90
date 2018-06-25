      subroutine wet_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use conditional_module
      use reservoir_module
      
      implicit none

      character (len=80) :: titldum      !           |title of file
      character (len=80) :: header       !           |header of file
      integer :: eof                     !           |end of file
      integer :: imax                    !none       |determine max number for array (imax) and total number in file
      integer :: i_exist                 !none       |check to determine if file exists
      integer :: i                       !none       |counter
      integer :: ires                    !none       |counter 
      integer :: ihyd                    !none       |counter 
      integer :: iinit                   !none       |counter
      integer :: k                       !           |
      integer :: irel                    !none       |counter
      integer :: ised                    !none       |counter
      integer :: inut                    !none       |counter
      integer :: ipst                    !none       |counter
          
      real :: lnvol
      
      eof = 0
      imax = 0
            
      !read reservoir.res
      imax = 0
      inquire (file=in_res%wet, exist=i_exist)
      if (i_exist == 0 .or. in_res%wet == "null") then
        allocate (wet_dat_c(0:0))
        allocate (wet_dat(0:0))
      else   
      do
       open (105,file=in_res%wet)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
        
      db_mx%wet_dat = imax
       
      allocate (wet_dat_c(0:imax))
      allocate (wet_dat(0:imax))
      allocate (wet_ob(0:imax))
      allocate (wet_d(0:imax))
      allocate (wet_m(0:imax))
      allocate (wet_y(0:imax))
      allocate (wet_a(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
      
       do ires = 1, db_mx%wet_dat
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, wet_dat_c(ires)
         if (eof < 0) exit
         
         do iinit = 1, db_mx%res_init
           if (res_init(iinit)%name == wet_dat_c(ires)%init) then
             wet_dat(ires)%init = iinit
             exit
           end if
         end do
                         
         do ihyd = 1, db_mx%wet_hyd
             if (wet_hyd(ihyd)%name == wet_dat_c(ires)%hyd) then
             wet_dat(ires)%hyd = ihyd
             exit
           end if
         end do
       
          do irel = 1, db_mx%d_tbl
            if (d_tbl(irel)%name == wet_dat_c(ires)%release) then
             wet_dat(ires)%release = irel
             exit
            end if
          end do      
 
         do ised = 1, db_mx%res_sed
           if (res_sed(ised)%name == wet_dat_c(ires)%sed) then
             wet_dat(ires)%sed = ised
             exit
           end if
         end do      

         do inut = 1, db_mx%res_nut
           if (res_nut(inut)%name == wet_dat_c(ires)%nut) then
             wet_dat(ires)%nut = inut
             exit
           end if
         end do   
 
         do ipst = 1, db_mx%res_pst
           if (res_pst(ipst)%name == wet_dat_c(ires)%pst) then
             wet_dat(ires)%pst = ipst
             exit
           end if
         end do  
        if (wet_dat(ires)%init == 0) write (9001,*) wet_dat_c(ires)%init, " not found (wet-init)"
        if (wet_dat(ires)%hyd == 0) write (9001,*) wet_dat_c(ires)%hyd, " not found (wet-hyd)"
        if (wet_dat(ires)%release == 0) write (9001,*) wet_dat_c(ires)%release, " not found (wet-release)"
        if (wet_dat(ires)%sed == 0) write (9001,*) wet_dat_c(ires)%sed, " not found (wet-sed)"
        if (wet_dat(ires)%nut == 0) write (9001,*) wet_dat_c(ires)%nut, " not found (wet-nut)"
        if (wet_dat(ires)%pst == 0) write (9001,*) wet_dat_c(ires)%pst, " not found (wet-pst)"
       
       end do
       
      db_mx%wet_dat = imax
       
      close (105)
      exit
      enddo
      endif
      
      return
      end subroutine wet_read