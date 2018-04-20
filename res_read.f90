      subroutine res_read
      
      use basin_module
      use input_file_module
      use maximum_data_module
      use reservoir_data_module
      use conditional_module
      
      implicit none

      integer :: mon
      integer :: i
      real :: lnvol
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: i_exist              !none       |check to determine if file exists
      integer :: ires                 !none       |counter 
      integer :: k                    !           |
      integer :: iinit                !none       |counter 
      integer :: ihyd                 !none       |counter
      integer :: irel                 !none       |counter 
      integer :: ised                 !none       |counter
      integer :: inut                 !none       |counter
      integer :: ipst                 !none       |counter
       
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
        do while (eof == 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = imax + 1
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
         
       if (res_dat(ires)%init == 0) write (9001,*) res_dat_c(ires)%init, ' not found (res-init)'
       if (res_dat(ires)%hyd == 0) write (9001,*) res_dat_c(ires)%hyd, ' not found (res-hyd)'
       if (res_dat(ires)%release == 0) write (9001,*) res_dat_c(ires)%release, ' not found (res-release)'         
       if (res_dat(ires)%sed == 0) write (9001,*) res_dat_c(ires)%sed, ' not found (res-sed)'
       if (res_dat(ires)%nut == 0) write (9001,*) res_dat_c(ires)%nut, ' not found (res-nut)'
       if (res_dat(ires)%pst == 0) write (9001,*) res_dat_c(ires)%pst, ' not found (res-pst)'
       end do
       
      db_mx%res = imax
       
      close (105)
      exit
      enddo
      endif
      
      return
      end subroutine res_read