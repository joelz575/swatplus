      subroutine ch_read
      
      use basin_module
      use input_file_module
      use jrw_datalib_module

      character (len=80) :: titldum, header
      integer :: eof, mon, i, imax

      eof = 0
      imax = 0

      inquire (file=in_cha%dat, exist=i_exist)
      if (i_exist == 0 .or. in_cha%dat == 'null') then
        allocate (ch_dat(0:0))
        allocate (ch_dat_c(0:0))
      else   
      do
       open (105,file=in_cha%dat)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) i
          if (eof < 0) exit
          imax = Max(imax,i)
        end do
    
      db_mx%ch_dat = imax
      
      allocate (ch_dat(0:imax))
      allocate (ch_dat_c(0:imax))
      
      rewind (105)
      read (105,*) titldum
      read (105,*) header
     
       do ich = 1, db_mx%ch_dat
         read (105,*,iostat=eof) i
         backspace (105)
         read (105,*,iostat=eof) k, ch_dat_c(ich)
         if (eof < 0) exit

         
         do iinit = 1, db_mx%ch_init
           if (ch_init(iinit)%name == ch_dat_c(ich)%init) then
             ch_dat(ich)%init = iinit
             exit
           end if
         end do
         
       
         do ihyd = 1, db_mx%ch_hyd
           if (ch_hyd(ihyd)%name == ch_dat_c(ich)%hyd) then
             ch_dat(ich)%hyd = ihyd
             exit
           end if
         end do
       
         
         do ised = 1, db_mx%ch_sed
           if (ch_sed(ised)%name == ch_dat_c(ich)%sed) then
             ch_dat(ich)%sed = ised
             exit
           end if
         end do      

         do inut = 1, db_mx%ch_nut
           if (ch_nut(inut)%name == ch_dat_c(ich)%nut) then
             ch_dat(ich)%nut = inut
             exit
           end if
         end do   
 
         do ipst = 1, db_mx%ch_pst
           if (ch_pst(ipst)%name == ch_dat_c(ich)%pst) then
             ch_dat(ich)%pst = ipst
             exit
           end if
         end do
         
       if (ch_dat(ich)%init == 0) write (9001,*) ch_dat_c(ich)%init, ' not found (chan)'
       if (ch_dat(ich)%hyd == 0) write (9001,*) ch_dat_c(ich)%hyd, ' not found (chan)'
       if (ch_dat(ich)%sed == 0) write (9001,*) ch_dat_c(ich)%sed, ' not found (chan)'
       if (ch_dat(ich)%nut == 0) write (9001,*) ch_dat_c(ich)%nut, ' not found (chan)'
       if (ch_dat(ich)%pst == 0) write (9001,*) ch_dat_c(ich)%pst, ' not found (chan)'         
       
       end do
              
       close (105)
      exit
      enddo
      endif
      
      return
      
    end subroutine ch_read