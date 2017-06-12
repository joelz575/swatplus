      subroutine readpcom
      
      use input_file_module

      use parm
      use jrw_datalib_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file, name
      integer :: eof
      
      mcom = 0
      eof = 0
      imax = 0

!! Open plant community data file
      inquire (file=in_init%initial_plt, exist=i_exist)
      if (i_exist == 0 .or. in_init%initial_plt == 'null') then
        allocate (pcomdb(0:0))
        allocate (pcomdb(0)%pl(0:0))
        allocate (pcom_xw(0:0))
        db_mx%plantcom = mcom + 1
      else
      do     
       open (113,file=in_init%initial_plt)
       read (113,*,iostat=eof) titldum
       if (eof < 0) exit
       read (113,*,iostat=eof) header
       if (eof < 0) exit
          do while (eof == 0)
             read (113,*,iostat=eof) name, numb
             do ii = 1, numb
                read (113,*,iostat=eof) name
             end do
             if (eof < 0) exit
             imax = imax + 1
          end do
       allocate (pcomdb(0:imax))
       allocate (pcom_xw(0:imax))
       rewind (113)
       read (113,*,iostat=eof) titldum
       read (113,*,iostat=eof) header
       
       do icom = 1, imax
       ! loop through all plant communities
         read (113,*,iostat=eof)  pcomdb(icom)%name, pcomdb(icom)%plants_com

         pcom_xw(icom)%name = pcomdb(icom)%name
         mpcom = pcomdb(icom)%plants_com
         allocate (pcom_xw(icom)%pl_name(mpcom))
         allocate (pcomdb(icom)%pl(mpcom))
         do iplt = 1, mpcom
           read (113,*,iostat=eof) pcomdb(icom)%pl(iplt)%cpnm, pcomdb(icom)%pl(iplt)%igro,          &
             pcomdb(icom)%pl(iplt)%lai, pcomdb(icom)%pl(iplt)%bioms, pcomdb(icom)%pl(iplt)%phuacc,  &
             pcomdb(icom)%pl(iplt)%pop, pcomdb(icom)%pl(iplt)%yrmat, pcomdb(icom)%pl(iplt)%rsdin
           
          pcom_xw(icom)%pl_name(iplt) = pcomdb(icom)%pl(iplt)%cpnm
          
          do ipldb = 1, db_mx%plantparm
            if (pcomdb(icom)%pl(iplt)%cpnm == pldb(ipldb)%plantnm) then 
              pcomdb(icom)%pl(iplt)%db_num = ipldb
              exit
            end if
          end do 
           if (eof < 0) exit
         end do
       end do
      end do
      end if
      
      db_mx%plantcom = imax
      
      close (113)
      return
      end