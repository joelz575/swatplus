      subroutine readpcom
      
      use input_file_module
      use maximum_data_module
      use plant_data_module
      
      implicit none      
      
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=13) :: file      !           |
      character (len=13) :: name      !           |
      integer :: eof                  !           |end of file
      integer :: i_exist              !none       |check to determine if file exists
      integer :: mcom                 !           |
      integer :: icom                 !none       |counter
      integer :: imax                 !none       |counter
      integer :: numb                 !none       |end of loop
      integer :: ii                   !none       |counter
      integer :: mpcom                !none       |end of loop   
      integer :: iplt                 !none       |counter
      integer :: ipldb                !none       |counter
      
      
      mcom = 0
      eof = 0
      imax = 0

!! Open plant community data file
      inquire (file=in_init%plant, exist=i_exist)
      if (i_exist == 0 .or. in_init%plant == 'null') then
        allocate (pcomdb(0:0))
        allocate (pcomdb(0)%pl(0:0))
        db_mx%plantcom = mcom + 1
      else
      do     
       open (113,file=in_init%plant)
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
       rewind (113)
       read (113,*,iostat=eof) titldum
       read (113,*,iostat=eof) header
       
       do icom = 1, imax
       ! loop through all plant communities
         read (113,*,iostat=eof)  pcomdb(icom)%name, pcomdb(icom)%plants_com

         mpcom = pcomdb(icom)%plants_com
         allocate (pcomdb(icom)%pl(mpcom))
         do iplt = 1, mpcom
           read (113,*,iostat=eof) pcomdb(icom)%pl(iplt)%cpnm, pcomdb(icom)%pl(iplt)%igro,          &
             pcomdb(icom)%pl(iplt)%lai, pcomdb(icom)%pl(iplt)%bioms, pcomdb(icom)%pl(iplt)%phuacc,  &
             pcomdb(icom)%pl(iplt)%pop, pcomdb(icom)%pl(iplt)%yrmat, pcomdb(icom)%pl(iplt)%rsdin

          do ipldb = 1, db_mx%plantparm
            if (pcomdb(icom)%pl(iplt)%cpnm == pldb(ipldb)%plantnm) then 
              pcomdb(icom)%pl(iplt)%db_num = ipldb
              exit
            end if
          end do 
          if (pcomdb(icom)%pl(iplt)%db_num == 0) then
                 write (9001,*) ' plant com', icom, ' plant numb', iplt, pcomdb(icom)%pl(iplt)%cpnm, ' not found in plants.plt database' 
          end if
          if (eof < 0) exit
         end do
       end do
      end do
      end if
      
      db_mx%plantcom = imax
      
      close (113)
      return
      end