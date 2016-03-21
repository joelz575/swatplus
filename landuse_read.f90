      subroutine landuse_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      mlu = 0
      eof = 0
      imax = 0
      
      !! read all landuse data from landuse.dat
      inquire (file=in_lum%landuse_lum, exist=i_exist)
      if (i_exist == 0 .or. in_lum%landuse_lum == 'null') then
        allocate (lum(0:0))
        allocate (lum_str(0:0))
        allocate (landuse_xw(0:0))
      else
      do
       open (107,file=in_lum%landuse_lum)
       read (107,*,iostat=eof) titldum
       if (eof < 0) exit
       read (107,*,iostat=eof) header
       if (eof < 0) exit
       do while (eof >= 0)
         read (107,*,iostat=eof) titldum
         if (eof < 0) exit
         imax = imax + 1
       end do
       
       allocate (lum(0:imax))
       allocate (lum_str(0:imax))
       allocate (landuse_xw(0:imax))
       
       rewind (107)
       read (107,*) titldum
       read (107,*) header
      
       do ilu = 1, imax   
         read (107,*,iostat=eof) lum(ilu)
         
         if (lum(ilu)%plant_cov /= 'null') then
           do ipcom = 1, db_mx%plantcom
              if (lum(ilu)%plant_cov == pcom_xw(ipcom)%name) then
                 lum_str(ilu)%plant_cov = ipcom
                 exit
              end if
           end do
         end if
         
         if (lum(ilu)%mgt_ops /= 'null') then 
           do isched = 1, db_mx%mgt_ops
             if (lum(ilu)%mgt_ops == mgt_ops_xw(isched)) then
               lum_str(ilu)%mgt_ops = isched
               exit
             end if
           end do
         end if
         
         if (lum(ilu)%tiledrain /= 'null') then
           do ipr = 1, db_mx%sdr
             if (lum(ilu)%tiledrain == sdr(ipr)%name) then
               lum_str(ilu)%tiledrain = ipr
               exit
             end if
           end do
         end if
             
         if (lum(ilu)%septic /= 'null') then
           do ipr = 1, db_mx%septic
             if (lum(ilu)%septic == sep(ipr)%name) then
               lum_str(ilu)%septic = ipr
               exit
             end if
           end do
         end if
         
         if (lum(ilu)%fstrip /= 'null') then
           do ipr = 1, db_mx%filtop_db
             if (lum(ilu)%fstrip == filtstrip_db(ipr)%name) then
               lum_str(ilu)%fstrip = ipr
               exit
             end if
           end do
         end if
                  
         if (lum(ilu)%grassww /= 'null') then
           do ipr = 1, db_mx%grassop_db
             if (lum(ilu)%grassww == grwaterway_db(ipr)%name) then
               lum_str(ilu)%grassww = ipr
               exit
             end if
           end do
         end if
                           
         if (lum(ilu)%terrace /= 'null') then
           do ipr = 1, db_mx%terrop_db
             if (lum(ilu)%terrace == terrace_db(ipr)%name) then
               lum_str(ilu)%terrace = ipr
               exit
             end if
           end do
         end if
                                    
         if (lum(ilu)%contour /= 'null') then
           do ipr = 1, db_mx%contop_db
             if (lum(ilu)%contour == contour_db(ipr)%name) then
               lum_str(ilu)%contour = ipr
               exit
             end if
           end do
         end if
                                             
         if (lum(ilu)%stcrop /= 'null') then
           do ipr = 1, db_mx%stripop_db
             if (lum(ilu)%stcrop == stripcrop_db(ipr)%name) then
               lum_str(ilu)%stcrop = ipr
               exit
             end if
           end do
         end if
                                                      
         if (lum(ilu)%bmpuser /= 'null') then
           do ipr = 1, db_mx%bmpuserop_db
             if (lum(ilu)%bmpuser == bmpuser_db(ipr)%name) then
               lum_str(ilu)%bmpuser = ipr
               exit
             end if
           end do
         end if
         
         !! landuse.lum
         landuse_xw(ilu) = lum(ilu)%name
         
         if (eof < 0) exit
       end do
       exit
      end do
      endif
      
      close(107)
      
      db_mx%landuse = imax
      
      return  
      end subroutine landuse_read