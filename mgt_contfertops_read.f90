      subroutine mgt_contfertops_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      mcfertops = 0
      eof = 0
      imax = 0
                       
      !! read continuous fertilizer operations
      inquire (file=in_ops%contfert_ops, exist=i_exist)
      if (i_exist == 0 .or. in_ops%contfert_ops == 'null') then   
        allocate (contfertop_db(0:0))
        allocate (frtc_xw(0:0))
      else
      do
        open (107,file=in_ops%contfert_ops)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
           read (107,*,iostat=eof) titldum
           if (eof < 0) exit
           imax = imax + 1 
          end do
 
        allocate (contfertop_db(0:imax))
        allocate (frtc_xw(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header

        do icfertop = 1, imax
          read (107,*,iostat=eof) contfertop_db(icfertop)
          
          !! frtc
          frtc_xw(icfertop) = contfertop_db(icfertop)%name
          if (eof < 0) exit
        end do

        exit
      enddo
      endif
      close(107)
      
      db_mx%contfertop_db = imax
      
      return                            
      end subroutine mgt_contfertops_read