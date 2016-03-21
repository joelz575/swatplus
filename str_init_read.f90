      subroutine str_init_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, num_pr
      
      eof = 0
      imax = 0
      
      !! read all managment operations data from str_init.dat
      inquire (file=in_str%initial_str, exist=i_exist)
      if (i_exist == 0 .or. in_str%initial_str == 'null') then
        allocate (str_init(0:0))
        allocate (init_str_xw(0:0))
      else
      do
        open (107,file=in_str%initial_str)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
                
        allocate (str_init(0:imax))
        allocate (init_str_xw(0:imax))
        
        rewind (107) 
        read (107,*) titldum
        read (107,*) header
                
        do istr = 1, imax
          read (107,*,iostat=eof) str_init(istr)%name, num_pr
          allocate (str_init(istr)%prac(num_pr))
          allocate (str_init(istr)%prac_typ(num_pr))
          allocate (str_init(istr)%prac_num(num_pr))
          
          backspace (107) 
          read (107,*,iostat=eof) str_init(istr)%name, str_init(istr)%num_pr,      &
            (str_init(istr)%prac(ii), str_init(istr)%prac_typ(ii), ii = 1, num_pr)
          
          do ii = 1, num_pr
            select case (str_init(istr)%prac(ii))
                
            case ("terrace")
                do ipr = 1, db_mx%terrop_db
                   if (str_init(istr)%prac_typ(ii) == terr_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do
                
            case ("tile")
                do ipr = 1, db_mx%sdr
                   if (str_init(istr)%prac_typ(ii) == tdrain_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do

            case ("contour")
                do ipr = 1, db_mx%contop_db
                   if (str_init(istr)%prac_typ(ii) ==  cont_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 
                
            case ("filter")
                do ipr = 1, db_mx%filtop_db
                   if (str_init(istr)%prac_typ(ii) ==  fstrip_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 

 
           case ("stripcrop")
                do ipr = 1, db_mx%stripop_db
                   if (str_init(istr)%prac_typ(ii) ==  stripcr_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 
                
           case ("fire")
                do ipr = 1, db_mx%fireop_db
                   if (str_init(istr)%prac_typ(ii) ==  fire_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do    
    
          case ("grassww")
                do ipr = 1, db_mx%grassop_db
                   if (str_init(istr)%prac_typ(ii) ==  grassww_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 
                
          case ("plantup")
                do ipr = 1, db_mx%plparmop_db
                   if (str_init(istr)%prac_typ(ii) ==  plparms_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 
                
         case ("resman")
                do ipr = 1, db_mx%rsdmgtop_db
                   if (str_init(istr)%prac_typ(ii) ==   rsdmgt_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 
                
         case ("user_def")
                do ipr = 1, db_mx%bmpuserop_db
                   if (str_init(istr)%prac_typ(ii) ==   bmp_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 
                
         case ("septic")
                do ipr = 1, db_mx%septic
                   if (str_init(istr)%prac_typ(ii) ==   sept_str_xw(ipr)) then
                     str_init(istr)%prac_num(ii) = ipr
                     exit
                   end if
                end do 
                
            end select
          end do
              
          !! initial.str
          init_str_xw(istr) = str_init(istr)%name
          
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      
      db_mx%initop_db = imax
      close (107)
        
      return
      end subroutine str_init_read