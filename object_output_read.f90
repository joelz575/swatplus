      subroutine object_output_read
      
      use input_file_module
      !use hydrograph_module
      
      character (len=80) :: titldum, header
      integer :: i, iobj, eof
      
      mobj_out = 0
      imax = 0
      
      !! read old saveconc properties
      inquire (file=in_sim%object_prt,exist=i_exist)
      if (i_exist == 0 .or. in_sim%object_prt == 'null') then         
        allocate (ob_out(0:0))
      else
      do
        open (107,file=in_sim%object_prt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = Max(imax,i)
            mobj_out = mobj_out + 1
          end do
          
        allocate (ob_out(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header

        do i = 1, mobj_out
          read (107,*,iostat=eof) ii
          backspace (107)
          read (107,*,iostat=eof) k, ob_out(i)%obtyp,                    &
             ob_out(i)%obtypno, ob_out(i)%hydtyp, ob_out(i)%filename
          if (eof < 0) exit
          
          select case (ob_out(i)%obtyp)
            case ("hru")   !hru
              ob_out(i)%objno = sp_ob1%hru + ob_out(i)%obtypno - 1
            case ("hlt")   !hru_lte
              ob_out(i)%objno = sp_ob1%hru_lte + ob_out(i)%obtypno - 1
            case ("cha")   !channel
              ob_out(i)%objno = sp_ob1%chan + ob_out(i)%obtypno - 1
            case ("exc")   !export coefficient
              ob_out(i)%objno = sp_ob1%exco + ob_out(i)%obtypno - 1
            case ("del")   !delivery ratio
              ob_out(i)%objno = sp_ob1%dr + ob_out(i)%obtypno - 1
            case ("out")   !outlet
              ob_out(i)%objno = sp_ob1%outlet + ob_out(i)%obtypno - 1
            case ("sdc")   !swat-deg channel
              ob_out(i)%objno = sp_ob1%chandeg + ob_out(i)%obtypno - 1
          end select
      
          select case (ob_out(i)%hydtyp)
            case ("tot")   !total flow
               ob_out(i)%hydno = 1
            case ("rhg")   !recharge
               ob_out(i)%hydno = 2              
            case ("sur")   !surface
               ob_out(i)%hydno = 3 
            case ("lat")   !lateral
               ob_out(i)%hydno = 4
            case ("til")   !tile
               ob_out(i)%hydno = 5  
            end select
         iunit = ob_out(i)%unitno
         
         open (iunit+i,file = ob_out(i)%filename,recl=800)
         write (9000,*)   'OBJECT.PRT          ', ob_out(i)%filename
         
         write (iunit+i,100) hyd_hdr  !! write header !H
        enddo    
        exit
      enddo
      endif
        
      close (107)
      
      return
100   format (2a8,a8,a8, 25(a18)) !H
      end subroutine object_output_read