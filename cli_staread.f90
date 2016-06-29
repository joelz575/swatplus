    subroutine cli_staread 

    use input_file_module

    character (len=500) :: header
    character (len=80) :: titldum
    integer :: eof, imax
    eof = 0
    imax = 0
    mwst = 0

    inquire (file=in_cli%weat_sta, exist=i_exist)
    if (i_exist == 0 .or. in_cli%weat_sta == 'null') then
        mwst = 1
        allocate (wst_pointer(0:1))
        allocate (wst(0:1))
        allocate (npcp(0:1))
        npcp = 1
    else
        do
            !! read weather stations data from weather.wst - gages and meas/gen
            open (107,file=in_cli%weat_sta)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            !! determine max number for array (imax) and total number in file (mwst)
            do while (eof >= 0)
                read (107,*,iostat=eof) i
                if (eof < 0) exit
                imax = Max(imax,i)
                mwst = mwst + 1
            end do

            allocate (wst_pointer(mwst))
            allocate (wst(imax))
            do iwst = 1, imax
                allocate (wst(iwst)%weat%ts(time%step+1))
            end do
            allocate (npcp(imax))
            npcp = 1

            rewind (107)
            read (107,*) titldum
            !          allocate (raindst(imax,2))

            rewind (107)
            read (107,*) titldum
            read (107,*) header
            do iwst = 1, mwst
                read (107,*) i
                backspace (107)
                read (107,*,iostat=eof) wst_pointer(iwst), wst(i)%name,       & 
                wst(i)%wco_c
                
               do ii = 1, db_mx%wgnsta
                  if (wst(i)%wco_c%wgn == wgn(ii)%name) then            
                    wst(i)%wco%wgn = ii
                    exit
                 end if 
               end do
      
                do ii = 1, db_mx%pcpfiles
                    if (wst(i)%wco_c%pgage == pcp(ii)%filename) then            
                        wst(i)%wco%pgage = ii
                        exit
                    end if 
                end do
                do ii = 1, db_mx%tmpfiles
                    if (wst(i)%wco_c%tgage == tmp(ii)%filename) then            
                        wst(i)%wco%tgage = ii
                        exit
                    end if 
                end do
                do ii = 1, db_mx%slrfiles
                    if (wst(i)%wco_c%sgage == slr(ii)%filename) then            
                        wst(i)%wco%sgage = ii
                        exit
                    end if
                end do
                do ii = 1, db_mx%rhfiles
                    if (wst(i)%wco_c%hgage == hmd(ii)%filename) then            
                        wst(i)%wco%hgage = ii
                        exit
                    end if             
                end do 
                do ii = 1, db_mx%wndfiles
                    if (wst(i)%wco_c%wgage == wnd(ii)%filename) then            
                        wst(i)%wco%wgage = ii
                        exit
                    end if             
                end do 
                if (eof < 0) exit  
            end do
            exit
        enddo
    endif
                      
    close (107) 

    return
    end subroutine cli_staread         