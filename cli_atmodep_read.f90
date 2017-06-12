      subroutine cli_atmodep_read
      
      use parm
      use basin_module
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: file
      integer :: eof 
  
      matmodep = 0
      eof = 0


      inquire (file=in_cli%atmo_cli,exist=i_exist)
      if (i_exist == 0 .or. in_cli%atmo_cli == 'null') then
        !!no filename 
        allocate (atmodep(0:0))
        db_mx%atmodep = 0
      else
        do
          open (127,file = in_cli%atmo_cli)
            do iii = 1, 5
              read (127,*,iostat=eof) titldum
              if (eof < 0) exit
            end do
              read (127,*,iostat=eof) matmodep, momax
              if (eof < 0) exit
              allocate (atmodep(0:matmodep))
              db_mx%atmodep = matmodep
          
       if (bsn_cc%atmo == 1) then
          do iadep = 1, matmodep
            read (127,*,iostat=eof)   atmodep(iadep)%no3_rf,                 & 
                                      atmodep(iadep)%nh4_rf,                 &
                                      atmodep(iadep)%no3_dry,                &
                                      atmodep(iadep)%nh4_dry
            if (eof < 0) exit
          end do
       else if (bsn_cc%atmo == 2) then
            read (127,1001,iostat=eof) mo_atmo1, iyr_atmo1
1001        format (2i6)
            do iadep = 1, matmodep
              allocate (atmodep(iadep)%no3_rfmo(momax))
              allocate (atmodep(iadep)%nh4_rfmo(momax))
              allocate (atmodep(iadep)%no3_drymo(momax))
              allocate (atmodep(iadep)%nh4_drymo(momax))
              read (127,*) (atmodep(iadep)%nh4_rfmo(imo), imo = 1,momax)
              read (127,*) (atmodep(iadep)%no3_rfmo(imo), imo = 1,momax)
              read (127,*) (atmodep(iadep)%nh4_drymo(imo),imo = 1,momax)
              read (127,*) (atmodep(iadep)%no3_drymo(imo),imo = 1,momax)
            end do
       endif
       exit
       enddo
      endif

      return
      end subroutine cli_atmodep_read