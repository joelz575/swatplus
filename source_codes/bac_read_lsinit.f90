      subroutine bac_read_lsinit
      
      use input_file_module
      use bacteria_module
      use maximum_data_module
      
      implicit none

      character (len=80) :: titldum      !             |title of file
      character (len=80) :: header       !             |header of file
      integer :: mbac_db                 !             |
      integer :: ibac                    !none         |counter
      integer :: eof                     !             |end of file 
      integer :: i_exist                 !             |check to determine if file exists
      integer :: mbac                    !             |
      integer :: ibact                   !none         |counter
           
      mbac_db = 0
      
      !! allocate and initialize bacteria in soil and plant
      inquire (file=in_init%path_soil,exist=i_exist)
      if (i_exist == 0 .or. in_init%path_soil == "null") then
         allocate (bact(0:0))
         db_mx%bactdb = 0
      else
        do
          open (107,file=in_init%path_soil)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) mbac_db
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
          if (mbac_db > 0) then
            allocate (bact(mbac_db))
            do ibac = 1, mbac_db
              read (107,*,iostat=eof) bact(ibac)%num
              if (eof < 0) exit
              mbac = bact(ibac)%num
              allocate (bact(ibac)%bac(mbac+1))
              do ibact = 1, bact(ibac)%num
                read (107,*,iostat=eof) bact(mbac)%bac(ibac)%num_db,      &
                 bact(ibac)%bac(ibact)%plt, bact(ibac)%bac(ibact)%sol,    &
     &           bact(ibac)%bac(ibact)%sor
                if (eof < 0) exit
              end do
            end do
            db_mx%bactdb = mbac_db + 1
          end if
            exit
        end do
      end if
        close (107)

      return
      end subroutine bac_read_lsinit