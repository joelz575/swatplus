      subroutine bac_read_lsparms
      
      use input_file_module
      use bacteria_module, only : bac_db
      use maximum_data_module
      
      implicit none

      character (len=80) :: titldum      !              |title of file
      character (len=80) :: header       !              |header of file
      integer :: ibac                    !none          |counter  
      character (len=13) :: file         !              |file name
      integer :: eof                     !              |end of file
      integer :: i                       !none          |counter
      integer :: imax                    !none          |counter 
      integer :: i_exist                 !              |check to determine if file exists
      
      eof = 0
      imax = 0    
      
      !! read bacteria properties
      inquire (file=in_bac%bacteria,exist=i_exist)
      if (i_exist == 0 .or. in_bac%bacteria == 'null') then
         allocate (bac_db(0:0))
      else
      do
        open (107,file=in_bac%bacteria)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do

        db_mx%bac = imax
          
        allocate (bac_db(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header

        do ibac = 1, db_mx%bac
          read (107,*,iostat=eof) titldum
          backspace (107)
          read (107,*,iostat=eof) bac_db(ibac)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
      
      close (107)
      
      return
      end subroutine bac_read_lsparms