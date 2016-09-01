       subroutine readcio_read 
       
       character (len=80) :: titldum
       character (len=15) :: name
       integer :: eof
       
       eof = 0
       
       !! read file.cio
       inquire (file="file.cio", exist=i_exist)
       if (i_exist /= 0) then
         open (107,file="file.cio")
         read (107,*,iostat=eof) titldum
      do i = 1, 23
         read (107,*,iostat=eof) name, in_sim  
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cli
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_con
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cha
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_res
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_sub
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_hru
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_delr
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_aqu
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_herd
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_watrts
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_link
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_basin
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_hyd
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_exco
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_bac
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_str
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_parmdb
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_ops
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_sch
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_lum
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_chg
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_init
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_sol
         if (eof < 0) exit
         read (107,*,iostat=eof) name, in_cond
         if (eof < 0) exit
		 read (107,*,iostat=eof) name, in_const
		 if (eof < 0) exit
         exit
      enddo
      endif

       close (107)     
       return
      end subroutine readcio_read  