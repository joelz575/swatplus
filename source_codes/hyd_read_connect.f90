      subroutine hyd_read_connect(con_file, obtyp, nspu1, nspu, nhyds, ndsave)
    
      !  con_file ==> connect file for spatial object
      !  nspu     ==> number of spatial units
      !  nspu1    ==> first object number of the spatial unit
      !  nhyds    ==> number of hydrographs for each object
      !  ndsave   ==> number of days of hydrographs to save for subdaily

      use hydrograph_module
      use constituent_mass_module
      use time_module
      use climate_module
      use maximum_data_module
      
      implicit none
      
      integer, intent(in) :: nhyds    !           |
      integer, intent(in) :: ndsave   !           |
      integer, intent(in) :: nspu     !           | 
      integer, intent(in) :: nspu1    !           |
      character (len=80) :: titldum   !           |title of file
      character (len=80) :: header    !           |header of file
      character (len=16) :: namedum   !           |
      integer :: eof                  !           |end of file
      integer :: imax                 !none       |determine max number for array (imax) and total number in file
      integer :: i_exist              !none       |check to determine if file exists
      character (len=16) ::con_file   !           |
      character (len=3) :: ihtyp      !           |
      character (len=8) :: obtyp      !           |
      integer :: isp                  !none       |counter
      integer :: cmd_prev             !none       |previous command (object) number
      integer :: ob1                  !none       |beginning of loop
      integer :: ob2                  !none       |ending of loop
      integer :: iob
      integer :: i                    !none       |counter
      integer :: nout                 !           |       
      integer :: k                    !           |
      integer :: ics                  !           |
      integer :: ipestmx              !           |
      integer :: ihyd                 !           |hydrograph counter
      integer :: npests               !           |pesticides counter
      integer :: npaths               !           |pathogens counter
      integer :: nmetals              !           |heavy metals counter
      integer :: nsalts                !           |salts counter
      
      eof = 0
      imax = 0
      cmd_prev = 0
  
      !! read hru spatial data
      inquire (file=con_file, exist=i_exist)
      if (i_exist /= 0) then
        do
          open (107,file=con_file)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit

          if (nspu > 0) then
            ob1 = nspu1
            ob2 = nspu1 + nspu - 1
            
            !find maximum id number
            !do i = ob1, ob2
            !  read (107,*,iostat=eof) k
            !  id_max = Max (id_max, k)
            !end do
                

            do i = ob1, ob2
              ob(i)%typ = obtyp
              allocate (ob(i)%hd(nhyds))
              if (cs_db%num_tot_con > 0) then
                allocate (obcs(i)%hd(nhyds))
                
                do ihyd = 1, nhyds
                  npests = cs_db%num_pests
                  if (npests > 0) then 
                    allocate (obcs(i)%hd(ihyd)%pest(npests))
                    allocate (obcs(i)%hin%pest(npests))
                    allocate (obcs(i)%hin_s%pest(npests))
                  end if
                  npaths = cs_db%num_paths
                  if (npaths > 0) then 
                    allocate (obcs(i)%hd(ihyd)%path(npaths))
                    allocate (obcs(i)%hin%path(npaths))
                    allocate (obcs(i)%hin_s%path(npaths))
                  end if
                  nmetals = cs_db%num_metals
                  if (nmetals > 0) then 
                    allocate (obcs(i)%hd(ihyd)%hmet(nmetals))
                    allocate (obcs(i)%hin%hmet(nmetals))
                    allocate (obcs(i)%hin_s%hmet(nmetals))
                  end if
                  nsalts = cs_db%num_salts
                  if (nsalts > 0) then 
                    allocate (obcs(i)%hd(ihyd)%salt(nsalts))
                    allocate (obcs(i)%hin%salt(nsalts))
                    allocate (obcs(i)%hin_s%salt(nsalts))
                  end if
                end do
              end if
                
              if (time%step > 0) then
                ob(i)%day_max = ndsave
                allocate (ob(i)%ts(ob(i)%day_max,time%step))
                allocate (ob(i)%tsin(time%step))
              end if
              read (107,*,iostat=eof) ob(i)%num, ob(i)%name, ob(i)%gis_id, ob(i)%area_ha, ob(i)%lat, ob(i)%long, ob(i)%elev,   &
                ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot
              if (eof < 0) exit

              if (ob(i)%src_tot > 0) then
                nout = ob(i)%src_tot
                allocate (ob(i)%obj_out(nout))
                allocate (ob(i)%obtyp_out(nout))
                allocate (ob(i)%obtypno_out(nout))
                allocate (ob(i)%htyp_out(nout))
                allocate (ob(i)%ihtyp_out(nout))
                allocate (ob(i)%frac_out(nout))
                allocate (ob(i)%hout_m(nout))
                allocate (ob(i)%hout_y(nout))
                allocate (ob(i)%hout_a(nout))
                backspace (107)
                read (107,*,iostat=eof) ob(i)%num, ob(i)%name, ob(i)%gis_id, ob(i)%area_ha, ob(i)%lat, ob(i)%long, ob(i)%elev,    &
                  ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot,      &
                  (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), ob(i)%htyp_out(isp),                       &
                  ob(i)%frac_out(isp), isp = 1, nout)
                if (eof < 0) exit
              else
                !! set outflow object type to 0 - needed in final hyd_read_connect loop 
                allocate (ob(i)%obtypno_out(1))
                ob(i)%obtypno_out(1) = 0
              end if

              !set arrays for flow duration curves
              !if (printcode == 1) then
                allocate (ob(i)%fdc_ll(366))
                allocate (ob(i)%fdc_lla(time%nbyr))
                allocate (ob(i)%fdc%p(time%nbyr))
              !end if
          
            end do
          endif
          exit
        enddo
      endif
      
      !crosswalk weather station 
      do i = ob1, ob2
        call search (wst_n, db_mx%wst, ob(i)%wst_c, ob(i)%wst)
      end do
      
      close (107)
      
      return
      end subroutine hyd_read_connect