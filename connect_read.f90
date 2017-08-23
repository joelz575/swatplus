      subroutine connect_read(con_file, obtyp, nspu1, nspu, nhyds, ndsave)
    
      !  con_file ==> connect file for spatial object
      !  nspu     ==> number of spatial units
      !  nspu1    ==> first object number of the spatial unit
      !  nhyds    ==> number of hydrographs for each object
      !  ndsave   ==> number of days of hydrographs to save for subdaily

      use hydrograph_module
      use constituent_mass_module
      use time_module
      use climate_parms
      use jrw_datalib_module
      
      integer, intent(in) :: nhyds, ndsave, nspu, nspu1

      character (len=80) :: titldum, header
      character (len=16) :: namedum, con_file
      character (len=3) :: iob_out, ihtyp
      character (len=8) :: obtyp
      integer :: isp, cmdno, idone, hydno, cmd_prev, ob1, ob2
      integer :: iobj_tot
      integer :: eof, i, imax, nout
      
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
            do i = ob1, ob2
              ob(i)%typ = obtyp
              allocate (ob(i)%hd(nhyds))
              if (time%step > 0) then
                ob(i)%day_max = ndsave
                allocate (ob(i)%ts(ob(i)%day_max,time%step))
                allocate (ob(i)%tsin(time%step))
              end if
              read (107,*,iostat=eof) k, ob(i)%name, ob(i)%area_ha, ob(i)%lat, ob(i)%long, ob(i)%elev,      &
                ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot
              ob(i)%num = k
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
                read (107,*,iostat=eof) k, ob(i)%name, ob(i)%area_ha, ob(i)%lat, ob(i)%long, ob(i)%elev,    &
                  ob(i)%props, ob(i)%wst_c, ob(i)%constit, ob(i)%props2, ob(i)%ruleset, ob(i)%src_tot,      &
                  (ob(i)%obtyp_out(isp), ob(i)%obtypno_out(isp), ob(i)%htyp_out(isp),                       &
                  ob(i)%frac_out(isp), isp = 1, nout)
                if (eof < 0) exit
              else
                !! set outflow object type to 0 - needed in final hyd_read_connect loop 
                allocate (ob(i)%obtypno_out(1))
                ob(i)%obtypno_out(1) = 0
              end if
              !set the constituents
              ics = ob(i)%constit
              if (ics > 0) then
                if (cs_db(ics)%pest_com /= "null") then
                  ipestmx = pestcom_db(ics)%num
                  allocate (obcs(i)%pests(ipestmx))
                  allocate (obcs(i)%pest_num(ipestmx))
                  obcs(i)%num_pests = pestcom_db(ics)%num     !set number of pesticides of object
                  obcs(i)%pests = pestcom_db(ics)%pests       !set pesticide name array of object
                  obcs(i)%pest_num = pestcom_db(ics)%num_db   !set pesticide database number array of object
                end if
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
      end subroutine connect_read