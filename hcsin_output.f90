      subroutine hcsin_output
    
      use hydrograph_module
      use time_module
      use constituent_mass_module

      implicit none
      
      integer :: iin              !none          |counter
      integer :: ipest            !none          |pesticide counter
      integer :: ipath            !none          |pathogen counter
      integer :: imetal           !none          |heavy metal counter
      integer :: isalt            !none          |salt counter
           
      do icmd = 1, sp_ob%objs
        do iin = 1, ob(icmd)%rcv_tot
        !! daily print
         if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
          if (pco%hyd%d == 'y') then
            if (cs_db%num_pests > 0) then        !! pests
            write (2708,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                          &
             ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),               &
             ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == 'y') then
                write (2724,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),           &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2712,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == 'y') then
                write (2728,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2716,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == 'y') then
                write (2732,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2720,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),             &
                ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == 'y') then
                write (2736,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_d(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts
            end if       
          end if
                                                    
        hcs1 = obcs(icmd)%hcs_m(iin)
        hcs2 = obcs(icmd)%hcs_d(iin)
        call constit_hyd_add
        obcs(icmd)%hcs_m(iin) = hcs2
        obcs(icmd)%hcs_d(iin) = hin_csz

        !! monthly print
        if (time%end_mo == 1) then
          if (pco%hyd%m == 'y') then
              if (cs_db%num_pests > 0) then        !! pests
                write (2709,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                          &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),               &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == 'y') then
                write (2725,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),           &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2713,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == 'y') then
                write (2729,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2717,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == 'y') then
                write (2733,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2721,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),             &
                ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == 'y') then
                write (2737,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_m(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts
            end if
          end if
        
        hcs1 = obcs(icmd)%hcs_y(iin)
        hcs2 = obcs(icmd)%hcs_m(iin)
        call constit_hyd_add
        obcs(icmd)%hcs_y(iin) = hcs2
        obcs(icmd)%hcs_m(iin) = hin_csz
        
        !! yearly print
        if (time%end_yr == 1) then
          if (pco%hyd%y == 'y') then
            if (cs_db%num_pests > 0) then        !! pests
                write (2710,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                          &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),               &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == 'y') then
                write (2726,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),           &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2714,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == 'y') then
                write (2730,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2718,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == 'y') then
                write (2734,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2722,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),             &
                ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == 'y') then
                write (2738,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_y(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts              
           end if
          end if
          
        hcs1 = obcs(icmd)%hcs_a(iin)
        hcs2 = obcs(icmd)%hcs_y(iin)
        call constit_hyd_add
        obcs(icmd)%hcs_a(iin) = hcs2
        obcs(icmd)%hcs_y(iin) = hin_csz
        
        !! average annual print
        if (time%end_sim == 1 .and. pco%hyd%a == 'y') then
          ob(icmd)%hin_a(iin) = ob(icmd)%hin_a(iin) / time%yrs_prt
            if (cs_db%num_pests > 0) then        !! pests
                write (2711,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                          &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),               &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == 'y') then
                write (2727,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),           &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%pest(ipest), ipest = 1, cs_db%num_pests)
              end if                             !! cvs pests
            end if                               !! pests
              
              if (cs_db%num_paths > 0) then      !! paths
                write (2715,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%path(ipath), ipath = 1, cs_db%num_paths)          
              if (pco%csvout == 'y') then
                write (2731,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%path(ipath), ipath = 1, cs_db%num_paths)
              end if                            !! cvs paths
              end if                            !! paths
              
              if (cs_db%num_metals > 0) then    !! metals
                write (2719,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)          
              if (pco%csvout == 'y') then
                write (2735,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%hmet(imetal), imetal = 1, cs_db%num_metals)
              end if                            !! cvs metals
              end if                            !! metals
              
              if (cs_db%num_salts > 0) then     !! salts
                write (2723,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,                       &
                ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),             &
                ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%salt(isalt), isalt = 1, cs_db%num_salts)          
              if (pco%csvout == 'y') then
                write (2739,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num,        &
                 ob(icmd)%obtyp_in(iin), ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin),            &
                 ob(icmd)%frac_in(iin), (obcs(icmd)%hcs_a(iin)%salt(isalt), isalt = 1, cs_db%num_salts)
              end if                            !! cvs salts
              end if                            !! salts              
        end if
        
        end do   !! sp_ob%objs
      end do     !! ob(icmd)%rcv_tot 
      
      end subroutine hcsin_output