      subroutine channel_output
      
      use time_module
      use basin_module
      use jrw_datalib_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs channel output variables 

!   output_channel
      ch_d(jrch)%flo_in = ob(icmd)%hin%flo  / 86400. 
      ch_d(jrch)%flo_out = rtwtr / 86400. 
      ch_d(jrch)%evap = rtevp / 86400.   
      ch_d(jrch)%tloss = rttlc / 86400.  
      ch_d(jrch)%sed_in = ob(icmd)%hin%sed   
      ch_d(jrch)%sed_out = sedrch              
      ch_d(jrch)%sed_conc = sedcon             
      ch_d(jrch)%orgn_in = ob(icmd)%hin%orgn   
      ch_d(jrch)%orgn_out = ob(icmd)%hd(1)%orgn              
      ch_d(jrch)%orgp_in = ob(icmd)%hin%sedp    
      ch_d(jrch)%orgp_out = ob(icmd)%hd(1)%sedp              
      ch_d(jrch)%no3_in = ob(icmd)%hin%no3  
      ch_d(jrch)%no3_out = ob(icmd)%hd(1)%no3                     
      ch_d(jrch)%nh4_in = ob(icmd)%hin%nh3    
      ch_d(jrch)%nh4_out = ob(icmd)%hd(1)%nh3               
      ch_d(jrch)%no2_in = ob(icmd)%hin%no2 
      ch_d(jrch)%no2_out = ob(icmd)%hd(1)%no2                       
      ch_d(jrch)%solp_in = ob(icmd)%hin%solp         
      ch_d(jrch)%solp_out = ob(icmd)%hd(1)%solp                   
      ch_d(jrch)%chla_in = ob(icmd)%hin%chla     
      ch_d(jrch)%chla_out = ob(icmd)%hd(1)%chla                   
      ch_d(jrch)%cbod_in = ob(icmd)%hin%cbod    
      ch_d(jrch)%cbod_out = ob(icmd)%hd(1)%cbod                    
      ch_d(jrch)%dis_in = ob(icmd)%hin%dox         
      ch_d(jrch)%dis_out = ob(icmd)%hd(1)%dox                     
      ch_d(jrch)%solpst_in = ob(icmd)%hin%psol      
      ch_d(jrch)%solpst_out = ob(icmd)%hd(1)%psol                  
      ch_d(jrch)%sorbpst_in = ob(icmd)%hin%psor     
      ch_d(jrch)%sorbpst_out = ob(icmd)%hd(1)%psor                  
      ch_d(jrch)%react = reactw                                 
      ch_d(jrch)%volat = volatpst                           
      ch_d(jrch)%setlpst = setlpst                              
      ch_d(jrch)%resuspst = resuspst                            
      ch_d(jrch)%difus = -difus                                 
      ch_d(jrch)%reactb = reactb                               
      ch_d(jrch)%bury = bury                                    
      ch_d(jrch)%sedpest = sedpest                              
      ch_d(jrch)%bacp = ob(icmd)%hd(1)%bacp                        
      ch_d(jrch)%baclp = ob(icmd)%hd(1)%baclp                       
      ch_d(jrch)%met1 = ob(icmd)%hd(1)%met1                         
      ch_d(jrch)%met2 = ob(icmd)%hd(1)%met2                         
      ch_d(jrch)%met3 = ob(icmd)%hd(1)%met3                          
      ch_d(jrch)%sand_in = ob(icmd)%hin%san 
      ch_d(jrch)%sand_out = ob(icmd)%hd(1)%san                         
      ch_d(jrch)%silt_in = ob(icmd)%hin%sil         
      ch_d(jrch)%silt_out = ob(icmd)%hd(1)%sil                       
      ch_d(jrch)%clay_in = ob(icmd)%hin%cla            
      ch_d(jrch)%clay_out = ob(icmd)%hd(1)%cla                         
      ch_d(jrch)%smag_in = ob(icmd)%hin%sag             
      ch_d(jrch)%smag_out = ob(icmd)%hd(1)%sag                       
      ch_d(jrch)%lag_in = ob(icmd)%hin%lag           
      ch_d(jrch)%lag_out = ob(icmd)%hd(1)%lag                        
      ch_d(jrch)%grvl_in = ob(icmd)%hin%grv          
      ch_d(jrch)%grvl_out = ob(icmd)%hd(1)%grv                      
      ch_d(jrch)%bnk_ero = bnkrte
      ch_d(jrch)%ch_deg = degrte
!!    Channel Deposition (Only new deposits during the current time step)
      if (ch(jrch)%depch >= ch(jrch)%depprch) then
	  ch_d(jrch)%ch_dep = ch(jrch)%depch - ch(jrch)%depprch
	else
	  ch_d(jrch)%ch_dep = 0.
	end if
!!    Floodplain Deposition (Only new deposits during the current time step)
      if (ch(jrch)%depfp >= ch(jrch)%depprfp) then
	  ch_d(jrch)%fp_dep = ch(jrch)%depfp - ch(jrch)%depprfp
	else
	  ch_d(jrch)%fp_dep = 0.
	end if
!!    Total suspended sediments (only silt and clay)
      if (ch_sed(jsed)%eqn == 0) then
        ch_d(jrch)%tot_ssed = sedrch
      else
        ch_d(jrch)%tot_ssed = rch_sil + rch_cla
      endif

      ch_m(jrch) = ch_m(jrch) + ch_d(jrch)
      
!!!!! daily print
      if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                            .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
        if (pco%chan == 'day') then
          write (4400,100) time%day, time%yrc, jrch, ch_d(jrch)
          if (pco%csvout == 'yes') then
            write (4402,'(*(g0.3,:","))') time%day, time%yrc, jrch, ch_d(jrch)
          end if 
        end if 
      end if

!!!!! monthly print
      if (time%end_mo == 1) then
        ch_y(jrch) = ch_y(jrch) + ch_m(jrch)
        if (pco%chan == 'mon') then
          write (4400,100) time%mo, time%yrc, jrch, ch_m(jrch)
          if (pco%csvout == 'yes') then
            write (4402,'(*(g0.3,:","))') time%mo, time%yrc, jrch, ch_m(jrch)
          end if
        end if
        ch_m(jrch) = chz
      end if

!!!!! yearly print
      if (time%end_yr == 'year') then
        ch_a(jrch) = ch_a(jrch) + ch_y(jrch)
        if (pco%chan == 'year') then 
          write (4400,100) time%day, time%yrs, jrch, ch_y(jrch)
          if (pco%csvout == 'yes') then
            write (4402,'(*(g0.3,:","))') time%day, time%yrs, jrch, ch_y(jrch)
          end if
        end if
        
        ch_y(jrch) = chz
      end if

!!!!! average annual print
      if (time%end_sim == 1 .and. pco%chan /= 'null') then
        ch_a(jrch) = ch_a(jrch) / time%yrs_prt
        write (4401,100) time%day, time%yrs, jrch, ch_a(jrch)
        if (pco%csvout == 'yes') then
          write (4403,'(*(g0.3,:","))') time%day, time%yrs, jrch, ch_a(jrch)
        end if
      end if

100   format (2i6,i8,60(1x,e15.4))
      return
      end subroutine channel_output