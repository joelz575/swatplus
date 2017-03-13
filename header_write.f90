     subroutine header_write
    
     use basin_module
     use aquifer_module
     use channel_module
     use reservoir_module
     use hydrograph_module
     use parm
    
!!   open soils.out file       
      if (pco%solout /= 'avann' .and. pco%solout /= 'null') then
        open (121,file="soils.out")
        write (121,1002)       
        write (9000,*)   'SOILS               soils.out'
        if (pco%csvout == 'yes') then
          open (123,file="soils.csv")
          write (123,1002)
          write (9000,*) 'SOILS               soils.csv'
        end if
      end if
      
!!   open mgt.out file 
      if (pco%mgtout /= 'avann' .and. pco%mgtout /= 'null') then
        open (143,file="mgt.out",recl=800)
        write (143,*) mgt_hdr
        write (143,*) mgt_hdr_unt1
        write (143,*) mgt_hdr_unt2
        write (9000,*) 'MGT                 mgt.out'
      end if
      
!!  yield biomass file
      if (pco%mgtout /= 'avann' .and. pco%mgtout /= 'null') then
        open (4700,file="yield.out", recl=800)
        write (9000,*) 'YLD                 yield.out'
        if (pco%csvout == 'yes') then
          open (4701,file="yield.csv", recl=800)
          write (9000,*) 'YLD                 yield.csv'
        end if
      end if  
      
      if (pco%hydcon /= 'avann' .and. pco%hydcon /= 'null') then
        open (7000,file="hydcon.out")
        write (9000,*) 'HYDCON              hydcon.out'
          if (pco%csvout == 'yes') then
            open (7001,file="hydcon.csv")
            write (9000,*) 'HYDCON              hydcon.csv'
          end if
      end if
      
      if (pco%hyd /= 'avann' .and. pco%hyd /= 'null') then
        open (5001,file="hyd-out.out",recl=800)
        write (5001,*) hyd_hdr
        write (9000,*) 'HYDOUT              hyd-out.out'
        if (pco%csvout == 'yes') then
          open (5007,file="hyd-out.csv",recl=800)
          write (5007,'(*(G0.3,:","))') hyd_hdr
          write (9000,*)   'HYDOUT              hyd-out.csv'
        end if 
       
        open (5004,file="hyd-in.out",recl=800)
        write (5004,*) hyd_hdr
        write (9000,*)     'HYDIN               hyd-in.out'
          if (pco%csvout == 'yes') then
            open (5008,file="hyd-in.csv",recl=800)
            write (5008,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'HYDIN               hyd-in.csv'
          end if 
        
        open (5005,file="deposition.out",recl=800)
        write (5005,*) hyd_hdr
        write (9000,*) 'DEPO                deposition.out'
          if (pco%csvout == 'yes') then
            open (5009,file="deposition.csv",recl=800)
            write (5009,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'DEPO                deposition.csv'
          end if
      end if
      
      if (pco%res /= 'avann' .and. sp_ob%res > 0 ) then
        open (5002,file="reservoir.out",recl=800)
        write (9000,*) 'RES                 reservoir.out'
        write (5002,*) res_hdr
        write (5002,*) res_hdr_unt
          if (pco%csvout == 'yes') then
            open (5006,file="reservoir.csv",recl=800)
            write (5006,'(*(G0.3,:","))') res_hdr
            write (5006,'(*(G0.3,:","))') res_hdr_unt
            write (9000,*) 'RES                 reservoir.csv'
          end if
      end if
      
!!! print average annual always
      if (sp_ob%res > 0) then
        open (7008,file="reservoir_aa.out",recl = 800)
        write (7008,*) res_hdr
        write (7008,*) res_hdr_unt
        write (9000,*) 'RES                 reservoir_aa.out'
          if (pco%csvout == 'yes') then
            open (7009,file="reservoir_aa.csv",recl=800)
            write (7009,'(*(G0.3,:","))') res_hdr
            write (7009,'(*(G0.3,:","))') res_hdr_unt
            write (9000,*) 'RES                 reservoir_aa.csv'
          end if
      end if
          
      
      if (pco%fdcout == 'yes') then
        open (6000,file="flow_duration_curve.out", recl=800)
        write (9000,*) 'FDC                 flow_duration_curve.out'
        write (6000,*) fdc_hdr
      end if 

!!!!!! hru-out.cal - hru soft calibration output including soft and predicted budgets and 
!!!!!! calibration parameter adjustments
	  open (4999,file="hru-out.cal", recl = 800)
	  write (9000,*)   'HRU SOFT CALIB OUT      hru-out.cal'
	  write (4999,*) calb_hdr
      
!!!!!! hru-new.cal - hru soft calibration output file.  The same format as calibration.upd and
!!!!!! can be used as input (calibration.upd) in subsequent simulations
      open (5000,file="hru-new.cal", recl = 800)
      write (5000,*) ' calibration.upd_developed_from_soft_data_calibration'
	  write (9000,*)   'HRU SOFT OUT CALIB      hru-new.cal'
      write (5000,*) calb3_hdr
      
!!!!!! hru-lte-out.cal - hru lte soft calibration output including soft and predicted budgets and 
!!!!!! calibration parameter adjustments
      open (5003,file="hru-lte-out.cal", recl = 800)
	  write (9000,*)   'LTE SOFT OUT CALIB      hru-lte-out.cal'
	  write (5003,*) calb_hdr
	  
!!!!!! hru-lte-new.cal - hru lte soft calibration output file.  The same format as hru-lte.hru and
!!!!!! can be used as input (hru-lte.hru) in subsequent simulations 
      open (5002,file="hru-lte-new.cal", recl = 800)
	  write (9000,*)   'LTE SOFT CALIB INPUT    hru-lte-new.cal'
	  write (5002,*) calb2_hdr
      
!! BASIN AQUIFER OUTPUT
        if (pco%aqu_bsn /= 'avann' .and. pco%aqu_bsn /= 'null') then
          open (4504,file="bsn_aqu.out", recl = 1500)
          write (4504,*) aqu_hdr  
          write (9000,*) 'BASIN AQUIFER               bsn_aqu.out'
          if (pco%csvout == 'yes') then 
            open (4505,file="bsn_aqu.csv", recl = 1500)
            write (4505,'(*(G0.3,:","))') aqu_hdr
            write (9000,*) 'BASIN AQUIFER               bsn_aqu.csv'
          end if
        endif
        
        open (4506,file="bsn_aqu_aa.out",recl = 1500)      
        write (4506,*) aqu_hdr  
        write (9000,*) 'BASIN AQUIFER AA               bsn_aqu_aa.out'
       if (pco%csvout == 'yes') then 
          open (4507,file="bsn_aqu_aa.csv",recl = 1500)
          write (4507,'(*(G0.3,:","))') aqu_hdr 
          write (9000,*) 'BASIN AQUIFER               bsn_aqu_aa.csv'
       end if
!! BASIN AQUIFER OUTPUT

!! BASIN RESERVOIR OUTPUT
        if (pco%res_bsn /= 'avann' .and. pco%res_bsn /= 'null') then
          open (7002,file="bsn_res.out", recl = 1500)
          write (7002,*) res_hdr  
          write (9000,*) 'BASIN RESERVOIR               bsn_res.out'
          if (pco%csvout == 'yes') then 
            open (7003,file="bsn_res.csv", recl = 1500)
            write (7003,'(*(G0.3,:","))') res_hdr
            write (9000,*) 'BASIN RESERVOIR               bsn_res.csv'
          end if
        endif
        
        open (7006,file="bsn_res_aa.out",recl = 1500)      
        write (7006,*) res_hdr  
        write (9000,*) 'BASIN RESERVOIR AA               bsn_res_aa.out'
       if (pco%csvout == 'yes') then 
          open (7007,file="bsn_res_aa.csv",recl = 1500)
          write (7007,'(*(G0.3,:","))') res_hdr 
          write (9000,*) 'BASIN RESERVOIR               bsn_res_aa.csv'
       end if
!! BASIN RESERVOIR OUTPUT
      
!! BASIN CHANNEL OUTPUT
        if (pco%chan_bsn /= 'avann' .and. pco%chan_bsn /= 'null') then
          open (4404,file="bsn_chan.out", recl = 1500)
          write (4404,*) ch_hdr  
          write (9000,*) 'BASIN CHANNEL               bsn_chan.out'
          if (pco%csvout == 'yes') then 
            open (4405,file="bsn_chan.csv", recl = 1500)
            write (4405,'(*(G0.3,:","))') ch_hdr
            write (9000,*) 'BASIN CHANNEL               bsn_chan.csv'
          end if
        endif
        
        open (4406,file="bsn_chan_aa.out",recl = 1500)      
        write (4406,*) ch_hdr  
        write (9000,*) 'BASIN CHANNEL AA               bsn_chan_aa.out'
       if (pco%csvout == 'yes') then 
          open (4407,file="bsn_chan_aa.csv",recl = 1500)
          write (4407,'(*(G0.3,:","))') ch_hdr 
          write (9000,*) 'BASIN CHANNEL               bsn_chan_aa.csv'
       end if
!! BASIN CHANNEL OUTPUT

!! BASIN RECALL OUTPUT
        if (pco%recall_bsn /= 'avann' .and. pco%recall_bsn /= 'null') then
          open (8000,file="bsn_rec.out", recl = 1500)
          write (8000,*) hyd_hdr  
          write (9000,*) 'BASIN RECALL               bsn_rec.out'
          if (pco%csvout == 'yes') then 
            open (8002,file="bsn_rec.csv", recl = 1500)
            write (8002,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'BASIN RECALL              bsn_rec.csv'
          end if
        endif
        
        open (8001,file="bsn_rec_aa.out",recl = 1500)      
        write (8001,*) hyd_hdr  
        write (9000,*) 'BASIN RECALL AA               bsn_rec_aa.out'
       if (pco%csvout == 'yes') then 
          open (8003,file="bsn_rec_aa.csv",recl = 1500)
          write (8003,'(*(G0.3,:","))') hyd_hdr 
          write (9000,*) 'BASIN RECALL               bsn_rec_aa.csv'
       end if
!! BASIN RECALL OUTPUT


 1002 format (t15,'SURFACE',t29,'-------  SOIL PROFILE  -------',/,   &
        t8,'DAY',t15,'SOL_RSD',t27,'SOL_P',t38,                       &
        'NO3',t47,'ORG_N',t57,'ORG_P',t70,'CN'/,t16,                  &                   
        '(t/ha)',t25,'(kg/ha)',t35,                                   &                                   
        '(kg/ha)',t45,'(kg/ha)',t56,'(kg/ha)')
      
      return
      end subroutine header_write  