      subroutine output_landscape_init

      use parm
      use channel_module
      use sd_channel_module
      use hru_module
      use basin_module
      use jrw_datalib_module
      use aquifer_module

      if (sp_ob%hru > 0) then
!!!  HRU - Water balance
        if (pco%wb_hru > 0) then
          open (4000,file="waterbal.hru",recl = 1500)
          write (4000,*) wb_hdr  !! hru
            if (pco%csvout == 1 .and. pco%wb_hru > 0) then
              open (4015,file="waterbal_hru.csv",recl = 1500)
              write (4015,'(*(G0.3,:","))') wb_hdr  !! hru
            end if 
          write (9000,*) 'HRU     waterbal.hru'
        endif
        open (4004,file="waterbal_aa.hru",recl = 1500)
        write (4004,*) wb_hdr   !! hru
          if (pco%csvout == 1) then
            open (4016,file="waterbal_aa_hru.csv",recl = 1500)
            write (4016,'(*(G0.3,:","))') wb_hdr   !! hru
           end if
        write (9000,*) 'HRU     waterbal_aa.hru'
!!!  HRU - Nutrient balance
        if (pco%nb_hru > 0) then
          open (4001,file="nutbal.hru", recl = 1500)
          write (4001,*) nb_hdr
            if (pco%csvout == 1 .and. pco%nb_hru > 0) then
              open (4017,file="nutbal_hru.csv", recl = 1500)
              write (4017,'(*(G0.3,:","))') nb_hdr
            end if
          write (9000,*) 'HRU     nutbal.hru'
        endif
        open (4005,file="nutbal_aa.hru", recl = 1500)
        write (4005,*) nb_hdr
        if (pco%csvout == 1) then
          open (4018,file="nutbal_aa_hru.csv", recl = 1500)
          write (4018,'(*(G0.3,:","))') nb_hdr
        end if
        write (9000,*) 'HRU     nutbal_aa.hru'
!!!  HRU - Losses
        if (pco%ls_hru > 0) then
          open (4002,file="losses.hru", recl = 1500)
          write (4002,*) ls_hdr    !! hru
            if (pco%csvout == 1 .and. pco%ls_hru > 0) then
              open (4019,file="losses_hru.csv", recl = 1500)
             write (4019,'(*(G0.3,:","))') ls_hdr    !! hru 
            end if 
          write (9000,*) 'HRU     losses.hru'
        endif
        open (4006,file="losses_aa.hru",recl = 1500)
        write (4006,*) ls_hdr  !! hru
          if (pco%csvout == 1) then 
            open (4020,file="losses_aa_hru.csv",recl = 1500)
            write (4020,'(*(G0.3,:","))') ls_hdr  !! hru
          end if 
        write (9000,*) 'HRU     losses_aa.hru'
!!!  HRU - Plant/Weather
        if (pco%pw_hru > 0) then
          open (4003,file="plantwx.hru", recl = 1500)
          write (4003,*) pw_hdr  !! hru 
            if (pco%csvout == 1 .and. pco%pw_hru > 0) then 
              open (4021,file="plantwx_hru.csv", recl = 1500)
              write (4021,'(*(G0.3,:","))') pw_hdr  !! hru
            end if 
          write (9000,*) 'HRU     plantwx.hru'
        endif
        open (4007,file="plantwx_aa.hru",recl = 1500)      
        write (4007,*) pw_hdr  !! hru
          if (pco%csvout == 1) then 
            open (4022,file="plantwx_aa_hrucsv",recl = 1500)      
            write (4022,'(*(G0.3,:","))') pw_hdr  !! hru
          end if 
        write (9000,*) 'HRU     plantwx_aa.hru'
      endif
      
 
      if (sp_ob%hru_lte > 0) then        
 !!! SWAT-DEG - Water Balance 
        if (pco%wb_sd > 0) then
          open (4100,file="waterbal.sd",recl = 1500)
          write (4100,*) wb_hdr  !! swat-deg
            if (pco%csvout == 1 .and. pco%wb_sd > 0) then 
              open (4023,file="waterbal_sd.csv",recl = 1500)
              write (4023,'(*(G0.3,:","))') wb_hdr  !! swat-deg
            end if 
          write (9000,*) 'SWAT-DEG     waterbal.sd'
        endif
        open (4104,file="waterbal_aa.sd",recl = 1500)
        write (4104,*) wb_hdr   !! swat deg 
          if (pco%csvout == 1) then 
            open (4024,file="waterbal_aa_sd.csv",recl = 1500)
            write (4024,'(*(G0.3,:","))') wb_hdr   !! swat deg 
          end if 
        write (9000,*) 'SWAT-DEG     waterbal_aa.sd'
!!!  SWAT-DEG - Nutrient Balance
!       open (4101,file="nutbal.sd", recl = 1500)  !! no nuts in SWAT-DEG
!       write (4101,*) nb_hdr
!       open (4105,file="nutbal_aa.sd", recl = 1500)
!       write (4105,*) nb_hdr
!       if (pco%csvout == 1) then 
!         open (4025,file="nutbal_sd.csv", recl = 1500)  !! no nuts in SWAT-DEG
!         write (4025,*) nb_hdr
!         open (4026,file="nutbal_aa_sd.csv", recl = 1500)
!         write (4026,*) nb_hdr
!       end if 
!!!  SWAT-DEG - Losses
        if (pco%ls_sd > 0) then
          open (4102,file="losses.sd",recl = 1500)
          write (4102,*) ls_hdr    !! swat-deg
            if (pco%csvout == 1 .and. pco%ls_sd > 0) then 
              open (4027,file="losses_sd.csv",recl = 1500)
              write (4027,'(*(G0.3,:","))') ls_hdr    !! swat-deg 
            end if 
          write (9000,*) 'SWAT-DEG     losses.sd'
        endif
        open (4106,file="losses_aa.sd",recl = 1500)
        write (4106,*) ls_hdr  !! swat-deg
        if (pco%csvout == 1) then 
          open (4028,file="losses_aa_sd.csv",recl = 1500)
          write (4028,'(*(G0.3,:","))') ls_hdr  !! swat-deg
        end if 
        write (9000,*) 'SWAT-DEG     losses_aa.sd'
!!!  SWAT-DEG - Plant/Weather
        if (pco%pw_sd > 0) then
          open (4103,file="plantwx.sd",recl = 1500) 
          write (4103,*) pw_hdr  !! swat-deg
           if (pco%csvout == 1 .and. pco%pw_sd > 0) then 
             open (4029,file="plantwx_sd.csv",recl = 1500) 
             write (4029,'(*(G0.3,:","))') pw_hdr  !! swat-deg
           end if
          write (9000,*) 'SWAT-DEG     plantwx.sd'
        endif
        open (4107,file="plantwx_aa.sd",recl = 1500)
        write (4107,*) pw_hdr !! swat-deg 
        if (pco%csvout == 1) then 
          open (4030,file="plantwx_aa_sd.csv",recl = 1500)
          write (4030,'(*(G0.3,:","))') pw_hdr !! swat-deg
        end if 
        write (9000,*) 'SWAT-DEG     plantwx_aa.sd'
      endif
      

      if (sp_ob%sub > 0 .and. time%step == 0) then   
!!! SUBBASIN - Water Balance
        if (pco%wb_sub > 0) then
          open (4200,file="waterbal.sub",recl = 1500)
          write (4200,*) wb_hdr  !! subbasin
          if (pco%csvout == 1 .and. pco%wb_sub > 0) then 
            open (4031,file="waterbal_sub.csv",recl = 1500)
            write (4031,'(*(G0.3,:","))') wb_hdr  !! subbasin
          end if 
          write (9000,*) 'SUBBASIN     waterbal.sub'
        endif
        open (4204,file="waterbal_aa.sub",recl = 1500) 
        write (4204,*) wb_hdr   !! subbasin
        if (pco%csvout == 1) then
          open (4032,file="waterbal_aa_sub.csv",recl = 1500) 
          write (4032,'(*(G0.3,:","))') wb_hdr   !! subbasin
        end if 
        write (9000,*) 'SUBBASIN     waterbal_aa.sub'
!!! SUBBASIN - Nutrient Balance
        if (pco%nb_sub > 0) then
          open (4201,file="nutbal.sub",recl = 1500)
          write (4201,*) nb_hdr
          if (pco%csvout == 1 .and. pco%nb_sub > 0) then 
            open (4033,file="nutbal_sub.csv",recl = 1500)
            write (4033,'(*(G0.3,:","))') nb_hdr
          end if 
          write (9000,*) 'SUBBASIN     nutbal.sub'
        endif
        open (4205,file="nutbal_aa.sub", recl = 1500)
        write (4205,*) nb_hdr
        if (pco%csvout == 1) then
          open (4034,file="nutbal_aa_sub.csv", recl = 1500)
          write (4034,'(*(G0.3,:","))') nb_hdr
        end if 
        write (9000,*) 'SUBBASIN     nutbal_aa.sub'
!!! SUBBASIN - Losses
        if (pco%ls_sub > 0) then
          open (4202,file="losses.sub",recl = 1500)
          write (4202,*) ls_hdr    !! subbasin
          if (pco%csvout == 1 .and. pco%ls_sub > 0) then 
            open (4035,file="losses_sub.csv",recl = 1500)
            write (4035,*) ls_hdr    !! subbasin
          end if 
          write (9000,*) 'SUBBASIN     losses.sub'
        endif
        open (4206,file="losses_aa.sub",recl = 1500)
        write (4206,*) ls_hdr  !! subbasin 
        if (pco%csvout == 1) then 
          open (4036,file="losses_aa_sub.csv",recl = 1500)
          write (4036,'(*(G0.3,:","))') ls_hdr  !! subbasin 
        end if 
        write (9000,*) 'SUBBASIN     losses_aa.sub'
!!! SUBBASIN - Plant/Weather
        if (pco%pw_sub > 0) then
          open (4203,file="plantwx.sub",recl = 1500)
          write (4203,*) pw_hdr  !! subbasin
          if (pco%csvout == 1 .and. pco%pw_sub > 0) then 
            open (4037,file="plantwx_sub.csv",recl = 1500)
            write (4037,*) pw_hdr  !! subbasin
          end if 
          end if 
          write (9000,*) 'SUBBASIN     plantwx.sub'
        endif
        open (4207,file="plantwx_aa.sub",recl = 1500)
        write (4207,*) pw_hdr  !! subbasin
        if (pco%csvout == 1) then 
          open (4038,file="plantwx_aa_sub.csv",recl = 1500)
          write (4038,'(*(G0.3,:","))') pw_hdr  !! subbasin
        end if 
        write (9000,*) 'SUBBASIN     plantwx_aa.sub'

!!!  BASIN - Water balance
 
      if (time%step == 0) then
        if (pco%wb_bsn > 0) then
          open (4300,file="waterbal.bsn",recl = 1500)
          write (4300,*) wb_hdr  !! bsn
          if (pco%csvout == 1 .and. pco%wb_bsn > 0) then 
            open (4039,file="waterbal_bsn.csv",recl = 1500)
            write (4039,'(*(G0.3,:","))') wb_hdr  !! bsn
          end if 
          write (9000,*) 'BASIN     waterbal.bsn'
        endif
        open (4304,file="waterbal_aa.bsn",recl = 1500)
        write (4304,*) wb_hdr   !! bsn
        if (pco%csvout == 1) then 
          open (4040,file="waterbal_aa_bsn.csv",recl = 1500)
          write (4040,'(*(G0.3,:","))') wb_hdr   !! bsn
        end if 
        write (9000,*) 'BASIN     waterbal_aa.bsn'
!!!  BASIN - Nutrient balance
        if (pco%nb_bsn > 0) then
          open (4301,file="nutbal.bsn", recl = 1500)
          write (4301,*) nb_hdr
          if (pco%csvout == 1 .and. pco%nb_bsn > 0) then 
            open (4041,file="nutbal_bsn.csv", recl = 1500)
            write (4041,'(*(G0.3,:","))') nb_hdr
          end if 
          write (9000,*) 'BASIN     nutbal.bsn'
        endif
        open (4305,file="nutbal_aa.bsn", recl = 1500)
        write (4305,*) nb_hdr
        if (pco%csvout == 1) then 
          open (4042,file="nutbal_aa_bsn.csv", recl = 1500)
          write (4042,'(*(G0.3,:","))') nb_hdr
        end if 
        write (9000,*) 'BASIN     nutbal_aa.bsn'
!!!  BASIN - Losses
        if (pco%ls_bsn > 0) then
          open (4302,file="losses.bsn", recl = 1500)
          write (4302,*) ls_hdr    !! bsn
          if (pco%csvout == 1 .and. pco%ls_bsn > 0) then 
            open (4043,file="losses_bsn.csv", recl = 1500)
            write (4043,'(*(G0.3,:","))') ls_hdr    !! bsn
          end if 
          write (9000,*) 'BASIN     losses.bsn'
        endif
        open (4306,file="losses_aa.bsn",recl = 1500)
        write (4306,*) ls_hdr     !! bsn
        if (pco%csvout == 1) then 
          open (4044,file="losses_aa_bsn.csv",recl = 1500)
          write (4044,'(*(G0.3,:","))') ls_hdr     !! bsn
        end if
        write (9000,*) 'BASIN     losses_aa.bsn'
!!!  BASIN - Plant/Weather
        if (pco%pw_bsn > 0) then
          open (4303,file="plantwx.bsn", recl = 1500)
          write (4303,*) pw_hdr  !! bsn 
          if (pco%csvout == 1 .and. pco%pw_bsn > 0) then 
            open (4045,file="plantwx_bsn.csv", recl = 1500)
            write (4045,'(*(G0.3,:","))') pw_hdr  !! bsn 
          end if
          write (9000,*) 'BASIN     plantwx.bsn'
        endif
        open (4307,file="plantwx_aa.bsn",recl = 1500)      
        write (4307,*) pw_hdr  !! bsn
        write (9000,*) 'BASIN     plantwx_aa.bsn'
      end if
      
!!!  CHANNEL
        if (pco%chan > 0) then
          open (4400,file="channel.out",recl = 1500)
          write (4400,*) ch_hdr !! channel
!!!   open channel output in csv format
          if (pco%csvout == 1 .and. pco%chan > 0)  then
            open (4402,file="channel.csv",recl = 1500)
            write (4402,'(*(G0.3,:","))') ch_hdr !! channel header csv format
          end if
          write (9000,*) 'CHANNEL     channel.out'
        endif
        if (sp_ob%chan > 0) then
          open (4401,file="channel_aa.out",recl = 1500)
          write (4401,*) ch_hdr   !! channel
          if (pco%csvout == 1 .and. sp_ob%chan > 0) then
            open (4403,file="channel_aa.csv",recl = 1500)
            write (4403,'(*(G0.3,:","))') ch_hdr   !! channel aa header csv format
          end if
          write (9000,*) 'CHANNEL     channel_aa.out'
        end if
              
!!!  SWAT-DEG CHANNEL
        if (pco%chan > 0) then
          open (4600,file="sd_channel.out",recl = 1500)
          write (4600,*) sdch_hdr !! swat deg channel
          if (pco%csvout == 1 .and. pco%chan > 0) then
            open (4602,file="sd_channel.csv",recl = 1500)
            write (4602,'(*(G0.3,:","))') sdch_hdr !! swat deg channel csv
          end if
          write (9000,*) 'SWAT-DEG CHANNEL     sd_channel.out'
        endif
        if (sp_ob%chandeg > 0) then
          open (4601,file="sd_channel_aa.out",recl = 1500)
          write (4601,*) sdch_hdr   !! swat deg channel
          if (pco%csvout == 1) then
            open (4603,file="sd_channel_aa.csv",recl = 1500)
            write (4603,'(*(G0.3,:","))') sdch_hdr   !! swat deg channel csv
          end if
          write (9000,*) 'SWAT-DEG CHANNEL     sd_channel_aa.out'
        end if
        
!!!  AQUIFER
        if (pco%aqu > 0) then
          open (4500,file="aquifer.out",recl = 1500)
          write (4500,*) aqu_hdr !! aquifer
         if (pco%csvout == 1 .and. pco%aqu > 0) then
            open (4502,file="aquifer.csv",recl = 1500)
            write (4502,'(*(G0.3,:","))') sdch_hdr   !! aquifer csv
          end if
          write (9000,*) 'AQUIFER     aquifer.out'
        endif
        if (sp_ob%aqu > 0) then
          open (4501,file="aquifer_aa.out",recl = 1500)
          write (4501,*) aqu_hdr   !! aquifer
          if (pco%csvout == 1) then
            open (4503,file="aquifer_aa.csv",recl = 1500)
            write (4503,'(*(G0.3,:","))') sdch_hdr   !! aquifer csv
          end if
          write (9000,*) 'AQUIFER     aquifer_aa.out'
        end if
        
!!! CROP YIELDS
      if (sp_ob%hru > 0) then
        open (4008,file="crop_yld_aa.out")
          write (4008,1000)
1000    format (1x,' TIME',1x,' YEAR',1x,'   UNIT',1x,'   PLANTNM',   &
                 1x,'   YIELD')
        if (pco%csvout == 1 .and. sp_ob%hru > 0) then
            open (4009,file="crop_yld_aa.csv")
            write (4009,'(*(G0.3,:","))') "time","year","unit","plantnm","yield"
        end if 
        write (9000,*) 'CROP     crop_yld_aa.out'
      end if
           
      return
      end subroutine output_landscape_init