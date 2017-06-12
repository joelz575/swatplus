     subroutine header_soils
    
     use basin_module
     !use aquifer_module
     !use channel_module
     !use reservoir_module
     !use hydrograph_module
     !use sd_channel_module
     !use parm
    
!!   open soils.out file       
      if (pco%solout == 'y') then
        open (2610,file="soilsurf_out.sol")
        write (2610,1002)       
        write (9000,*)   'SOILS               soilsurf_out.sol'
        if (pco%csvout == 'y') then
          open (2611,file="soilsurf_out.csv")
          write (2611,1002)
          write (9000,*) 'SOILS               soilsurf_out.csv'
        end if
      end if
      
 1002 format (t15,'SURFACE',t29,'-------  SOIL PROFILE  -------',/,   &
        t2,'DAY',t8,'HRU',t15,'SOL_RSD',t27,'SOL_P',t38,              &
        'NO3',t47,'ORG_N',t57,'ORG_P',t70,'CN'/,t15,                  &                   
        '(t/ha)',t25,'(kg/ha)',t35,                                   &                                   
        '(kg/ha)',t45,'(kg/ha)',t56,'(kg/ha)')
        
      return
      end subroutine header_soils 