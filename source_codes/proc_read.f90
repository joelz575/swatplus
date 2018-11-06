      subroutine proc_read
     
      implicit none
             
      call cli_read_atmodep
      call cli_staread

      call constit_db_read

      call soil_plant_init
      call solt_db_read
      call pest_hru_init
      call path_hru_init
      call hmet_hru_init
      call salt_hru_init

      call topo_read
      call field_read
      call hydrol_read
      
      call snowdb_read
      call soil_db_read
      call soil_lte_db_read
      
	  return
      
      end subroutine proc_read