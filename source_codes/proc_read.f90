      subroutine proc_read
     
      implicit none
             
      call cli_read_atmodep
      call cli_staread
   
      call sep_read
      call solt_db_read
      call topo_read
      call field_read
      call hydrol_read
      
      call sdr_read
      call snowdb_read
      call soil_db_read
      call soil_lte_db_read
      
	  return
      
      end subroutine proc_read