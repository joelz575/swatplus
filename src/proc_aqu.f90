      subroutine proc_aqu
    
      use hydrograph_module
      
      implicit none

      call aqu_read
      call aqu_initial
      call aqu_read_init
      
	  return
      
      end subroutine proc_aqu
