      subroutine hru_output_allo

      use output_landscape_module
      use hydrograph_module
      
      implicit none
      
      integer :: mhru           !               |
      
      mhru = sp_ob%hru
     
      !!Section 3
      !!this section sets parameters related to soil and other processes

      !! dimension hru output variables
      allocate (hwb_d(mhru))
      allocate (hwb_m(mhru))
      allocate (hwb_y(mhru))
      allocate (hwb_a(mhru))
      allocate (hnb_d(mhru))
      allocate (hnb_m(mhru))
      allocate (hnb_y(mhru))
      allocate (hnb_a(mhru))
      allocate (hls_d(mhru))
      allocate (hls_m(mhru))
      allocate (hls_y(mhru))
      allocate (hls_a(mhru))
      allocate (hpw_d(mhru))
      allocate (hpw_m(mhru))
      allocate (hpw_y(mhru))
      allocate (hpw_a(mhru))

      return
      end subroutine hru_output_allo