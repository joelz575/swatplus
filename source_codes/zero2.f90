      subroutine zero2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros all array values

      use hru_module, only : bactlpq,bactlps,bactpq,bactps,bio_init,clayld,flowfr,fsred,harveff,   &
       hru,imp_trig,lagyld,lai_init,ndeat,ovrlnd,par,sagyld,sanyld,sci,                            &
       sedyld,silyld,smx,snotmp,surf_bs,twash,wpstaao,wrt
      
      use bacteria_module
      
      implicit none
      
      real :: bactlpcnst            !                 |                   
      real :: bactlpmon             !                 |  
      real :: bactlpyr              !                 |  
      real :: bactpcnst             !                 | 
      real :: bactpmon              !                 |  
      real :: bactpyr               !                 | 
      real :: cklsp                 !                 |
      real :: cmtl1cnst             !                 |
      real :: cmtl1mon              !                 |
      real :: cmtl1yr               !                 |
      real :: cmtl2cnst             !                 |
      real :: cmtl2mon              !                 |
      real :: cmtl2yr               !                 | 
      real :: cmtl3cnst             !                 |
      real :: cmtl3mon              !                 |
      real :: fcimp                 !fraction         |fraction of HRU area that is classified
                                    !                 |as directly connected impervious
      integer :: irelease           !                 |
      integer :: minpcnst           !                 |
      integer :: minpmon            !                 |
      integer :: minpyr             !                 | 
      integer :: nh3cnst            !                 |
      integer :: nh3mon             !                 |
      integer :: nh3yr              !                 |
      integer :: no2cnst            !                 |
      integer :: no2mon             !                 | 
      integer :: no2yr              !                 |
      integer :: no3cnst            !                 |
      integer :: no3mon             !                 |
      real :: fr_curb               !none             |availability factor, the fraction of the 
                                    !                 |curb length that is sweepable
      integer :: no3yr              !                 |
      real :: orgncnst              !                 |
      real :: orgnmon               !                 |
      real :: orgnyr                !                 |
      real :: orgpcnst              !                 |
      real :: orgpmon               !                 |
      real :: orgpyr                !                 |  
      real :: rchstor               !m^3 H2O          |water stored in reach 
      real :: res_out               !none             |max number of reservoir regions for output
      real :: sedmon                !                 |
      real :: sedyr                 !                 |
      real :: yldn                  !                 |
      real :: zdb                   !mm               |division term from net pesticide equation

      fr_curb = 0.
      bactlpcnst = 0.
      bactlpmon = 0.
      bactlpq = 0.
      bactlps = 0.
      bactlpyr = 0.
      bactpcnst = 0.
      bactpmon = 0.
      bactpq = 0.
      bactps = 0.
      bactpyr = 0.
      cklsp = 0.
      cmtl1cnst = 0.
      cmtl1mon = 0.
      cmtl1yr = 0.
      cmtl2cnst = 0.
      cmtl2mon = 0.
      cmtl2yr = 0.
      cmtl3cnst = 0.
      cmtl3mon = 0.
      fcimp = 0.
      flowfr = 0.
      fsred = 0.
      harveff = 0.

      irelease = 0
      imp_trig = 1
      minpcnst = 0.
      minpmon = 0.
      minpyr = 0.
      ndeat = 0
      nh3cnst = 0.
      nh3mon = 0.
      nh3yr = 0.
      no2cnst = 0.
      no2mon = 0.
      no2yr = 0.
      no3cnst = 0.
      no3mon = 0.
      no3yr = 0.
      orgncnst = 0.
      orgnmon = 0.
      orgnyr = 0.
      orgpcnst = 0.
      orgpmon = 0.
      orgpyr = 0.
      ovrlnd = 0.
      rchstor = 0.
      res_out = 0.
      sci = 0.
      sedmon = 0.
      sedyr = 0.

	  sedyld = 0.
	  sanyld = 0.
	  silyld = 0.
	  clayld = 0.
	  sagyld = 0.
	  lagyld = 0.
      smx = 0.
      snotmp = 0.
      surf_bs = 0.
      twash = 0.
      wpstaao = 0.
      wrt = 0.
      yldn = 0.
      zdb = 0.
	bio_init = 0
	lai_init = 0
	    
      return
      end