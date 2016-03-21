      subroutine smp_bmpfixed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies fixed removal eff. from the .ops to upland loads 
                  
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    bmp_sed(:)    |%             | Sediment removal by BMP
!!    bmp_pp(:)     |%             | Particulate P removal by BMP
!!    bmp_sp(:)     |%             | Soluble P removal by BMP  
!!    bmp_pn(:)     |%             | Particulate N removal by BMP  
!!    bmp_sn(:)     |%             | Soluble N removal by BMP  
!!    bmp_bac(:)    |%             | Bacteria removal by BMP  
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    bactrolp      |# cfu/m^2     |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# cfu/m^2     |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# cfu/m^2     |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# cfu/m^2     |persistent bacteria transported with
!!                                 |sediment in surface runoff

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day\
!!    bactrolp      |# cfu/m^2     |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# cfu/m^2     |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# cfu/m^2     |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# cfu/m^2     |persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!	set variables
      j = ihru


!! Subtract reductions from sediment, nutrients, bacteria, NOT SURFACE RUNOFF to protect water balance
!! Sediment
	sedyld(j) = sedyld(j) * (1 - bmp_sed(j)/100)

!! Phosphorus
      !! Particulate
	sedminpa(j) = sedminpa(j) * (1 - bmp_pp(j)/100)
	sedminps(j) = sedminps(j) * (1 - bmp_pp(j)/100)
	sedorgp(j) = sedorgp(j) * (1 - bmp_pp(j)/100)
      !! Soluble
	surqsolp(j) = surqsolp(j) * (1 - bmp_sp(j)/100)

!! Nitrogen
	!! Particulate
	sedorgn(j) = sedorgn(j) * (1 - bmp_pn(j)/100)
      !! Soluble
      surqno3(j) = surqno3(j) * (1 - bmp_sn(j)/100)
	latno3(j) = latno3(j) * (1 - bmp_sn(j)/100)

!! Bacteria 
      bactrop = bactrop * (1 - bmp_bac(j)/100)
      bactrolp = bactrolp * (1 - bmp_bac(j)/100)
      bactsedp = bactsedp * (1 - bmp_bac(j)/100)
      bactsedlp = bactsedlp * (1 - bmp_bac(j)/100)

      return
      end subroutine smp_bmpfixed