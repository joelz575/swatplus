	   subroutine est_weirdim(depwid,desdis,wwidth,wdepth,cd)

   !!	This program estimates rectangular weir dimensions based on 
   !!	width-depth ratio of wier at different stages

	   real, intent(in) :: depwid,cd,desdis
	   real, intent(out) :: wdepth,wwidth
         real :: tempvar
      
	   tempvar=depwid**1.5
	   wwidth=(desdis*tempvar/(1.84*cd))**0.4
	   wdepth=wwidth/depwid

	   return
	   end subroutine est_weirdim