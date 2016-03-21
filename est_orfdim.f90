	   subroutine est_orfdim(desdis,dia,cd)

   !!	This program estimates orifice dimensions based on 
   !!	design discharge at different stages

	   real, intent(in) :: cd,desdis
	   real, intent(out) :: dia
	   real :: tempvar,pi

	   pi = 3.14159
	   tempvar = 0.6 * cd * pi * sqrt(9.81)
	   dia = (4.0 * desdis / tempvar) ** 0.4

	   return
	   end subroutine est_orfdim