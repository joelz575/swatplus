     subroutine header_yield
    
     use basin_module
     
     implicit none 
    
!!  yield biomass file
      if (pco%mgtout == "y") then
        open (4700,file="yield.out", recl=800)
        write (9000,*) "YLD                       yield.out"
        if (pco%csvout == "y") then
          open (4701,file="yield.csv", recl=800)
          write (9000,*) "YLD                     yield.csv"
        end if
      end if  
      
      return
      end subroutine header_yield  