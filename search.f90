    subroutine search(a, max, cfind, iseq)
    
    character(len=16)  :: cfind
    integer :: iseq, max
    
    character(len=16), dimension(max) ::  a 
    
    nf = 1
    nl = max
    
    do int = 1, 25  !max for 100,000,000
      nn = (nl - nf) / 2 + nf
      
      if (nl - nf == 1) then
        if (a(nl) == cfind) then
          iseq = nl
          return
        end if
        if (a(nf) == cfind) then
          iseq = nf
          return
        end if
      end if
        
      if (a(nn) == cfind) then
        iseq = nn
        return
      end if
      
      if (a(nn) > cfind) then
        nl = nn
      end if
      
      if (a(nn) < cfind) then 
        nf = nn
      end if
    end do

100  format (1x,2a) 
    
    return
    end subroutine search