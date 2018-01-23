    function fcgd(xx)
      tn = -5.
	  top = 35.
      tx = 50.
      qq = (tn - top)/(top - tx)
	  fcgd = ((xx-tn)**qq)*(tx-xx)/(((top-tn)**qq)*(tx-top))
      if (fcgd < 0.) fcgd = 0.
    end function