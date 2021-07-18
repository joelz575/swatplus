
! split a string into 2 either side of a delimiter token
  SUBROUTINE split_string(instring, instring_len, string1, string2, delim)
    INTEGER :: instring_len
    CHARACTER(instring_len) :: instring
    character(1) :: delim
    CHARACTER(instring_len), INTENT(OUT):: string1,string2
    INTEGER :: index

    print *, "we are in splitstring..."
    instring = TRIM(instring)
    index = SCAN(instring,delim)
    string1 = trim(instring(1:index-1))
    string2 = trim(instring(index+1:))

  END SUBROUTINE split_string
