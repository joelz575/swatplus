
! split a string into 2 either side of a delimiter token
  SUBROUTINE split_string(instring, instring_len, string1, string2, delim)
    INTEGER :: instring_len
    CHARACTER(instring_len) :: instring
    character(1) :: delim
    CHARACTER(instring_len), INTENT(OUT):: string1,string2
    INTEGER :: index

    print *, "we are in splitstring..."
    instring = TRIM(instring)
    !print *, "we trimmed instring... delim is: ", delim
    index = SCAN(instring,delim)
    !print *, "we scanned instring..., index is: ", index
    string1 = trim(instring(1:index-1))
    !print *, "we made string1..., string1 is: ", string1
    string2 = trim(instring(index+1:))
    !print *, "we made string2..., string2: ", string2

  END SUBROUTINE split_string
