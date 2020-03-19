
PROC  main()
   LOCAL a := 4
#ifdef _DEBUG
   a+=2
    ? "file compiled with debug symbols"
#else
   a+=4
    ? "file compiled withOUT debug symbols"
#endif
   ? a

#pragma -B-
proc undebuggedCode()
#ifdef _DEBUG
    ? "this should not happen"
#else
    ? "This message is showed always"
#endif

#pragma -B+
proc debuggedCode()
#ifdef _DEBUG
   ? "This message is showed always TOO"
#else
   ? "this should not happen"
#endif
