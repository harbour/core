/* Harbour GTNAP demo */

PROC MAIN

IF HB_GTVERSION()=="NAP"
    NAP_INIT("GTNAP Hello Demo", 35, 110, {|| RUN_MAIN() })
ELSE
    SETMODE(35,110)
    RUN_MAIN()
ENDIF

/*---------------------------------------------------------------------------*/

PROC RUN_MAIN

IF HB_GTVERSION()=="NAP"
    NAP_TERMINAL()
ENDIF

? "Hello, world!"

QUIT
