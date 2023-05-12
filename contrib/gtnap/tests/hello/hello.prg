/* Harbour GTNAP demo */

REQUEST HB_CODEPAGE_PTISO

PROC MAIN

// Application global SETTERS
SET CURSOR OFF
// SET SCOR OFF
// SET EPOCH TO 1940
// SET CENT ON
// SET DATE BRIT
// SET DELE ON
// SETBLINK(.F.)    // "*" passa a indicar background intenso
// SET AUTOPEN OFF  // tornar a abertura do indice cdx não automatica
// // Desativar na "cuademo", para página de código ser a padrão Windows/Linux
// // hb_cdpSelect("PT850")
// // hb_LangSelect("pt_BR","PT850")
//hb_cdpSelect("UTF8EX")



hb_cdpSelect("PTISO")
//hb_LangSelect("pt_BR","PTISO")

// hb_LangSelect("pt_BR","PTISO")

// REQUEST DBFCDX
// RDDSETDEFAULT("DBFCDX")
// RDDINFO(RDDI_MEMOTYPE,1)

// #DEFINE _COR2_PADRAO   "N/W,N/BG*,N,,N/W*"
// SETCOLOR(_COR2_PADRAO)
// MSETCURSOR( .T. )

//
// Event-driven applications (especially GTK+3 and macOS-Cocoa) cannot be started directly from main().
// They need to set up an event execution loop and other internal structures. In GTNAP based applications,
// we need to move the "main" to another procedure, which will be called from GTNAP when
// the application is ready to start. {|| RUN_MAIN() }
//
IF HB_GTVERSION()=="NAP"
    NAP_INIT("GTNAP Hello Demo", 35, 110, {|| RUN_MAIN() })

 ELSE
    SETMODE(35,110)
    RUN_MAIN()

 ENDIF

//
// This is the "real" main procedure for a GTNAP application.
// From here, all CUALIB based code will be the same as original implementation
//
PROC RUN_MAIN

    ? "Hello, world!"
    wait

IF HB_GTVERSION()=="NAP"
    NAP_EXIT()
ENDIF

QUIT

// PROCEDURE Main()

//     LOCAL nChoice

//     ? "Hello, world!"

//     wait
//     // // display a two line menu with status line at the bottom
//     // // let the user select favorite day
//     // SET MESSAGE TO 24 CENTER
//     // @ 10, 2 PROMPT "Sunday" MESSAGE "This is the 1st item"
//     // @ 11, 2 PROMPT "Monday" MESSAGE "Now we're on the 2nd item"
//     // MENU TO nChoice
//     // DO CASE
//     // CASE nChoice == 0           // user press <Esc> key
//     //    QUIT
//     // CASE nChoice == 1           // user select 1st menu item
//     //    ? "Guess you don't like Mondays"
//     // CASE nChoice == 2           // user select 2nd menu item
//     //    ? "Just another day for some"
//     // ENDCASE

//     RETURN
