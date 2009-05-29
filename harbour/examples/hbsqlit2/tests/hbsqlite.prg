/*
 * $Id$
 */

/*
 *------------------------------------------------------------------------
 *                  HARBOUR INTERFACE for SQLITE  
 *------------------------------------------------------------------------
 *
 * Copyright 2003 Alejandro de Garate <alex_degarate@hotmail.com>
 *
 * License: General Public License (GNU)
 * 
 * Developed using:
 *     Harbour 0.42 or upper 
 *     Borland C++ BCC 5.5.1 
 *
 * History:
 *
 * Ver 0.40 30 December 2003 Fixed an opening error not detected
 *             It seems is a problem with BCC compiler.  
 *             If "xxFile" database is not found, an empty file is
 *             created with the same name, given an incorrect signal
 *             to FILE() function. File is empty but exists (Oh man...)
 *             I fix it from harbour code, when have more spare time
 *             I will look in depth.
 *
 * Ver 0.30 28 December 2003 Pick tables, fields and DB structure,
 *                           you can import some dbf (not finish yet)
 *
 * Ver 0.20  5 December 2003 changes in design, A front end is started
 *             Shows database struc, table struct, field type
 *
 * Ver 0.10 26 November 2003 first intempts, open connection, list data
 *             close connection
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */

MEMVAR cDatabase
MEMVAR cCurrTable
MEMVAR ins_on

PROCEDURE MAIN()
*---------------------------------------------------------------------------
* Main procedure
*---------------------------------------------------------------------------
 LOCAL lError := .F.
 LOCAL nError := 0, nOption

 LOCAL cDbase
 LOCAL cDTable

 PUBLIC cDatabase         // only one DB in use
 PUBLIC cCurrTable        // Table currently in use
 PUBLIC ins_on := .F.     // if Insert key is On

 #define CRLF  CHR(13) + CHR(10)
 #define cSpace  " "
 #define cQuote  "'"

 #include "setcurs.ch"
 #include "box.ch"
 #include "inkey.ch"
 #include "hbsqlite.ch"   // REQUIRED !

 // basic setup
 SET WRAP    ON
 SET CENTURY ON
 SET DATE BRITISH
 SET SCOREBOARD OFF
 SET MESSAGE TO 23 CENTER
 SET KEY K_F1 TO HELP()
 SET KEY 22 TO INSERT

 
 SETCOLOR("W+/BG,W+/N,N,N,N/W*")
 CLS
 @ 0,0 TO 24,79 DOUBLE
 @ 22,0 TO 24,79 DOUBLE
 @ 22, 0 SAY "Ì"
 @ 22,79 SAY "¹"
 @  0,22 SAY " Harbour Interface for SQLite " + HB_HB4SQLITE_VER COLOR "GR+/BG"

 cDbase := PickSQLiteFile()

 cDatabase := IIF( ! EMPTY(cDbase), LOWER( cDbase ), "" )  // set public var

* cDBase := "example.db"   // sample database with a couple of tables
* cDbase := ChooseDB()
 
 IF EMPTY( cDbase )
    CLS
    ? "Harbour for SQLite"
    ?
    ? "Database was not supplied"
    ?
    QUIT
 ENDIF

 * Open SQLite database
 nError := SQLITE_OPEN( cDbase )

 IF nError > 0
    ALERT("Error number:" + STR( nError, 3) +;
          ";Error when opening database: " + cDbase + ;
          ";;Error explanation: ;" + SQLITE_ERROR() )
    QUIT
 ENDIF

 DataBar("None")

 nOption := 1


DO WHILE nOption != 0

   @ 1,1 CLEAR TO 21,78

   @  2,2 PROMPT "See SQLite Version"  MESSAGE;
               "Take a look what version SQLite is"

   @  4,2 PROMPT "See HbSQLite Info"  MESSAGE;
               "Take a look what version Harbour for SQLite is"

   @  6,2 PROMPT "See ALL tables" MESSAGE;
               "List ALL the tables inside the SQLite database"

   @  8,2 PROMPT "See ALL Columns (Fields)" MESSAGE;
               "List ALL Fields of the selected table inside the SQLite database"

   @ 10,2 PROMPT "See Table struct (Fields)" MESSAGE;
               "Show table structure and list ALL Fields of the selected table "

   @ 12,2 PROMPT "Show data" MESSAGE;
               "List some data from test table"

   @ 14,2 PROMPT "APPEND FROM DBF" MESSAGE;
               "Create a table and append from an external DBF file "

   @ 16,2 PROMPT "DROP TABLE" MESSAGE;
               "Erase a table from the database"

   @ 18,2 PROMPT "EXIT" MESSAGE;
               "QUIT from Harbour Interface for SQLite"
   MENU TO nOption

   @ 23,1 CLEAR TO 23,78  // clean msg area

   DO CASE
      CASE nOption == 1        // See SQLite Version
           ShowVersion()

      CASE nOption == 2        // See Hb4SQLite Info
           SQLITE_HB4SQLITE()

      CASE nOption == 3        // See ALL tables
           ShowTables(.F.)

      CASE nOption == 4        // See ALL Columns (Fields)
           cDTable := ShowTables(.T.) 
           DataBar( cDTable )  // status bar
           ShowFields( cDTable )

      CASE nOption == 5        // See Table struct (Fields)       
           cDTable := ShowTables(.T.) 
           DataBar( cDTable )  // status bar
           ShowCOLInfo( cDTable )

      CASE nOption == 6        // Show data
           cDTable := ShowTables(.T.) 
           DataBar( cDTable )  // status bar
           ShowData( cDTable )

      CASE nOption == 7        // APPEND FROM DBF
           AppendFrom()

      CASE nOption == 8        // DROP TABLE
           SQLITE_DROPTABLE()

      OTHERWISE
           EXIT
   ENDCASE

ENDDO

 // close the connection
 SQLITE_CLOSE()

 CLS
 @ 2,1 SAY "Successfully quiting from Harbour for SQLite ..."
 @ 23,1 SAY "."
 SET COLOR TO

RETURN // End Main


*------------------------------
 FUNCTION ShowCOLInfo( cTable )
*---------------------------------------------------------------------------
* Shows Information about fields...
*---------------------------------------------------------------------------
 LOCAL aResult, nChoices, n, aBrowse := {}, nLen, cData := ""
 LOCAL nOldCursor, nOldRow, nOldCol, cOldScreen, cOldColor, cDflt
 LOCAL nFrom, nTo
 LOCAL cc, clen
 #define  FLD_NAME    1
 #define  FLD_DFLT    2
 #define  FLD_TYPE    3
//#define FLD_LENGTH  3    // It's embebed in field type
 #define  FLD_PRIMKEY 4
 #define  cDel  SPACE(2)

 * save status
 nOldRow := ROW()
 nOldCol := COL()
 nOldCursor := SETCURSOR( SC_NONE )
 cOldScreen := SAVESCREEN( 0, 0, MAXROW(), MAXCOL() )
 cOldColor  := SETCOLOR()

 IF !( VALTYPE( cTable ) == "C" ) .OR. EMPTY( cTable ) .OR. cTable == NIL
    RETURN ""
 ENDIF

 aResult := SQLITE_SYSCOLUMNS( cTable )

 nLen := 2 + aResult[ 2 ]

FOR n := 3 TO nLen
    cDflt := IIF( EMPTY( aResult[ n ][FLD_DFLT]) , ;
               PADR( "SPACES", 12), PADR( aResult[ n ][FLD_DFLT], 12) )

    * Try to get Lenght
    cc := aResult[ n ][FLD_TYPE]
    nFrom := AT( "(", cc ) + 1
    nTo   := AT( ")", cc )

    IF nFrom > 1
       cLen := PADL( SUBST( cc, nFrom, nTo - nFrom), 6)
    ELSE
       * Get length from special array of datatypes
       cLen := STR( GetFldLen( UPPER( ALLTRIM(cc) ) ), 6)  //SPACE(6)
    ENDIF

    * build the display array
    cData := STR( n-2, 2) + " "+;
             PADR( aResult[ n ][FLD_NAME], 23 ) + cDel + ;
             cDflt  + " " + ;
             PADR( aResult[ n ][FLD_TYPE], 12 ) + cDel + ;
             cLen  + SPACE(4) + ;
             IIF( aResult[ n ][FLD_PRIMKEY], "TRUE ", "FALSE")

    AADD( aBrowse, cData)
    cData := ""
NEXT

  SETCOLOR( "W+/BG,W+/B,N,N,N/W*" )
  DISPBOX( 2, 2, 21, 77, B_DOUBLE + ' ', "W+/BG,W+/B" ) // draw box

  @  2,27 SAY " Table Structure Data " COLOR "GR+/BG"
  @  3, 4 SAY "      Table Name: " + aResult[ 1 ]        COLOR "W+/BG"
  @  4, 4 SAY "Number of fields: " + STR( aResult[2], 3) COLOR "W+/BG"
  @  5, 4 SAY "Number of reccds: " 
  @  7, 2 SAY "Ì" + REPLIC( "Í", 74) + "¹"
  @  7,30 SAY " Field Data " COLOR "GR+/BG"
  @  8,03 SAY "    Name" + SPACE(21) + "Default Val." + SPACE(3) + "Type" +;
              SPACE(10) + "Len  Primary Key" COLOR "N/W"

  nChoices := ACHOICE( 9, 4, 20, 75, aBrowse )

  * restore status
  RESTSCREEN( 0, 0, MAXROW(), MAXCOL(), cOldScreen )
  SETCURSOR( nOldCursor )
  SETPOS( nOldRow, nOldCol )
  SETCOLOR( cOldColor )

RETURN 0



*----------------------------
 FUNCTION SQLITE_HB4SQLITE()
*----------------------------
* Shows specific info 
*---------------------------------------------------------------------------
  LOCAL aInfo [6], nWide := 60, dUpdate
  #define this_UPDATE  __DATE__   // constant from C compiler

  dUpdate := STOD( this_UPDATE )  // undocumented in Clipper

  aInfo [1] := "       Version: " + HB_HB4SQLITE_VER 
  aInfo [2] := "   Last Update: " + DTOC( dUpdate )
  aInfo [3] := " Harbour Build: " + VERSION()
  aInfo [4] := "  C++ Compiler: " + HB_COMPILER()
  aInfo [5] := "Operat. System: " + OS()
  aInfo [6] := "        Author: Alejandro de Gárate"

  ALERT( "Harbour for SQlite;;"   +;
        PADR( aInfo [1], nWide ) + ";" +;
        PADR( aInfo [2], nWide ) + ";" +;
        PADR( aInfo [3], nWide ) + ";" +;
        PADR( aInfo [4], nWide ) + ";" +;
        PADR( aInfo [5], nWide ) + ";" ,;
        NIL, "1/15")

RETURN 0


*------------------------
 FUNCTION SQLITE_TABLES()
*---------------------------------------------------------------------------
* Uses a (special) master table where the names of all tables are stored
* Returns an array with names of tables inside of the database
*---------------------------------------------------------------------------
LOCAL aTables, cStatment, nLen
  /* execte a query
    if( c=='t' && n>1 && strncmp(azArg[0], "tables", n)==0 ){
    char **azResult;
    int nRow, rc;
    char *zErrMsg;
    open_db(p);
    if( nArg==1 )
      rc = sqlite_get_table(p->db,
        "SELECT name FROM sqlite_master "
        "WHERE type IN ('table','view') "
        "UNION ALL "
        "SELECT name FROM sqlite_temp_master "
        "WHERE type IN ('table','view') "
        "ORDER BY 1",
        &azResult, &nRow, 0, &zErrMsg
      );
 */

cStatment := "SELECT name FROM sqlite_master "      +;
             "WHERE type IN ('table','view') "      +;
             "UNION ALL "                           +;
             "SELECT name FROM sqlite_temp_master " +;
             "WHERE type IN ('table','view') "      +;
             "ORDER BY 1;"

aTables := SQLITE_QUERY( cStatment )  // query master table
nlen := LEN( aTables )
ADEL( aTables, 1)         // delete field title
ASIZE( aTables, nLen-1 )  // resize array according delete
RETURN( aTables )


*---------------------------------------------------------------------------
*               ===> H E L P E R     F U N C T I O N S <===
*---------------------------------------------------------------------------

*------------------------------
 FUNCTION CreatefromDBF( cSQL )
*---------------------------------------------------------------------------
*      *** UNDER DEVELOPMENT *** 
*---------------------------------------------------------------------------
LOCAL aStruct, cData := "", cDBase, n, cFlist := ""
LOCAL nFields
LOCAL cField_Def
LOCAL cHeader
#include "dbstruct.ch"

*COPY STRUCTURE EXTENDED TO struc //.dbf
*USE struc NEW
*LIST field_name, field_type, field_len ,field_dec
aStruct := DBSTRUCT()

AEVAL( aStruct, {| aField | cData := cData + ;
                            aField[ DBS_NAME ] + " " + ;
                            aField[ DBS_TYPE ] + " " +;
                LTRIM( STR( aField[ DBS_LEN ] )) + " " +;
                LTRIM( STR( aField[ DBS_DEC ] )) + CRLF } )
? cData

cDBase := "Tablon"

* Build field list
nFields := LEN( aStruct )

/*
FOR n := 1 TO nFields
    cFList := cFList + ;
              cSpace + aStruct[ n][DBS_NAME] + cSpace +;
              ConvertFldType( aStruct[ n][DBS_TYPE] ) +;
              IIF( ConvertFldLen( aStruct[ n][DBS_TYPE] ), ;
                   "(" + LTRIM( STR( aStruct[ n][DBS_LEN] )) + ")", ;
                   "") +;
              IIF( n < nFields, "," , "")
NEXT
*/
cField_Def := LOWER( cFList )

* Create table
cHeader := "create table" + cSpace + cDBase + "(" + cField_Def + ");"
? cHeader
*  aResult := SQLITE_EXECUTE( cHeader )
* ? SQLITE_ERROR()

* Repeat for every reccord...
DO WHILE ! EOF()

   * Put all fields in a row list comma separated
   cFList := ""
   FOR n := 1 TO nFields
       cFList := cFList + ;
       cQuote + RTRIM( xconvert( FIELDGET( n )) ) + cQuote+ ;
       IIF( n < FCOUNT(), ",", "")
   NEXT

   cSQL := "insert into " + cDBase + cSpace + "values(" + cFList + ");"
*  ? cFList
*  ? cSQL

   SQLITE_EXECUTE( cSQL )   // insert reccord

   IF SQLITE_ERROR() != NIL
      ALERT( "Error !;;" + SQLITE_ERROR() )
   ENDIF

   SKIP
ENDDO

inkey(0)
RETURN( "")


*---------------------
 FUNCTION AppendFrom()
*---------------------------------------------------------------------------
*---------------------------------------------------------------------------
 SELECT 1
 USE TEST
* BROWSE()

 CreatefromDBF("")
 USE
inkey(0)
RETURN 0



*--------------------------------
 FUNCTION ConvertFldType( cType )
*---------------------------------------------------------------------------
* Get a Clipper type and return closest SQLite type
*---------------------------------------------------------------------------
LOCAL aTypes := { { "C", "CHAR"  }, { "L", "BOOLEAN" },;
                  { "M", "TEXT"  }, { "O", "BINARY" },;
                  { "N", "DOUBLE"}, { "D", "TIMESTAMP" } }
*             { "C", "" },;
LOCAL nPos := ASCAN( aTypes, {|aVal| aVal[1] == cType })
RETURN( IIF( nPos == 0, "", aTypes[nPos][2] ))


*-------------------------------
 FUNCTION ConvertFldLen( cType )
*---------------------------------------------------------------------------
* Get a Clipper type and return if field Len is required for SQLite type
*---------------------------------------------------------------------------
LOCAL aTypes := { { "C", "CHAR" ,  .T. }, { "L", "BOOLEAN",  .F. },;
                  { "M", "TEXT" ,  .T. }, { "O", "BINARY" ,  .T. },;
                  { "N", "DOUBLE", .F. }, { "D", "TIMESTAMP",.F. } }
LOCAL nPos := ASCAN( aTypes, {|aVal| aVal[1] == cType })
RETURN( IIF( nPos == 0, .F., aTypes[nPos][3] ))


*---------------------------
 FUNCTION GetFldLen( cType )
*---------------------------------------------------------------------------
* Get a SQLite type and return A TENTATIVE length (till I can find the 
* correct one)  (:{)
* I have the intention of centralize all non explicit lengths to this
* function.
* It is needed a way of get binary/blob length
*---------------------------------------------------------------------------
LOCAL aTypes := {{ "BOOLEAN", 1 },  { "BOOL",       1 }, ;
                 { "INTEGER", 4 }, ;
                 { "FLOAT",   4 },  { "DOUBLE",     8 }, ;
                 { "DATE",   10 },  { "TIMESTAMP", 18 }, ;
                 { "BINARY",  0 },  { "BLOB"  ,     0 }  }
LOCAL nPos := ASCAN( aTypes, {|aVal| aVal[1] == cType } )
RETURN( IIF( nPos == 0, 0, aTypes[nPos][2] ))



*---------------------------------------------------------------------------
*   Samples functions to show posibilities...
*---------------------------------------------------------------------------

*----------------------
 FUNCTION ShowVersion()
*---------------------------------------------------------------------------
* Shows SQLite version
*---------------------------------------------------------------------------
LOCAL aInfo
 aInfo := SQLITE_INFO()   // Get gral info about SQLite
ALERT( "SQLITE INFO ;;" +;
            " <sqlite.h> header = " + aInfo [1] + ";" +;
            "   version library = " + aInfo [2] + ";" +;
            "     encoding library = " + aInfo [3],;
       NIL, "1/15")
RETURN 0


*-------------------------------
 FUNCTION ShowTables( lMsgShow )
*---------------------------------------------------------------------------
* Shows all tables inside the database
*---------------------------------------------------------------------------
 LOCAL aResult, nChoices
 LOCAL nOldCursor, nOldRow, nOldCol, cOldScreen, cOldColor

 * save status
 nOldRow := ROW()
 nOldCol := COL()
 nOldCursor := SETCURSOR( SC_NONE )
 cOldScreen := SAVESCREEN( 0, 0, MAXROW(), MAXCOL() )
 cOldColor  := SETCOLOR()

  * Show all tables inside database
  aResult := SQLITE_TABLES()

  SETCOLOR( "W+/BG,W+/B,N,N,N/W*" )
  DISPBOX( 9,9, 21, 33, B_DOUBLE + ' ', "W+/BG,W+/B" ) // draw box

  @  9,17 SAY " Tables " COLOR "GR+/BG"

  IF VALTYPE(lMsgShow) == "L" .AND. lMsgShow == .T.
     @ 21,15 SAY " Pick a Table " COLOR "W+/BG"
  ENDIF
  nChoices := ACHOICE( 10, 11, 20, 31, aResult )

  // Restore status
  RESTSCREEN( 0, 0, MAXROW(), MAXCOL(), cOldScreen )
  SETCURSOR( nOldCursor )
  SETPOS( nOldRow, nOldCol )
  SETCOLOR( cOldColor )

RETURN( IIF( nChoices > 0, aResult[ nChoices ], "") )



*-----------------------------
 FUNCTION ShowFields( cTable )
*---------------------------------------------------------------------------
* Shows fields on a box from given table
*---------------------------------------------------------------------------
 LOCAL aResult, nChoices
 LOCAL nOldCursor, nOldRow, nOldCol, cOldScreen, cOldColor

 * save status
 nOldRow := ROW()
 nOldCol := COL()
 nOldCursor := SETCURSOR( SC_NONE )
 cOldScreen := SAVESCREEN( 0, 0, MAXROW(), MAXCOL() )
 cOldColor  := SETCOLOR()

  * Show all tables inside database
  IF !( VALTYPE( cTable ) == "C" ) .OR. EMPTY( cTable ) .OR. cTable == NIL
     RETURN ""
  ELSE
     aResult := SQLITE_FIELDS( cTable )
  ENDIF

  AEVAL( aResult, {| aVal, nIndex | ;
               aResult[ nIndex] := STR( nIndex, 3) + ". " + aVal } )

  SETCOLOR( "W+/BG,W+/B,N,N,N/W*" )
  DISPBOX( 9,9, 21, 33, B_DOUBLE + ' ', "W+/BG,W+/B" ) // draw box

  @ 9,17 SAY " Fields " COLOR "GR+/BG"
  nChoices := ACHOICE( 10,11, 20, 31, aResult )

  // Restore status
  RESTSCREEN( 0, 0, MAXROW(), MAXCOL(), cOldScreen )
  SETCURSOR( nOldCursor )
  SETPOS( nOldRow, nOldCol )
  SETCOLOR( cOldColor )

RETURN( IIF( nChoices > 0, aResult[ nChoices ], "") )



*---------------------------
 FUNCTION SQLITE_DROPTABLE()
*---------------------------------------------------------------------------
* Deletes a table from current database
* WARNING !!   It deletes forever...
*---------------------------------------------------------------------------
LOCAL cTable := ShowTables(), aOpt := {" Yes ", " No "}
LOCAL nOpt
LOCAL aResult
nOpt := ALERT("Warning!;;The table selected will be erased;" +;
              "without any choice to recover;Continue ? ", aOpt )
IF nOpt == 1 // Yes
   aResult := SQLITE_EXECUTE( "drop table " + cTable )
ENDIF
RETURN 0


*--------------------------------
 FUNCTION xConvert( xData, nPad )
*---------------------------------------------------------------------------
* Conversion scheme of types from Clipper/Harbour to SQLite
* Warning !!
* It's a work in progress and would have some errors
*---------------------------------------------------------------------------
LOCAL cData, cType, nLen
cType := VALTYPE( xData )

DO CASE
   CASE cType == "N"
        cData := STR( xData )
   CASE cType == "C"
        cData := xData
   CASE cType == "L"
       cData := IIF( xData == .T., "TRUE", "FALSE")
   CASE cType == "A"
        nLen := LEN(xData)
        cData := "ARRAY[" + IIF( nLen > 0, STR( nLen, 2), "0") + "]"
   CASE cType == "B"
        cData := "BLOCK"
   CASE cType == "U"
        cData := "UNDEF"
   CASE cType == "D"
        ? xData
        cData := DTOS( xData )
   OTHERWISE
        cData := VALTYPE( xData)
ENDCASE

IF nPad == NIL .OR. !( VALTYPE( nPad ) == "N" )
ELSE
   cData := PADL( LTRIM(cData), nPad, " ")
ENDIF

RETURN( cData)


*---------------------------
 FUNCTION ShowData( cDBase )
*---------------------------------------------------------------------------
* Shows data
*---------------------------------------------------------------------------
LOCAL aResult, nFields, nRecc, i, j
LOCAL cQuery := ".", cQuery1
LOCAL cQuery2 := SPACE(74)

 IF cDBase == NIL .OR. EMPTY(cDBase)
    RETURN 0
 ENDIF

 cQuery1 := PADR("select * from " + cDBase, 74)


 DO WHILE ! EMPTY( cQuery )

    cQuery := GetQuery( cQuery1 )    

    aResult := SQLITE_QUERY( RTRIM( cQuery ) + ";")

    @ 1,1 CLEAR TO 21,78
    @ 23,1 CLEAR TO 23,78

    nFields := SQLITE_GETCOLS()
    nRecc   := SQLITE_GETROWS()

    IF nRecc < 1

       IF SQLITE_ERROR() != NIL
          ALERT( "Error!;;" + SQLITE_ERROR() )
          LOOP
       ENDIF

    ENDIF

    IF nRecc < 2
       IF Answer("No record match your query. Retry ?") == 2
          RETURN 0
       ENDIF
       LOOP
    ENDIF

    DataBar( cDBase )  // status bar

    IF nRecc > 15
       nRecc := 15
    ENDIF

    FOR i := 1 TO nRecc   // skip title field

        IF nFields > 1

           // print the headers of fields

           // print data
           FOR j := 1 TO nFields
               IF i == 1
                  SETCOLOR("N/BG")
               ELSE
                  SETCOLOR("W+/BG,W+/N,N,N,N/W*")
               ENDIF
               @ 1+i,(j*12)-10 SAY aResult[ i][j]
           NEXT

        ELSE
           FOR j := 1 TO nFields
               @ 1+i,(j*12)-10 SAY aResult[ i]
           NEXT

        ENDIF

    NEXT

    IF Answer("New Query ?") != 1
       EXIT
    ENDIF

 ENDDO

RETURN 0


*---------------------------
 FUNCTION ShowData2( cDBase )
*---------------------------------------------------------------------------
* Shows data I will change to use TBrowse object
*---------------------------------------------------------------------------
LOCAL aResult, nFields, nRecc, i, j
LOCAL cQuery := ".", cQuery1
LOCAL cQuery2 := SPACE(74)
LOCAL GetList := {}

 IF cDBase == NIL .OR. EMPTY(cDBase)
    RETURN 0
 ENDIF

 cQuery1 := PADR("select * from " + cDBase, 74)

 DO WHILE ! EMPTY( cQuery )

    @ 1,1 CLEAR TO 21,78    
    DISPBOX( 9,1, 14, 78, B_DOUBLE + ' ', "W+/BG,W+/B" )

    SET CURSOR ON
    @  9,38 SAY " QUERY "
    @ 10,03 SAY "Input your SQL query to table"
    @ 11,03 GET cQuery1   COLOR "N/W*,N/W*"
    @ 12,03 GET cQuery2   COLOR "N/W*,N/W*"
    READ

    cQuery := RTRIM( cQuery1 ) + RTRIM( cQuery2 )

    IF LASTKEY() == 27 // ! EMPTY( cQuery )
       EXIT
    ENDIF

    aResult := SQLITE_QUERY( RTRIM( cQuery ) + ";")

    @ 1,1 CLEAR TO 21,78
    @ 23,1 CLEAR TO 23,78

    nFields := SQLITE_GETCOLS()
    nRecc   := SQLITE_GETROWS()

    IF nRecc < 1
       ALERT( "Error!;" + SQLITE_ERROR() )
       LOOP
    ENDIF

    IF nRecc < 2
       IF Answer("No record match your query. Retry ?") == 2
          RETURN 0
       ENDIF
       LOOP
    ENDIF

    DataBar( cDBase )  // status bar

    IF nRecc > 15
       nRecc := 15
    ENDIF

    FOR i := 1 TO nRecc   // skip title field

        IF nFields > 1

           // print the headers of fields
           // print data
           FOR j := 1 TO nFields
               IF i == 1
                  SETCOLOR("N/BG")
               ELSE
                  SETCOLOR("W+/BG,W+/N,N,N,N/W*")
               ENDIF
               @ 1+i,(j*12)-10 SAY aResult[ i][j]
           NEXT

        ELSE
           FOR j := 1 TO nFields
               @ 1+i,(j*12)-10 SAY aResult[ i]
           NEXT

        ENDIF

    NEXT

    IF Answer("New Query ?") != 1
       EXIT
    ENDIF

 ENDDO

RETURN 0



*--------------------------
 FUNCTION DataBar( cTable )
*---------------------------------------------------------------------------
IF cTable == NIL
   cTable := ""
ENDIF

@ 24,02 SAY REPLIC( "Í", 74) COLOR "W+/BG"

@ 24,02 SAY "db: " COLOR "W+/BG"
@ 24,20 SAY " Table: " COLOR "W+/BG"
@ 24,42 SAY " Rows: " COLOR "W+/BG"
@ 24,58 SAY " Cols: " COLOR "W+/BG"
@ 24,06 SAY cDatabase
@ 24,28 SAY cTable
@ 24,48 SAY STR( SQLITE_GETROWS(), 5) COLOR "W+/BG"
@ 24,64 SAY STR( SQLITE_GETCOLS(), 3) COLOR "W+/BG"

Ins_Stat()  // shows Insert key status
RETURN 0


*------------------------
 FUNCTION Answer( cMsg )
*---------------------------------------------------------------------------
LOCAL nOpt, nLen := LEN( cMsg )
LOCAL nCol := 40 - INT( (11 + nLen) / 2)
@ 23,1 CLEAR TO 23,78
@ 23, nCol SAY cMsg
@ 23, nCol+nLen + 2 PROMPT " Yes "
@ 23, nCol+nLen + 9 PROMPT " No "
MENU TO nOpt
@ 23,1 CLEAR TO 23,78
RETURN( nOpt )



*-------------------
 FUNCTION ChooseDB()
*---------------------------------------------------------------------------
* Enter a Database to work with it
*---------------------------------------------------------------------------
 LOCAL cDB := PADR("example.db", 25)
 LOCAL nOldCursor, nOldRow, nOldCol, cOldScreen, cOldColor
 LOCAL GetList := {}

 * save status
 nOldRow := ROW()
 nOldCol := COL()
 nOldCursor := SETCURSOR( .t. )
 cOldScreen := SAVESCREEN( 0, 0, MAXROW(), MAXCOL() )
 cOldColor  := SETCOLOR()

  SETCOLOR( "W+/BG,W+/B,N,N,N/W*" )
  DISPBOX( 8,9, 13, 40, B_DOUBLE + ' ', "W+/BG,W+/B" ) // draw box
  @  8,13 SAY " Main Database " COLOR "GR+/BG"
  @ 10,11 SAY "Input the working database:"

  Ins_Stat()

  DO WHILE LASTKEY() != 27

     @ 12,11 GET cDB  COLOR "N/W*"
     READ
        
     IF ! EMPTY( cDB )

        IF ! REALFILE( RTRIM( cDB ) )
           * file not exist!
           IF ALERT('FUNCTION ChooseDB;; File "' + RTRIM( cDB ) + ;
                  '"  Not found ! ;;;' +;
                  'Input another file ?', { " No ", " Yes "} ) < 2
              cDB := ""   // return an empty db
              EXIT
           ELSE
              LOOP
           ENDIF 

        ELSE
           * file exist
           EXIT
        ENDIF

     ENDIF

  ENDDO

  // Restore status
  RESTSCREEN( 0, 0, MAXROW(), MAXCOL(), cOldScreen )
  SETCURSOR( nOldCursor )
  SETPOS( nOldRow, nOldCol )
  SETCOLOR( cOldColor )

  cDatabase := IIF( ! EMPTY(cDB), LOWER( RTRIM( cDB ) ), "" )

  RETURN( cDataBase )



*------------------------------
 FUNCTION REALFILE( cFilename )
*---------------------------------------------------------------------------
* It's odd but necessarily. We need to identify if it's a SQLite database
*---------------------------------------------------------------------------
LOCAL lFound := .F., nHandle, nOfs := 0
LOCAL cBuffer, cMarker
LOCAL nLength
#include "fileio.ch"

IF FILE( cFilename )

   nHandle := FOPEN( cFilename, FO_READ)

   * Don't show any error here
*   IF FERROR() != 0
*      ALERT("Can't open file: " + cFilename + ";check the PATH.")
*   ENDIF

   nLength := FSEEK( nHandle, nOfs, FS_END )
   
   FSEEK( nHandle, nOfs, FS_SET )  // goto begining
   
   IF nLength > 0
      cBuffer  := SPACE( 50 )

      * The following is needed because for an unknown reason some
      * empty files or with a few bytes (not SQLite database) don't 
      * give any error ???!!
      * So we read the SQLite Tag at the beginning of the database 
      * (used to identify the database version).  In that way we are
      * sure now if it is really an SQLite database.
      * 
      * This behavior was found 

      IF FREAD( nHandle, @cBuffer, XSQLITE_TAG_LEN ) == XSQLITE_TAG_LEN

         cMarker := UPPER( LEFT( cBuffer, XSQLITE_TAG_LEN ))

         IF cMarker == UPPER( XSQLITE_TAG )
            lFound := .T.
         ENDIF
      ENDIF

   ENDIF

   FCLOSE( nHandle)    

ENDIF

RETURN( lFound )

* sqlite-users-digest-subscribe@sqlite.org


PROCEDURE HELP
 LOCAL nWide := 40

  ALERT( "HARBOUR INTERFACE for SQLITE;;"   +;
         "Version: " + HB_HB4SQLITE_VER + ";;" +;
       "Copyright 2003 Alejandro de Garate;"   +;
       "<alex_degarate@hotmail.com>" ,, "N/*W")

RETURN


*----------------------------- 
 FUNCTION GetQuery( cDfltTxt )
*---------------------------------------------------------------------------
* Open a window an let you to type a query, and returns it as string
*---------------------------------------------------------------------------
LOCAL cQuery, cQuery1, GetList :={}
LOCAL cQuery2 := SPACE(74)
LOCAL cQuery3 := SPACE(74)
LOCAL nOldCursor, nOldRow, nOldCol, cOldScreen, cOldColor
LOCAL nWide := 74         // length of edit line

 cQuery1 := PADR( cDfltTxt, nWide)  // initial text as a guide

 * save status
 nOldRow := ROW()
 nOldCol := COL()
 nOldCursor := SETCURSOR( SC_NONE )
 cOldScreen := SAVESCREEN( 0, 0, MAXROW(), MAXCOL() )
 cOldColor  := SETCOLOR()

    DISPBOX( 9,1, 15, 78, B_DOUBLE + ' ', "W+/BG,W+/B" )

    SET CURSOR ON
    @  9,38 SAY " QUERY " COLOR "GR+/BG,N/W*"
    @ 10,03 SAY "Input your SQL query to table"  COLOR "W+/BG,N/W*"

  cQuery := cDfltTxt


 DO WHILE ! EMPTY( cQuery )
 
    SET COLOR TO "N/W*,N/W*"
*    cQuery := MEMOEDIT( cQuery, 11, 3, 13, 77, .T., "", nWide )

    @ 11,03 GET cQuery1   COLOR "N/W*,N/W*"
    @ 12,03 GET cQuery2   COLOR "N/W*,N/W*"
    @ 13,03 GET cQuery3   COLOR "N/W*,N/W*"
    READ

    cQuery := cQuery1 + cQuery2 + cQuery3

    cQuery := ALLTRIM( cQuery ) 

    DO CASE
       CASE LASTKEY() == 27
            EXIT

       CASE LASTKEY() == 13
            IF ! EMPTY( cQuery )
               EXIT
            ELSE
               LOOP
            ENDIF
       OTHERWISE
    ENDCASE

 ENDDO

  * restore status
  RESTSCREEN( 0, 0, MAXROW(), MAXCOL(), cOldScreen )
  SETCURSOR( nOldCursor )
  SETPOS( nOldRow, nOldCol )
  SETCOLOR( cOldColor )

RETURN( cQuery ) 


*-----------------
 PROCEDURE INSERT
*---------------------------------------------------------------------------
* insert
* CAMBIA EL MODO INSERT ON /OFF DESDE UN "READ"
*---------------------------------------------------------------------------
ins_on := .NOT. ins_on
IIF(ins_on, READINSERT(.T.), READINSERT(.F.))
Ins_stat()
RETURN

*------------------
 FUNCTION Ins_stat
*---------------------------------------------------------------------------
*       update the status line in the browse window
*---------------------------------------------------------------------------
LOCAL c:= 24, r:= 72, nRow, nCol, cOldColor
* Relocate the status line
nRow := ROW()
nCol := COL()

cOldColor := SETCOLOR()
SET COLOR TO "W+/BG"

* display record pointer information
@ r, c SAY IIF( ins_on, "Insert ", "Replace")
SETCOLOR( cOldColor )

* restore cursor position
@ nRow, nCol SAY ""
RETURN 0


*--------------------------------- 
 FUNCTION PickSQLiteFile( cAtrib )
*---------------------------------------------------------------------------
* Pick a SQLite file  *** UNDER DEVELOPMENT *** 
*---------------------------------------------------------------------------
 #include "directry.ch"
 LOCAL aFiles, aNames := {}, aShow := {}, cPick := ""
 LOCAL nOldCursor, nOldRow, nOldCol, cOldScreen, cOldColor
 LOCAL n
 LOCAL cFile
 LOCAL nPick

 IF cAtrib == NIL
    cAtrib := "*.*"
 ENDIF

 * save status
 nOldRow := ROW()
 nOldCol := COL()
 nOldCursor := SETCURSOR( SC_NONE )
 cOldScreen := SAVESCREEN( 0, 0, MAXROW(), MAXCOL() )
 cOldColor  := SETCOLOR()

 DISPBOX( 9, 10, 13, 70, B_DOUBLE + ' ', "W+/BG,W+/B" )
 
 @ 11,12 SAY "Searching for SQLite databases...on current directory"

 aFiles := DIRECTORY( cAtrib )

/*
 AEVAL( aFiles, { | file| ;
  IIF( Realfile( file[F_NAME]), AADD( aNames, file[F_NAME]);
     AADD( aNames, PADR( file[F_NAME], 25) +;
     "  " + TRANSF( file[F_SIZE], "@E 999,999,999") ), ) } )
*/

 FOR n := 1 TO LEN( aFiles )
     cFile := aFiles[ n ][F_NAME]

     IF Realfile( cFile )
        AADD( aShow, PADR( cFile, 25) + "  " + ;
                     TRANSF( aFiles[n][F_SIZE], "@E 999,999,999") )
        AADD( aNames, cFile )
     ENDIF
    
 NEXT

 @ 9, 10 clear to 13, 70
 
 IF LEN( aNames ) > 0

    DISPBOX( 6,8, 22, 50, B_DOUBLE + ' ', "W+/BG,W+/B" )
    @ 6,20 SAY " Pick a database "    
    @ 8,10 SAY " Name"+ SPACE(28) +"Size  " COLOR "N/W*"   

    nPick := ACHOICE( 10, 10, 20, 48, aShow )
 ELSE
    ALERT("Notice;; I can't found a SQLite db on current directory")
 ENDIF

 IF nPick > 0
    cPick := ALLTRIM( aNames[ nPick ] )
 ENDIF

  * restore status
  RESTSCREEN( 0, 0, MAXROW(), MAXCOL(), cOldScreen )
  SETCURSOR( nOldCursor )
  SETPOS( nOldRow, nOldCol )
  SETCOLOR( cOldColor )

RETURN cPick
