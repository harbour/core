/*
 * File......: TBWHILE.PRG
 * Author....: Jim Orlowski
 * CIS ID....: ?
 *
 * This is an original work by Jim Orlowski and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   28 Sep 1991 02:56:56   GLENN
 * Moved Jim's "Tricks used" comment out of the file header and
 * into the source code area.
 *
 *    Rev 1.3   28 Sep 1991 02:52:22   GLENN
 * Jim's modifications:
 *
 *  1.  Changed SAVESCREEN() and RESTSCREEN to use MaxRow(), MaxCol()
 *      instead of 24,79
 *
 *  2.  Added Nantucket's cleaner code for:
 *        - Cleaned up logic around line 334 while loop section
 *        - Added refreshCurrent and another stabilize around line 349
 *        - TbSkipWhile was redone
 *             Note: Leo's line was changed to:
 *                 ELSEIF ( n > 0 .AND. RECNO() <> LASTREC() + 1)
 *
 *  3.  Added DispBegin() and DispEnd() around both Stabilize sections
 *
 *
 *
 *
 *    Rev 1.2   15 Aug 1991 23:04:20   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:08   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:22   GLENN
 * Nanforum Toolkit
 *
 */



/* The tricks are:
 *
 * 1. Setting up functions for goTop() and goBottom() so that you can
 *    quickly move to the right record when the user presses the
 *    Ctrl-PgUp ( goTop() ) and Ctrl-PgDn ( goBottom() ) keys.
 *
 * 2. Passing and evaluating the block for the TbSkipWhil().
 */


#command DEFAULT <param> TO <val> [, <paramn> TO <valn> ];
=> ;
         <param> := IIF(<param> = NIL, <val>, <param> ) ;
         [; <paramn> := IIF(<paramn> = NIL, <valn>, <paramn> ) ]
#include "inkey.ch"


#ifdef FT_TEST

  /*
   *   THIS DEMO SHOWS TBNAMES.DBF CONSISTING OF LAST, FIRST, ADDR, CITY,
   *   STATE, ZIP WITH ACTIVE INDEX ON LAST + FIRST.  IT SHOWS LAST NAME,
   *   FIRST NAME, CITY ONLY FOR THOSE LAST NAMES THAT BEGIN WITH LETTER
   *   THAT YOU INPUT FOR THE CKEY GET.
   *
   *   TBNAMES.DBF/.NTX ARE AUTOMATICALLY CREATED BY THIS TEST PROGRAM
   */

  #INCLUDE "SETCURS.CH"

  FUNCTION TBWHILE()
     LOCAL aFields := {}, cKey := "O", cOldColor
     LOCAL nFreeze := 1, lSaveScrn := .t., nRecSel
     LOCAL cColorList := "N/W, N/BG, B/W, B/BG, B/W, B/BG, R/W, B/R"
     LOCAL cColorShad := "N/N"
     FIELD last, first
     MEMVAR GetList

     IF ! FILE( "TBNAMES.DBF" )
        MAKE_DBF()
     ENDIF

     USE TBNames

     IF ! FILE( "TBNAMES.NTX" )
        INDEX ON last + first TO TBNAMES
     ENDIF

     SET INDEX TO TBNAMES

     * Pass Heading as character and Field as Block including Alias
     * To eliminate the need to use FIELDWBLOCK() function in FT_BRWSWHL()

     AADD(aFields, {"Last Name" , {||TBNames->Last}  } )
     AADD(aFields, {"First Name", {||TBNames->First} } )
     AADD(aFields, {"City"      , {||TBNames->City}  } )

     cOldColor := SetColor("N/BG")
     CLEAR SCREEN
     @ 5,10 SAY "Enter First Letter Of Last Name:" GET cKey PICTURE "!"
     READ

     * TBNames->Last = cKey is the Conditional Block passed to this function
     * you can make it as complicated as you want, but you would then
     * have to modify TBWhileSet() to find first and last records
     * matching your key.
     nRecSel := FT_BRWSWHL( aFields, {||TBNames->Last = cKey}, cKey, nFreeze,;
        lSaveScrn, cColorList, cColorShad, 3, 6, MaxRow() - 2, MaxCol() - 6)
     * Note you can use Compound Condition
     * such as cLast =: "Pierce            " and cFirst =: "Hawkeye  "
     * by changing above block to:
     *    {||TBNames->Last = cLast .AND. TBNames->First = cFirst}
     * and setting cKey := cLast + cFirst

     ?
     IF nRecSel == 0
        ? "Sorry, NO Records Were Selected"
     ELSE
        ? "You Selected " + TBNames->Last +" "+ ;
           TBNames->First +" "+ TBNames->City
     ENDIF
     ?

     WAIT
     SetColor(cOldColor)
     CLEAR SCREEN
  RETURN nil

  STATIC FUNCTION make_dbf
  LOCAL x, aData := {                                                               ;
     { "SHAEFER","KATHRYN","415 WEST CITRUS ROAD #150","LOS ANGELES","CA","90030" },;
     { "OLSON","JAMES","225 NORTH RANCH ROAD","LOS ANGELES","CA","90023"          },;
     { "KAYBEE","JOHN","123 SANDS ROAD","CAMARILLO","CA","93010"                  },;
     { "HERMAN","JIM","123 TOON PAGE ROAD","VENTURA","CA","93001"                 },;
     { "BURNS","FRANK","123 VIRGINA STREET","OXNARD","CA","93030"                 },;
     { "PIERCE","HAWKEYE","123 OLD TOWN ROAD","PORT MUGU","CA","93043"            },;
     { "MORGAN","JESSICA","123 FRONTAGE ROAD","CAMARILLO","CA","93010"            },;
     { "POTTER","ROBERT","123 FIR STREET","OXNARD","CA","93030"                   },;
     { "WORTH","MARY","123-1/2 JOHNSON DRIVE","OXNARD","CA","93033"               },;
     { "JOHNSON","SUSAN","123 QUEENS STREET","OXNARD","CA","93030"                },;
     { "SAMSON","SAM","215 MAIN STREET","OXNARD","CA","93030"                     },;
     { "NEWNAME","JAMES","215 MAIN STREET","LOS ANGELES","CA","90000"             },;
     { "OLEANDAR","JILL","425 FLORAL PARK DRIVE","FLORAL PARK","NY","10093"       },;
     { "SUGARMAN","CANDY","1541 SWEETHEART ROAD","HERSHEY","PA","10132"           } }

  DbCreate( "TBNAMES", { { "LAST ", "C", 18, 0, } ,;
                         { "FIRST", "C",  9, 0, } ,;
                         { "ADDR ", "C", 28, 0, } ,;
                         { "CITY ", "C", 21, 0, } ,;
                         { "STATE", "C",  2, 0, } ,;
                         { "ZIP  ", "C",  9, 0, } } )
  USE tbnames
  FOR x := 1 TO Len( aData )
     APPEND BLANK
     Aeval( aData[x], {|e,n| FieldPut( n, e ) } )
  NEXT
  USE
  RETURN NIL

#endif

/* ------------------------------------------------------------------- */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_BRWSWHL()
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Browse an indexed database limited to a while condition
 *  $SYNTAX$
 *     FT_BRWSWHL( <aFields>, <bWhileCond>, <cKey>,                  ;
 *                 [ <nFreeze> ], [ <lSaveScrn> ], [ <cColorList> ], ;
 *                 [ <cColorShadow> ], [ <nTop> ], [ <nLeft> ],      ;
 *                 [ <nBottom> ], [ <nRight> ] -> nRecno
 *  $ARGUMENTS$
 *     <aFields> is array of field blocks of fields you want to display.
 *        Example to set up last name and first name in array:
 *        aFields := {}
 *        AADD(aFields, {"Last Name" , {||Names->Last}  } )
 *        AADD(aFields, {"First Name", {||Names->First} } )
 *
 *     <bWhileCond> is the limiting WHILE condition as a block.
 *        Example 1: { ||Names->Last == "JONES" }
 *        Example 2: { ||Names->Last == "JONES" .AND. Names->First == "A"  }
 *
 *     <cKey> is the key to find top condition of WHILE.
 *        cLast  := "JONES     "
 *        cFirst := "A"
 *        Example 1: cKey := cLast
 *        Example 2: cKey := cLast + cFirst
 *
 *     <nFreeze> is number of fields to freeze in TBrowse.  Defaults
 *     to 0 if not passed.
 *
 *     <lSaveScrn> is a logical indicating whether or not you want to
 *     save the screen from the calling program.  Defaults to .T. if
 *     not passed.
 *
 *     <cColorList> is a list of colors for the TBrowse columns.
 *     The 1st color is used as SAY/TBrowse Background and the
 *     3rd and 4th colors are used as part of column:defColor := {3, 4}

 *     Thus if you pass a cColorList, you MUST pass at least 4 colors.
 *     Defaults to "N/W, N/BG, B/W, B/BG, B/W, B/BG, R/W, B/R" if not passed.
 *
 *     <cColorShad> is the color of the TBrowse box shadow.  Defaults
 *     to "N/N" if not passed.
 *
 *     <nTop>, <nLeft>, <nBottom>, <nRight> are the coordinates of
 *     the area to display the TBrowse in.  Defaults to 2, 2,
 *     MAXROW() - 2, MAXCOL() - 2 with shadowed box, i.e. full screen.
 *  $RETURNS$
 *     nRecno is the number of the record selected by the <Enter> key.
 *     0 is returned if there are either no records matching the WHILE
 *     condition or an <Esc> is pressed instead of an <Enter>
 *  $DESCRIPTION$
 *     This is a demonstration of TBrowse with a WHILE condition for an
 *     indexed database.
 *  $EXAMPLES$
 *     * This example will only show those people with last name of "JONES"
 *     * in the TBNames.dbf which contains at least the fields:
 *     * Last, First, City AND is indexed on Last + First.
 *     LOCAL nRecSel    := 0
 *     LOCAL aFields    := {}
 *     LOCAL bWhile     := {||TBNames->Last = "JONES"}
 *     LOCAL cKey       := "JONES"
 *     LOCAL nFreeze    := 1
 *     LOCAL lSaveScrn  := .t.
 *     LOCAL cColorList := "N/W, N/BG, B/W, B/BG, B/W, B/BG, R/W, B/R"
 *     LOCAL cColorShad := "N/N"
 *
 *     USE TBNames INDEX TBNames NEW // indexed on Last + First
 *
 *     * Pass Heading as character and Field as Block including Alias
 *     * To eliminate the need to use FIELDWBLOCK() function in FT_BRWSWHL()
 *     AADD(aFields, {"Last Name" , {||TBNames->Last}  } )
 *     AADD(aFields, {"First Name", {||TBNames->First} } )
 *     AADD(aFields, {"City"      , {||TBNames->City}  } )
 *
 *     IF FT_BRWSWHL( aFields, bWhile, cKey, nFreeze, lSaveScrn, ;
 *        cColorList, cColorShad, 3, 6, MaxRow() - 2, MaxCol() - 6) == 0
 *        ? "Sorry, NO Records Were Selected"
 *     ELSE
 *        ? "You Selected: " + TBNames->Last +" "+ ;
 *           TBNames->First +" "+ TBNames->City
 *     ENDIF
 *  $END$
 */


FUNCTION FT_BRWSWHL(aFields, bWhileCond, cKey, nFreeze, lSaveScrn, ;
                    cColorList, cColorShad, nTop, nLeft, nBottom, nRight )

   LOCAL b, column, cType, i
   LOCAL cHead, bField, lKeepScrn, cScrnSave
   LOCAL cColorSave, cColorBack, nCursSave
   LOCAL lMore, nKey, nPassRec
   DEFAULT nFreeze TO 0, ;
           lSaveScrn  TO .t., ;
           cColorList TO "N/W, N/BG, B/W, B/BG, B/W, B/BG, R/W, B/R", ;
           cColorShad TO "N/N", ;
           nTop       TO 2, ;
           nLeft      TO 2, ;
           nBottom    TO MaxRow() - 2, ;
           nRight     TO MaxCol() - 2
   lKeepScrn := (PCOUNT() > 6)

   SEEK cKey
   IF .NOT. FOUND() .OR. LASTREC() == 0
      RETURN(0)
   ENDIF

   /* make new browse object */
   b := TBrowseDB(nTop, nLeft, nBottom, nRight)

   /* default heading and column separators */
   b:headSep := "мям"
   b:colSep  := " Ё "
   b:footSep := "мом"

   /* add custom 'TbSkipWhil' (to handle passed condition) */
   b:skipBlock := {|x| TbSkipWhil(x, bWhileCond)}

   /* Set up substitute goto top and goto bottom */
   /* with While's top and bottom records        */
   b:goTopBlock    := {|| TbWhileTop(cKey)}
   b:goBottomBlock := {|| TbWhileBot(cKey)}

   /* colors */
   b:colorSpec := cColorList

   /* add a column for each field in the current workarea */
   FOR i = 1 TO LEN(aFields)
      cHead  := aFields[i, 1]
      bField := aFields[i, 2]

      /* make the new column */
      column := TBColumnNew( cHead, bField )

      /* these are color setups from tbdemo.prg from Nantucket */
      * IF ( cType == "N" )
      *   column:defColor := {5, 6}
      *   column:colorBlock := {|x| if( x < 0, {7, 8}, {5, 6} )}
      *ELSE
      *   column:defColor := {3, 4}
      *ENDIF

      /* To simplify I just used 3rd and 4th colors from passed cColorList */
      /* This way 1st is SAY, 2nd is GET, 3rd and 4th are used here,
      /* 5th is Unselected Get, extras can be used as in tbdemo.prg */
      column:defColor := {3, 4}

      b:addColumn(column)
   NEXT

   /* freeze columns */
   IF nFreeze <> 0
      b:freeze := nFreeze
   ENDIF

   /* save old screen and colors */
   IF lSaveScrn
      cScrnSave = SAVESCREEN(0, 0, MaxRow(), MaxCol())
   ENDIF
   cColorSave := SetColor()

   /* Background Color Is Based On First Color In Passed cColorList
   cColorBack := IF(',' $ cColorList, ;
      SUBSTR(cColorList, 1, AT(',', cColorList) - 1), cColorList )

   IF .NOT. lKeepScrn
      SetColor(cColorBack)
      CLEAR SCREEN
   ENDIF

   /* make a window shadow */
   SetColor(cColorShad)
   @ nTop+1, nLeft+1 CLEAR TO nBottom+1, nRight+1
   SetColor(cColorBack)
   @ nTop, nLeft CLEAR TO nBottom, nRight
   SetColor(cColorSave)

   nCursSave := SetCursor(0)

   lMore := .t.
   WHILE (lMore)
      /* stabilize the display */
      nKey := 0
      DISPBEGIN()
      DO WHILE nKey == 0 .AND. .NOT. b:stable
          b:stabilize()
          nKey := InKey()
      ENDDO
      DISPEND()

      IF ( b:stable )
         /* display is stable */
         IF ( b:hitTop .OR. b:hitBottom )
            Tone(125, 0)
         ENDIF

         // Make sure that the current record is showing
         // up-to-date data in case we are on a network.
         DISPBEGIN()
         b:refreshCurrent()
         DO WHILE .NOT. b:stabilize()
         ENDDO
         DISPEND()

         /* everything's done; just wait for a key */
         nKey := INKEY(0)
      ENDIF

      /* process key */
      DO CASE
      CASE ( nKey == K_DOWN )
         b:down()

      CASE ( nKey == K_UP )
         b:up()

      CASE ( nKey == K_PGDN )
         b:pageDown()

      CASE ( nKey == K_PGUP )
         b:pageUp()

      CASE ( nKey == K_CTRL_PGUP )
         b:goTop()

      CASE ( nKey == K_CTRL_PGDN )
         b:goBottom()

      CASE ( nKey == K_RIGHT )
         b:right()

      CASE ( nKey == K_LEFT )
         b:left()

      CASE ( nKey == K_HOME )
         b:home()

      CASE ( nKey == K_END )
         b:end()

      CASE ( nKey == K_CTRL_LEFT )
         b:panLeft()

      CASE ( nKey == K_CTRL_RIGHT )
         b:panRight()

      CASE ( nKey == K_CTRL_HOME )
         b:panHome()

      CASE ( nKey == K_CTRL_END )
         b:panEnd()

      CASE ( nKey == K_ESC )
         nPassRec := 0
         lMore := .f.

      CASE ( nKey == K_RETURN )
         nPassRec := RECNO()
         lMore := .f.
      ENDCASE
   ENDDO  // for WHILE (lmore)

   /* restore old screen */
   IF lSaveScrn
      RESTSCREEN(0, 0, MaxRow(), MaxCol(), cScrnSave)
   ENDIF
   SetCursor(nCursSave)
   SetColor(cColorSave)

RETURN (nPassRec)

/* -------------------------------------------------------------------- */

STATIC FUNCTION TbSkipWhil(n, bWhileCond)
   LOCAL i := 0
   IF n == 0 .OR. LASTREC() == 0
      SKIP 0  // significant on a network

   ELSEIF ( n > 0 .AND. RECNO() <> LASTREC() + 1)
      WHILE ( i < n )
         SKIP 1
         IF ( EOF() .OR. .NOT. Eval(bWhileCond) )
            SKIP -1
            EXIT
         ENDIF
         i++
      ENDDO

   ELSEIF ( n < 0 )
      WHILE ( i > n )
         SKIP -1
         IF ( BOF() )
            EXIT
         ELSEIF .NOT. Eval( (bWhileCond) )
            SKIP
            EXIT
         ENDIF
         i--
      ENDDO
   ENDIF
RETURN (i)
* EOFcn TbSkipWhil()

/* -------------------------------------------------------------------- */

STATIC FUNCTION TbWhileTop(cKey)
   SEEK cKey
RETURN NIL

/* -------------------------------------------------------------------- */

STATIC FUNCTION TbWhileBot(cKey)
   * SeekLast: Finds Last Record For Matching Key
   * Developed By Jon Cole
   * With softseek set on, seek the first record after condition.
   * This is accomplished by incrementing the right most character of the
   * string cKey by one ascii character.  After SEEKing the new string,
   * back up one record to get to the last record which matches cKey.
   #include "set.ch"
   LOCAL cSoftSave := SET(_SET_SOFTSEEK, .t.)
   SEEK LEFT(cKey, LEN(cKey) -1) + CHR( ASC( RIGHT(cKey,1) ) +1)
   SET(_SET_SOFTSEEK, cSoftSave)
   SKIP -1
RETURN NIL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
