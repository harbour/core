/*
 * File......: DFILE.PRG
 * Author....: Mike Taylor
 * CIS ID....: ?
 *
 * This is an original work by Mike Taylor and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:24:14   GLENN
 * Don Caton corrected some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:03:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:32   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:08   GLENN
 * Nanforum Toolkit
 *
 */



static nHandle := 0

#ifdef FT_TEST

    FUNCTION MAIN()

    @ 0,0 CLEAR

    cInFile   := "FT_DFILE.PRG"
    CKEY      := ""
    NNCOLOR   := 7
    NHCOLOR   := 15
    NCOLSKIP  := 5
    NRMARGIN  := 132
    CEXITKEYS := "AABBC       "
    LBROWSE   := .F.
    NSTART    := 1
    NBUFFSIZE := 4096

    @ 0,0  SAY "ENTER FILENAME: "   GET CINFILE
    @ 1,0  SAY "    FOREGROUND: "   GET NNCOLOR   PICTURE "999"
    @ 2,0  SAY "     HIGHLIGHT: "   GET NHCOLOR   PICTURE "999"
    @ 3,0  SAY "     EXIT KEYS: "   GET CEXITKEYS
    @ 4,0  SAY "   BUFFER SIZE: "   GET NBUFFSIZE PICTURE "9999"
    @ 1,40 SAY "COLUMN INCREMENT: " GET NCOLSKIP  PICTURE "999"
    @ 2,40 SAY "   MAX LINE SIZE: " GET NRMARGIN  PICTURE "999"
    @ 3,40 SAY "     BROWSE MODE? " GET LBROWSE   PICTURE "Y"

    READ

    /*
     * REMEMBER A WINDOW WILL BE ONE SIZE LESS AND GREATER THAN THE PASSED COORD.'S
     *
     * THE 9TH PARAMETER CONTAINS THE KEYS THAT THE ROUTINE WILL TERMINATE ON
     * AND THE CHR(143) represents the F3 key.
     *
     */

    @ 4,9 TO 11,71

    FT_DFSETUP(cInFile, 5, 10, 10, 70, nStart,;
               nNColor, nHColor, cExitKeys + CHR(143),;
               lBrowse, nColSkip, nRMargin, nBuffSize)

    cKey := FT_DISPFILE()

    FT_DFCLOSE()

    @ 20,0 SAY "Key pressed was: " + '[' + cKey + ']'

    return (NIL)

#endif




/*  $DOC$
 *  $FUNCNAME$
 *     FT_DFSETUP()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Set up parameters for FT_DISPFILE()
 *  $SYNTAX$
 *     FT_DFSETUP( <cInFile>, <nTop>, <nLeft>, <nBottom>, <nRight>, ;
 *              <nStart>, <nCNormal>, <nCHighlight>, <cExitKeys>,   ;
 *              <lBrowse>, <nColSkip>, <nRMargin>, <nBuffSize> ) -> nResult
 *  $ARGUMENTS$
 *        <cInFile>     - text file to display (full path and filename)
 *        <nTop>        - upper row of window
 *        <nLeft>       - left col of window
 *        <nBottom>     - lower row of window
 *        <nRight>      - right col of window
 *        <nStart>      - line to place highlight at startup
 *        <nCNormal>    - normal text color     (numeric attribute)
 *        <nCHighlight> - text highlight color  (numeric attribute)
 *        <cExitKeys>   - terminating key list  (each byte of string is a
 *                        key code)
 *        <lBrowse>     - act-like-a-browse-routine flag
 *        <nColSkip>    - col increment for left/right arrows
 *        <nRMargin>    - right margin - anything to right is truncated
 *        <nBuffSize>   - size of the paging buffer
 *  $RETURNS$
 *     0 if successful, FError() code if not
 *  $DESCRIPTION$
 *     Note: make sure you allocate a buffer large enough to hold enough
 *     data for the number of lines that you have in the window.  Use the
 *     following formula as a guideline:
 *
 *        buffer size = (# of line) + 1 * RMargin
 *
 *     This is the smallest you should make the buffer.  For normal use,
 *     4096 bytes is recommended
 *  $EXAMPLES$
 *     @ 4,9 TO 11,71
 *
 *     FT_DFSETUP("test.txt", 5, 10, 10, 70, 1, 7, 15,;
 *                "AaBb" + Chr(143), .T., 5, 132, 4096)
 *
 *     cKey = FT_DISPFILE()
 *
 *     FT_DFCLOSE()
 *
 *     @ 20,0 SAY "Key that terminated FT_DISPFILE() was: " + '[' + cKey + ']'
 *  $SEEALSO$
 *     FT_DISPFILE() FT_DFCLOSE()
 *  $END$
 */



function FT_DFSETUP(cInFile, nTop, nLeft, nBottom, nRight,;
                    nStart, nCNormal, nCHighlight, cExitKeys,;
                    lBrowse, nColSkip, nRMargin, nBuffSize )

  local rval := 0

  if File(cInFile)
     nTop    := if(ValType(nTop)    == "N", nTop,           0)
     nLeft   := if(ValType(nLeft)   == "N", nLeft,          0)
     nBottom := if(ValType(nBottom) == "N", nBottom, MaxRow())
     nRight  := if(ValType(nRight)  == "N", nRight,  MaxCol())

     nCNormal     := if(ValType(nCNormal)    == "N", nCNormal,     7)
     nCHighlight := if(ValType(nCHighlight) == "N", nCHighlight, 15)

     nStart    := if(ValType(nStart)    == "N", nStart,      1)
     nColSkip  := if(ValType(nColSkip)  == "N", nColSkip,    1)
     lBrowse   := if(ValType(lBrowse)   == "L", lBrowse,   .F.)

     nRMargin  := if(ValType(nRMargin)  == "N", nRMargin,   255)
     nBuffSize := if(ValType(nBuffSize) == "N", nBuffSize, 4096)

     cExitKeys := if(ValType(cExitKeys) == "C", cExitKeys,  "")

     cExitKeys := if(Len(cExitKeys) > 25, SubStr(cExitKeys, 1, 25), cExitKeys)

     nHandle := FOpen(cInFile)

     rval := FError()

     if ( rval == 0 )
           rval := _FT_DFINIT(nHandle, nTop, nLeft, nBottom, nRight,;
                              nStart, nCNormal, nCHighlight, cExitKeys,;
                              lBrowse, nColSkip, nRMargin, nBuffSize)
     endif
  else
     rval := 2       // simulate a file-not-found DOS file error
  endif

return (rval)



/*  $DOC$
 *  $FUNCNAME$
 *     FT_DFCLOSE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Close file displayed by FT_DISPFILE()
 *  $SYNTAX$
 *     FT_DFCLOSE() -> NIL
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     Closes the file opened by FT_DFSETUP()
 *  $EXAMPLES$
 *     @ 4,9 TO 11,71
 *
 *     FT_DFSETUP("test.txt", 5, 10, 10, 70, 1, 7, 15,;
 *                 "AaBb" + Chr(143), .T., 5, 132, 4096)
 *
 *     cKey = FT_DISPFILE()
 *
 *     FT_DFCLOSE()
 *
 *     @ 20,0 SAY "Key that terminated FT_DISPFILE() was: " + '[' + cKey + ']'
 *  $SEEALSO$
 *     FT_DFSETUP() FT_DISPFILE()
 *  $END$
 */




function FT_DFCLOSE()

  if ( nHandle > 0 )
     _FT_DFCLOS()

     FClose(nHandle)

     nHandle := 0
  endif

  return (NIL)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
