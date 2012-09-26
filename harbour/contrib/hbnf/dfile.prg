/*
 * $Id$
 */

/*
 * File......: dfile.prg
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

THREAD STATIC t_nHandle := 0

#ifdef FT_TEST

PROCEDURE Main()

   @ 0, 0 CLEAR

   cInFile   := "ft_dfile.prg"
   CKEY      := ""
   NNCOLOR   := 7
   NHCOLOR   := 15
   NCOLSKIP  := 5
   NRMARGIN  := 132
   CEXITKEYS := "AABBC       "
   LBROWSE   := .F.
   NSTART    := 1
   NBUFFSIZE := 4096

   @ 0, 0  SAY "ENTER FILENAME: "   GET CINFILE
   @ 1, 0  SAY "    FOREGROUND: "   GET NNCOLOR   PICTURE "999"
   @ 2, 0  SAY "     HIGHLIGHT: "   GET NHCOLOR   PICTURE "999"
   @ 3, 0  SAY "     EXIT KEYS: "   GET CEXITKEYS
   @ 4, 0  SAY "   BUFFER SIZE: "   GET NBUFFSIZE PICTURE "9999"
   @ 1, 40 SAY "COLUMN INCREMENT: " GET NCOLSKIP  PICTURE "999"
   @ 2, 40 SAY "   MAX LINE SIZE: " GET NRMARGIN  PICTURE "999"
   @ 3, 40 SAY "     BROWSE MODE? " GET LBROWSE   PICTURE "Y"

   READ

    /*
     * REMEMBER A WINDOW WILL BE ONE SIZE LESS AND GREATER THAN THE PASSED COORD.'S
     *
     * THE 9TH PARAMETER CONTAINS THE KEYS THAT THE ROUTINE WILL TERMINATE ON
     * AND THE CHR(143) represents the F3 key.
     *
     */

   @ 4, 9 TO 11, 71

   FT_DFSETUP( cInFile, 5, 10, 10, 70, nStart, ;
      nNColor, nHColor, cExitKeys + Chr( 143 ), ;
      lBrowse, nColSkip, nRMargin, nBuffSize )

   cKey := FT_DISPFILE()

   FT_DFCLOSE()

   @ 20, 0 SAY "Key pressed was: " + '[' + cKey + ']'

   RETURN

#endif

FUNCTION FT_DFSETUP( cInFile, nTop, nLeft, nBottom, nRight, ;
      nStart, nCNormal, nCHighlight, cExitKeys, ;
      lBrowse, nColSkip, nRMargin, nBuffSize )

   LOCAL rval

   IF File( cInFile )
      nTop    := iif( HB_ISNUMERIC( nTop )   , nTop,           0 )
      nLeft   := iif( HB_ISNUMERIC( nLeft )  , nLeft,          0 )
      nBottom := iif( HB_ISNUMERIC( nBottom ), nBottom, MaxRow() )
      nRight  := iif( HB_ISNUMERIC( nRight ) , nRight,  MaxCol() )

      nCNormal    := iif( HB_ISNUMERIC( nCNormal )   , nCNormal,     7 )
      nCHighlight := iif( HB_ISNUMERIC( nCHighlight ), nCHighlight, 15 )

      nStart    := iif( HB_ISNUMERIC( nStart )   , nStart,      1 )
      nColSkip  := iif( HB_ISNUMERIC( nColSkip ) , nColSkip,    1 )
      lBrowse   := iif( HB_ISLOGICAL( lBrowse )  , lBrowse,   .F. )

      nRMargin  := iif( HB_ISNUMERIC( nRMargin ) , nRMargin,   255 )
      nBuffSize := iif( HB_ISNUMERIC( nBuffSize ), nBuffSize, 4096 )

      cExitKeys := iif( HB_ISSTRING( cExitKeys ) , cExitKeys,  "" )

      cExitKeys := iif( Len( cExitKeys ) > 25, SubStr( cExitKeys, 1, 25 ), cExitKeys )

      t_nHandle := FOpen( cInFile )

      rval := FError()

      IF rval == 0
         rval := _FT_DFINIT( t_nHandle, nTop, nLeft, nBottom, nRight, ;
            nStart, nCNormal, nCHighlight, cExitKeys, ;
            lBrowse, nColSkip, nRMargin, nBuffSize )
      ENDIF
   ELSE
      rval := 2       // simulate a file-not-found DOS file error
   ENDIF

   RETURN rval

FUNCTION FT_DFCLOSE()

   IF t_nHandle > 0
      _FT_DFCLOS()

      FClose( t_nHandle )

      t_nHandle := 0
   ENDIF

   RETURN NIL
