/*
 * File......: XBOX.PRG
 * Author....: Don Opperthauser
 * CIS ID....: ?
 *
 * This is an original work by Don Opperthauser and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:47:06   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:05:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 17:55:50   GLENN
 * Fixed bug where extra blank line was displayed in the box.
 *
 *    Rev 1.0   01 Apr 1991 01:02:34   GLENN
 * Nanforum Toolkit
 *
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_XBOX()
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Display a self-sizing message box and message
 *  $SYNTAX$
 *     FT_XBOX( [ <cJustType> ], [ <cRetWait> ], [ <cBorType> ],   ;
 *              [ <cBorColor> ], [ <cBoxColor> ], [ <nStartRow> ], ;
 *              [ <nStartCol> ], <cLine1>,  <cLine2>, <cLine3>,    ;
 *              <cLine4>, <cLine5>, <cLine6>, <cLine7>, <cLine8> ) -> NIL
 *  $ARGUMENTS$
 *     <cJustType> is a character indicating the type of text justification.
 *     "L" or "l" will cause the text to be left-justified in the box.
 *     Centered text is the default.
 *
 *     <cRetWait> is a character which determines if the function will wait
 *     for a keypress after displaying the box.  "W" or "w" will cause the
 *     function to wait for a keypress before returning control to the
 *     calling routine.  Not waiting is the default
 *
 *     <cBorType> is a character which determines whether a single or double
 *     border will be displayed.  "D" or "d" will cause a double border to
 *     be displayed.  A single border is the default.
 *
 *     <cBorColor> is a character string denoting the border color.  'N/W' is
 *     the default if this parameter is not a string.
 *
 *     <cBoxColor> is a character string denoting the text color.  'W/N' is
 *     the default if this parameter is not a string.
 *
 *     <nStartRow> is a number denoting the starting row.  If '99' is passed,
 *     the box is centered vertically.  If necessary, nStartRow is decreased
 *     so the entire box can be displayed.
 *
 *     <nStartCol> is a number denoting the starting column.  If '99' is passed,
 *     the box is centered horizontally.  If necessary, nStartCol is decreased
 *     so the entire box can be displayed.
 *
 *     <cLine1> thru <cLine8> are 1 to 8 character strings to be displayed.
 *     They are truncated to fit on the screen if necessary.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     FT_XBOX() allows the programmer to display a message box on the screen
 *     without needing to calculate the dimensions of the box.  Only the upper
 *     left corner needs to be defined.  The function will calculate the lower
 *     right corner based on the number and length of strings passed.
 *
 *     A maximum of eight strings can be displayed.  If a string is too long
 *     to fit on the screen it is truncated.
 *
 *     The first seven parameters are optional.  The default settings are:
 *        Lines of text are centered.
 *        Control is returned to the calling routine immediately.
 *        A single line border is painted.
 *        The border is black on white.
 *        The text is white on black.
 *        The box is centered both vertically and horizontally.
 *
 *     WARNING:  Shadowing is achieved by a call to FT_SHADOW(), an assembly
 *               routine not found in this .PRG.  In order to use XBOX,
 *               SHADOW.OBJ must also be present somewhere (if you are using
 *               NANFOR.LIB, then it is).
 *  $EXAMPLES$
 *     The following displays a two-line box with default settings:
 *
 *       FT_XBOX(,,,,,,,'This is a test','of the XBOX() function')
 *
 *     The following uses all optional parameters and displays a three-line
 *     box.  The box is left-justified with a double border.  It has a yellow
 *     on red border and white on blue text.  The function will wait for a
 *     keypress before returning control to the calling routine.
 *
 *       FT_XBOX('L','W','D','GR+/R','W/B',5,10,'It is so nice',;
 *                       'to not have to do the messy chore',;
 *                       'of calculating the box size!')
 *  $END$
 */


#ifdef FT_TEST
   FUNCTION MAIN()
	   local i
	   setcolor('W/B')
	   * clear screen
	   for i = 1 to 24
		   @ i, 0 say replicate('@', 80)
	   next

       FT_XBOX(,,,,,,,'This is a test','of the XBOX() function')
       FT_XBOX('L','W','D','GR+/R','W/B',1,10,'It is so nice',;
                         'to not have to do the messy chore',;
						 'of calculating the box size!')
       FT_XBOX(,'W','D','GR+/R','W/B',16,10,'It is so nice',;
                         'to not have to do the messy chore',;
						 'of calculating the box size!',;
						 'Even though this line is way too long, and is in fact more than 80 characters long, if you care to check!')

   return ( nil )
#endif


FUNCTION FT_XBOX(cJustType,; // "L" = left, otherwise centered
                cRetWait, ; // "W" = wait for keypress before continuing
                cBorType, ; // "D" = double, anything else single border
                cBorColor,; // color string for border
                cBoxColor,; // color string for text
                nStartRow,; // upper row of box.  99=center vertically
                nStartCol,; // left edge of box.  99=center horizontally
                cLine1, cLine2, cLine3, cLine4, cLine5, cLine6, cLine7, cLine8)

  LOCAL nLLen := 0, ;
        cOldColor,  ;
        nLCol,      ;
        nRCol,      ;
        nTRow,      ;
        nBRow,      ;
        nLoop,      ;
        cSayStr,    ;
        nSayRow,    ;
        nSayCol,    ;
        nNumRows,   ;
        aLines_[8]

  // validate parameters
  cJustType := if(ValType(cJustType)='C',Upper(cJustType),'')
  cRetWait  := if(ValType(cRetWait )='C',Upper(cRetWait), '')
  cBorType  := if(ValType(cBorType )='C',Upper(cBorType), '')
  cBorColor := if(ValType(cBoxColor)='C',cBorColor, 'N/W')
  cBoxColor := if(ValType(cBoxColor)='C',cBoxColor, 'W/N')
  nStartRow := if(ValType(nStartRow)='N',nStartRow,99)
  nStartCol := if(ValType(nStartCol)='N',nStartCol,99)

  nNumRows := Min(PCount()-7,8)

  //establish array of strings to be displayed
  aLines_[1] := if(ValType(cLine1) = 'C',AllTrim(SubStr(cLine1,1,74)),'')
  aLines_[2] := if(ValType(cLine2) = 'C',AllTrim(SubStr(cLine2,1,74)),'')
  aLines_[3] := if(ValType(cLine3) = 'C',AllTrim(SubStr(cLine3,1,74)),'')
  aLines_[4] := if(ValType(cLine4) = 'C',AllTrim(SubStr(cLine4,1,74)),'')
  aLines_[5] := if(ValType(cLine5) = 'C',AllTrim(SubStr(cLine5,1,74)),'')
  aLines_[6] := if(ValType(cLine6) = 'C',AllTrim(SubStr(cLine6,1,74)),'')
  aLines_[7] := if(ValType(cLine7) = 'C',AllTrim(SubStr(cLine7,1,74)),'')
  aLines_[8] := if(ValType(cLine8) = 'C',AllTrim(SubStr(cLine8,1,74)),'')
  ASize(aLines_,Min(nNumRows,8))

  // determine longest line
  nLoop := 1
  AEVAL(aLines_,{|| nLLen:=Max(nLLen,Len(aLines_[nLoop])),nLoop++})

  // calculate corners
  nLCol = if(nStartCol=99,Int((76-nLLen)/2),Min(nStartCol,74-nLLen))
  nRCol = nLCol+nLLen+3
  nTRow = if(nStartRow=99,INT((24-nNumRows)/2),Min(nStartRow,22-nNumRows))
  nBRow = nTRow+nNumRows+1

  // form box and border

  // save screen color and set new color
  cOldColor = SetColor(cBoxColor)
  @ nTRow,nLCol Clear to nBRow,nRCol

  // draw border
  SetColor(cBorColor)
  IF cBorType = "D"
    @ nTRow,nLCol TO nBRow,nRCol double
  ELSE
    @ nTRow,nLCol TO nBRow,nRCol
  ENDIF


  // write shadow
  FT_SHADOW(nTRow,nLCol,nBRow,nRCol)

  // print text in box
  SetColor(cBoxColor)
  nLoop :=1
  AEVAL(aLines_,{|cSayStr|;
                 nSayRow := nTRow+nLoop,;
                 nSayCol := if(cJustType = 'L',;
                               nLCol+2,;
                               nLCol+2+(nLLen-Int(Len(aLines_[nLoop])))/2),;
                 nLoop++,;
                 _FTSAY(nSayRow,nSayCol,cSayStr);
                })

  // wait for keypress if desired
  IF cRetWait ='W'
    Inkey(0)
  ENDIF

  RETURN NIL


STATIC FUNCTION _FTSAY(nSayRow,nSayCol,cSayStr)
    @ nSayRow,nSayCol SAY cSayStr
    RETURN NIL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
