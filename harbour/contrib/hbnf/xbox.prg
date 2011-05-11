/*
 * $Id$
 */

/*
 * File......: xbox.prg
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

#ifdef FT_TEST
   FUNCTION MAIN()
      local i
      setcolor('W/B')
      * clear screen
      for i := 1 to 24
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

   RETURN NIL
#endif

/* NOTE: In original NF, flag parameters were also accepted when
         having extra characters (f.e. "DOUBLE" instead of "D"),
         but only if _SET_EXACT was set to .F., Harbour accepts them
         that way regardless of _SET_EXACT setting. [vszakats] */

FUNCTION FT_XBOX(cJustType,; // "L" -> left, otherwise centered
                cRetWait, ; // "W" -> wait for keypress before continuing
                cBorType, ; // "D" -> double, anything else single border
                cBorColor,; // color string for border
                cBoxColor,; // color string for text
                nStartRow,; // upper row of box.  99=center vertically
                nStartCol,; // left edge of box.  99=center horizontally
                cLine1, cLine2, cLine3, cLine4, cLine5, cLine6, cLine7, cLine8)

  LOCAL nLLen := 0, ;
    ;// cOldColor,  ;
        nLCol,      ;
        nRCol,      ;
        nTRow,      ;
        nBRow,      ;
        nLoop,      ;
        nSayRow,    ;
        nSayCol,    ;
        nNumRows,   ;
        aLines_[8]

  IF cJustType == NIL
     cJustType := ""
  ENDIF
  IF cRetWait == NIL
     cRetWait := ""
  ENDIF
  IF cBorType == NIL
     cBorType := ""
  ENDIF

  // validate parameters
  cJustType := iif(ValType(cJustType)=='C',Upper(cJustType),'')
  cRetWait  := iif(ValType(cRetWait )=='C',Upper(cRetWait), '')
  cBorType  := iif(ValType(cBorType )=='C',Upper(cBorType), '')
  cBorColor := iif(ValType(cBoxColor)=='C',cBorColor, 'N/W')
  cBoxColor := iif(ValType(cBoxColor)=='C',cBoxColor, 'W/N')
  nStartRow := iif(ValType(nStartRow)=='N',nStartRow,99)
  nStartCol := iif(ValType(nStartCol)=='N',nStartCol,99)

  nNumRows := Min(PCount()-7,8)

  //establish array of strings to be displayed
  aLines_[1] := iif(ValType(cLine1) == 'C',AllTrim(SubStr(cLine1,1,74)),'')
  aLines_[2] := iif(ValType(cLine2) == 'C',AllTrim(SubStr(cLine2,1,74)),'')
  aLines_[3] := iif(ValType(cLine3) == 'C',AllTrim(SubStr(cLine3,1,74)),'')
  aLines_[4] := iif(ValType(cLine4) == 'C',AllTrim(SubStr(cLine4,1,74)),'')
  aLines_[5] := iif(ValType(cLine5) == 'C',AllTrim(SubStr(cLine5,1,74)),'')
  aLines_[6] := iif(ValType(cLine6) == 'C',AllTrim(SubStr(cLine6,1,74)),'')
  aLines_[7] := iif(ValType(cLine7) == 'C',AllTrim(SubStr(cLine7,1,74)),'')
  aLines_[8] := iif(ValType(cLine8) == 'C',AllTrim(SubStr(cLine8,1,74)),'')
  ASize(aLines_,Min(nNumRows,8))

  // determine longest line
  nLoop := 1
  AEVAL(aLines_,{|| nLLen:=Max(nLLen,Len(aLines_[nLoop])),nLoop++})

  // calculate corners
  nLCol := iif(nStartCol==99,Int((76-nLLen)/2),Min(nStartCol,74-nLLen))
  nRCol := nLCol+nLLen+3
  nTRow := iif(nStartRow==99,INT((24-nNumRows)/2),Min(nStartRow,22-nNumRows))
  nBRow := nTRow+nNumRows+1

  // form box and border

  // save screen color and set new color
  //cOldColor := SetColor(cBoxColor)
  @ nTRow,nLCol Clear to nBRow,nRCol

  // draw border
  SetColor(cBorColor)
  IF Left( cBorType, 1 ) == "D"
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
                 nSayCol := iif( Left( cJustType, 1 ) == 'L',;
                                nLCol+2,;
                                nLCol+2+(nLLen-Int(Len(aLines_[nLoop])))/2),;
                 nLoop++,;
                 _FTSAY(nSayRow,nSayCol,cSayStr);
                })

  // wait for keypress if desired
  IF Left( cRetWait, 1 ) == 'W'
    Inkey(0)
  ENDIF

  RETURN NIL

STATIC FUNCTION _FTSAY(nSayRow,nSayCol,cSayStr)
    @ nSayRow,nSayCol SAY cSayStr
    RETURN NIL
