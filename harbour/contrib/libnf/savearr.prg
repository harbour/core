/*
 * File......: SAVEARR.PRG
 * Author....: David Barrett
 * CIS ID....: 72037,105
 *
 * This is an original work by David Barrett and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 22:04:18   GLENN
 * A few users have reported that these functions do not support
 * multi-dimensional arrays.  Until the bugs are verified and
 * workarounds or re-writes devised, a warning has been placed in the
 * documentation.
 *
 *    Rev 1.2   15 Aug 1991 23:06:06   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:39:38   GLENN
 * Initial revision.
 *
 *
 */



MEMVAR lRet

#ifdef FT_TEST              // test program to demonstrate functions

 LOCAL  aArray := { {'Invoice 1', CTOD('04/15/91'), 1234.32, .T.},;
                {'Invoice 2', DATE(), 234.98, .F.},;
                {'Invoice 3', DATE() + 1, 0, .T.}  }, aSave
 LOCAL nErrorCode := 0
 FT_SAVEARR(aArray,'INVOICE.DAT',@nErrorCode)
 IF nErrorCode = 0
   CLS
   DispArray(aArray)
   aSave := FT_RESTARR('INVOICE.DAT',@nErrorCode)
   IF nErrorCode = 0
     DispArray(aSave)
   ELSE
      ? 'Error restoring array'
   ENDIF
 ELSE
   ? 'Error writing array'
 ENDIF
 RETURN

 FUNCTION DispArray(aTest)
   LOCAL nk
   FOR nk := 1 TO LEN(aTest)
     ? aTest[nk, 1]
     ?? '  '
     ?? DTOC(aTest[nk, 2])
     ?? '  '
     ?? STR(aTest[nk, 3])
     ?? '  '
     ?? IF(aTest[nk, 4], 'true', 'false')
   NEXT
 RETURN Nil
#endif




/*  $DOC$
 *  $FUNCNAME$
 *     FT_SAVEARR()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Save Clipper array to a disc file.
 *  $SYNTAX$
 *     FT_SAVEARR( <aArray>, <cFileName>, <nErrorCode> ) -> lRet
 *  $ARGUMENTS$
 *     <aArray> is any Clipper array except those containing
 *     compiled code blocks.
 *
 *     <cFileName> is a DOS file name.
 *
 *     <nErrorCode> will return any DOS file error.
 *
 *     All arguments are required.
 *
 *  $RETURNS$
 *     .F. if there was a DOS file error or the array contained
 *     code blocks, otherwise returns .T.
 *  $DESCRIPTION$
 *     FT_SAVEARR() saves any Clipper array, except those
 *     containing compiled code blocks, to a disc file.  The
 *     array can be restored from the disc file using
 *     FT_RESTARR().
 *
 *     [10/1/92 Librarian note:
 *
 *     This function does not appear to work with multi-dimensional
 *     arrays.  If you'd care to modify it to support this feature,
 *     please do and send it to Glenn Scott 71620,1521.]
 *
 *
 *  $EXAMPLES$
 *    aArray := { {'Invoice 1',CTOD('04/15/91'),1234.32,.T.},;
 *                {'Invoice 2',DATE(),234.98,.F.},;
 *                {'Invoice 3',DATE() + 1,0,.T.}  }
 *    nErrorCode := 0
 *    FT_SAVEARR(aArray,'INVOICE.DAT',@nErrorCode)
 *    IF nErrorCode = 0
 *      aSave := FT_RESTARR('INVOICE.DAT',@nErrorCode)
 *      IF nErrorCode # 0
 *         ? 'Error restoring array'
 *      ENDIF
 *    ELSE
 *      ? 'Error writing array'
 *    ENDIF
 *
 *  $SEEALSO$
 *     FT_RESTARR()
 *  $END$
 */


FUNCTION FT_SAVEARR(aArray, cFileName, nErrorCode)
 LOCAL nHandle, lRet
 nHandle = FCREATE(cFileName)
 nErrorCode = FError()
 IF nErrorCode = 0
   lRet := _ftsavesub(aArray, nHandle, @nErrorCode)
   FCLOSE(nHandle)
   IF (lRet) .AND. (FERROR() # 0)
      nErrorCode = FERROR()
      lRet = .F.
    ENDIF
 ELSE
   lRet = .F.
 ENDIF
 RETURN lRet

STATIC FUNCTION _ftsavesub(xMemVar, nHandle, nErrorCode)
 LOCAL cValType, nLen, cString
 PRIVATE lRet       // accessed in code block
 lRet := .T.
 cValType := ValType(xMemVar)
 FWrite(nHandle, cValType, 1)
 IF FError() = 0
   DO CASE
     CASE cValType = "A"
       nLen := Len(xMemVar)
       FWrite(nHandle, L2Bin(nLen), 4)
       IF FError() = 0
         AEVAL(xMemVar, {|xMemVar1| lRet := _ftsavesub(xMemVar1, nHandle) } )
       ELSE
         lRet = .F.
       ENDIF
     CASE cValType = "B"
       lRet := .F.
     CASE cValType = "C"
       nLen := Len(xMemVar)
       FWrite(nHandle, L2Bin(nLen), 4)
       FWrite(nHandle, xMemVar)
     CASE cValType = "D"
       nLen := 8
       FWrite(nHandle, L2Bin(nLen), 4)
       FWrite(nHandle, DTOC(xMemVar))
     CASE cValType = "L"
       nLen := 1
       FWrite(nHandle, L2Bin(nLen), 4)
       FWrite(nHandle, IF(xMemVar, "T", "F") )
     CASE cValType = "N"
       cString := STR(xMemVar)
       nLen := LEN(cString)
       FWrite(nHandle, L2Bin(nLen), 4)
       FWrite(nHandle, cString)
   ENDCASE
 ELSE
   lRet = .F.
 ENDIF
 nErrorCode = FError()
 RETURN lRet


/*  $DOC$
 *  $FUNCNAME$
 *     FT_RESTARR()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Restore a Clipper array from a disc file
 *  $SYNTAX$
 *     FT_RESTARR( <cFileName>, <nErrorCode> ) -> aArray
 *  $ARGUMENTS$
 *     <cFileName> is a DOS file name.
 *
 *     <nErrorCode> will return any DOS file error.
 *
 *     All arguments are required.
 *  $RETURNS$
 *     Return an array variable.
 *  $DESCRIPTION$
 *     FT_RESTARR() restores an array which was saved to
 *     a disc file using FT_SAVEARR().
 *
 *     [10/1/92 Librarian note:
 *
 *     This function does not appear to work with multi-dimensional
 *     arrays.  If you'd care to modify it to support this feature,
 *     please do and send it to Glenn Scott 71620,1521.]
 *
 *  $EXAMPLES$
 *    aArray := { {'Invoice 1',CTOD('04/15/91'),1234.32,.T.},;
 *                {'Invoice 2',DATE(),234.98,.F.},;
 *                {'Invoice 3',DATE() + 1,0,.T.}  }
 *    nErrorCode := 0
 *    FT_SAVEARR(aArray,'INVOICE.DAT',@nErrorCode)
 *    IF nErrorCode = 0
 *      aSave := FT_RESTARR('INVOICE.DAT',@nErrorCode)
 *      IF nErrorCode # 0
 *         ? 'Error restoring array'
 *      ENDIF
 *    ELSE
 *      ? 'Error writing array'
 *    ENDIF
 *
 *  $SEEALSO$
 *     FT_SAVEARR()
 *  $END$
 */

FUNCTION FT_RESTARR(cFileName, nErrorCode)
 LOCAL nHandle, aArray
 nHandle := FOPEN(cFileName)
 nErrorCode := FError()
 IF nErrorCode = 0
  aArray := _ftrestsub(nHandle, @nErrorCode)
  FCLOSE(nHandle)
 ELSE
   aArray := {}
 ENDIF
 RETURN aArray

STATIC FUNCTION _ftrestsub(nHandle, nErrorCode)
  LOCAL cValType, nLen, cLenStr, xMemVar, cMemVar, nk
  cValType := ' '
  FREAD(nHandle, @cValType, 1)
  cLenStr := SPACE(4)
  FREAD(nHandle, @cLenStr, 4)
  nLen = Bin2L(cLenStr)
  nErrorCode = FError()
  IF nErrorCode = 0
    DO CASE
      CASE cValType = "A"
        xMemVar := {}
        FOR nk := 1 TO nLen
          AADD(xMemVar, _ftrestsub(nHandle))      // Recursive call
        NEXT
      CASE cValType = "C"
        xMemVar := SPACE(nLen)
        FREAD(nHandle, @xMemVar, nLen)
      CASE cValType = "D"
        cMemVar = SPACE(8)
        FREAD(nHandle, @cMemVar,8)
        xMemVar := CTOD(cMemVar)
      CASE cValType = "L"
        cMemVar := ' '
        FREAD(nHandle, @cMemVar, 1)
        xMemVar := (cMemVar =  "T")
      CASE cValType = "N"
        cMemVar := SPACE(nLen)
        FREAD(nHandle, @cMemVar, nLen)
        xMemVar = VAL(cMemVar)
    ENDCASE
    nErrorCode := FERROR()
  ENDIF
  RETURN xMemVar
