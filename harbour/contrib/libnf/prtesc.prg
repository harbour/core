/*
 * File......: PRTESC.PRG
 * Author....: Steven Tyrakowski
 * CIS ID....: ?
 *
 * This is an original work by Steven Tyrakowski and is placed
 * in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:26   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:42   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:02   GLENN
 * Nanforum Toolkit
 *
 */

#ifdef FT_TEST
  FUNCTION MAIN( cParm1 )
     *-------------------------------------------------------
     * Sample routine to test function from command line
     *-------------------------------------------------------

    IF PCount() > 0
      ? FT_ESCCODE( cParm1 )
    ELSE
      ? "Usage: PRT_ESC  'escape code sequence' "
      ? "            outputs converted code to  standard output"
      ?
    ENDIF
  RETURN (nil)
#endif

/*  $DOC$
 *  $FUNCNAME$
 *     FT_ESCCODE()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Convert Lotus style escape codes
 *  $SYNTAX$
 *     FT_ESCCODE( <cASCII> )  -> <cPrinterFormat>
 *  $ARGUMENTS$
 *     <cASCII> is the ASCII representation of the printer control
 *		codes in Lotus 123 format (e.g. "\027E" for Chr(27)+"E")
 *
 *		"\nnn" will be converted to Chr(nnn)
 *		"\\" will be converted to "\"
 *  $RETURNS$
 *     The binary version of an ASCII coded printer setup string.
 *  $DESCRIPTION$
 *     This function is useful for allowing the user to enter printer
 *     control codes in Lotus-style ASCII format, and then having
 *     this function convert that code to the format that the printer
 *     needs to receive.
 *  $EXAMPLES$
 *     cSetup = "\015"          // default = Epson compressed print
 *     UserInput( @cSetup )     // Let user modify setup code
 *     SET DEVICE TO PRINT      // get ready to print
 *     ?? FT_ESCCODE( cSetup )  // Output the converted code
 *  $END$
 */


FUNCTION FT_ESCCODE( cInput )

LOCAL cOutput  := ""             ,;
	  cCurrent					 ,;
	  nPointer := 1 			 ,;
	  nLen	   := Len( cInput )

  DO WHILE nPointer <= nLen

	cCurrent := Substr( cInput, nPointer, 1 )

    DO CASE

       CASE cCurrent == "\" .AND. ;
		IsDigit(Substr(cInput, nPointer+1, 1) ) .AND. ;
		IsDigit(Substr(cInput, nPointer+2, 1) ) .AND. ;
		IsDigit(Substr(cInput, nPointer+3, 1) )
	   cOutput  += Chr(Val(Substr(cInput, nPointer+1,3)))
	   nPointer += 4

       CASE cCurrent == "\" .AND. ;
		 Substr(cInput, nPointer+1, 1) == "\"
	   cOutput += "\"
	   nPointer += 2

       OTHERWISE
	   cOutput += cCurrent
	   nPointer++

    ENDCASE
  ENDDO

RETURN cOutput
