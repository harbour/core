/*
 * File......: FindIth.Prg
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:36   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:52   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:22   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_FINDITH()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Find the "ith" occurrence of a substring within a string
 *  $SYNTAX$
 *     FT_FINDITH( <cCheckFor>, <cCheckIn>, <nWhichOccurrence> ;
 *                 [, <lIgnoreCase> ] ) -> <nStringPosition>
 *  $ARGUMENTS$
 *     <cCheckFor> is the string to search for.
 *
 *     <cCheckIn> is the string to search.
 *
 *     <nWhichOccurrence> is the number of the occurrence to find.
 *
 *     <lIgnoreCase> is a logical indicating if the search is to be case
 *        sensitive.  The default is no case sensitivity (.F.).
 *  $RETURNS$
 *     The position in the string cCheckIn of the ith occurrence of cCheckFor.
 *  $DESCRIPTION$
 *     This function finds the position in a string of the "ith" time another
 *     string appears in it.
 *  $EXAMPLES$
 *     // Find the Position in cMemoString of
 *     // the 10th Occurrence of "the", case
 *     // insensitive
 *
 *     nNextPosition := FT_FINDITH("the", cMemoString, 10)
 *  $SEEALSO$
 *     FT_AT2()
 *  $END$
 */


#define IS_NOT_LOGICAL(x)               (VALTYPE(x) != "L")
#define MAKE_UPPER(cString)             (cString := UPPER(cString))
#define NULL                            ""

#ifdef FT_TEST
  FUNCTION MAIN( cCk, cStr, nOcc, xCase )
     LOCAL nFind
     if pcount() != 4
        QOut( "usage: findith cCk cStr nOcc xCase")
        quit
     endif

     xCase := iif( xCase == "Y", .t., .f. )
     nOcc  := val(nOcc)
     QOut( iif( xCase, "Ignoring ", "Observing ") + "case:" )

     QOut( cStr )
     nFind := FT_FINDITH( cCk, cStr, nOcc, xCase )
     QOut( iif( nFind > 0, space( nFind - 1) + "^" , "Not found" ) )
  RETURN nil
#endif

FUNCTION FT_FINDITH(cCheckFor,cCheckIn,nWhichOccurrence,lIgnoreCase)

   LOCAL nIthOccurrence

                                        // Is Case Sensitivity Important??
   IF IS_NOT_LOGICAL(lIgnoreCase) .OR. ;
      lIgnoreCase

      MAKE_UPPER(cCheckFor)             // No, Force Everything to Uppercase
      MAKE_UPPER(cCheckIn)

   ENDIF                                // IS_NOT_LOGICAL(lIgnoreCase) or
                                        // lIgnoreCase

   RETURN (IF(nWhichOccurrence == 1, ;
              AT(cCheckFor, cCheckIn), ;
              IF((nIthOccurrence := AT(cCheckFor, ;
                                      STRTRAN(cCheckIn, cCheckFor, ;
                                              NULL, 1, ;
                                              nWhichOccurrence-1))) == 0, ;
                 0, ;
                 nIthOccurrence + ((nWhichOccurrence - 1) * LEN(cCheckFor)))))
