/*
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:34   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   27 May 1991 13:04:20   GLENN
 * Minor documentation change.
 *
 *    Rev 1.0   01 Apr 1991 01:02:06   GLENN
 * Nanforum Toolkit
 *
 */

#define FT_EXTRA_SETS    2
#define FT_SET_CENTURY   _SET_COUNT + 1
#define FT_SET_BLINK     _SET_COUNT + 2

FUNCTION ft_RestSets( aOldSets )

   AEval( aOldSets, ;
      {| xElement, nElementNo | ;
      Set( nElementNo, xElement ) }, ;
      1, _SET_COUNT )

   ft_SetCentury( aOldSets[ FT_SET_CENTURY ] )
   SetBlink( aOldSets[ FT_SET_BLINK ] )

   RETURN NIL                         // FT_RestSets
