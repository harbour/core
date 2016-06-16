/* This is an original work by David Husnian and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:05:06   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   12 Apr 1991 00:18:04   GLENN
   There was a call to SetCentury() that should have been ft_SetCentury().
   Another one of those errors that came from testing earlier versions of
   a routine before FT_ prefix was added to function names.  Lesson learned.
   Minor documentation change.

      Rev 1.0   01 Apr 1991 01:02:10   GLENN
   Nanforum Toolkit
 */

#define FT_EXTRA_SETS   2
#define FT_SET_CENTURY  _SET_COUNT + 1
#define FT_SET_BLINK    _SET_COUNT + 2

FUNCTION ft_SaveSets()

   LOCAL aOldSets := Array( _SET_COUNT + FT_EXTRA_SETS )

   AEval( aOldSets, {| xElement, nElementNo | HB_SYMBOL_UNUSED( xElement ), ;
      aOldSets[ nElementNo ] := Set( nElementNo ) } )

   aOldSets[ FT_SET_CENTURY ] := __SetCentury()
   aOldSets[ FT_SET_BLINK ]   := SetBlink()

   RETURN aOldSets

PROCEDURE ft_RestSets( aOldSets )

   AEval( aOldSets, {| xElement, nElementNo | Set( nElementNo, xElement ) }, 1, _SET_COUNT )

   __SetCentury( aOldSets[ FT_SET_CENTURY ] )
   SetBlink( aOldSets[ FT_SET_BLINK ] )

   RETURN
