/*
 * File......: CntrySet.Prg
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:20   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:58   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SETCENTURY()
 *  $CATEGORY$
 *     Environment
 *  $ONELINER$
 *     Check/Set the CENTURY Setting
 *  $SYNTAX$
 *     FT_SETCENTURY( [ <lNewSetState> ] ) -> <lOldState>
 *  $ARGUMENTS$
 *     lNewSetState - Boolean to Set CENTURY
 *                      .F. - Toggle CENTURY off
 *                      .T. - Toggle CENTURY on
 *                      If not specified, leave CENTURY as is
 *  $RETURNS$
 *     The state of the CENTURY setting upon entry to the routine
 *  $DESCRIPTION$
 *     This function returns the state (ON/OFF, TRUE/FALSE) of the CENTURY
 *     and optionally sets it ON or OFF.
 *  $EXAMPLES$
 *     lOldState := FT_SETCENTURY()     // Get current CENTURY Setting
 *
 *     lOldState := FT_SETCENTURY(.T.)  // Get the current CENTURY Setting
 *                                      // and turn it on (set it to TRUE)
 *
 *     lOldState := FT_SETCENTURY(.F.)  // Get the current CENTURY Setting
 *                                      // and turn it off (set it to FALSE)
 *  $END$
 */


#define IS_LOGICAL(x)                (VALTYPE(x) == "L")

FUNCTION FT_SETCENTURY(lNewSetState)
                                        // Note that if CENTURY is ON then
                                        // DTOC() Will Return a String of Length
                                        // 10, Otherwise it Will be of Length 8
   LOCAL lOldSetState := (LEN(DTOC(DATE())) == 10)

   IF (IS_LOGICAL(lNewSetState))        // Did They Want it Set??
      SET CENTURY (lNewSetState)        // Yes, Set it
   ENDIF                                // IS_LOGICAL(lNewSetState)
   RETURN (lOldSetState)                // FT_SetCentury
