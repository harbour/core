/*
 * File......: RestSets.Prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_RESTSETS()
 *  $CATEGORY$
 *     Environment
 *  $ONELINER$
 *     Restore status of all SET command settings
 *  $SYNTAX$
 *     FT_RESTSETS( [ <aOldSets> ] ) -> NIL
 *  $ARGUMENTS$
 *     aOldSets is an array of SET settings created by FT_SAVESETS()
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     This function "restores" the SET Settings, i.e., it sets them to the
 *     values in the array aOldSets.  The following SETs are not currently
 *     supported: FILTER, FORMAT, FUNCTION, INDEX, KEYS, MODE, ORDER,
 *     PROCEDURE, RELATION, TYPEAHEAD
 *  $EXAMPLES$
 *     FT_RESTSETS(aOldSets)
 *  $INCLUDE$
 *     SET.CH
 *  $SEEALSO$
 *     FT_SAVESETS() FT_SETCENTURY()
 *  $END$
 */


#include "set.ch"

#Define FT_EXTRA_SETS    2
#DEFINE FT_SET_CENTURY   _SET_COUNT + 1
#DEFINE FT_SET_BLINK     _SET_COUNT + 2

FUNCTION  FT_RESTSETS(aOldSets)

   AEVAL(aOldSets, ;
         { | xElement, nElementNo | ;
           SET(nElementNo, xElement) }, ;
         1, _SET_COUNT )

   FT_SETCENTURY(aOldSets[FT_SET_CENTURY])
   SETBLINK(aOldSets[FT_SET_BLINK])

   RETURN (NIL)                         // FT_RestSets
