/*
 * $Id$
 */

/*
 * File......: STOD.C
 * Author....: Clayton Neff
 * CIS ID....:
 *
 * This is an original work by Clayton Neff and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:08:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:58   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:03:00   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_STOD()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Convert a date string to a Clipper date data type
 *  $SYNTAX$
 *     FT_STOD( <cDateStr> ) -> dDateType
 *  $ARGUMENTS$
 *     <cDateStr> is a Clipper string in the format "CCYYMMDD".
 *  $RETURNS$
 *     A Clipper date type.
 *  $DESCRIPTION$
 *     This function allows the programmer to hard code a date into the
 *     program without knowing what the current date type is.  This
 *     function is the converse of the Clipper DTOS() function.
 *  $EXAMPLES$
 *     LOCAL dMyDate
 *     dMyDate := FT_STOD( "19901127" )
 *  $END$
 */

#include "hbapi.h"

HB_FUNC(FT_STOD)
{
#if defined(HB_OS_DOS) || defined(HB_OS_WIN_32)
   {

      hb_retds( hb_parc(1) ) ;

      return;
   }
#endif
}
