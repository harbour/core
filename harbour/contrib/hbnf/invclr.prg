/*
 * $Id$
 */

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
 *    Rev 1.2   15 Aug 1991 23:03:44   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:00   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:30   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_InvClr( cDsrdColor )

   LOCAL cBackground                    // The Background Color, New Foreground
   LOCAL cForeground                    // The Foreground Color, New Background
   LOCAL cModifiers                     // Any Color Modifiers (+*)

   __defaultNIL( @cDsrdColor, SetColor() )

   // Remove Anything Past 1st Color
   cDsrdColor := Left( cDsrdColor, At( ",", cDsrdColor + "," ) - 1 )

   // Get Any Modifiers
   cModifiers := ;
      iif( "*" $ cDsrdColor, "*", "" ) + ;
      iif( "+" $ cDsrdColor, "+", "" )

   // Separate the Fore/Background Colors
   cForeground := AllTrim( Left( cDsrdColor,   At( "/", cDsrdColor ) - 1 ) )
   cBackground := AllTrim( SubStr( cDsrdColor, At( "/", cDsrdColor ) + 1 ) )

   RETURN ;
      StrTran( StrTran( cBackground, "+" ), "*" ) + cModifiers + "/" + ;
      StrTran( StrTran( cForeground, "+" ), "*" )
