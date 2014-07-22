/*
 * This is an original work by David Husnian and is placed in the public domain.
 *
 * Modification history:
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

   __defaultNIL( @cDsrdColor, SetColor() )

   // Remove Anything Past 1st Color
   cDsrdColor := Left( cDsrdColor, At( ",", cDsrdColor + "," ) - 1 )

   RETURN ;
      hb_StrReplace( AllTrim( SubStr( cDsrdColor, At( "/", cDsrdColor ) + 1 ) ) /* background */, "+*" ) + ;
      iif( "*" $ cDsrdColor, "*", "" ) + ;
      iif( "+" $ cDsrdColor, "+", "" ) + ;
      "/" + ;
      hb_StrReplace( AllTrim( Left( cDsrdColor, At( "/", cDsrdColor ) - 1 ) ) /* foreground */, "+*" )
