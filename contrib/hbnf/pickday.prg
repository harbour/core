/*
 * $Id$
 */

/*
 * Author....: Greg Lief
 * CIS ID....: 72460,1760
 *
 * This is an original work by Mr. Grump and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:40   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:00   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_PickDay()

   LOCAL days := { "SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", ;
      "FRIDAY", "SATURDAY" }
   LOCAL sel := 0
   LOCAL oldscrn := SaveScreen( 8, 35, 16, 45 ), oldcolor := SetColor( "+w/r" )

   hb_DispBox( 8, 35, 16, 45, hb_UTF8ToStrBox( "┌─┐│┘─└│ " ) )
   /* do not allow user to Esc out, which would cause array access error */
   DO WHILE sel == 0
      sel := AChoice( 9, 36, 15, 44, days )
   ENDDO
   /* restore previous screen contents and color */
   RestScreen( 8, 35, 16, 45, oldscrn )
   SetColor( oldcolor )

   RETURN days[ sel ]
