/*
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

FUNCTION ft_EscCode( cInput )

   LOCAL cOutput := ""
   LOCAL cCurrent
   LOCAL nPointer
   LOCAL nLen := hb_BLen( cInput )

   FOR nPointer := 1 TO nLen

      cCurrent := hb_BSubStr( cInput, nPointer, 1 )

      DO CASE
      CASE cCurrent == "\" .AND. ;
           IsDigit( hb_BSubStr( cInput, nPointer + 1, 1 ) ) .AND. ;
           IsDigit( hb_BSubStr( cInput, nPointer + 2, 1 ) ) .AND. ;
           IsDigit( hb_BSubStr( cInput, nPointer + 3, 1 ) )

         cOutput  += hb_BChar( Val( hb_BSubStr( cInput, nPointer + 1, 3 ) ) )
         nPointer += 3

      CASE cCurrent == "\" .AND. ;
           hb_BSubStr( cInput, nPointer + 1, 1 ) == "\"

         cOutput += "\"
         nPointer++

      OTHERWISE

         cOutput += cCurrent

      ENDCASE
   NEXT

   RETURN cOutput
