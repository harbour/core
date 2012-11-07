/*
 * $Id$
 */

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

   LOCAL cOutput  := ""
   LOCAL cCurrent
   LOCAL nPointer := 1
   LOCAL nLen     := Len( cInput )

   DO WHILE nPointer <= nLen

      cCurrent := SubStr( cInput, nPointer, 1 )

      DO CASE

      CASE cCurrent == "\" .AND. ;
            IsDigit( SubStr( cInput, nPointer + 1, 1 ) ) .AND. ;
            IsDigit( SubStr( cInput, nPointer + 2, 1 ) ) .AND. ;
            IsDigit( SubStr( cInput, nPointer + 3, 1 ) )
         cOutput  += Chr( Val( SubStr( cInput, nPointer + 1, 3 ) ) )
         nPointer += 4

      CASE cCurrent == "\" .AND. ;
            SubStr( cInput, nPointer + 1, 1 ) == "\"
         cOutput += "\"
         nPointer += 2

      OTHERWISE
         cOutput += cCurrent
         nPointer++

      ENDCASE
   ENDDO

   RETURN cOutput
