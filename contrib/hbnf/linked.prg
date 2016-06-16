/* This is an original work by Brian Loesgen and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:05:52   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:52:08   GLENN
   Minor edit to file header

      Rev 1.0   13 Jun 1991 15:21:26   GLENN
   Initial revision.
 */

// A function is detected by the left parenthesis, "(", and it begins
// at the space, comma or start-of-string preceeding the "("

// Returns: .T. if all functions are available,
//          .F. if not

FUNCTION ft_Linked( cFuncs )

   LOCAL aFuncArray, nSpace, nComma, nFEnd

   IF "(" $ cFuncs
      aFuncArray := {}
      DO WHILE ( nFEnd := At( "(", cFuncs ) ) > 0
         // Add the current function to the array of functions
         AAdd( aFuncArray, Left( cFuncs, nFEnd ) + ")" )
         // Remove the current function from the string
         cFuncs := SubStr( cFuncs, nFEnd + 1 )
         DO WHILE ;
               ( ( nComma := At( ",", cFuncs ) ) > 0 .AND. nComma < nFEnd ) .OR. ;
               ( ( nSpace := At( " ", cFuncs ) ) > 0 .AND. nSpace < nFEnd )
            // We have extra parameters or spaces prior to the start
            // of the function. Strip them out.
            DO CASE
            CASE nComma > 0 ; cFuncs := SubStr( cFuncs, nComma + 1 )
            CASE nSpace > 0 ; cFuncs := SubStr( cFuncs, nSpace + 1 )
            ENDCASE
         ENDDO
      ENDDO
      // Scan through the array of functions, stop after the first occurence
      // of a function which returns a Type() of "U" (hence is not linked in)
      RETURN AScan( aFuncArray, {| element | Type( element ) == "U" } ) == 0
   ENDIF

   // No functions in string
   Alert( "Warning: Expected function(s) in ft_Linked(), but none were found" )

   RETURN .F.
