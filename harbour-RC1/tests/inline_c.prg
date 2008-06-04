//NOTEST
//
// $Id$
//

// This file use the old HB_INLINE syntax which is no longer supported.
// It was replace with more intuitve syntax, see hbinline.prg
// [ckedem]

PROCEDURE MAIN( cLine, cDelim )

   LOCAL a, i

   IF EMPTY( cLine )
      cLine := "This is a test"
   END IF

   a := aTokens( cLine, cDelim )
   FOR i := 1 TO LEN( a )
      ? '"' + a[ i ] + '"'
   NEXT i

   QOut( HB_INLINE() )
         { hb_retc( "\na C String, including { and \" { \n" ); }

   QOut( C_Func() )

   QOut( EndDumpTest() )

RETURN

FUNCTION aTokens( cLine, cDelimiter )

   LOCAL aTokens := {}

   #ifdef __HARBOUR__

      IF cDelimiter == NIL
         cDelimiter := ' '
      ENDIF

      HB_INLINE( aTokens, cLine, Asc( cDelimiter ) )
      {  // Note including {
         PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
         char cDelimiter = (char) hb_parni(3), * szLine = hb_parc( 2 );
         size_t i, len = hb_parclen( 2 ), iOffset = 0, iIndex = 1;

         /* Comment including { */
         for( i = 0; i < len; i++ )
         {
            if( szLine[i] == cDelimiter )
            {
               hb_arraySize( pArray, iIndex );
               hb_storclen( szLine + iOffset, i - iOffset, 1, iIndex );
               iOffset = i + 1;
               iIndex++;
            }
         }
         if( iOffset < len - 1 )
         {
            hb_arraySize( pArray, iIndex );
            hb_storclen( szLine + iOffset, len - iOffset, 1, iIndex );
         }
      }

   #else

      LOCAL nLen := Len( cLine ), i, nOffset := 1

      IF cDelimiter == NIL
         cDelimiter := ' '
      ENDIF

      FOR i := 1 to nLen
         IF SubStr( cLine, i, 1 ) == cDelimiter
            aAdd( aTokens, SubStr( cLine, nOffset, i - nOffset ) )
            nOffset := i + 1
         ENDIF
      NEXT
      IF nOffset < nLen - 1
         aAdd( aTokens, SubStr( cLine, nOffset ) )
      ENDIF

   #endif

RETURN aTokens

#pragma BEGINDUMP
HB_FUNC( C_FUNC )
{
   hb_retc( "returned from C_FUN\n" );
}
#pragma ENDDUMP

Function EndDumpTest()
RETURN "End Dump Test"
