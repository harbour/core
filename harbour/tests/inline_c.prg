/*
 * $Id$
 */
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

   QOut( PostDumpTest() )

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
         PHB_ITEM pLine  = hb_param( 2, HB_IT_STRING );
         char cDelimiter = (char) hb_parni(3);
         size_t i, iOffset = 0, iIndex = 1;

         /* Comment including { */
         for( i = 0; i < pLine->item.asString.length; i++ )
         {
            if( pLine->item.asString.value[i] == cDelimiter )
            {
               hb_arraySize( pArray, iIndex );
               hb_storclen( pLine->item.asString.value + iOffset, i - iOffset, 1, iIndex );
               iOffset = i + 1;
               iIndex++;
            }
         }
         if( iOffset < pLine->item.asString.length - 1 )
         {
            hb_arraySize( pArray, iIndex );
            hb_storclen( pLine->item.asString.value + iOffset, pLine->item.asString.length - iOffset, 1, iIndex );
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

Function PostDumpTest()
RETURN "Post Dump Test"