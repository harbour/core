Function Main()

   LOCAL cVar := "Hello"

   cVar := HB_INLINE( cVar )
   {
      if( ISCHAR(1) )
      {
         char *szPar1 = hb_parc(1);

         if( strcmp( szPar1, "Hello") == 0 )
         {
            hb_retc( "It was Hello" );
         }
         else
         {
            hb_retc( "No, it was not Hello" );
         }
      }
      else
      {
         hb_retc( "No Param passed" );
      }
   }

   ? cVar

RETURN NIL
