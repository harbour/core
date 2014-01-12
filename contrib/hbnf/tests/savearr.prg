#require "hbnf"

PROCEDURE Main()

   LOCAL aArray := { ;
      { "Invoice-1", 0d19910415, 1234.32, .T. }, ;
      { "Invoice-2", Date(), 234.98, .F. }, ;
      { "Invoice-3", Date() + 1, 0, .T. } }, aSave
   LOCAL nErrorCode

   Set( _SET_DATEFORMAT, "yy-mm-dd" ) /* TOFIX: RTEs with "yyyy-mm-dd" */

   ft_SaveArr( aArray, "invoice.dat", @nErrorCode )
   IF nErrorCode == 0
      DispArray( aArray )

      aSave := ft_RestArr( "invoice.dat", @nErrorCode )
      IF nErrorCode == 0
         DispArray( aSave )
      ELSE
         ? "Error restoring array"
      ENDIF
   ELSE
      ? "Error writing array"
   ENDIF

   RETURN

STATIC PROCEDURE DispArray( aTest )

   LOCAL nk

   FOR EACH nk IN aTest
      ? nk[ 1 ], nk[ 2 ], nk[ 3 ], nk[ 4 ]
   NEXT
   ?

   RETURN
