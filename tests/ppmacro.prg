REQUEST __pp_StdRules
REQUEST __Wait

PROCEDURE Main( cFileName )

   LOCAL cLine, pPP, oErr

   pPP := __pp_Init()
   BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
      FOR EACH cLine IN hb_ATokens( StrTran( __pp_Process( pPP, ;
            hb_MemoRead( hb_defaultValue( cFileName, hb_FNameExtSet( __FILE__, ".dat" ) ) ) ), Chr( 13 ) ), Chr( 10 ) )
         BEGIN SEQUENCE
            IF ! Empty( cLine )
               &cLine
            ENDIF
         RECOVER USING oErr
            ? "Macro compiler error at line:", ;
               hb_ntos( cLine:__enumIndex() )
            ? cLine
            ? ErrMsg( oErr )
         END SEQUENCE
      NEXT
   RECOVER USING oErr
      ? ErrMsg( oErr )
   END SEQUENCE
   ?

   RETURN

STATIC FUNCTION ErrMsg( oErr )
   RETURN "Error " + ;
      iif( HB_ISSTRING( oErr:subsystem ), ;
      oErr:subsystem, "???" ) + ;
      iif( HB_ISNUMERIC( oErr:subCode ), ;
      "/" + hb_ntos( oErr:subCode ), "/???" ) + ;
      iif( HB_ISSTRING( oErr:description ), ;
      " " + oErr:description, "" ) + ;
      iif( ! Empty( oErr:filename ), ;
      " " + oErr:filename, ;
      iif( ! Empty( oErr:operation ), ;
      " " + oErr:operation, "" ) )
