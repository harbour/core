REQUEST __pp_StdRules
REQUEST __Wait

PROCEDURE Main( cFileName )

   LOCAL cLine, oErr

   LOCAL pPP := __pp_Init()

   BEGIN SEQUENCE WITH __BreakBlock()
      FOR EACH cLine IN hb_ATokens( __pp_Process( pPP, ;
            hb_MemoRead( hb_defaultValue( cFileName, hb_FNameExtSet( __FILE__, ".dat" ) ) ) ), .T. )
         BEGIN SEQUENCE
            IF ! Empty( cLine )
               &cLine
            ENDIF
         RECOVER USING oErr
            ? "Macro compiler error at line:", hb_ntos( cLine:__enumIndex() )
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
      iif( ! HB_ISNULL( oErr:filename ), ;
      " " + oErr:filename, ;
      iif( ! Empty( oErr:operation ), ;
      " " + oErr:operation, "" ) )
