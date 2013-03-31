/*
 * Downloads .po files from Transifex localization site
 *
 * Copyright 2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * Requires: curl (built with SSL)
 * Reference: http://help.transifex.com/features/api/api-v2.1.html
 *
 */

#pragma -w3

PROCEDURE Main( cLogin )

   LOCAL cBase := hb_DirBase()

   LOCAL json
   LOCAL cLang
   LOCAL cTemp

   LOCAL cProject := "harbour"
   LOCAL cMain := cBase + "hbmk2.prg"
   LOCAL cPO_Dir := cBase + hb_DirSepToOS( "po/" )

   IF Empty( cLogin )
      cLogin := GetEnv( "HB_TRANSIFEX_LOGIN" )  /* Format: username:password */
   ENDIF

   FClose( hb_FTempCreateEx( @cTemp ) )

   ? "pulling .po files:"

   FOR EACH cLang IN hb_ATokens( hb_regexAll( "-lng=([a-zA-Z0-9_\-,]*)", hb_MemoRead( hb_FNameExtSet( cMain, ".hbp" ) ),,,,, .T. )[ 1 ][ 2 ], "," )

      ?? "", cLang

      hb_run( hb_StrFormat( "curl -s -i -L --user %1$s -X " + ;
         "GET https://www.transifex.com/api/2/project/%2$s/resource/%3$s/translation/%4$s/" + ;
         " -o %5$s", ;
         cLogin, cProject, hb_FNameName( cMain ), cLang, cTemp ) )

      IF hb_jsonDecode( GetJSON( hb_MemoRead( cTemp ) ), @json ) > 0
         hb_MemoWrit( cPO_Dir + hb_FNameName( cMain ) + "." + cLang + ".po", json[ "content" ] )
         /* should only do this if the translation is primarily done
            on Transifex website. This encouraged and probably the case
            in practice. Delete source information, delete empty
            translations and apply some automatic transformation for
            common translation mistakes. */
         PO_Clean( cPO_Dir + hb_FNameName( cMain ) + "." + cLang + ".po", .F., .F., @DoctorTranslation() )
      ELSE
         ? "API error"
      ENDIF
   NEXT

   FErase( cTemp )

   RETURN

STATIC FUNCTION DoctorTranslation( cString )

   cString := Unspace( AllTrim( cString ) )
   cString := StrTran( cString, hb_UChar( 0x23CE ), e"\n" )
   cString := StrTran( cString, e"\n ", e"\n" )
   cString := StrTran( cString, "( ", "(" )
   cString := StrTran( cString, " )", ")" )

   RETURN cString

/* Converts multiple spaces to just one */
STATIC FUNCTION Unspace( cString )

   LOCAL cResult := ""
   LOCAL cChar, cCharPrev
   LOCAL tmp

   FOR tmp := 1 TO Len( cString )

      cChar := SubStr( cString, tmp, 1 )

      IF !( cChar == " " ) .OR. !( cCharPrev == " " )
         cResult += cChar
      ENDIF

      cCharPrev := cChar
   NEXT

   RETURN cResult

STATIC FUNCTION PO_Clean( cFileName, ... )

   LOCAL aTrans
   LOCAL cErrorMsg

   IF ( aTrans := __i18n_potArrayLoad( cFileName, @cErrorMsg ) ) != NIL .AND. ;
      __i18n_potArraySave( cFileName, __i18n_potArrayClean( aTrans, ... ), @cErrorMsg )
      RETURN .T.
   ENDIF

   ? cErrorMsg

   RETURN .F.

STATIC FUNCTION GetJSON( cString )

   cString := SubStr( cString, At( "{", cString ) )
   cString := Left( cString, RAt( "}", cString ) )

   RETURN cString
