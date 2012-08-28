         /*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               11Jun2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"
#include "hbide.ch"
#include "xbp.ch"

/*----------------------------------------------------------------------*/

#define __checkDictToPrg_stateChanged__           2056
#define __checkDictToC_stateChanged__             2057
#define __checkDictToCpp_stateChanged__           2058
#define __checkDictToCh_stateChanged__            2059
#define __checkDictToH_stateChanged__             2060
#define __checkDictToIni_stateChanged__           2061
#define __checkDictToTxt_stateChanged__           2062
#define __checkDictToHbp_stateChanged__           2063
#define __checkDictActive_stateChanged__          2064
#define __checkDictCaseSens_stateChanged__        2065
#define __checkDictBold_stateChanged__            2066
#define __checkDictItalic_stateChanged__          2067
#define __checkDictULine_stateChanged__           2068
#define __checkDictColorText_stateChanged__       2069
#define __checkDictColorBack_stateChanged__       2070
#define __radioDictConvNone_clicked__             2071
#define __radioDictToLower_clicked__              2072
#define __radioDictToUpper_clicked__              2073
#define __radioDictAsIn_clicked__                 2074

#define DIC_FILENAME                              1
#define DIC_ACTIVE                                2
#define DIC_APPLYTO                               3
#define DIC_CONVMODE                              4
#define DIC_CASESENSTITIVE                        5
#define DIC_BOLD                                  6
#define DIC_ITALIC                                7
#define DIC_UNDERLINE                             8
#define DIC_TXTCOLOR                              9
#define DIC_BGCOLOR                               10

#define DIC_NUM_VRBLS                             10

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadUserDictionaries( oIde )
   LOCAL aDict := oIde:oINI:aDictionaries
   LOCAL oDict, i

   FOR i := 1 TO Len( aDict )
      oDict := IdeDictionary():new( oIde ):create()
      oDict:load( aDict[ i ] )
      aadd( oIde:aUserDict, oDict )
   NEXT

   RETURN NIL

/*----------------------------------------------------------------------*/

CLASS IdeDictionary INHERIT IdeObject

   DATA   cDictInfo                               INIT ""
   DATA   cFilename                               INIT ""
   DATA   lActive                                 INIT .T.
   DATA   cApplyTo                                INIT ""
   DATA   lToPrg                                  INIT .T.
   DATA   lToC                                    INIT .F.
   DATA   lToCPP                                  INIT .F.
   DATA   lToCH                                   INIT .F.
   DATA   lToH                                    INIT .F.
   DATA   lToIni                                  INIT .F.
   DATA   lToTxt                                  INIT .F.
   DATA   lToHbp                                  INIT .F.
   DATA   cConvMode                               INIT "ASIS"
   DATA   lCaseSens                               INIT .F.
   DATA   lBold                                   INIT .F.
   DATA   lItalic                                 INIT .F.
   DATA   lULine                                  INIT .F.
   DATA   lTxtColor                               INIT .F.
   DATA   lBgColor                                INIT .F.
   DATA   cTxtColor                               INIT ""
   DATA   cBgColor                                INIT ""
   DATA   aRawLines                               INIT {}

   DATA   hItems                                  INIT {=>}

   DATA   aTxtRGB                                 INIT {}
   DATA   aBgRGB                                  INIT {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()                               VIRTUAL
   METHOD load( cDict )
   METHOD toString()
   METHOD populateUI( oUI )
   METHOD execColorDialog( oUI, cMode )
   METHOD setButtonColors( oUI )
   METHOD checkStateChanged( oUI, p, p1 )
   METHOD radioButtonClicked( oUI, p )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:new( oIde )

   ::oIde := oIde
   hb_HCaseMatch( ::hItems, .F. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:create( oIde )
   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:load( cDict )
   LOCAL a_:= hb_aTokens( cDict, ";" )
   LOCAL s, b_, n, n1, cKeyword, cSyntax, cDesc, c_

   IF !empty( a_ ) .AND. HB_ISARRAY( a_ )
      asize( a_, DIC_NUM_VRBLS )

      DEFAULT a_[ DIC_FILENAME       ] TO ""
      DEFAULT a_[ DIC_ACTIVE         ] TO "YES"
      DEFAULT a_[ DIC_APPLYTO        ] TO ".PRG"
      DEFAULT a_[ DIC_CONVMODE       ] TO "ASIS"
      DEFAULT a_[ DIC_CASESENSTITIVE ] TO "NO"
      DEFAULT a_[ DIC_BOLD           ] TO "NO"
      DEFAULT a_[ DIC_ITALIC         ] TO "NO"
      DEFAULT a_[ DIC_UNDERLINE      ] TO "NO"
      DEFAULT a_[ DIC_TXTCOLOR       ] TO ""
      DEFAULT a_[ DIC_BGCOLOR        ] TO ""


      ::cDictInfo       := cDict
      ::cFilename       := a_[ DIC_FILENAME ]
      ::lActive         := a_[ DIC_ACTIVE ] == "YES"
      ::cApplyTo        := a_[ DIC_APPLYTO ]
      ::lToPRG          := ".PRG" $ a_[ DIC_APPLYTO ]
      ::lToC            := ".C"   $ a_[ DIC_APPLYTO ]
      ::lToCPP          := ".CPP" $ a_[ DIC_APPLYTO ]
      ::lToCH           := ".CH"  $ a_[ DIC_APPLYTO ]
      ::lToH            := ".H"   $ a_[ DIC_APPLYTO ]
      ::lToIni          := ".INI" $ a_[ DIC_APPLYTO ]
      ::lToTxt          := ".TXT" $ a_[ DIC_APPLYTO ]
      ::lToHbp          := ".HBP" $ a_[ DIC_APPLYTO ]
      ::cConvMode       := a_[ DIC_CONVMODE ]
      ::lCaseSens       := a_[ DIC_CASESENSTITIVE ] == "YES"
      ::lBold           := a_[ DIC_BOLD ] == "YES"
      ::lItalic         := a_[ DIC_ITALIC ] == "YES"
      ::lULine          := a_[ DIC_UNDERLINE ] == "YES"
      ::lTxtColor       := ! Empty( a_[ DIC_TXTCOLOR ] )
      ::lBgColor        := ! Empty( a_[ DIC_BGCOLOR ] )
      ::cTxtColor       := a_[ DIC_TXTCOLOR ]
      ::cBgColor        := a_[ DIC_BGCOLOR ]

      IF ! Empty( ::cTxtColor )
         c_:= hbide_evalAsIs( ::cTxtColor )
         IF HB_ISARRAY( c_ ) .AND. Len( c_ ) == 3
            ::aTxtRGB := AClone( c_ )
         ENDIF
      ENDIF
      IF ! Empty( ::cBgColor )
         c_:= hbide_evalAsIs( ::cBgColor )
         IF HB_ISARRAY( c_ ) .AND. Len( c_ ) == 3
            ::aBgRGB := AClone( c_ )
         ENDIF
      ENDIF

      IF !empty( ::cFilename ) .AND. hb_fileExists( ::cFilename )
         IF Lower( hb_FNameExt( ::cFilename ) ) == ".hbx"
            b_:= hbide_getHbxFunctions( hb_MemoRead( ::cFilename ) )
         ELSEIF Lower( hb_FNameExt( ::cFilename ) ) == ".tag"
            c_:= hb_deserialize( hb_memoRead( ::cFilename ) )
            IF Empty( c_ ) .OR. ! HB_ISARRAY( c_ )
               c_:= {}
            ENDIF
            b_:= {}
            FOR EACH s IN c_
               AAdd( b_, s[ 5 ] )
            NEXT
         ELSE
            b_:= hbide_readSource( ::cFilename )
         ENDIF
         ::aRawLines := b_
         FOR EACH s IN b_
            s := alltrim( s )
            IF empty( s )
               LOOP
            ENDIF
            cKeyword := ""
            cSyntax  := ""
            cDesc    := ""
            IF ( n := at( "(", s ) ) > 0
               IF ( n1 := at( ")", s ) ) > 0
                  cKeyword := alltrim( substr( s, 1, n - 1 ) )
                  cSyntax  := strtran( substr( s, 1, n1 ), " (", "(" )
                  cDesc    := alltrim( substr( s, n1 + 1 ) )
               ENDIF
            ELSE
               cKeyword := s
            ENDIF

            IF !empty( cKeyword )
               ::hItems[ cKeyword ] := { cKeyword, cSyntax, cDesc }
            ENDIF
         NEXT
      ENDIF

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:toString()

   ::cDictInfo := ::cFilename + ";" + ;
                  iif( ::lActive  , "YES", "NO" ) + ";" + ;
                  iif( ::lToPRG   , ".PRG", ""  ) + ;
                  iif( ::lToC     , ".C"  , ""  ) + ;
                  iif( ::lToCPP   , ".CPP", ""  ) + ;
                  iif( ::lToCH    , ".CPP", ""  ) + ;
                  iif( ::lToH     , ".H"  , ""  ) + ;
                  iif( ::lToIni   , ".INI", ""  ) + ;
                  iif( ::lToTxt   , ".TXT", ""  ) + ;
                  iif( ::lToHbp   , ".HBP", ""  ) + ";" + ;
                  ::cConvMode                     + ";" + ;
                  iif( ::lCaseSens, "YES", "NO" ) + ";" + ;
                  iif( ::lBold    , "YES", "NO" ) + ";" + ;
                  iif( ::lItalic  , "YES", "NO" ) + ";" + ;
                  iif( ::lULine   , "YES", "NO" ) + ";" + ;
                  ::cTxtColor                     + ";" + ;
                  ::cBgColor                      + ";"

   RETURN ::cDictInfo

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:populateUI( oUI )

   oUI:checkDictToPrg     : setChecked( ".PRG" $ ::cApplyTo )
   oUI:checkDictToC       : setChecked( ".C"   $ ::cApplyTo )
   oUI:checkDictToCpp     : setChecked( ".CPP" $ ::cApplyTo )
   oUI:checkDictToCh      : setChecked( ".CH"  $ ::cApplyTo )
   oUI:checkDictToh       : setChecked( ".H"   $ ::cApplyTo )
   oUI:checkDictToIni     : setChecked( ".INI" $ ::cApplyTo )
   oUI:checkDictToTxt     : setChecked( ".TXT" $ ::cApplyTo )
   oUI:checkDictToHbp     : setChecked( ".HBP" $ ::cApplyTo )

   oUI:checkDictActive    : setChecked( ::lActive )
   oUI:checkDictCaseSens  : setChecked( ::lCaseSens )
   oUI:checkDictBold      : setChecked( ::lBold   )
   oUI:checkDictItalic    : setChecked( ::lItalic )
   oUI:checkDictULine     : setChecked( ::lULine  )
   oUI:checkDictColorText : setChecked( ! Empty( ::cTxtColor ) )
   oUI:checkDictColorBack : setChecked( ! Empty( ::cBgColor  ) )

   oUI:radioDictConvNone  : setChecked( ::cConvMode == "NONE"  )
   oUI:radioDictToLower   : setChecked( ::cConvMode == "LOWER" )
   oUI:radioDictToUpper   : setChecked( ::cConvMode == "UPPER" )
   oUI:radioDictAsIn      : setChecked( ::cConvMode == "ASIS"  )

   oUI:plainKeywords      : clear()
   oUI:plainKeywords      : setPlainText( hbide_arrayToMemo( ::aRawLines ) )

   ::setButtonColors( oUI )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:setButtonColors( oUI )

   oUI:btnDictColorText:setStyleSheet( "" )
   oUI:btnDictColorBack:setStyleSheet( "" )
   IF ! Empty( ::aTxtRGB )
      oUI:btnDictColorText: setStyleSheet( "background-color: rgb(" + hb_ntos( ::aTxtRGB[ 1 ] ) + "," + hb_ntos( ::aTxtRGB[ 2 ] ) + "," + hb_ntos( ::aTxtRGB[ 3 ] ) + ");" )
   ENDIF
   IF ! Empty( ::aBgRGB )
      oUI:btnDictColorBack: setStyleSheet( "background-color: rgb(" + hb_ntos( ::aBgRGB[ 1 ] ) + "," + hb_ntos( ::aBgRGB[ 2 ] ) + "," + hb_ntos( ::aBgRGB[ 3 ] ) + ");" )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:execColorDialog( oUI, cMode )
   LOCAL qColorDlg := QColorDialog( oUI:oWidget )
   LOCAL nRet, qColor

   IF cMode == "back" .AND. ! Empty( ::aBgRGB )
      qColorDlg:SetCurrentColor( QColor( ::aBgRGB[ 1 ], ::aBgRGB[ 2 ], ::aBgRGB[ 3 ] ) )
   ELSEIF cMode == "text" .AND. ! Empty( ::aTxtRGB )
      qColorDlg:SetCurrentColor( QColor( ::aTxtRGB[ 1 ], ::aTxtRGB[ 2 ], ::aTxtRGB[ 3 ] ) )
   ENDIF

   nRet := qColorDlg:exec()

   IF nRet ==  QDialog_Accepted
      qColor := qColorDlg:selectedColor()
      IF cMode == "back"
         ::aBgRGB := Array( 3 )
         ::aBgRGB[ 1 ] := qColor:red()
         ::aBgRGB[ 2 ] := qColor:green()
         ::aBgRGB[ 3 ] := qColor:blue()
         ::cBgColor := "{" + hb_ntos( ::aBgRGB[ 1 ] ) + "," + hb_ntos( ::aBgRGB[ 2 ] ) + "," + hb_ntos( ::aBgRGB[ 3 ] ) + "}"
         ::lBgColor := .T.
         oUI:checkDictColorBack : setChecked( ! Empty( ::cBgColor  ) )
      ELSE
         ::aTxtRGB := Array( 3 )
         ::aTxtRGB[ 1 ] := qColor:red()
         ::aTxtRGB[ 2 ] := qColor:green()
         ::aTxtRGB[ 3 ] := qColor:blue()
         ::cTxtColor := "{" + hb_ntos( ::aTxtRGB[ 1 ] ) + "," + hb_ntos( ::aTxtRGB[ 2 ] ) + "," + hb_ntos( ::aTxtRGB[ 3 ] ) + "}"
         ::lTxtColor := .T.
         oUI:checkDictColorText : setChecked( ! Empty( ::cTxtColor ) )
      ENDIF
      ::setButtonColors( oUI )
   ENDIF
   qColorDlg:setParent( QWidget() )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:checkStateChanged( oUI, p, p1 )
   LOCAL lChecked := p1 != 0

   SWITCH p
   CASE __checkDictActive_stateChanged__     ; ::lActive   := lChecked ; EXIT
   CASE __checkDictToPrg_stateChanged__      ; ::lToPrg    := lChecked ; EXIT
   CASE __checkDictToC_stateChanged__        ; ::lToC      := lChecked ; EXIT
   CASE __checkDictToCpp_stateChanged__      ; ::lToCPP    := lChecked ; EXIT
   CASE __checkDictToCh_stateChanged__       ; ::lToCH     := lChecked ; EXIT
   CASE __checkDictToH_stateChanged__        ; ::lToH      := lChecked ; EXIT
   CASE __checkDictToIni_stateChanged__      ; ::lToIni    := lChecked ; EXIT
   CASE __checkDictToTxt_stateChanged__      ; ::lToTxt    := lChecked ; EXIT
   CASE __checkDictToHbp_stateChanged__      ; ::lToHbp    := lChecked ; EXIT
   CASE __checkDictActive_stateChanged__     ; ::lActive   := lChecked ; EXIT
   CASE __checkDictCaseSens_stateChanged__   ; ::lCaseSens := lChecked ; EXIT
   CASE __checkDictBold_stateChanged__       ; ::lBold     := lChecked ; EXIT
   CASE __checkDictItalic_stateChanged__     ; ::lItalic   := lChecked ; EXIT
   CASE __checkDictULine_stateChanged__      ; ::lULine    := lChecked ; EXIT
   CASE __checkDictColorText_stateChanged__  ; ::lTxtColor := lChecked ; EXIT
   CASE __checkDictColorBack_stateChanged__  ; ::lBgColor  := lChecked ; EXIT
   ENDSWITCH

   IF ! ::lTxtColor
      ::aTxtRGB := {}
      ::cTxtColor := ""
   ENDIF
   IF ! ::lBgColor
      ::aBgRGB := {}
      ::cBgColor := ""
   ENDIF
   ::setButtonColors( oUI )

   ::toString()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:radioButtonClicked( oUI, p )

   SWITCH p
   CASE __radioDictConvNone_clicked__  ; ::cConvMode := "NONE"  ; EXIT
   CASE __radioDictToLower_clicked__   ; ::cConvMode := "LOWER" ; EXIT
   CASE __radioDictToUpper_clicked__   ; ::cConvMode := "UPPER" ; EXIT
   CASE __radioDictAsIn_clicked__      ; ::cConvMode := "ASIS"  ; EXIT
   ENDSWITCH

   ::toString( oUI )

   RETURN NIL

/*----------------------------------------------------------------------*/
