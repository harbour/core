/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               31Dec2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"
#include "hbide.ch"

/*----------------------------------------------------------------------*/

FUNCTION LoadThemes( oIde )

   IF empty( oIde:cIniThemes )
      oIde:cIniThemes := hb_dirBase() + "projects" + hb_OsPathSeparator() + "hbide.thm"
   ENDIF

   oIde:oThemes := IdeThemes():new( oIde, oIde:cIniThemes ):create()

   RETURN nil

/*----------------------------------------------------------------------*/

CLASS IdeThemes

   VAR    oIde
   VAR    lEdited                                 INIT .f.
   VAR    cIniFile                                INIT ""
   VAR    ini_                                    INIT {}
   VAR    thm_                                    INIT {}
   VAR    hControls                               INIT hb_hash()
   VAR    hItems                                  INIT hb_hash()
   VAR    oThemes
   VAR    oUI

   METHOD new()
   METHOD create()
   METHOD destroy()
   METHOD load()
   METHOD save()
   METHOD fetch()
   METHOD apply()
   METHOD parse()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeThemes:new( oIde, cIniFile )

   ::oIde  := oIde
   ::cIniFile := cIniFile

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeThemes:create( oIde, cIniFile )

   DEFAULT oIde     TO ::oIde
   DEFAULT cIniFile TO ::cIniFile

   ::oIde  := oIde
   ::cIniFile := cIniFile

   ::load( ::cIniFile )
   ::parse()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeThemes:destroy()

   IF !empty( ::oThemes )

   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeThemes:load( cFile )

   IF hb_isChar( cFile ) .AND. !empty( cFile ) .AND. file( cFile )
      ::ini_:= ReadSource( cFile )
   ENDIF
   IF empty( ::ini_ )
      ::ini_:= LoadDefaultThemes()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeThemes:save( lAs )

   DEFAULT lAs TO .f.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeThemes:fetch()

   IF empty( ::oUI )
      ::oUI := XbpQtUiLoader():new( ::oIde:oDlg )
      ::oUI:file := ::oIde:resPath + "themes.ui"
      ::oUI:create()
      ::oUI:setWindowFlags( Qt_Sheet )

      ::oUI:signal( "buttonClose", "clicked()", ;
            {|| ::oIde:aIni[ INI_HBIDE, ThemesDialogGeometry ] := PosAndSize( ::oUI:oWidget ), ::oUI:hide() } )

      ::oIde:setPosByIni( ::oUI:oWidget, ThemesDialogGeometry )

      /* Insert Themes */
      aeval( ::thm_  , {|e| ::oUI:qObj[ "comboThemes" ]:addItem( e[ 1 ] ) } )
      hb_heval( ::hItems, {|e| ::oUI:qObj[ "comboItems" ]:addItem( e ) } )

   ENDIF

   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeThemes:apply()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeThemes:parse()
   LOCAL s, n, cKey, cVal, nPart, nTheme, hTheme

   IF empty( ::ini_ )
      RETURN Self
   ENDIF

   FOR EACH s IN ::ini_
      IF !empty( s := alltrim( s ) ) .and. !left( s, 1 ) == "#" /* Comment */
         DO case
         CASE s == "[ Controls ]"
            nPart := 1
         CASE s == "[ Items ]"
            nPart := 2
         CASE left( s, 7 ) == "[ Theme"
            IF ( n := at( ":", s ) ) > 0
               cKey := alltrim( strtran( substr( s, n+1 ), "]" ) )
            ENDIF
            HB_TRACE( HB_TR_ALWAYS, cKey )
            IF !empty( cKey )
               nPart := 3
               IF ( nTheme := ascan( ::thm_, {|e_| e_[ 1 ] == cKey } ) ) == 0
                  aadd( ::thm_, { cKey, hb_hash() } )
                  nTheme := len( ::thm_ )
               ENDIF
            ELSE
               nPart := 0
               nTheme := 0
            ENDIF
         OTHERWISE
            DO CASE
            CASE nPart == 1 /* Controls */
               IF ParseKeyValPair( s, @cKey, @cVal )
                  ::hControls[ cKey ] := cVal
               ENDIF
            CASE nPart == 2 /* Items   */
               IF ParseKeyValPair( s, @cKey, @cVal )
                  ::hItems[ cKey ] := cVal
               ENDIF
            CASE nPart == 3 /* Themes  */
               hTheme := ::thm_[ nTheme, 2 ]
               IF ParseKeyValPair( s, @cKey, @cVal )
                  hTheme[ cKey ] := cVal
               ENDIF
            ENDCASE
         ENDCASE
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

FUNCTION SetSyntaxHilighting( qEdit, qHiliter )
   LOCAL a_, b_, qFormat

   HB_SYMBOL_UNUSED( qEdit )

   /* Compiler Directives */
   b_:= { "include","ifdef","else","endif","command","xcommand","translate","xtranslate" }
   a_:= {}; aeval( b_, {|e| aadd( a_, "#" + upper( e ) + "\b|#" + e + "\b" ) } )
   SetSyntaxAttrbs( qHiliter, a_, { 120, 26,213 }, .t., .t., .f. )

   /* Operators */
   a_:= { "\:\=|\:|\+|\-|\\|\*|\ IN\ |\ in\ |\=|\>|\<|\^|\%|\$|\&|\@|\.or\.|\.and\.|\.OR\.|\.AND\." }
   SetSyntaxAttrbs( qHiliter, a_, { 255,120,  0 }, .f., .f., .f. )

   /* Numerics */
   a_:= { "\b[0-9.]+\b" }
   SetSyntaxAttrbs( qHiliter, a_, { 127,127,127 }, .f., .f., .f. )

   /* Parenthesis and Braces */
   a_:= { "\(|\)|\{|\}|\[|\]|\|" }
   SetSyntaxAttrbs( qHiliter, a_, { 255,127,200 }, .f., .f., .f. )

   /* Harbour Keywords */
   b_:= { 'function','return','static','local', ;
          'if','else','elseif','endif','end', ;
          'docase','case','endcase','otherwise', ;
          'do','while','exit',;
          'for','each','next','step','to',;
          'class','endclass','method','data','var','destructor','inline','assign','access','inherit','init','create',;
          'begin','sequence','try','catch','always','recover','default','hb_symbol_unused' }
   a_:= {}; aeval( b_, {|e| aadd( a_, "\b" + upper( e ) + "\b|\b" + e + "\b" ) } )
   SetSyntaxAttrbs( qHiliter, a_, { 40,120,240 }, .f., .t., .f. )

   /* Functions in General */
   a_:= { "\b[A-Za-z0-9_]+(?=\()" }
   SetSyntaxAttrbs( qHiliter, a_, { 128,0,64 }, .f., .f., .f. )

   /* Strings */
   a_:= {}
   aadd( a_, '\".*\"' )
   aadd( a_, "\'.*\'" )
   SetSyntaxAttrbs( qHiliter, a_, { 64,128,128 }, .f., .f., .f. )

   /* Single Line Comments */
   a_:= { "//[^\n]*" }
   SetSyntaxAttrbs( qHiliter, a_, { 190,190,190 }, .f., .f., .f. )

   qFormat := QTextCharFormat():new()
   qFormat:setFontItalic( .t. )
   qFormat:setForeGround( QBrush():new( "QColor", QColor():new( 190,190,190 ) ) )
   qHiliter:setHBMultiLineCommentFormat( qFormat )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION SetSyntaxAttrbs( qHiliter, aRegExp, aRGB, lItalic, lBold, lUnderline )
   LOCAL qStrList, qFormat

   qStrList := QStringList():new()
   aeval( aRegExp, {|e| qStrList:append( e ) } )

   qFormat  := QTextCharFormat():new()

   IF hb_isLogical( lItalic )
      qFormat:setFontItalic( lItalic )
   ENDIF
   IF hb_isLogical( lBold ) .and. lBold
      qFormat:setFontWeight( 1000 )
   ENDIF
   IF hb_isLogical( lUnderline )
      qFormat:setFontUnderline( lUnderline )
   ENDIF
   IF hb_isArray( aRGB )
      qFormat:setForeGround( QBrush():new( "QColor", QColor():new( aRgb[ 1 ], aRgb[ 2 ], aRgb[ 3 ] ) ) )
   ENDIF

   qHiliter:setHBCompilerDirectives( qStrList, qFormat )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION LoadDefaultThemes()
   LOCAL ini_:= {}

   IF .t.
      aadd( ini_, "[ Controls ]                                                        " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "Dialog_Title               = HBIDE - Source Syntax Highlighting     " )
      aadd( ini_, "Label_Items                = Items                                  " )
      aadd( ini_, "Label_Theme                = Theme                                  " )
      aadd( ini_, "Label_Color                = Color                                  " )
      aadd( ini_, "Check_Italic               = Italic                                 " )
      aadd( ini_, "Check_Bold                 = Bold                                   " )
      aadd( ini_, "Check_Underline            = Underline                              " )
      aadd( ini_, "Button_Text_Save           = Save                                   " )
      aadd( ini_, "Button_Text_SaveAs         = Save As                                " )
      aadd( ini_, "Button_Text_Apply          = Apply                                  " )
      aadd( ini_, "Button_Text_Close          = Close                                  " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "[ Items ]                                                           " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "Background                 = Background                             " )
      aadd( ini_, "BookMarkLineBackground     = Bookmark Line Background               " )
      aadd( ini_, "BracketsAndBraces          = Brackets and Braces                    " )
      aadd( ini_, "CommentsAndRemarks         = Comments and Remarks                   " )
      aadd( ini_, "NumericalConstants         = Numerical Constants                    " )
      aadd( ini_, "Operators                  = Operators                              " )
      aadd( ini_, "PreprocessorDirectives     = Preprocessor Directives                " )
      aadd( ini_, "SelectionBackground        = Selected Text Background               " )
      aadd( ini_, "TerminatedStringConstants  = Terminated String Constants            " )
      aadd( ini_, "UnrecognizedText           = Unrecognized Text                      " )
      aadd( ini_, "UnterminatedStrings        = Unterminated Strings                   " )
      aadd( ini_, "CurrentLineBackColor       = Current Line Background                " )
      aadd( ini_, "FunctionsNotInDictionaries = Functions not in Dictionaries          " )
      aadd( ini_, "ConstantsDictionary        = Constants Dictionary                   " )
      aadd( ini_, "WAPIDictionary             = WAPI Dictionary                        " )
      aadd( ini_, "HashDictionary             = Hash Dictionary                        " )
      aadd( ini_, "CLanguageDictionary        = C-CPP Language Dictionary              " )
      aadd( ini_, "UserDictionary             = User Dictionary                        " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "[ Theme : Classic ]                                                 " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "Background                 = 255,255,255   , No, No, No,            " )
      aadd( ini_, "BookMarkLineBackground     = 0,255,255     , No, No, No,            " )
      aadd( ini_, "BracketsAndBraces          = 64,0,0        , No, No, No,            " )
      aadd( ini_, "CommentsAndRemarks         = 0,128,255     , No, No, No,            " )
      aadd( ini_, "NumericalConstants         = 0,128,0       , No, No, No,            " )
      aadd( ini_, "Operators                  = 0,0,0         , No, No, No,            " )
      aadd( ini_, "PreprocessorDirectives     = 128,128,0     , No, No, No,            " )
      aadd( ini_, "SelectionBackground        = 255,128,255   , No, No, No,            " )
      aadd( ini_, "TerminatedStringConstants  = 255,0,0       , No, No, No,            " )
      aadd( ini_, "UnrecognizedText           = 0,0,0         , No, No, No,            " )
      aadd( ini_, "UnterminatedStrings        = 255,128,128   , No, No, No,            " )
      aadd( ini_, "CurrentLineBackColor       = 128,0,0       , No, No, No,            " )
      aadd( ini_, "FunctionsNotInDictionaries = 0,0,192       , No, No, No,            " )
      aadd( ini_, "ConstantsDictionary        = 128,0,128     , No, No, No,            " )
      aadd( ini_, "WAPIDictionary             = 0,0,128       , No, No, No,            " )
      aadd( ini_, "HashDictionary             = 255,255,255   , No, No, No,            " )
      aadd( ini_, "CLanguageDictionary        = 0,0,128       , No, No, No,            " )
      aadd( ini_, "UserDictionary             = 0,0,0         , No, No, No,            " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "[ Theme : City Lights ]                                             " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "Background                 = 0,0,0         , No, No, No,            " )
      aadd( ini_, "BookMarkLineBackground     = 128,128,128   , No, No, No,            " )
      aadd( ini_, "BracketsAndBraces          = 255,128,128   , No, No, No,            " )
      aadd( ini_, "CommentsAndRemarks         = 255,255,0     , No, No, No,            " )
      aadd( ini_, "NumericalConstants         = 0,255,255     , No, No, No,            " )
      aadd( ini_, "Operators                  = 128,255,0     , No, No, No,            " )
      aadd( ini_, "PreprocessorDirectives     = 255,0,0       , No, No, No,            " )
      aadd( ini_, "SelectionBackground        = 255,128,255   , No, No, No,            " )
      aadd( ini_, "TerminatedStringConstants  = 0,255,0       , No, No, No,            " )
      aadd( ini_, "UnrecognizedText           = 255,255,255   , No, No, No,            " )
      aadd( ini_, "UnterminatedStrings        = 255,255,255   , No, No, No,            " )
      aadd( ini_, "CurrentLineBackColor       = 0,0,255       , No, No, No,            " )
      aadd( ini_, "FunctionsNotInDictionaries = 0,0,192       , No, No, No,            " )
      aadd( ini_, "ConstantsDictionary        = 128,0,128     , No, No, No,            " )
      aadd( ini_, "WAPIDictionary             = 0,0,128       , No, No, No,            " )
      aadd( ini_, "HashDictionary             = 0,0,0         , No, No, No,            " )
      aadd( ini_, "CLanguageDictionary        = 0,0,128       , No, No, No,            " )
      aadd( ini_, "UserDictionary             = 0,0,0         , No, No, No,            " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "[ Theme : Evening Glamour ]                                         " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "Background                 = 0,64,128      , No, No, No,            " )
      aadd( ini_, "BookMarkLineBackground     = 128,0,255     , No, No, No,            " )
      aadd( ini_, "BracketsAndBraces          = 128,255,255   , No, No, No,            " )
      aadd( ini_, "CommentsAndRemarks         = 192,192,192   , No, No, No,            " )
      aadd( ini_, "NumericalConstants         = 0,255,0       , No, No, No,            " )
      aadd( ini_, "Operators                  = 255,255,255   , No, No, No,            " )
      aadd( ini_, "PreprocessorDirectives     = 255,128,192   , No, No, No,            " )
      aadd( ini_, "SelectionBackground        = 0,128,255     , No, No, No,            " )
      aadd( ini_, "TerminatedStringConstants  = 255,255,128   , No, No, No,            " )
      aadd( ini_, "UnrecognizedText           = 255,255,255   , No, No, No,            " )
      aadd( ini_, "UnterminatedStrings        = 255,128,64    , No, No, No,            " )
      aadd( ini_, "CurrentLineBackColor       = 128,255,255   , No, No, No,            " )
      aadd( ini_, "FunctionsNotInDictionaries = 128,255,128   , No, No, No,            " )
      aadd( ini_, "ConstantsDictionary        = 255,128,192   , No, No, No,            " )
      aadd( ini_, "WAPIDictionary             = 128,128,64    , No, No, No,            " )
      aadd( ini_, "HashDictionary             = 0,64,128      , No, No, No,            " )
      aadd( ini_, "CLanguageDictionary        = 0,0,128       , No, No, No,            " )
      aadd( ini_, "UserDictionary             = 0,0,0         , No, No, No,            " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "[ Theme : Sand Storm ]                                              " )
      aadd( ini_, "                                                                    " )
      aadd( ini_, "Background                 = 255,255,192   , No, No, No,            " )
      aadd( ini_, "BookMarkLineBackground     = 0,255,255     , No, No, No,            " )
      aadd( ini_, "BracketsAndBraces          = 0,0,0         , No, No, No,            " )
      aadd( ini_, "CommentsAndRemarks         = 128,128,128   , No, No, No,            " )
      aadd( ini_, "NumericalConstants         = 0,128,128     , No, No, No,            " )
      aadd( ini_, "Operators                  = 0,0,0         , No, No, No,            " )
      aadd( ini_, "PreprocessorDirectives     = 255,0,0       , No, No, No,            " )
      aadd( ini_, "SelectionBackground        = 255,0,255     , No, No, No,            " )
      aadd( ini_, "TerminatedStringConstants  = 0,128,0       , No, No, No,            " )
      aadd( ini_, "UnrecognizedText           = 0,0,0         , No, No, No,            " )
      aadd( ini_, "UnterminatedStrings        = 128,128,0     , No, No, No,            " )
      aadd( ini_, "CurrentLineBackColor       = 128,0,0       , No, No, No,            " )
      aadd( ini_, "FunctionsNotInDictionaries = 0,0,192       , No, No, No,            " )
      aadd( ini_, "ConstantsDictionary        = 128,0,128     , No, No, No,            " )
      aadd( ini_, "WAPIDictionary             = 0,0,128       , No, No, No,            " )
      aadd( ini_, "HashDictionary             = 255,255,192   , No, No, No,            " )
      aadd( ini_, "CLanguageDictionary        = 0,0,128       , No, No, No,            " )
      aadd( ini_, "UserDictionary             = 0,0,0         , No, No, No,            " )
      //
      aadd( ini_, "                                                                    " )
   ENDIF
   RETURN ini_

/*----------------------------------------------------------------------*/
#if 0
                            [Classic]      [CityLights]  [Evening]     [SandStorm]

Background                 = 255,255,255   0,0,0         0,64,128      255,255,192
BookMarkLineBackground     = 0,255,255     128,128,128   128,0,255     0,255,255
BracketsAndBraces          = 64,0,0        255,128,128   128,255,255   0,0,0
CommentsAndRemarks         = 0,128,255     255,255,0     192,192,192   128,128,128
NumericalConstants         = 0,128,0       0,255,255     0,255,0       0,128,128
Operators                  = 0,0,0         128,255,0     255,255,255   0,0,0
PreprocessorDirectives     = 128,128,0     255,0,0       255,128,192   255,0,0
SelectionBackground        = 255,128,255   255,128,255   0,128,255     255,0,255
TerminatedStringConstants  = 255,0,0       0,255,0       255,255,128   0,128,0
UnrecognizedText           = 0,0,0         255,255,255   255,255,255   0,0,0
UnterminatedStrings        = 255,128,128   255,255,255   255,128,64    128,128,0
CurrentLineBackColor       = 128,0,0       0,0,255       128,255,255   128,0,0
FunctionsNotInDictionaries = 0,0,192       0,0,192       128,255,128   0,0,192
ConstantsDictionary        = 128,0,128     128,0,128     255,128,192   128,0,128
WAPIDictionary             = 0,0,128       0,0,128       128,128,64    0,0,128
HashDictionary             = 255,255,255   0,0,0         0,64,128      255,255,192
CLanguageDictionary        = 0,0,128       0,0,128       0,0,128       0,0,128
UserDictionary             = 0,0,0         0,0,0         0,0,0         0,0,0


[Controls]

Dialog_Title               = HBIDE - Source Syntax Highlighting
Label_Items                = Items
Label_Theme                = Theme
Label_Color                = Color
Check_Italic               = Italic
Check_Bold                 = Bold
Check_Underline            = Underline
Button_Text_Save           = Save
Button_Text_SaveAs         = Save As
Button_Text_Apply          = Apply
Button_Text_Cancel         = Cancel

[Items]

Background                 = Background
BookMarkLineBackground     = Bookmark Line Background
BracketsAndBraces          = Brackets and Braces
CommentsAndRemarks         = Comments and Remarks
NumericalConstants         = Numerical Constants
Operators                  = Operators
PreprocessorDirectives     = Preprocessor Directives
SelectionBackground        = Selected Text Background
TerminatedStringConstants  = Terminated String Constants
UnrecognizedText           = Unrecognized Text
UnterminatedStrings        = Unterminated Strings
CurrentLineBackColor       = Current Line Background
FunctionsNotInDictionaries = Functions not in Dictionaries
ConstantsDictionary        = Constants Dictionary
WAPIDictionary             = WAPI Dictionary
HashDictionary             = Hash Dictionary
CLanguageDictionary        = C-CPP Language Dictionary
UserDictionary             = User Dictionary


[Theme : Classic]

Background                 = 255,255,255   , No, No, No,
BookMarkLineBackground     = 0,255,255     , No, No, No,
BracketsAndBraces          = 64,0,0        , No, No, No,
CommentsAndRemarks         = 0,128,255     , No, No, No,
NumericalConstants         = 0,128,0       , No, No, No,
Operators                  = 0,0,0         , No, No, No,
PreprocessorDirectives     = 128,128,0     , No, No, No,
SelectionBackground        = 255,128,255   , No, No, No,
TerminatedStringConstants  = 255,0,0       , No, No, No,
UnrecognizedText           = 0,0,0         , No, No, No,
UnterminatedStrings        = 255,128,128   , No, No, No,
CurrentLineBackColor       = 128,0,0       , No, No, No,
FunctionsNotInDictionaries = 0,0,192       , No, No, No,
ConstantsDictionary        = 128,0,128     , No, No, No,
WAPIDictionary             = 0,0,128       , No, No, No,
HashDictionary             = 255,255,255   , No, No, No,
CLanguageDictionary        = 0,0,128       , No, No, No,
UserDictionary             = 0,0,0         , No, No, No,


[Theme : City Lights]

Background                 = 0,0,0         , No, No, No,
BookMarkLineBackground     = 128,128,128   , No, No, No,
BracketsAndBraces          = 255,128,128   , No, No, No,
CommentsAndRemarks         = 255,255,0     , No, No, No,
NumericalConstants         = 0,255,255     , No, No, No,
Operators                  = 128,255,0     , No, No, No,
PreprocessorDirectives     = 255,0,0       , No, No, No,
SelectionBackground        = 255,128,255   , No, No, No,
TerminatedStringConstants  = 0,255,0       , No, No, No,
UnrecognizedText           = 255,255,255   , No, No, No,
UnterminatedStrings        = 255,255,255   , No, No, No,
CurrentLineBackColor       = 0,0,255       , No, No, No,
FunctionsNotInDictionaries = 0,0,192       , No, No, No,
ConstantsDictionary        = 128,0,128     , No, No, No,
WAPIDictionary             = 0,0,128       , No, No, No,
HashDictionary             = 0,0,0         , No, No, No,
CLanguageDictionary        = 0,0,128       , No, No, No,
UserDictionary             = 0,0,0         , No, No, No,


[Theme : Evening Glamour]

Background                 = 0,64,128      , No, No, No,
BookMarkLineBackground     = 128,0,255     , No, No, No,
BracketsAndBraces          = 128,255,255   , No, No, No,
CommentsAndRemarks         = 192,192,192   , No, No, No,
NumericalConstants         = 0,255,0       , No, No, No,
Operators                  = 255,255,255   , No, No, No,
PreprocessorDirectives     = 255,128,192   , No, No, No,
SelectionBackground        = 0,128,255     , No, No, No,
TerminatedStringConstants  = 255,255,128   , No, No, No,
UnrecognizedText           = 255,255,255   , No, No, No,
UnterminatedStrings        = 255,128,64    , No, No, No,
CurrentLineBackColor       = 128,255,255   , No, No, No,
FunctionsNotInDictionaries = 128,255,128   , No, No, No,
ConstantsDictionary        = 255,128,192   , No, No, No,
WAPIDictionary             = 128,128,64    , No, No, No,
HashDictionary             = 0,64,128      , No, No, No,
CLanguageDictionary        = 0,0,128       , No, No, No,
UserDictionary             = 0,0,0         , No, No, No,


[Theme : Sand Storm]

Background                 = 255,255,192   , No, No, No,
BookMarkLineBackground     = 0,255,255     , No, No, No,
BracketsAndBraces          = 0,0,0         , No, No, No,
CommentsAndRemarks         = 128,128,128   , No, No, No,
NumericalConstants         = 0,128,128     , No, No, No,
Operators                  = 0,0,0         , No, No, No,
PreprocessorDirectives     = 255,0,0       , No, No, No,
SelectionBackground        = 255,0,255     , No, No, No,
TerminatedStringConstants  = 0,128,0       , No, No, No,
UnrecognizedText           = 0,0,0         , No, No, No,
UnterminatedStrings        = 128,128,0     , No, No, No,
CurrentLineBackColor       = 128,0,0       , No, No, No,
FunctionsNotInDictionaries = 0,0,192       , No, No, No,
ConstantsDictionary        = 128,0,128     , No, No, No,
WAPIDictionary             = 0,0,128       , No, No, No,
HashDictionary             = 255,255,192   , No, No, No,
CLanguageDictionary        = 0,0,128       , No, No, No,
UserDictionary             = 0,0,0         , No, No, No,

#endif

/*----------------------------------------------------------------------*/

