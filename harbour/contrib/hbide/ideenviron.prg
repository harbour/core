/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               01Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "hbqt.ch"
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/
//
//                           Class IdeEnvironments
//
/*----------------------------------------------------------------------*/

CLASS IdeEnvironments INHERIT IdeObject

   DATA   cEnvFile
   DATA   aNames                                  INIT {}
   DATA   aEnvrns                                 INIT {}
   DATA   aShellContents                          INIT {}
   DATA   aCommons                                INIT {}
   DATA   oUI_1

   METHOD new( oIDE, cEnvFile )
   METHOD create( oIDE, cEnvFile )
   METHOD destroy()
   METHOD parse( cEnvFile )
   METHOD prepareBatch( cEnvName )
   METHOD getNames()                              INLINE ::aNames
   METHOD saveEnv()
   METHOD show()
   METHOD execEnv( nMode, p )
   METHOD fetchNew()
   METHOD getHbmk2Commands( cEnvName )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:new( oIDE, cEnvFile )

   ::oIDE := oIDE
   ::cEnvFile := cEnvFile

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:create( oIDE, cEnvFile )
   LOCAL cPath, cFile

   DEFAULT oIDE     TO ::oIDE
   DEFAULT cEnvFile TO ::cEnvFile

   ::oIDE     := oIDE
   ::cEnvFile := cEnvFile

   IF empty( ::cEnvFile )
      cFile := ::oIde:aINI[ INI_HBIDE, PathEnv ]
      cFile := iif( empty( cFile ), cFile, hbide_pathAppendLastSlash( cFile ) )
      IF empty( cFile )
         cFile := oIde:cProjIni
      ENDIF
      hb_fNameSplit( cFile, @cPath )
      ::cEnvFile := cPath + "hbide.env"
   ENDIF

   hb_fNameSplit( ::cEnvFile, @cPath )
   ::oIDE:cWrkPathEnv := hbide_pathNormalized( cPath, .f. )
   ::oIde:aINI[ INI_HBIDE, PathEnv ] := ::oIDE:cWrkPathEnv

   IF !empty( ::cEnvFile ) .AND. hb_fileExists( ::cEnvFile )
      ::parse( ::cEnvFile )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:destroy()

   IF !empty( ::oUI )
      ::oUI:destroy()
   ENDIF
   IF !empty( ::oUI_1 )
      ::oUI_1:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:parse( cEnvFile )
   LOCAL s, cPart, cEnv, a_, cKey, cVal
   LOCAL aContents := hbide_readSource( cEnvFile )

   ::aNames  := {}
   ::aEnvrns := {}
   a_        := {}
   cEnv      := ""

   FOR EACH s IN aContents
      s := alltrim( s )
      IF empty( s ) .OR. left( s, 1 ) == "#"                     /* Remark */
         LOOP
      ENDIF
      IF left( s, 1 ) == "["
         s := alltrim( strtran( s, "[", "" ) )
         s := alltrim( strtran( s, "]", "" ) )
         IF lower( s ) == "common"
            cPart := "common"
         ELSE
            cPart := "environment"
            IF ( s != cEnv ) .AND. !empty( cEnv )
               aadd( ::aNames, cEnv )
               aadd( ::aEnvrns, { cEnv, a_ } )
            ENDIF
            cEnv := s
            a_:= {}
         ENDIF
      ELSE
         IF cPart == "common"
            IF hbide_parseKeyValPair( s, @cKey, @cVal )
               aadd( ::aCommons, { lower( cKey ), cVal } )       /* Format Later */
            ENDIF
         ELSEIF cPart == "environment"
            IF hbide_parseFilter( s, @cKey, @cVal )
               aadd( a_, { lower( cKey ), cVal } )
            ENDIF
         ENDIF
      ENDIF
   NEXT
   IF !empty( cEnv ) .AND. !empty( a_ )
      aadd( ::aNames, cEnv )
      aadd( ::aEnvrns, { cKey, a_ } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:getHbmk2Commands( cEnvName )
   LOCAL n, s, a_, aCmd := {}

   IF ( n := ascan( ::aEnvrns, {|e_| e_[ 1 ] == cEnvName } ) ) > 0
      FOR EACH a_ IN ::aEnvrns[ n, 2 ]
         s := a_[ 1 ]
         IF s == "hbmk2"
            aadd( aCmd, a_[ 2 ] )
         ENDIF
      NEXT
   ENDIF

   RETURN aCmd

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:prepareBatch( cEnvName )
   LOCAL n, s, a_, aCmd := {}

   IF ( n := ascan( ::aEnvrns, {|e_| e_[ 1 ] == cEnvName } ) ) > 0
      FOR EACH a_ IN ::aEnvrns[ n, 2 ]
         s := a_[ 1 ]
         IF s == "content"
            aadd( aCmd, a_[ 2 ] )
         ENDIF
      NEXT
   ENDIF

   RETURN hbide_getShellCommandsTempFile( aCmd )

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:show()

   IF empty( ::oUI )
      ::oUI := HbQtUI():new( hbide_uic( "environments" ) ):build()
      ::oEnvironDock:oWidget:setWidget( ::oUI )

      ::oUI:q_buttonPathMk2:setIcon( hbide_image( "folder" ) )
      ::oUI:q_buttonPathEnv:setIcon( hbide_image( "folder" ) )

      ::oUI:signal( "buttonCn"      , "clicked()", {|| ::oEnvironDock:hide() } )
      ::oUI:signal( "buttonSave"    , "clicked()", {|| ::saveEnv()    } )
      ::oUI:signal( "buttonSaveExit", "clicked()", {|| ::saveEnv(), ::oEnvironDock:hide() } )
      ::oUI:signal( "buttonPathMk2" , "clicked()", {|| ::execEnv( 1 ) } )
      ::oUI:signal( "buttonPathEnv" , "clicked()", {|| ::execEnv( 2 ) } )

      ::oUI:q_editCompilers:setFont( ::oFont:oWidget )

      #if 0
      ::oUI:q_editPathEnv:setFont( ::oFont:oWidget )
      ::oUI:q_editPathMk2:setFont( ::oFont:oWidget )
      #endif
   ENDIF

   ::oUI:q_editPathMk2  :setText( ::aINI[ INI_HBIDE, PathMk2 ] )
   ::oUI:q_editPathEnv  :setText( ::aINI[ INI_HBIDE, PathEnv ] )
   ::oUI:q_editCompilers:setPlainText( hb_memoread( hbide_pathFile( ::aINI[ INI_HBIDE, PathEnv ], "hbide.env" ) ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:execEnv( nMode, p )
   LOCAL cPath, cP

   HB_SYMBOL_UNUSED( p )

   DO CASE
   CASE nMode == 1
      cPath := hbide_fetchAFile( ::oDlg, "Select location of hbmk2", ;
                                   { { "Harbour Projects Builder - hbmk2", "*" } }, ::cWrkPathMK2 )
      IF !empty( cPath )
         hb_fNameSplit( cPath, @cP )
         ::oIde:cWrkPathMK2 := cP
         ::oUI:q_editPathMk2:setText( hbide_pathStripLastSlash( cP ) )
      ENDIF

   CASE nMode == 2
      cPath := hbide_fetchAFile( ::oDlg, "Select location of hbide.env", ;
                                   { { "Harbour Projects", "*.env" } }, ::cWrkPathEnv )
      IF !empty( cPath )
         hb_fNameSplit( cPath, @cP )
         ::oIDE:cWrkPathEnv := cP
         ::oUI:q_editPathEnv:setText( cP ) // hbide_pathStripLastSlash( cP ) )

         ::oUI:q_editCompilers:setPlainText( ;
             hb_memoread( hbide_pathFile( ::oUI:q_editPathEnv:text(), "hbide.env" ) ) )
      ENDIF

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:saveEnv()
   LOCAL cText, cPathMk2, cPathEnv, cEnvFile
   LOCAL oUIEnv := ::oUI

   cPathMk2 := oUIEnv:q_editPathMk2:text()
   cPathEnv := oUIEnv:q_editPathEnv:text()

   ::oIDE:aINI[ INI_HBIDE, PathMk2 ] := cPathMk2
   ::oIDE:aINI[ INI_HBIDE, PathEnv ] := cPathEnv
   //
   ::oIDE:cWrkPathMk2 := cPathMk2
   ::oIDE:cWrkPathEnv := cPathEnv

   IF !empty( cText := oUIEnv:q_editCompilers:toPlainText() )
      cEnvFile := hbide_pathFile( cPathEnv, "hbide.env" )
      hb_MemoWrit( cEnvFile, cText )

      ::parse( cEnvFile )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:fetchNew()

   IF empty( ::oUI_1 )
      ::oUI_1 := HbQtUI():new( hbide_uic( "environ" ) ):build()
      ::oUI_1:setWindowFlags( Qt_Sheet )
   ENDIF
   ::oUI_1:show()

   RETURN Self

/*----------------------------------------------------------------------*/
