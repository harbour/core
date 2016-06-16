/*
 * Harbour i18n .pot/.hbl file manager
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbgtinfo.ch"
#include "hbver.ch"
#include "directry.ch"

#define _HB_I18N_MERGE   1
#define _HB_I18N_GENHBL  2
#define _HB_I18N_TRANS   3

#define I_( x )          hb_UTF8ToStr( hb_i18n_gettext( x ) )

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

PROCEDURE Main( ... )

   LOCAL aParams, aFiles
   LOCAL cFileOut, cFileIn
   LOCAL lError, lEmpty, lQuiet
   LOCAL nMode, n
   LOCAL cParam

   IF Empty( aParams := hb_AParams() )
      Syntax()
   ENDIF

   lError := lEmpty := lQuiet := .F.
   aFiles := {}
   nMode := 0
   FOR n := 1 TO Len( aParams )
      IF hb_LeftEq( aParams[ n ], "-" )
         cParam := SubStr( aParams[ n ], 2 )
         DO CASE
         CASE cParam == "m"
            IF nMode != 0
               lError := .T.
            ELSE
               nMode := _HB_I18N_MERGE
            ENDIF
         CASE cParam == "g"
            IF nMode != 0
               lError := .T.
            ELSE
               nMode := _HB_I18N_GENHBL
            ENDIF
         CASE cParam == "a"
            IF nMode != 0
               lError := .T.
            ELSE
               nMode := _HB_I18N_TRANS
            ENDIF
         CASE hb_LeftEq( cParam, "o" )
            IF ! HB_ISNULL( cParam := SubStr( cParam, 2 ) )
               cFileOut := cParam
            ELSEIF n < Len( aParams ) .AND. ! hb_LeftEq( aParams[ n + 1 ], "-" )
               cFileOut := aParams[ ++n ]
            ELSE
               lError := .T.
            ENDIF
         CASE cParam == "e"
            lEmpty := .T.
         CASE cParam == "q"
            lQuiet := .T.
         OTHERWISE
            lError := .T.
         ENDCASE
      ELSE
         AAdd( aFiles, aParams[ n ] )
      ENDIF
      IF lError
         Syntax()
      ENDIF
   NEXT

   aFiles := ExpandWildCards( aFiles )

   IF nMode == _HB_I18N_TRANS
      FOR n := 1 TO Len( aFiles )
         IF ! Lower( hb_FNameExt( aFiles[ n ] ) ) == ".hbl"
            cFileIn := aFiles[ n ]
            hb_ADel( aFiles, n, .T. )
            EXIT
         ENDIF
      NEXT
   ENDIF

   IF nMode == 0 .OR. Empty( aFiles ) .OR. ;
      ( nMode == _HB_I18N_TRANS .AND. Empty( cFileIn ) )
      Syntax()
   ENDIF

   IF ! lQuiet
      Logo()
   ENDIF

   SWITCH nMode
   CASE _HB_I18N_MERGE
      Merge( aFiles, cFileOut )
      EXIT
   CASE _HB_I18N_GENHBL
      GenHBL( aFiles, cFileOut, lEmpty )
      EXIT
   CASE _HB_I18N_TRANS
      AutoTrans( cFileIn, aFiles, cFileOut )
      EXIT
   ENDSWITCH

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN hb_StrFormat( "%d.%d.%d%s (%s) (%s)", ;
      hb_Version( HB_VERSION_MAJOR ), ;
      hb_Version( HB_VERSION_MINOR ), ;
      hb_Version( HB_VERSION_RELEASE ), ;
      hb_Version( HB_VERSION_STATUS ), ;
      hb_Version( HB_VERSION_ID ), ;
      "20" + Transform( hb_Version( HB_VERSION_REVISION ), "99-99-99 99:99" ) )

STATIC PROCEDURE Logo()

   OutStd( ;
      "Harbour i18n .pot/.hbl file manager " + HBRawVersion() + hb_eol() + ;
      "Copyright (c) 2009-2016, Przemyslaw Czerpak" + hb_eol() + ;
      hb_Version( HB_VERSION_URL_BASE ) + hb_eol() + ;
      hb_eol() )

   RETURN

STATIC PROCEDURE Syntax()

   Logo()
   OutStd( ;
      I_( "Syntax: hbi18n -m | -g | -a [-o<outfile>] [-e] [-q] <files1[.pot] ...>" ) + hb_eol() + ;
      hb_eol() + ;
      "    -m          " + I_( "merge given .pot files" ) + hb_eol() + ;
      "    -g          " + I_( "generate .hbl file from given .pot files" ) + hb_eol() + ;
      "    -a          " + I_( "add automatic translations to 1st .pot file using translations from other .pot/.hbl files" ) + hb_eol() + ;
      "    -o<outfile> " + I_( "output file name" ) + hb_eol() + ;
      "                " + I_( "default is first .pot file name with .po (merge) or .hbl extension" ) + hb_eol() + ;
      "    -e          " + I_( "do not strip empty translation rules from .hbl files" ) + hb_eol() + ;
      "    -q          " + I_( "quiet mode" ) + hb_eol() + ;
      hb_eol() )

   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( I_( "Press any key to continue..." ) )
      Inkey( 0 )
   ENDIF
   ErrorLevel( 1 )
   QUIT

STATIC PROCEDURE ErrorMsg( cErrorMsg )

   OutStd( hb_StrFormat( I_( "error: %1$s" ), StrTran( cErrorMsg, ";", hb_eol() ) ) + hb_eol() )

   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( I_( "Press any key to continue..." ) )
      Inkey( 0 )
   ENDIF
   ErrorLevel( 1 )
   QUIT

STATIC FUNCTION ExpandWildCards( aFiles )

/* Do not expand wild cards in environments where SHELL already does it.
   In *nixes it's possible that file name will contains "*" or "?". */

#ifndef __PLATFORM__UNIX

   LOCAL cFile, cRealFile
   LOCAL aRealFiles
   LOCAL lWild := .F.

   FOR EACH cFile IN aFiles
      IF "*" $ cFile .OR. "?" $ cFile
         lWild := .T.
         EXIT
      ENDIF
   NEXT
   IF lWild
      aRealFiles := {}
      FOR EACH cFile IN aFiles
         IF "*" $ cFile .OR. "?" $ cFile
            FOR EACH cRealFile IN hb_vfDirectory( cFile )
               AAdd( aRealFiles, cRealFile[ F_NAME ] )
            NEXT
         ELSE
            AAdd( aRealFiles, cFile )
         ENDIF
      NEXT
      aFiles := aRealFiles
   ENDIF
#endif

   RETURN aFiles

STATIC FUNCTION LoadFiles( aFiles )

   LOCAL aTrans, aTrans2
   LOCAL hIndex
   LOCAL cErrorMsg
   LOCAL n

   IF ( aTrans := __i18n_potArrayLoad( aFiles[ 1 ], @cErrorMsg ) ) == NIL
      ErrorMsg( cErrorMsg )
   ENDIF
   FOR n := 2 TO Len( aFiles )
      IF ( aTrans2 := __i18n_potArrayLoad( aFiles[ n ], @cErrorMsg ) ) == NIL
         ErrorMsg( cErrorMsg )
      ENDIF
      __i18n_potArrayJoin( aTrans, aTrans2, @hIndex )
   NEXT

   RETURN aTrans

STATIC FUNCTION LoadFilesAsHash( aFiles )

   LOCAL cTrans, cErrorMsg
   LOCAL hTrans
   LOCAL aTrans
   LOCAL cFile

   FOR EACH cFile IN aFiles
      IF Lower( hb_FNameExt( cFile ) ) == ".hbl"
         IF ! hb_i18n_Check( cTrans := hb_MemoRead( cFile ) )
            ErrorMsg( hb_StrFormat( I_( "Wrong file format: %1$s" ), cFile ) )
         ENDIF
         IF hTrans == NIL
            hTrans := __i18n_hashTable( hb_i18n_RestoreTable( cTrans ) )
         ELSE
            __i18n_hashJoin( hTrans, __i18n_hashTable( hb_i18n_RestoreTable( cTrans ) ) )
         ENDIF
      ELSE
         IF ( aTrans := __i18n_potArrayLoad( cFile, @cErrorMsg ) ) == NIL
            ErrorMsg( cErrorMsg )
         ENDIF
         hTrans := __i18n_potArrayToHash( aTrans,, hTrans )
      ENDIF
   NEXT

   RETURN hTrans

STATIC PROCEDURE Merge( aFiles, cFileOut )

   LOCAL cErrorMsg

   IF ! HB_ISSTRING( cFileOut ) .OR. HB_ISNULL( cFileOut )
      cFileOut := hb_FNameExtSet( aFiles[ 1 ], ".po" )
   ELSE
      cFileOut := hb_FNameExtSetDef( cFileOut, ".po" )
   ENDIF

   IF ! __i18n_potArraySave( cFileOut, LoadFiles( aFiles ), @cErrorMsg )
      ErrorMsg( cErrorMsg )
   ENDIF

   RETURN

STATIC PROCEDURE GenHBL( aFiles, cFileOut, lEmpty )

   IF ! HB_ISSTRING( cFileOut ) .OR. HB_ISNULL( cFileOut )
      cFileOut := hb_FNameExtSet( aFiles[ 1 ], ".hbl" )
   ELSE
      cFileOut := hb_FNameExtSetDef( cFileOut, ".hbl" )
   ENDIF

   IF ! hb_MemoWrit( cFileOut, hb_i18n_SaveTable( ;
      __i18n_hashTable( __i18n_potArrayToHash( LoadFiles( aFiles ), lEmpty ) ) ) )

      ErrorMsg( hb_StrFormat( I_( "cannot create file: %1$s" ), cFileOut ) )
   ENDIF

   RETURN

STATIC PROCEDURE AutoTrans( cFileIn, aFiles, cFileOut )

   LOCAL cErrorMsg

   IF ! HB_ISSTRING( cFileOut ) .OR. HB_ISNULL( cFileOut )
      cFileOut := hb_FNameExtSet( cFileIn, ".po" )
   ELSE
      cFileOut := hb_FNameExtSetDef( cFileOut, ".po" )
   ENDIF

   IF ! __i18n_potArraySave( cFileOut, ;
      __i18n_potArrayTrans( LoadFiles( { cFileIn } ), ;
                            LoadFilesAsHash( aFiles ) ), @cErrorMsg )
      ErrorMsg( cErrorMsg )
   ENDIF

   RETURN
