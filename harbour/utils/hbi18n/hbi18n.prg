/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Harbour i18n .pot/.hbl file manger
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbgtinfo.ch"
#include "directry.ch"

#define _HB_I18N_MERGE  1
#define _HB_I18N_GENHBL 2
#define _HB_I18N_TRANS  3

#define LEFTEQUAL( l, r )       ( Left( l, Len( r ) ) == r )

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

PROCEDURE Main( ... )
   LOCAL aParams, aFiles
   LOCAL cFileOut, cFileIn, cExt
   LOCAL lError, lEmpty, lQuiet
   LOCAL nMode, n
   LOCAL param

   aParams := hb_aParams()
   IF Empty( aParams )
      Syntax()
   ENDIF

   lError := lEmpty := lQuiet := .F.
   aFiles := {}
   nMode := 0
   FOR n := 1 TO Len( aParams )
      IF LEFTEQUAL( aParams[ n ], "-" )
         param := SubStr( aParams[ n ], 2 )
         IF param == "m"
            IF nMode != 0
               lError := .T.
            ELSE
               nMode := _HB_I18N_MERGE
            ENDIF
         ELSEIF param == "g"
            IF nMode != 0
               lError := .T.
            ELSE
               nMode := _HB_I18N_GENHBL
            ENDIF
         ELSEIF param == "a"
            IF nMode != 0
               lError := .T.
            ELSE
               nMode := _HB_I18N_TRANS
            ENDIF
         ELSEIF LEFTEQUAL( param, "o" )
            IF !Empty( param := SubStr( param, 2 ) )
               cFileOut := param
            ELSEIF n < Len( aParams ) .AND. aParams[ n + 1 ] != "-"
               cFileOut := aParams[ ++n ]
            ELSE
               lError := .T.
            ENDIF
         ELSEIF param == "e"
            lEmpty := .T.
         ELSEIF param == "q"
            lQuiet := .T.
         ELSE
            lError := .T.
         ENDIF
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
         hb_FNameSplit( aFiles[ n ],,, @cExt )
         IF !Lower( cExt ) == ".hbl"
            cFileIn := aFiles[ n ]
            HB_ADel( aFiles, n, .T. )
            EXIT
         ENDIF
      NEXT
   ENDIF

   IF nMode == 0 .OR. Empty( aFiles ) .OR. ;
      ( nMode == _HB_I18N_TRANS .AND. Empty( cFileIn ) )
      Syntax()
   ENDIF

   IF !lQuiet
      Logo()
   ENDIF

   IF nMode == _HB_I18N_MERGE
      Merge( aFiles, cFileOut )
   ELSEIF nMode == _HB_I18N_GENHBL
      GenHBL( aFiles, cFileOut, lEmpty )
   ELSEIF nMode == _HB_I18N_TRANS
      AutoTrans( cFileIn, aFiles, cFileOut )
   ENDIF

   RETURN


STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )

STATIC PROCEDURE Logo()

   OutStd( "Harbour i18n .pot/.hbl file manager " + HBRawVersion() + hb_eol() +;
           "Copyright (c) 2009-2012, Przemyslaw Czerpak" + hb_eol() + ;
           "http://harbour-project.org/" + hb_eol() +;
           hb_eol() )
   RETURN


STATIC PROCEDURE Syntax()

   Logo()
   OutStd( "Syntax: hbi18n -m | -g | -a [-o<outfile>] [-e] [-q] <files1[.pot] ...>" + hb_eol() + ;
           hb_eol() + ;
           "    -m          merge given .pot files" + hb_eol() + ;
           "    -g          generate .hbl file from given .pot files" + hb_eol() + ;
           "    -a          add automatic translations to 1-st .pot file using" + hb_eol() + ;
           "                translations from other .pot or .hbl files" + hb_eol() + ;
           "    -o<outfile> output file name" + hb_eol() + ;
           "                default is first .pot file name with" + hb_eol() + ;
           "                .po (merge) or .hbl extension" + hb_eol() + ;
           "    -e          do not strip empty translation rules from .hbl files" + hb_eol() + ;
           "    -q          quiet mode" + hb_eol() + ;
           hb_eol() )

   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( "Press any key to continue..." )
      Inkey( 0 )
   ENDIF
   ErrorLevel( 1 )
   QUIT


STATIC PROCEDURE ErrorMsg( cErrorMsg )

   OutStd( "error: " + StrTran( cErrorMsg, ";", hb_eol() ) + hb_eol() )

   IF hb_gtInfo( HB_GTI_ISGRAPHIC )
      OutStd( "Press any key to continue..." )
      Inkey( 0 )
   ENDIF
   ErrorLevel( 1 )
   QUIT


STATIC FUNCTION FileExt( cFile, cDefExt, lForce )
   LOCAL cPath, cName, cExt

   hb_FNameSplit( cFile, @cPath, @cName, @cExt )
   IF lForce .OR. Empty( cExt )
      cFile := hb_FNameMerge( cPath, cName, cDefExt )
   ENDIF
   RETURN cFile


STATIC FUNCTION ExpandWildCards( aFiles )

/* do not expand wild cards in environments where SHELL already does it.
 * In *nixes it's possible that file name will contains "*" or "?".
 */
#ifndef __PLATFORM__UNIX
   LOCAL cFile, cRealFile
   LOCAL aRealFiles
   LOCAL lWild

   lWild := .F.
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
            FOR EACH cRealFile IN Directory( cFile )
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

   aTrans := __I18N_potArrayLoad( aFiles[ 1 ], @cErrorMsg )
   IF aTrans == NIL
      ErrorMsg( cErrorMsg )
   ENDIF
   FOR n := 2 TO Len( aFiles )
      aTrans2 := __I18N_potArrayLoad( aFiles[ n ], @cErrorMsg )
      IF aTrans2 == NIL
         ErrorMsg( cErrorMsg )
      ENDIF
      __I18N_potArrayJoin( aTrans, aTrans2, @hIndex )
   NEXT

   RETURN aTrans


STATIC FUNCTION LoadFilesAsHash( aFiles )
   LOCAL cTrans, cExt, cErrorMsg
   LOCAL hTrans
   LOCAL aTrans
   LOCAL n

   FOR n := 1 TO Len( aFiles )
      hb_FNameSplit( aFiles[ n ],,, @cExt )
      IF Lower( cExt ) == ".hbl"
         cTrans := hb_memoRead( aFiles[ n ] )
         IF !HB_I18N_Check( cTrans )
            ErrorMsg( "Wrong file format: " + aFiles[ n ] )
         ENDIF
         IF hTrans == NIL
            hTrans := __I18N_hashTable( HB_I18N_RestoreTable( cTrans ) )
         ELSE
            __I18N_hashJoin( hTrans, __I18N_hashTable( HB_I18N_RestoreTable( cTrans ) ) )
         ENDIF
      ELSE
         aTrans := __I18N_potArrayLoad( aFiles[ n ], @cErrorMsg )
         IF aTrans == NIL
            ErrorMsg( cErrorMsg )
         ENDIF
         hTrans := __I18N_potArrayToHash( aTrans,, hTrans )
      ENDIF
   NEXT

   RETURN hTrans


STATIC PROCEDURE Merge( aFiles, cFileOut )
   LOCAL cErrorMsg

   IF Empty( cFileOut )
      cFileOut := FileExt( aFiles[ 1 ], ".po", .T. )
   ELSE
      cFileOut := FileExt( cFileOut, ".po", .F. )
   ENDIF

   IF !__I18N_potArraySave( cFileOut, LoadFiles( aFiles ), @cErrorMsg )
      ErrorMsg( cErrorMsg )
   ENDIF

   RETURN


STATIC PROCEDURE GenHBL( aFiles, cFileOut, lEmpty )
   LOCAL cHBLBody
   LOCAL pI18N

   IF Empty( cFileOut )
      cFileOut := FileExt( aFiles[ 1 ], ".hbl", .T. )
   ELSE
      cFileOut := FileExt( cFileOut, ".hbl", .F. )
   ENDIF

   pI18N := __I18N_hashTable( __I18N_potArrayToHash( LoadFiles( aFiles ), ;
                                                     lEmpty ) )
   cHBLBody := HB_I18N_SaveTable( pI18N )
   IF !hb_memoWrit( cFileOut, cHBLBody )
      ErrorMsg( "cannot create file: " + cFileOut )
   ENDIF

   RETURN


STATIC PROCEDURE AutoTrans( cFileIn, aFiles, cFileOut )
   LOCAL cErrorMsg

   IF Empty( cFileOut )
      cFileOut := FileExt( cFileIn, ".po", .T. )
   ELSE
      cFileOut := FileExt( cFileOut, ".po", .F. )
   ENDIF

   IF !__I18N_potArraySave( cFileOut, ;
         __I18N_potArrayTrans( LoadFiles( { cFileIn } ), ;
                               LoadFilesAsHash( aFiles ) ), @cErrorMsg )
      ErrorMsg( cErrorMsg )
   ENDIF

   RETURN
