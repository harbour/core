/*
 * Harbour Project source code:
 * Harbour source code formatter (cmdline wrapper)
 *
 * Copyright 2009 Alexander S.Kresin <alex@belacy.belgorod.su>
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

#require "hbformat"

#include "directry.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

#define I_( x )                 hb_UTF8ToStr( hb_i18n_gettext( x ) )

PROCEDURE Main( ... )

   LOCAL oRef, aParams, cFileName, cInitDir, i, cParam, lRecursive := .F.

   // AltD( 2 ); AltD()
   aParams := hb_AParams()

   IF Empty( aParams ) .OR. ( Left( cFileName := ATail( aParams ), 1 ) $ "@/-" )
      About()
      RETURN
   ENDIF

   FOR EACH cParam IN aParams
      IF Left( cParam, 1 ) $ "-/"
         IF SubStr( cParam, 2 ) == "r"
            lRecursive := .T.
            cParam := "#"
            EXIT
         ENDIF
      ENDIF
   NEXT

   oRef := HBFormatCode():New( aParams, hb_FNameMerge( hb_DirBase(), "hbformat.ini" ) )
   IF oRef:nErr > 0
      OutStd( hb_StrFormat( iif( oRef:nLineErr == 0, ;
         I_( "Initialization error %1$d in parameter: %2$s" ), ;
         I_( "Initialization error %1$d on line %3%d: %2$s" ) ), oRef:nErr, oRef:cLineErr, oRef:nLineErr ) + hb_eol() )
   ENDIF

   oRef:bCallBack := {| a, i | FCallBack( a, i ) }

   IF "*" $ cFileName
      IF ( i := RAt( ".", cFileName ) ) == 0 .OR. SubStr( cFileName, i + 1, 1 ) < "A"
         OutErr( I_( "Wrong mask" ) + hb_eol() )
      ELSE
         cInitDir := ;
            iif( ( i := RAt( "\", cFileName ) ) == 0, ;
            iif( ( i := RAt( "/", cFileName ) ) == 0, ;
            "." + hb_ps(), ;
            Left( cFileName, i ) ), ;
            Left( cFileName, i ) )
         cFileName := iif( i == 0, cFileName, SubStr( cFileName, i + 1 ) )
         DirEval( cInitDir, cFileName, lRecursive, {| name | Reformat( oRef, name ) } )
      ENDIF
   ELSE
      Reformat( oRef, cFileName )
   ENDIF

   RETURN

STATIC PROCEDURE FCallBack( aFile, nItem )

   IF nItem % Int( Len( aFile ) / 40 ) == 1
      OutStd( "." )
   ENDIF

   RETURN

STATIC PROCEDURE Reformat( oRef, cFileName )

   LOCAL aFile

   IF ! Empty( aFile := oRef:File2Array( cFileName ) )
      OutStd( hb_StrFormat( I_( "Reformatting %1$s (%2$d lines)" ), cFileName, Len( aFile ) ) + hb_eol() )
      OutStd( "<" )
      IF oRef:Reformat( aFile )
         oRef:Array2File( cFileName, aFile )
         OutStd( ">" + hb_eol() )
      ELSE
         OutErr( hb_StrFormat( I_( "Error %1$d on line %2$d: %3$s" ), oRef:nErr, oRef:nLineErr, oRef:cLineErr ) + hb_eol() )
      ENDIF
   ELSE
      OutErr( hb_StrFormat( I_( "'%1$s' is not found..." ), cFileName ) + hb_eol() )
   ENDIF

   RETURN

STATIC PROCEDURE DirEval( cInitDir, cMask, lRecur, bCode )

   LOCAL file

   cInitDir := hb_DirSepAdd( cInitDir )
   cMask := iif( cMask == NIL, hb_osFileMask(), cMask )

   FOR EACH file IN Directory( cInitDir + cMask, "HSD" )
      IF "D" $ file[ F_ATTR ]
         IF !( "." == file[ F_NAME ] ) .AND. ;
            !( ".." == file[ F_NAME ] ) .AND. lRecur
            DirEval( cInitDir + file[ F_NAME ], cMask, lRecur, bCode )
         ENDIF
      ELSE
         IF bCode != NIL
            Eval( bCode, cInitDir + file[ F_NAME ] )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE About()

   OutStd( ;
      "Harbour Source Formatter " + HBRawVersion() + hb_eol() + ;
      "Copyright (c) 2009-2014, Alexander S.Kresin" + hb_eol() + ;
      "http://harbour-project.org/" + hb_eol() + ;
      hb_eol() )

   OutStd( ;
      I_( "Syntax:  hbformat [options] [@config] <file[s]>" ) + hb_eol() + ;
      hb_eol() )

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
