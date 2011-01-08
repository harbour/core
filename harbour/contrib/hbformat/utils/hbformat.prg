/*
 * $Id$
 */

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

#include "common.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

FUNCTION MAIN( ... )

   LOCAL oRef, aParams, cFileName, cInitDir, i, lRecursive := .F.

   // Altd( 2 ); Altd()
   aParams := hb_AParams()

   IF Empty( aParams ) .OR. ( Left( cFileName := Atail(aParams ), 1 ) $ "@/-" )
      About()
      RETURN Nil
   ENDIF

   FOR i := 1 TO Len( aParams )
      IF Left( aParams[i], 1 ) $ "-/"
         IF SubStr( aParams[i], 2 ) == "r"
            lRecursive := .T.
            aParams[i] := "#"
            EXIT
         ENDIF
      ENDIF
   NEXT

   oRef := hbFormatCode():New( aParams, hb_ArgV( 0 ) )
   IF oRef:nErr > 0
      ? "Initialization error", oRef:nErr, Iif( oRef:nLineErr == 0, "in parameter", "on line " + hb_ntos( oRef:nLineErr ) ), ":", oRef:cLineErr
      RETURN Nil
   ENDIF

   oRef:bCallBack := { |a, i|FCallBack( a, i ) }

   IF "*" $ cFileName
      IF ( i := Rat( ".", cFileName ) ) == 0 .OR. Substr( cFileName,i+1,1 ) < "A"
         ? "Wrong mask"
      ELSE
         cInitDir := Iif( ( i := Rat( "\", cFileName ) ) == 0, ;
            Iif( ( i := Rat( "/", cFileName ) ) == 0, ;
            "." + Set( _SET_DIRSEPARATOR ), Left( cFileName, i ) ), ;
            Left( cFileName, i ) )
         cFileName := Iif( i == 0, cFileName, SubStr( cFileName, i + 1 ) )
         DirEval( cInitDir, cFileName, lRecursive, { |name|Reformat( oRef,name ) } )
      ENDIF
   ELSE
      Reformat( oRef, cFileName )
   ENDIF
   ?

   RETURN Nil

STATIC FUNCTION FCallBack( aFile, nItem )

   LOCAL n := Int( Len( aFile ) / 40 )

   IF nItem % n == 1
      ?? "."
   ENDIF

   RETURN Nil

STATIC FUNCTION Reformat( oRef, cFileName )

   LOCAL aFile

   IF !Empty( aFile := oRef:File2Array( cFileName ) )
      ? "Reformatting " + cFileName
      ? "<"
      IF oRef:Reformat( aFile )
         oRef:Array2File( cFileName, aFile )
         ?? ">"
      ELSE
         ? "Error", oRef:nErr, "on line", oRef:nLineErr, ":", oRef:cLineErr
      ENDIF
   ELSE
      ? cFileName, "isn't found ..."
   ENDIF

   RETURN Nil

STATIC FUNCTION CmpMsk( strcmp, mask )

   LOCAL lenm := Len( mask ), i, nPos1 := 1, nPos2, c

   FOR i := 1 TO lenm
      c := SubStr( mask, i, 1 )
      IF c == "*"
         IF i == lenm
            RETURN .T.
         ELSE
            c := SubStr( mask, i + 1, 1 )
            DO WHILE .T.
               nPos2 := At( c, SubStr( strcmp, nPos1 ) )
               IF nPos2 == 0
                  RETURN .F.
               ENDIF
               nPos1 += nPos2 - 1
               IF CmpMsk( SubStr( strcmp, nPos1 ), SubStr( mask, i + 1 ) )
                  RETURN .T.
               ENDIF
               nPos1 ++
            ENDDO
         ENDIF
      ELSE
         IF c != SubStr( strcmp, nPos1, 1 )
            RETURN .F.
         ENDIF
         nPos1 ++
      ENDIF
   NEXT

   RETURN .T.

FUNCTION DirEval( cInitDir, cMask, lRecur, bCode )

   LOCAL i, nLen, aFiles

   IF Right( cInitDir, 1 ) != Set( _SET_DIRSEPARATOR )
      cInitDir += Set( _SET_DIRSEPARATOR )
   ENDIF
   cMask := Iif( cMask == Nil, hb_osFileMask(), Upper( cMask ) )

   aFiles := Directory( cInitDir + hb_osFileMask(), "HSD" )
   nLen := Len( aFiles )
   FOR i := 1 TO nLen
      IF "D" $ aFiles[ i,5 ]
         IF "." != aFiles[ i,1 ] .AND. ".." != aFiles[ i,1 ] .AND. lRecur
            DirEval( cInitDir + aFiles[ i,1 ], cMask, lRecur, bCode )
         ENDIF
      ELSEIF CmpMsk( Upper( aFiles[ i,1 ] ), cMask )
         IF bCode != Nil
            Eval( bCode, cInitDir + aFiles[ i,1 ] )
         ENDIF
      ENDIF
   NEXT

   RETURN Nil

STATIC FUNCTION About()

   ?? "Harbour Source Formatter " + HBRawVersion()
   ? "Copyright (c) 2009-2011, Alexander S.Kresin"
   ? "http://harbour-project.org/"
   ?
   ? "Syntax:  hbformat [options] [@config] file[s]"
   ?

   RETURN Nil

STATIC FUNCTION HBRawVersion()
   RETURN StrTran( Version(), "Harbour " )
