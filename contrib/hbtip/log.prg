/*
 * Harbour Project source code:
 * TIP simple logger class
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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

#include "hbclass.ch"

#include "fileio.ch"

CREATE CLASS TIPLog

   METHOD New( cFileName )
   METHOD Add( cMsg )
   METHOD Close()
   METHOD Clear()

   PROTECTED:

   VAR cFileName
   VAR fhnd

ENDCLASS

METHOD New( cFileName ) CLASS TIPLog

   hb_default( @cFileName, "hbtip" )

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".log" )
   ENDIF

   ::cFileName := cFileName

   RETURN Self

METHOD Add( cMsg ) CLASS TIPLog

   LOCAL cDir, cName, cExt
   LOCAL n

   IF Empty( ::fhnd ) .OR. ::fhnd == F_ERROR

      hb_FNameSplit( ::cFileName, @cDir, @cName, @cExt )

      n := 1
      DO WHILE .T.
         ::fhnd := hb_FCreate( hb_FNameMerge( cDir, cName + "-" + hb_ntos( n ), cExt ), NIL, FO_EXCL )
         IF ::fhnd != F_ERROR .OR. ;
            FError() == 3 /* path not found */
            EXIT
         ENDIF
         n++
      ENDDO
   ENDIF

   IF ! Empty( ::fhnd ) .AND. ::fhnd != F_ERROR
      RETURN FWrite( ::fhnd, cMsg ) == hb_BLen( cMsg )
   ENDIF

   RETURN .F.

METHOD Close() CLASS TIPLog

   LOCAL lRetVal

   IF ! Empty( ::fhnd ) .AND. ::fhnd != F_ERROR
      lRetVal := FClose( ::fhnd )
      ::fhnd := NIL
      RETURN lRetVal
   ENDIF

   RETURN .F.

METHOD Clear() CLASS TIPLog
   RETURN ::Close() .AND. FErase( ::cFileName ) == 0
