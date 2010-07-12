/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Document generator - HBFDB output
 *
 * Copyright 2010 Viktor Szakats (harbour 01 syenar hu)
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

#include "hbclass.ch"
#include "inkey.ch"
#include "fileio.ch"
#include "hbdoc2.ch"

CLASS GenerateHBFDB FROM TPLGenerate
HIDDEN:

PROTECTED:

EXPORTED:
   METHOD NewIndex( cFolder, cFilename, cTitle )
   METHOD NewDocument( cFolder, cFilename, cTitle )
   METHOD AddEntry( oEntry )
   METHOD AddIndex( oEntry ) HIDDEN
   METHOD BeginSection( cSection, cFilename )
   METHOD EndSection( cSection, cFilename )
   METHOD Generate()

   METHOD WriteEntry( cCaption, cEntry, lPreformatted ) HIDDEN

   VAR aProto INIT {}

ENDCLASS

METHOD NewDocument( cFolder, cFilename, cTitle ) CLASS GenerateHBFDB
   HB_SYMBOL_UNUSED( cFolder )
   HB_SYMBOL_UNUSED( cFileName )
   HB_SYMBOL_UNUSED( cTitle )
   RETURN self

METHOD NewIndex( cFolder, cFilename, cTitle ) CLASS GenerateHBFDB
   HB_SYMBOL_UNUSED( cFolder )
   HB_SYMBOL_UNUSED( cFileName )
   HB_SYMBOL_UNUSED( cTitle )
   RETURN self

METHOD BeginSection( cSection, cFilename ) CLASS GenerateHBFDB
   HB_SYMBOL_UNUSED( cSection )
   HB_SYMBOL_UNUSED( cFileName )
   RETURN self

METHOD EndSection( cSection, cFilename ) CLASS GenerateHBFDB
   HB_SYMBOL_UNUSED( cSection )
   HB_SYMBOL_UNUSED( cFilename )
   RETURN self

METHOD AddIndex( oEntry ) CLASS GenerateHBFDB
   HB_SYMBOL_UNUSED( oEntry )
   RETURN self

METHOD AddEntry( oEntry ) CLASS GenerateHBFDB
   LOCAL idx

   HB_SYMBOL_UNUSED( oEntry )

   FOR idx := 1 TO Len( oEntry:Fields )
      IF oEntry:Fields[ idx ][ 1 ] == "SYNTAX"
         AAdd( ::aProto, oEntry:&( oEntry:Fields[ idx ][ 1 ] ) )
      ENDIF
   NEXT

   RETURN self

METHOD Generate() CLASS GenerateHBFDB

   hb_MemoWrit( "proto.hbfdb", hb_serialize( ::aProto ) )

   RETURN self

METHOD PROCEDURE WriteEntry( cCaption, cEntry, lPreformatted ) CLASS GenerateHBFDB
   HB_SYMBOL_UNUSED( cCaption )
   HB_SYMBOL_UNUSED( cEntry )
   HB_SYMBOL_UNUSED( lPreformatted )

   RETURN
