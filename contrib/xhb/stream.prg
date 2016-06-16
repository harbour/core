/*
 * xhb stream classes
 *
 * Copyright 2009 {list of individual authors and e-mail addresses}
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

#define BUFFER_SIZE 16384

FUNCTION FileReader( cFile, nMode )
   RETURN TStreamFileReader():New( cFile, nMode )

FUNCTION FileWriter( cFile, nMode )
   RETURN TStreamFileWriter():New( cFile, nMode )

CREATE CLASS TStream

   VAR lCanRead  INIT .F.
   VAR lCanWrite INIT .F.
   VAR lCanSeek  INIT .F.

   VAR nLength   INIT 0
   VAR nPosition INIT 0

   METHOD Close() VIRTUAL
   METHOD Flush() VIRTUAL
   METHOD Seek( nOffset, Origin ) VIRTUAL

   METHOD Read( sBuffer, nOffset, nCount ) VIRTUAL
   METHOD ReadByte() VIRTUAL

   METHOD Write( sBuffer, nOffset, nCount ) VIRTUAL
   METHOD WriteByte() VIRTUAL

   METHOD CopyTo( oTargetStream )

ENDCLASS

METHOD PROCEDURE CopyTo( oTargetStream ) CLASS TStream

   LOCAL nBytesToRead := ::nLength
   LOCAL sBuffer := Space( BUFFER_SIZE )
   LOCAL nRead
   LOCAL nPosition

   IF ! oTargetStream:lCanWrite
      Throw( xhb_ErrorNew( "Stream", 0, 1001, ProcName(), "Target not writable.", hb_AParams() ) )
   ENDIF

   // Save
   nPosition := ::nPosition

   ::Seek( 0, FS_SET )
   oTargetStream:Seek( 0, FS_SET )

   DO WHILE nBytesToRead > 0
      nRead := ::Read( @sBuffer, 0, BUFFER_SIZE )
      oTargetStream:Write( sBuffer, 0, nRead )
      nBytesToRead -= nRead
   ENDDO

   // Truncate in case it was a bigger file
   oTargetStream:Write( "", 0, 0 )

   // Restore
   ::Seek( nPosition, FS_SET )

   RETURN

CREATE CLASS TStreamFileReader INHERIT TStream

   VAR cFile
   VAR hFile

   METHOD New( cFile, nMode ) CONSTRUCTOR
   METHOD Handle() INLINE hb_vfHandle( ::hFile )

   METHOD Close() INLINE iif( ::hFile != NIL, hb_vfClose( ::hFile ), ), ::hFile := NIL
   METHOD Seek( nOffset Origin ) INLINE hb_vfSeek( ::hFile, nOffset, Origin )

   METHOD Read( sBuffer, nOffset, nCount )
   METHOD ReadByte()

   DESTRUCTOR Finalize

ENDCLASS

METHOD New( cFile, nMode ) CLASS TStreamFileReader

   ::lCanRead := .T.

   ::cFile := cFile

   IF ( ::hFile := hb_vfOpen( cFile, hb_defaultValue( nMode, FO_READ ) ) ) == NIL
      Throw( xhb_ErrorNew( "Stream", 0, 1004, ProcName(), "Open Error: " + hb_ntos( FError() ), hb_AParams() ) )
   ENDIF

   ::nPosition := 0
   ::nLength := hb_vfSize( ::hFile )

   hb_vfSeek( ::hFile, 0, FS_SET )

   RETURN Self

METHOD PROCEDURE Finalize CLASS TStreamFileReader

   ::Close()

   RETURN

METHOD Read( sBuffer, nOffset, nCount ) CLASS TStreamFileReader

   LOCAL nRead
   LOCAL cBuffer

   IF HB_ISNUMERIC( nOffset ) .AND. nOffset >= 1
      cBuffer := Space( nCount )
      IF ( nRead := hb_vfRead( ::hFile, @cBuffer, nCount ) ) > 0
         sBuffer := Stuff( sBuffer, nOffSet + 1, nRead, hb_BLeft( cBuffer, nRead ) )  /* TOFIX: use hb_BStuff() */
      ENDIF
   ELSE
      nRead := hb_vfRead( ::hFile, @sBuffer, nCount )
   ENDIF

   IF nRead > 0
      ::nPosition += nRead
      RETURN nRead
   ENDIF

   RETURN 0

METHOD ReadByte() CLASS TStreamFileReader

   LOCAL sBuffer := Space( 1 )

   IF hb_vfRead( ::hFile, @sBuffer, 1 ) == 1
      ::nPosition++
      RETURN sBuffer
   ENDIF

   RETURN ""

CREATE CLASS TStreamFileWriter INHERIT TStream

   VAR cFile
   VAR hFile

   METHOD New( cFile, nMode, nAttr ) CONSTRUCTOR
   METHOD Handle() INLINE hb_vfHandle( ::hFile )

   METHOD Close() INLINE iif( ::hFile != NIL, hb_vfClose( ::hFile ), ), ::hFile := NIL
   METHOD Seek( nOffset Origin ) INLINE ::nPosition := hb_vfSeek( ::hFile, nOffset, Origin )

   METHOD Write( sBuffer, nOffset, nCount )
   METHOD WriteByte( cByte )

   DESTRUCTOR Finalize

ENDCLASS

METHOD New( cFile, nMode, nAttr ) CLASS TStreamFileWriter

   LOCAL lExisting := hb_vfExists( cFile )

   ::lCanWrite := .T.
   ::cFile := cFile

   IF ( ::hFile := hb_vfOpen( cFile, hb_bitOr( FO_CREAT, hb_defaultValue( nMode, FO_READWRITE ) ) ) ) == NIL
      Throw( xhb_ErrorNew( "Stream", 0, 1004, ProcName(), "Open/Create Error: " + hb_ntos( FError() ), hb_AParams() ) )
   ENDIF

   IF ! lExisting
      hb_vfAttrSet( cFile, nAttr )
   ENDIF

   ::nPosition := ::nLength := hb_vfSeek( ::hFile, 0, FS_END )

   RETURN Self

METHOD PROCEDURE Finalize CLASS TStreamFileWriter

   ::Close()

   RETURN

METHOD Write( sBuffer, nOffset, nCount ) CLASS TStreamFileWriter

   LOCAL nWritten

   IF HB_ISNUMERIC( nOffset ) .AND. nOffset >= 1
      IF ! HB_ISNUMERIC( nCount )
         nCount := hb_BLen( sBuffer ) - nOffset
      ENDIF
      sBuffer := hb_BSubStr( sBuffer, nOffSet + 1 )
   ENDIF

   IF ( nWritten := hb_vfWrite( ::hFile, sBuffer, nCount ) ) > 0
      ::nPosition += nWritten
   ENDIF

   IF nWritten != nCount
      Throw( xhb_ErrorNew( "Stream", 0, 1003, ProcName(), "Write failed - written: " + hb_ntos( nWritten ) + " bytes", hb_AParams() ) )
   ENDIF

   RETURN Max( nWritten, 0 )

METHOD PROCEDURE WriteByte( cByte ) CLASS TStreamFileWriter

   LOCAL nWritten

   IF ( nWritten := hb_vfWrite( ::hFile, cByte, 1 ) ) > 0
      ::nPosition += nWritten
   ENDIF

   IF nWritten != 1
      Throw( xhb_ErrorNew( "Stream", 0, 1006, ProcName(), "Write failed", hb_AParams() ) )
   ENDIF

   RETURN
