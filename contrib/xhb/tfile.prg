/*
 * Base fileIO class.
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (Porting this library to Harbour)
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
#include "cgi.ch"

CREATE CLASS TCgiFile

   VAR Buffer    INIT ""
   VAR Name      INIT ""
   VAR hFile
   VAR FileSize  INIT 0
   VAR BytesRead INIT 0
   VAR cPage     INIT ""
   VAR nPage     INIT 0
   VAR nPageSize INIT 1024
   VAR nRecord   INIT 0

   METHOD New( cName )
   METHOD Handle() INLINE hb_vfHandle( ::hFile )
   METHOD Open( nMode )
   METHOD Close() INLINE hb_vfClose( ::hFile ), ::hFile := NIL
   METHOD Rename( c ) INLINE hb_vfRename( ::File, c ) != F_ERROR
   METHOD Erase() INLINE hb_vfErase( ::File ) != F_ERROR
   METHOD Exists() INLINE hb_vfExists( ::File )
   METHOD Error() INLINE FError() != 0
   METHOD Tell() INLINE hb_vfSeek( ::hFile, 0, FS_RELATIVE )
   METHOD Pointer() INLINE hb_vfSeek( ::hFile, 0, FS_RELATIVE )
   METHOD ReadStr( n ) INLINE ::Buffer := hb_vfReadLen( ::hFile, n )
   METHOD Write( c, n ) INLINE Max( hb_vfWrite( ::hFile, c, n ), 0 )
   METHOD WriteByte( nByte )
   METHOD WriteInt( nInt )
   METHOD WriteLong( nLong )
   METHOD GetBuffer() INLINE ::Buffer
   METHOD GoTop() INLINE hb_vfSeek( ::hFile, 0 )
   METHOD GoBottom() INLINE hb_vfSeek( ::hFile, 0, FS_END )
   METHOD Bof() INLINE hb_vfSeek( ::hFile, 0, FS_RELATIVE ) == 0
   METHOD Eof() INLINE hb_vfSeek( ::hFile, 0, FS_RELATIVE ) == ::FileSize
   METHOD Seek( n, o ) INLINE hb_vfSeek( ::hFile, n, o )
   METHOD Create( nAttr )
   METHOD Size()
   METHOD _Read( nSize, cBuff )
   METHOD ReadAhead( nSize, cBuff )
   METHOD ReadLine( nSize )
   METHOD PrevLine( nBytes )
   METHOD ReadByte()
   METHOD ReadInt()
   METHOD ReadLong()
   METHOD Goto( nLine )
   METHOD Skip( nLines )
   METHOD MaxPages( nPageSize )
   METHOD PrevPage( nBytes )
   METHOD NextPage( nBytes )

ENDCLASS

METHOD New( cName ) CLASS TCgiFile

   ::Name := cName

   RETURN Self

METHOD Open( nMode ) CLASS TCgiFile

   IF ( ::hFile := hb_vfOpen( ::Name, hb_defaultValue( nMode, FO_EXCLUSIVE ) ) ) != NIL
      ::Size()
   ENDIF

   RETURN ::hFile != NIL

METHOD Create( nAttr ) CLASS TCgiFile

   IF ( ::hFile := hb_vfOpen( ::Name, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
      IF HB_ISNUMERIC( nAttr )
         hb_vfAttrSet( ::Name, nAttr )
      ENDIF
   ENDIF

   RETURN ::hFile != NIL

/* Returns the size in bytes of the current file. */
METHOD Size() CLASS TCgiFile

   LOCAL nCurrent := hb_vfSeek( ::hFile, 0, FS_RELATIVE )
   LOCAL nLength  := hb_vfSize( ::hFile, 0 )

   hb_vfSeek( ::hFile, nCurrent )

   RETURN ::FileSize := nLength

METHOD _Read( nSize, /* @ */ cBuff ) CLASS TCgiFile

   hb_default( @nSize, 1024 )
   hb_default( @cBuff, Space( nSize ) )

   ::BytesRead := Max( hb_vfRead( ::hFile, @cBuff, nSize ), 0 )
   ::Buffer    := cBuff

   RETURN cBuff    // nBytesRead

/* Read forward in the file without moving the pointer. */
METHOD ReadAhead( nSize, /* @ */ cBuff ) CLASS TCgiFile

   LOCAL nCurrent

   hb_default( @nSize, 1024 )
   hb_default( @cBuff, Space( nSize ) )

   // save position in file
   nCurrent := hb_vfSeek( ::hFile, 0, FS_RELATIVE )

   // read ahead
   ::BytesRead := Max( hb_vfRead( ::hFile, @cBuff, nSize ), 0 )

   // return to saved position
   hb_vfSeek( ::hFile, nCurrent )

   RETURN cBuff

METHOD Readline( nSize ) CLASS TCgiFile

   LOCAL cString
   LOCAL nCurrent
   LOCAL nCr

   hb_default( @nSize, 1024 )

   IF nSize <= 0
      RETURN ""
   ENDIF

   nCurrent := hb_vfSeek( ::hFile, 0, FS_RELATIVE )
   cString  := hb_vfReadLen( ::hFile, nSize )
   nCr      := hb_BAt( Chr( 13 ), cString )

   hb_vfSeek( ::hFile, nCurrent, FS_SET )
   hb_vfSeek( ::hFile, nCr + 1, FS_RELATIVE )

   ::Buffer := hb_BLeft( cString, nCr - 1 )
   ::nRecord++

   RETURN ::Buffer

/* ::ReadByte() --> nByte or -1 if unsuccessful */
METHOD ReadByte() CLASS TCgiFile

   LOCAL cBuff := Space( 1 )
   LOCAL nBytes := hb_vfRead( ::hFile, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, hb_BCode( cBuff ), -1 )

/* ::ReadInt() --> nUnsignedInt or -1 if unsuccessful */
METHOD ReadInt() CLASS TCgiFile

   LOCAL cBuff  := Space( 2 )
   LOCAL nBytes := hb_vfRead( ::hFile, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, Bin2I( cBuff ), -1 )

/* ::ReadLong() --> nLong or -1 if unsuccessful */
METHOD ReadLong() CLASS TCgiFile

   LOCAL cBuff  := Space( 4 )
   LOCAL nBytes := hb_vfRead( ::hFile, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, Bin2L( cBuff ), -1 )

/* ::WriteByte( nByte ) --> lSuccess */
METHOD WriteByte( nByte ) CLASS TCgiFile
   RETURN FWrite( ::nHandle, hb_BCode( nByte ), 1 ) == 1

/* ::WriteInt( nInt ) --> lSuccess */
METHOD WriteInt( nInt ) CLASS TCgiFile
   RETURN FWrite( ::nHandle, I2Bin( nInt ), 2 ) == 2

/* ::WriteLong( nLong ) --> lSuccess */
METHOD WriteLong( nLong ) CLASS TCgiFile
   RETURN FWrite( ::nHandle, L2Bin( nLong ), 4 ) == 4

/* ::GOTO( <nLine> ) --> nPrevPos
   Skips to line <nLine> from top. RETURNs previous position in file. */
METHOD Goto( nLine ) CLASS TCgiFile

   LOCAL nCount := 1
   LOCAL nPos   := hb_vfSeek( ::hFile, 0, FS_RELATIVE )

   ::GoTop()

   IF nLine < 0     // don't accept < 0
      RETURN nPos
   ELSEIF nLine == 0
      nLine     := 1
      ::nRecord := 1
      ::GoTop()
      RETURN nPos
   ENDIF

   DO WHILE ! ::Eof()

      ::ReadLine()

      IF nCount == nLine
         EXIT
      ENDIF

      nCount++
   ENDDO

   RETURN nPos

/* ::Skip( [<nLines>] ) --> nPrevPos
   Skips to line <nLine> from top. RETURNs previous position in file. */
METHOD Skip( nLines ) CLASS TCgiFile

   LOCAL nCount := 0
   LOCAL nPos   := hb_vfSeek( ::hFile, 0, FS_RELATIVE )

   hb_default( @nLines, 1 )

   IF nLines <= 0   // don't accept < 0
      RETURN nPos
   ENDIF

   DO WHILE ! ::Eof()

      IF nCount == nLines
         EXIT
      ENDIF

      ::ReadLine()
      nCount++
   ENDDO

   RETURN nPos

/* ::MaxPages( <nPageSize> ) --> nMaxPages */
METHOD MaxPages( nPageSize ) CLASS TCgiFile

   hb_default( @nPageSize, ::nPageSize )

   RETURN ::Size() / nPageSize

/* ::PrevPage( [<nBytes>] ) --> cPage */
METHOD PrevPage( nBytes ) CLASS TCgiFile

   hb_default( @nBytes, 1024 )

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF ! ::Bof()
      hb_vfSeek( ::hFile, -nBytes, FS_RELATIVE )
      ::cPage := hb_vfReadLen( ::hFile, nBytes )
      hb_vfSeek( ::hFile, -nBytes, FS_RELATIVE )
      ::nPage--
   ENDIF

   RETURN ::cPage

/* ::NextPage( [<nBytes>] ) --> cPage */
METHOD NextPage( nBytes ) CLASS TCgiFile

   hb_default( @nBytes, 1024 )

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF ! ::Eof()
      ::cPage := hb_vfReadLen( ::hFile, nBytes )
      ::nPage++
   ENDIF

   RETURN ::cPage

/* ::PrevLine( [<nBytes>] ) --> ::Buffer */
METHOD PrevLine( nBytes ) CLASS TCgiFile

   LOCAL hFile := ::hFile
   LOCAL nMaxRead
   LOCAL nNewPos
   LOCAL lMoved
   LOCAL cBuff
   LOCAL nWhereEOL
   LOCAL nPrev
   LOCAL cEOL

   IF hb_vfSeek( hFile, 0, FS_RELATIVE ) == 0
      lMoved := .F.
   ELSE
      lMoved := .T.

      cEOL := Chr( 13 ) + Chr( 10 )  /* TOFIX: EOL detection to be multi-platform */

      // Check preceeding chars for EOL
      hb_vfSeek( hFile, -hb_BLen( cEOL ), FS_RELATIVE )
      IF hb_vfReadLen( hFile, hb_BLen( cEOL ) ) == cEOL
         hb_vfSeek( hFile, -hb_BLen( cEOL ), FS_RELATIVE )
      ENDIF

      nMaxRead := Min( hb_defaultValue( nBytes, 256 ), hb_vfSeek( hFile, 0, FS_RELATIVE ) )

      cBuff   := Space( nMaxRead )
      nNewPos := hb_vfSeek( hFile, -nMaxRead, FS_RELATIVE )
      hb_vfRead( hFile, @cBuff, nMaxRead )
      IF ( nWhereEOL := hb_BRAt( cEOL, cBuff ) ) == 0
         nPrev    := nNewPos
         ::Buffer := cBuff
      ELSE
         nPrev    := nNewPos + nWhereEOL + 1
         ::Buffer := hb_BSubStr( cBuff, nWhereEOL + hb_BLen( cEOL ) )
      ENDIF

      hb_vfSeek( hFile, nPrev, FS_SET )
   ENDIF

   RETURN iif( lMoved, ::Buffer, "" )
