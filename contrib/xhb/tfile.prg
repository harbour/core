/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base fileIO class.
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "fileio.ch"
#include "cgi.ch"

#translate FPOS( <f> ) => FSeek( <f>, 0, FS_RELATIVE )

CREATE CLASS TCgiFile

   VAR Buffer INIT ""
   VAR Name INIT ""
   VAR Handle
   VAR FileSize INIT 0
   VAR BytesRead INIT 0
   VAR cPage INIT ""
   VAR nPage INIT 0
   VAR nPageSize INIT 1024
   VAR nRecord INIT 0

   METHOD New( cName )

   METHOD Open( nMode )

   METHOD Close() INLINE FClose( ::Handle ), ;
      ::Handle := -999

   METHOD Rename( c ) INLINE FRename( ::File, c ) == 0

   METHOD Erase() INLINE FErase( ::File ) == 0

   METHOD Exists() INLINE hb_FileExists( ::File )

   METHOD Error() INLINE FError() != 0

   METHOD Tell() INLINE FSeek( ::handle, 0, FS_RELATIVE )

   METHOD Pointer() INLINE FPOS( ::handle )

   METHOD ReadStr( n ) INLINE ::Buffer := ;
      FReadStr( ::Handle, n )
   METHOD Write( c, n ) INLINE FWrite( ::Handle, c, n )

   METHOD WriteByte( nByte )

   METHOD WriteInt( nInt )

   METHOD WriteLong( nLong )

   METHOD GetBuffer() INLINE ::Buffer

   METHOD GoTop() INLINE FSeek( ::Handle, 0 )

   METHOD GoBottom() INLINE FSeek( ::Handle, 0, FS_END )

   METHOD Bof() INLINE( FPOS( ::Handle ) == 0 )

   METHOD Eof() INLINE FPOS( ::Handle ) == ::FileSize

   METHOD Seek( n, o ) INLINE FSeek( ::Handle, n, o )

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

   ::Name      := cName
   ::Buffer    := ""
   ::Handle    := 0
   ::FileSize  := 0
   ::BytesRead := 0
   ::cPage     := ""
   ::nPage     := 0
   ::nPageSize := 1024
   ::nRecord   := 0

   RETURN Self

/*
**   ::Open( [<nMode>] ) --> lSuccess
*/

METHOD Open( nMode ) CLASS TCgiFile

   __defaultNIL( @nMode, FO_EXCLUSIVE )
   ::Handle := FOpen( ::Name, nMode )
   IF ::Handle > 0
      ::Size()
   ENDIF

   RETURN ::Handle > 0

/*
**   ::Create( [<nAttrib>] ) --> lSuccess
*/

METHOD Create( nAttr ) CLASS TCgiFile

   LOCAL nSuccess

   __defaultNIL( @nAttr, 0 )
   nSuccess := FCreate( ::Name, nAttr )
   ::Handle := nSuccess

   RETURN nSuccess != F_ERROR

/*
**   ::Size() --> nFileSize
**
**   RETURNs the size in bytes of the current file.
*/

METHOD Size() CLASS TCgiFile

   LOCAL nCurrent
   LOCAL nLength

   nCurrent := FPOS( ::Handle )
   nLength  := FSeek( ::Handle, 0, FS_END )

   FSeek( ::Handle, nCurrent )
   ::FileSize := nLength

   RETURN nLength

/*
**   ::Read( [<nSize>], [@<cBuff>] ) --> nBytesRead
*/

METHOD _Read( nSize, cBuff ) CLASS TCgiFile

   __defaultNIL( @nSize, 1024 )
   __defaultNIL( @cBuff, Space( nSize ) )

   ::BytesRead := FRead( ::Handle, @cBuff, nSize )
   ::Buffer    := cBuff

   RETURN cBuff    // nBytesRead )

/*
**   ::ReadAhead( [<nSize>], [@<cBuff>] ) --> nBytesRead
**
**    Read forward in the file without moving the pointer.
*/

METHOD ReadAhead( nSize, cBuff ) CLASS TCgiFile

   LOCAL nCurrent

   __defaultNIL( @nSize, 1024 )
   __defaultNIL( @cBuff, Space( nSize ) )

   // --> save position in file
   nCurrent := FPOS( ::Handle )

   // --> read ahead
   ::BytesRead := FRead( ::Handle, @cBuff, nSize )

   // --> RETURN to saved position
   FSeek( ::Handle, nCurrent )

   RETURN cBuff

/*
**   ::ReadLine( [<nBytes>] ) --> cLine
*/

METHOD Readline( nSize ) CLASS TCgiFile

   LOCAL cString
   LOCAL nCurrent
   LOCAL nCr

   __defaultNIL( @nSize, 1024 )

   IF nSize <= 0
      RETURN ""
   ENDIF

   nCurrent := FSeek( ::Handle, 0, FS_RELATIVE )
   cString  := FReadStr( ::Handle, nSize )
   nCr      := At( Chr( 13 ), cString )

   FSeek( ::Handle, nCurrent, FS_SET )
   FSeek( ::Handle, nCr + 1, FS_RELATIVE )

   ::Buffer := SubStr( cString, 1, nCr - 1 )
   ::nRecord++

   RETURN ::Buffer

/*
**   ::ReadByte() --> nByte or -1 if unsuccessfull
*/

METHOD ReadByte() CLASS TCgiFile

   LOCAL nBytes
   LOCAL cBuff  := Space( 1 )

   nBytes := FRead( ::Handle, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, Asc( cBuff ), -1 )

/*
**   ::ReadInt() --> nUnsignedInt or -1 if unsuccessfull
*/

METHOD ReadInt() CLASS TCgiFile

   LOCAL nBytes
   LOCAL cBuff  := Space( 2 )

   nBytes := FRead( ::Handle, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, Bin2I( cBuff ), -1 )

/*
**   ::ReadLong() --> nLong or -1 if unsuccessfull
*/

METHOD ReadLong() CLASS TCgiFile

   LOCAL nBytes
   LOCAL cBuff  := Space( 4 )

   nBytes := FRead( ::Handle, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, Bin2L( cBuff ), -1 )

/*
**   ::WriteByte( nByte ) --> lSuccess
*/

METHOD WriteByte( nByte ) CLASS TCgiFile

   LOCAL lSuccess := ( FWrite( ::nHandle, hb_BCode( nByte ), 1 ) == 1 )

   RETURN lSuccess

/*
**   ::WriteInt( nInt ) --> lSuccess
*/

METHOD WriteInt( nInt ) CLASS TCgiFile

   LOCAL lSuccess := ( FWrite( ::nHandle, I2Bin( nInt ), 2 ) == 2 )

   RETURN lSuccess

/*
**   ::WriteLong( nLong ) --> lSuccess
*/

METHOD WriteLong( nLong ) CLASS TCgiFile

   LOCAL lSuccess := ( FWrite( ::nHandle, L2Bin( nLong ), 4 ) == 4 )

   RETURN lSuccess

/*
**   ::GOTO( <nLine> ) --> nPrevPos
**
**   Skips to line <nLine> from top. RETURNs previous position in file.
**
*/

METHOD Goto( nLine ) CLASS TCgiFile

   LOCAL nCount := 1
   LOCAL nPos   := FPOS( ::Handle )

   ::GoTop()

   IF nLine < 0     // don't accept < 0
      RETURN nPos
   ELSEIF nLine == 0
      nLine     := 1
      ::nRecord := 1
      ::GoTop()
      RETURN nPos
   ENDIF

   WHILE ! ::Eof()

      ::ReadLine()

      IF nCount == nLine
         EXIT
      ENDIF

      nCount++
   ENDDO

   RETURN nPos

/*
**   ::Skip( [<nLines>] ) --> nPrevPos
**
**   Skips to line <nLine> from top. RETURNs previous position in file.
**
*/

METHOD Skip( nLines ) CLASS TCgiFile

   LOCAL nCount := 0
   LOCAL nPos   := FPOS( ::Handle )

   __defaultNIL( @nLines, 1 )

   IF nLines <= 0   // don't accept < 0

      RETURN nPos

   ENDIF

   WHILE ! ::Eof()

      IF nCount == nLines
         EXIT
      ENDIF

      ::ReadLine()
      nCount++
   ENDDO

   RETURN nPos

/*
**   ::MaxPages( <nPageSize> ) --> nMaxPages
*/

METHOD MaxPages( nPageSize ) CLASS TCgiFile

   __defaultNIL( @nPageSize, ::nPageSize )

   RETURN ::Size() / nPageSize

/*
**   ::PrevPage( [<nBytes>] ) --> cPage
*/

METHOD PrevPage( nBytes ) CLASS TCgiFile

   __defaultNIL( @nBytes, 1024 )

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF ! ::Bof()
      FSeek( ::Handle, - nBytes, FS_RELATIVE )
      ::cPage := FReadStr( ::Handle, nBytes )
      FSeek( ::Handle, - nBytes, FS_RELATIVE )
      ::nPage --
   ENDIF

   RETURN ::cPage

/*
**   ::NextPage( [<nBytes>] ) --> cPage
*/

METHOD NextPage( nBytes ) CLASS TCgiFile

   __defaultNIL( @nBytes, 1024 )

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF ! ::Eof()
      ::cPage := FReadStr( ::Handle, nBytes )
      ::nPage++
   ENDIF

   RETURN ::cPage

/*
**   ::PrevLine( [<nBytes>] ) --> ::Buffer
*/

METHOD PrevLine( nBytes ) CLASS TCgiFile

   LOCAL fHandle    := ::Handle
   LOCAL nOrigPos   := FPOS( fHandle )
   LOCAL nMaxRead
   LOCAL nNewPos
   LOCAL lMoved
   LOCAL cBuff
   LOCAL nWhereCrLf
   LOCAL nPrev
   LOCAL cTemp

   __defaultNIL( @nBytes, 256 )

   IF nOrigPos == 0

      lMoved := .F.

   ELSE

      lMoved := .T.

      //  Check preceeding 2 chars for CR+LF
      FSeek( fHandle, -2, FS_RELATIVE )
      cTemp := Space( 2 )
      FRead( fHandle, @cTemp, hb_BLen( cTemp ) )

      IF cTemp == CRLF()
         FSeek( fHandle, -2, FS_RELATIVE )
      ENDIF

      nMaxRead := Min( nBytes, FPOS( fHandle ) )

      cBuff   := Space( nMaxRead )
      nNewPos := FSeek( fHandle, -nMaxRead, FS_RELATIVE )
      FRead( fHandle, @cBuff, nMaxRead )
      nWhereCrLf := RAt( CRLF(), cBuff )
      IF nWhereCrLf == 0

         nPrev    := nNewPos
         ::Buffer := cBuff

      ELSE

         nPrev    := nNewPos + nWhereCrLf + 1
         ::Buffer := SubStr( cBuff, nWhereCrLf + 2 )

      ENDIF

      FSeek( fHandle, nPrev, FS_SET )

   ENDIF

   RETURN iif( lMoved, ::Buffer, "" )
