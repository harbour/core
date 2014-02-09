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

CREATE CLASS TCgiFile

   VAR Buffer    INIT ""
   VAR Name      INIT ""
   VAR Handle    INIT F_ERROR
   VAR FileSize  INIT 0
   VAR BytesRead INIT 0
   VAR cPage     INIT ""
   VAR nPage     INIT 0
   VAR nPageSize INIT 1024
   VAR nRecord   INIT 0

   METHOD New( cName )
   METHOD Open( nMode )
   METHOD Close() INLINE FClose( ::Handle ), ::Handle := F_ERROR
   METHOD Rename( c ) INLINE FRename( ::File, c ) != F_ERROR
   METHOD Erase() INLINE FErase( ::File ) != F_ERROR
   METHOD Exists() INLINE hb_FileExists( ::File )
   METHOD Error() INLINE FError() != 0
   METHOD Tell() INLINE FSeek( ::handle, 0, FS_RELATIVE )
   METHOD Pointer() INLINE FSeek( ::handle, 0, FS_RELATIVE )
   METHOD ReadStr( n ) INLINE ::Buffer := FReadStr( ::Handle, n )
   METHOD Write( c, n ) INLINE FWrite( ::Handle, c, n )
   METHOD WriteByte( nByte )
   METHOD WriteInt( nInt )
   METHOD WriteLong( nLong )
   METHOD GetBuffer() INLINE ::Buffer
   METHOD GoTop() INLINE FSeek( ::Handle, 0 )
   METHOD GoBottom() INLINE FSeek( ::Handle, 0, FS_END )
   METHOD Bof() INLINE FSeek( ::Handle, 0, FS_RELATIVE ) == 0
   METHOD Eof() INLINE FSeek( ::Handle, 0, FS_RELATIVE ) == ::FileSize
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

   ::Name := cName

   RETURN Self

/* ::Open( [<nMode>] ) --> lSuccess
*/
METHOD Open( nMode ) CLASS TCgiFile

   IF ( ::Handle := FOpen( ::Name, hb_defaultValue( nMode, FO_EXCLUSIVE ) ) ) != F_ERROR
      ::Size()
   ENDIF

   RETURN ::Handle != F_ERROR

/* ::Create( [<nAttrib>] ) --> lSuccess
*/
METHOD Create( nAttr ) CLASS TCgiFile

   ::Handle := FCreate( ::Name, hb_defaultValue( nAttr, FC_NORMAL ) )

   RETURN ::Handle != F_ERROR

/* ::Size() --> nFileSize
** RETURNs the size in bytes of the current file.
*/
METHOD Size() CLASS TCgiFile

   LOCAL nCurrent := FSeek( ::Handle, 0, FS_RELATIVE )
   LOCAL nLength  := FSeek( ::Handle, 0, FS_END )

   FSeek( ::Handle, nCurrent )
   ::FileSize := nLength

   RETURN nLength

/* ::Read( [<nSize>], [@<cBuff>] ) --> nBytesRead
*/
METHOD _Read( nSize, cBuff ) CLASS TCgiFile

   hb_default( @nSize, 1024 )
   hb_default( @cBuff, Space( nSize ) )

   ::BytesRead := FRead( ::Handle, @cBuff, nSize )
   ::Buffer    := cBuff

   RETURN cBuff    // nBytesRead

/* ::ReadAhead( [<nSize>], [@<cBuff>] ) --> nBytesRead
** Read forward in the file without moving the pointer.
*/
METHOD ReadAhead( nSize, cBuff ) CLASS TCgiFile

   LOCAL nCurrent

   hb_default( @nSize, 1024 )
   hb_default( @cBuff, Space( nSize ) )

   // --> save position in file
   nCurrent := FSeek( ::Handle, 0, FS_RELATIVE )

   // --> read ahead
   ::BytesRead := FRead( ::Handle, @cBuff, nSize )

   // --> RETURN to saved position
   FSeek( ::Handle, nCurrent )

   RETURN cBuff

/* ::ReadLine( [<nBytes>] ) --> cLine
*/
METHOD Readline( nSize ) CLASS TCgiFile

   LOCAL cString
   LOCAL nCurrent
   LOCAL nCr

   hb_default( @nSize, 1024 )

   IF nSize <= 0
      RETURN ""
   ENDIF

   nCurrent := FSeek( ::Handle, 0, FS_RELATIVE )
   cString  := FReadStr( ::Handle, nSize )
   nCr      := hb_BAt( Chr( 13 ), cString )

   FSeek( ::Handle, nCurrent, FS_SET )
   FSeek( ::Handle, nCr + 1, FS_RELATIVE )

   ::Buffer := hb_BLeft( cString, nCr - 1 )
   ::nRecord++

   RETURN ::Buffer

/* ::ReadByte() --> nByte or -1 if unsuccessfull
*/
METHOD ReadByte() CLASS TCgiFile

   LOCAL cBuff := Space( 1 )
   LOCAL nBytes := FRead( ::Handle, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, hb_BCode( cBuff ), -1 )

/* ::ReadInt() --> nUnsignedInt or -1 if unsuccessfull
*/
METHOD ReadInt() CLASS TCgiFile

   LOCAL cBuff  := Space( 2 )
   LOCAL nBytes := FRead( ::Handle, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, Bin2I( cBuff ), -1 )

/* ::ReadLong() --> nLong or -1 if unsuccessfull
*/
METHOD ReadLong() CLASS TCgiFile

   LOCAL cBuff  := Space( 4 )
   LOCAL nBytes := FRead( ::Handle, @cBuff, hb_BLen( cBuff ) )

   RETURN iif( nBytes > 0, Bin2L( cBuff ), -1 )

/* ::WriteByte( nByte ) --> lSuccess
*/
METHOD WriteByte( nByte ) CLASS TCgiFile
   RETURN FWrite( ::nHandle, hb_BCode( nByte ), 1 ) == 1

/* ::WriteInt( nInt ) --> lSuccess
*/
METHOD WriteInt( nInt ) CLASS TCgiFile
   RETURN FWrite( ::nHandle, I2Bin( nInt ), 2 ) == 2

/* ::WriteLong( nLong ) --> lSuccess
*/
METHOD WriteLong( nLong ) CLASS TCgiFile
   RETURN FWrite( ::nHandle, L2Bin( nLong ), 4 ) == 4

/* ::GOTO( <nLine> ) --> nPrevPos
** Skips to line <nLine> from top. RETURNs previous position in file.
*/
METHOD Goto( nLine ) CLASS TCgiFile

   LOCAL nCount := 1
   LOCAL nPos   := FSeek( ::Handle, 0, FS_RELATIVE )

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
** Skips to line <nLine> from top. RETURNs previous position in file.
*/
METHOD Skip( nLines ) CLASS TCgiFile

   LOCAL nCount := 0
   LOCAL nPos   := FSeek( ::Handle, 0, FS_RELATIVE )

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

/* ::MaxPages( <nPageSize> ) --> nMaxPages
*/
METHOD MaxPages( nPageSize ) CLASS TCgiFile

   hb_default( @nPageSize, ::nPageSize )

   RETURN ::Size() / nPageSize

/* ::PrevPage( [<nBytes>] ) --> cPage
*/
METHOD PrevPage( nBytes ) CLASS TCgiFile

   hb_default( @nBytes, 1024 )

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF ! ::Bof()
      FSeek( ::Handle, -nBytes, FS_RELATIVE )
      ::cPage := FReadStr( ::Handle, nBytes )
      FSeek( ::Handle, -nBytes, FS_RELATIVE )
      ::nPage--
   ENDIF

   RETURN ::cPage

/* ::NextPage( [<nBytes>] ) --> cPage
*/
METHOD NextPage( nBytes ) CLASS TCgiFile

   hb_default( @nBytes, 1024 )

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF ! ::Eof()
      ::cPage := FReadStr( ::Handle, nBytes )
      ::nPage++
   ENDIF

   RETURN ::cPage

/* ::PrevLine( [<nBytes>] ) --> ::Buffer
*/
METHOD PrevLine( nBytes ) CLASS TCgiFile

   LOCAL fHandle := ::Handle
   LOCAL nMaxRead
   LOCAL nNewPos
   LOCAL lMoved
   LOCAL cBuff
   LOCAL nWhereCrLf
   LOCAL nPrev
   LOCAL cTemp

   IF FSeek( fHandle, 0, FS_RELATIVE ) == 0

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

      nMaxRead := Min( hb_defaultValue( nBytes, 256 ), FSeek( fHandle, 0, FS_RELATIVE ) )

      cBuff   := Space( nMaxRead )
      nNewPos := FSeek( fHandle, -nMaxRead, FS_RELATIVE )
      FRead( fHandle, @cBuff, nMaxRead )
      IF ( nWhereCrLf := RAt( CRLF(), cBuff ) ) == 0  /* TOFIX: should be hb_BRAt() */
         nPrev    := nNewPos
         ::Buffer := cBuff
      ELSE
         nPrev    := nNewPos + nWhereCrLf + 1
         ::Buffer := SubStr( cBuff, nWhereCrLf + 2 )  /* TOFIX: should be hb_BSubStr() */
      ENDIF

      FSeek( fHandle, nPrev, FS_SET )

   ENDIF

   RETURN iif( lMoved, ::Buffer, "" )
