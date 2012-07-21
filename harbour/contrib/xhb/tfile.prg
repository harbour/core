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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "common.ch"
#include "fileio.ch"
#include "cgi.ch"

#translate FPOS(<f>) => FSEEK( <f>, 0, FS_RELATIVE )

CLASS TCgiFile
   DATA Buffer INIT ""
   DATA Name INIT ""
   DATA Handle
   DATA FileSize INIT 0
   DATA BytesRead INIT 0
   DATA cPage INIT ""
   DATA nPage INIT 0
   DATA nPageSize INIT 1024
   DATA nRecord INIT 0

   METHOD New( cName )

   METHOD Open( nMode )

   METHOD CLOSE() INLINE Fclose( ::Handle ),;
          ::Handle := - 999

   METHOD RENAME( c ) INLINE Frename( ::File, c ) == 0

   METHOD ERASE() INLINE Ferase( ::File ) == 0

   METHOD Exists() INLINE File( ::File )

   METHOD Error() INLINE Ferror() != 0

   METHOD Tell() INLINE Fseek( ::handle, FS_RELATIVE, 0 )

   METHOD Pointer() INLINE FPOS( ::handle )

   METHOD ReadStr( n ) INLINE ::Buffer := ;
   Freadstr( ::Handle, n )
   METHOD Write( c, n ) INLINE Fwrite( ::Handle, c, n )

   METHOD WriteByte( nByte )

   METHOD WriteInt( nInt )

   METHOD WriteLong( nLong )

   METHOD GetBuffer() INLINE ::Buffer

   METHOD GoTop() INLINE Fseek( ::Handle, 0 )

   METHOD GoBottom() INLINE Fseek( ::Handle, 0, FS_END )

   METHOD Bof() INLINE( FPOS( ::Handle ) == 0 )

   METHOD Eof() INLINE FPOS( ::Handle ) == ::FileSize

   METHOD SEEK( n, o ) INLINE Fseek( ::Handle, n, o )

   METHOD CREATE( nAttr )

   METHOD Size()

   METHOD _Read( nSize, cBuff )

   METHOD ReadAhead( nSize, cBuff )

   METHOD ReadLine( nSize )

   METHOD PrevLine( nBytes )

   METHOD ReadByte()

   METHOD ReadInt()

   METHOD ReadLong()

   METHOD GOTO( nLine )

   METHOD SKIP( nLines )

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


   DEFAULT nMode TO FO_EXCLUSIVE
   ::Handle := Fopen( ::Name, nMode )
   IF ::Handle > 0
      ::Size()
   ENDIF
RETURN ::Handle > 0

/*
**   ::Create( [<nAttrib>] ) --> lSuccess
*/

METHOD CREATE ( nAttr ) CLASS TCgiFile


   LOCAL nSuccess
   DEFAULT nAttr TO 0
   nSuccess := Fcreate( ::Name, nAttr )
   ::Handle := nSuccess

RETURN nSuccess != - 1

/*
**   ::Size() --> nFileSize
**
**   RETURNs the size in bytes of the current file.
*/


METHOD Size() CLASS TCgiFile


   LOCAL nCurrent
   LOCAL nLength

   nCurrent := FPOS( ::Handle )
   nLength  := Fseek( ::Handle, 0, FS_END )

   Fseek( ::Handle, nCurrent )
   ::FileSize := nLength

RETURN nLength

/*
**   ::Read( [<nSize>], [@<cBuff>] ) --> nBytesRead
*/


METHOD _Read( nSize, cBuff ) CLASS TCgiFile

   DEFAULT nSize TO 1024
   DEFAULT cBuff TO Space( nSize )

   ::BytesRead := Fread( ::Handle, @cBuff, nSize )
   ::Buffer    := cBuff

RETURN cBuff    //nBytesRead )

/*
**   ::ReadAhead( [<nSize>], [@<cBuff>] ) --> nBytesRead
**
**    Read forward in the file without moving the pointer.
*/


METHOD ReadAhead( nSize, cBuff ) CLASS TCgiFile

   LOCAL nCurrent

   DEFAULT nSize TO 1024
   DEFAULT cBuff TO Space( nSize )

   // --> save position in file
   nCurrent := FPOS( ::Handle )

   // --> read ahead
   ::BytesRead := Fread( ::Handle, @cBuff, nSize )

   // --> RETURN to saved position
   Fseek( ::Handle, nCurrent )

RETURN cBuff

/*
**   ::ReadLine( [<nBytes>] ) --> cLine
*/


METHOD Readline( nSize ) CLASS TCgiFile


   LOCAL cString
   LOCAL nCurrent
   LOCAL nCr

   DEFAULT nSize TO 1024

   IF nSize <= 0
      RETURN ""
   ENDIF

   nCurrent := Fseek( ::Handle, 0, 1 )
   cString  := Freadstr( ::Handle, nSize )
   nCr      := At( Chr( 13 ), cString )

   Fseek( ::Handle, nCurrent, 0 )
   Fseek( ::Handle, nCr + 1, 1 )

   ::Buffer := Substr( cString, 1, nCr - 1 )
   ::nRecord ++

RETURN ::Buffer

/*
**   ::ReadByte() --> nByte  or  -1 if unsuccessfull
*/


METHOD ReadByte() CLASS TCgiFile

   LOCAL nBytes
   LOCAL cBuff  := Space( 1 )

   nBytes := Fread( ::Handle, @cBuff, hb_BLen( cBuff ) )

RETURN iif( nBytes > 0, Asc( cBuff ), - 1 )

/*
**   ::ReadInt() --> nUnsignedInt or -1 if unsuccessfull
*/


METHOD ReadInt() CLASS TCgiFile

   LOCAL nBytes
   LOCAL cBuff  := Space( 2 )

   nBytes := Fread( ::Handle, @cBuff, hb_BLen( cBuff ) )

RETURN iif( nBytes > 0, Bin2i( cBuff ), - 1 )

/*
**   ::ReadLong() --> nLong  or -1 if unsuccessfull
*/


METHOD ReadLong() CLASS TCgiFile

   LOCAL nBytes
   LOCAL cBuff  := Space( 4 )

   nBytes := Fread( ::Handle, @cBuff, hb_BLen( cBuff ) )

RETURN iif( nBytes > 0, Bin2l( cBuff ), - 1 )

/*
**   ::WriteByte( nByte ) --> lSuccess
*/


METHOD WriteByte( nByte ) CLASS TCgiFile


   LOCAL lSuccess := ( Fwrite( ::nHandle, hb_BCode( nByte ), 1 ) == 1 )
RETURN lSuccess

/*
**   ::WriteInt( nInt ) --> lSuccess
*/


METHOD WriteInt( nInt ) CLASS TCgiFile


   LOCAL lSuccess := ( Fwrite( ::nHandle, I2bin( nInt ), 2 ) == 2 )
RETURN lSuccess

/*
**   ::WriteLong( nLong ) --> lSuccess
*/


METHOD WriteLong( nLong ) CLASS TCgiFile


   LOCAL lSuccess := ( Fwrite( ::nHandle, L2bin( nLong ), 4 ) == 4 )
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

   WHILE !::EOF()

      ::ReadLine()

      IF nCount == nLine
         EXIT
      ENDIF

      nCount ++
   ENDDO

RETURN nPos

/*
**   ::Skip( [<nLines>] ) --> nPrevPos
**
**   Skips to line <nLine> from top. RETURNs previous position in file.
**
*/


METHOD SKIP( nLines ) CLASS TCgiFile


   LOCAL nCount := 0
   LOCAL nPos   := FPOS( ::Handle )

   DEFAULT nLines TO 1

   IF nLines <= 0   // don't accept < 0

      RETURN nPos

   ENDIF

   WHILE !::EOF()

      IF nCount == nLines
         EXIT
      ENDIF

      ::ReadLine()
      nCount ++
   ENDDO
RETURN nPos

/*
**   ::MaxPages( <nPageSize> ) --> nMaxPages
*/


METHOD MaxPages( nPageSize ) CLASS TCgiFile


   DEFAULT nPageSize TO ::nPageSize
RETURN ::Size() / nPageSize

/*
**   ::PrevPage( [<nBytes>] ) --> cPage
*/


METHOD PrevPage( nBytes ) CLASS TCgiFile



   DEFAULT nBytes TO 1024

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF !::BOF()
      Fseek( ::Handle, - nBytes, FS_RELATIVE )
      ::cPage := Freadstr( ::Handle, nBytes )
      Fseek( ::Handle, - nBytes, FS_RELATIVE )
      ::nPage --
   ENDIF

RETURN ::cPage

/*
**   ::NextPage( [<nBytes>] ) --> cPage
*/


METHOD NextPage( nBytes ) CLASS TCgiFile



   DEFAULT nBytes TO 1024

   IF nBytes <= 0
      RETURN ""
   ENDIF

   IF !::EOF()
      ::cPage := Freadstr( ::Handle, nBytes )
      ::nPage ++
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

   DEFAULT nBytes TO 256

   IF nOrigPos == 0

      lMoved := .F.

   ELSE

      lMoved := .T.

      //  Check preceeding 2 chars for CR+LF
      Fseek( fHandle, - 2, FS_RELATIVE )
      cTemp := Space( 2 )
      Fread( fHandle, @cTemp, hb_BLen( cTemp ) )

      IF cTemp == CRLF()
         Fseek( fHandle, - 2, FS_RELATIVE )
      ENDIF

      nMaxRead := Min( nBytes, FPOS( fHandle ) )

      cBuff   := Space( nMaxRead )
      nNewPos := Fseek( fHandle, - nMaxRead, FS_RELATIVE )
      Fread( fHandle, @cBuff, nMaxRead )
      nWhereCrLf := Rat( CRLF(), cBuff )
      IF nWhereCrLf == 0

         nPrev    := nNewPos
         ::Buffer := cBuff

      ELSE

         nPrev    := nNewPos + nWhereCrLf + 1
         ::Buffer := Substr( cBuff, nWhereCrLf + 2 )

      ENDIF

      Fseek( fHandle, nPrev, FS_SET )

   ENDIF

RETURN iif( lMoved, ::Buffer, "" )
