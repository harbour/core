/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base fileIO class.
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
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
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include "hbclass.ch"

#include "default.ch"
#include "fileio.ch"
#include "ferror.ch"
//#include "simpleio.ch"

#translate FPOS(<f>) => FSEEK( <f>, 0, FS_RELATIVE )

#ifdef TEST
PROC TEST()
LOCAL oF
oF := FileBase():New( "c:\autoexec.bat" )  //oFile.prg" )
oF:Open()
? of:handle
while !oF:EOF()
? oF:ReadLine()
inkey(0)
ENDDO
#endif


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
 CLASS FileBase // ALIAS FB
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
DATA Buffer          INIT ""      // Self[1]
DATA Name            INIT ""
DATA Handle          //INIT -999
DATA FileSize        INIT 0
DATA BytesRead       INIT 0
DATA cPage           INIT ""
DATA nPage           INIT 0
DATA nPageSize       INIT 1024
DATA nRecord         INIT 0

METHOD New( cName )
METHOD Open( nMode )
METHOD Close()       INLINE FClose( ::Handle ), ;
                            ::Handle := -999               

METHOD Rename(c)     INLINE FRename( ::File, c ) == 0      
METHOD Erase()       INLINE Ferase( ::File ) == 0          
METHOD Exists()      INLINE File( ::File )                 
METHOD Error()       INLINE Ferror() != 0                  

METHOD Tell()        INLINE Fseek(::handle,FS_RELATIVE,0)  
METHOD Pointer()     INLINE FPOS( ::handle )               
METHOD ReadStr(n)    INLINE ::Buffer := ;
                            FReadStr( ::Handle, n )        
METHOD Write( c, n ) INLINE FWrite( ::Handle, c, n )       
METHOD WriteByte( n )
METHOD WriteInt( n )
METHOD WriteLong( n )
METHOD GetBuffer()   INLINE ::Buffer                       

METHOD GoTop()       INLINE FSEEK( ::Handle, 0 )           
METHOD GoBottom()    INLINE FSEEK( ::Handle, 0, FS_END)    
METHOD Bof()         INLINE ( FPOS(::Handle) == 0)         
METHOD Eof()         INLINE FPOS(::Handle) == ::FileSize   
METHOD Seek( n, o )  INLINE FSeek( ::Handle, n, o)         
METHOD Create( n )
METHOD Size()

METHOD _Read( n, c )
METHOD ReadAhead( n, c )
METHOD ReadLine( n )
METHOD PrevLine( nBytes )
METHOD ReadByte()
METHOD ReadInt()
METHOD ReadLong()

METHOD GoTo( n )
METHOD Skip( n )

METHOD MaxPages(n)
METHOD PrevPage(n)
METHOD NextPage(n)

ENDCLASS



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD New( cName ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
::Name      := cName
::Buffer    := ""      // Self[1]
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

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD Open( nMode ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
DEFAULT nMode := FO_EXCLUSIVE //SHARED
::Handle := FOpen( ::Name, nMode )
IF ::Handle > 0
   ::Size()
ENDIF
RETURN ::Handle > 0


/*
**   ::Create( [<nAttrib>] ) --> lSuccess
*/
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD Create( nAttr ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nSuccess
DEFAULT nAttr := 0
nSuccess := FCreate( ::Name, nAttr )
::Handle := nSuccess
RETURN (nSuccess != -1)



/*
**   ::Size() --> nFileSize
**
**   RETURNs the size in bytes of the current file.
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD Size() CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nCurrent,nLength

nCurrent := FPOS( ::Handle )
nLength  := FSEEK( ::Handle, 0, FS_END )

FSEEK( ::Handle, nCurrent )
::FileSize := nLength

RETURN( nLength )



/*
**   ::Read( [<nSize>], [@<cBuff>] ) --> nBytesRead
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD _Read( nSize, cBuff ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nBytesRead

DEFAULT nSize := 1024
DEFAULT cBuff := SPACE(nSize)

::BytesRead   := FRead( ::Handle, @cBuff, nSize )
::Buffer      := cBuff

RETURN( cBuff ) //nBytesRead )



/*
**   ::ReadAhead( [<nSize>], [@<cBuff>] ) --> nBytesRead
**
**    Read forward in the file without moving the pointer.
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD ReadAhead( nSize, cBuff ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nBytesRead, nCurrent

DEFAULT nSize := 1024
DEFAULT cBuff := SPACE(nSize)

// --> save position in file
nCurrent      := FPOS( ::Handle )

// --> read ahead
::BytesRead   := FRead( ::Handle, @cBuff, nSize )

// --> RETURN to saved position
FSEEK( ::Handle, nCurrent )

RETURN( cBuff )



/*
**   ::ReadLine( [<nBytes>] ) --> cLine
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD Readline( nSize ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL cString, nCurrent, nCr

DEFAULT nSize := 1024

IF nSize <= 0
   RETURN( "" )
ENDIF

nCurrent :=  FSEEK( ::Handle, 0, 1 )
cString  :=  FREADSTR( ::Handle, nSize )
nCr      :=  AT( CHR(13), cString )

FSEEK( ::Handle,nCurrent,0 )
FSEEK( ::Handle,nCr+1,1 )

::Buffer := SUBSTR( cString, 1, nCr-1 )
::nRecord++

RETURN ::Buffer



/*
**   ::ReadByte() --> nByte  or  -1 if unsuccessfull
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD ReadByte() CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nRet, nBytes
LOCAL cBuff := SPACE( 1 )

nBytes := FRead( ::Handle, @cBuff, 1 )

RETURN( IF( nBytes > 0, ASC(cBuff), -1 ) )



/*
**   ::ReadInt() --> nUnsignedInt or -1 if unsuccessfull
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD ReadInt() CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nRet, nBytes
LOCAL cBuff := SPACE( 2 )

nBytes := FRead( ::Handle, @cBuff, 2 )

RETURN( IF( nBytes > 0, BIN2I(cBuff), -1 )  )



/*
**   ::ReadLong() --> nLong  or -1 if unsuccessfull
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD ReadLong() CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nRet, nBytes
LOCAL cBuff := SPACE( 4 )

nBytes := FRead( ::Handle, @cBuff, 4 )

RETURN( IF( nBytes > 0, BIN2L(cBuff), -1 )  )




/*
**   ::WriteByte( nByte ) --> lSuccess
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD WriteByte( nByte ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL lSuccess := ( FWrite( ::nHandle, chr( nByte ), 1 ) == 1 )
RETURN lSuccess




/*
**   ::WriteInt( nInt ) --> lSuccess
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD WriteInt( nInt ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL lSuccess := ( FWrite( ::nHandle, I2BIN( nInt ), 2 ) == 2 )
RETURN lSuccess



/*
**   ::WriteLong( nLong ) --> lSuccess
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD WriteLong( nLong ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL lSuccess := ( FWrite( ::nHandle, L2BIN( nLong ), 4 ) == 4 )
RETURN( lSuccess )


              
/*
**   ::GOTO( <nLine> ) --> nPrevPos
**
**   Skips to line <nLine> from top. RETURNs previous position in file.
**
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD GoTo( nLine ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nCount := 1
LOCAL nPos   := FPOS( ::Handle )

::GoTop()

IF nLine  < 0        // don't accept < 0
   RETURN( nPos )
ELSEIF nLine == 0
   nLine     := 1
   ::nRecord := 1
   ::GoTop()
   RETURN( nPos )
ENDIF

WHILE !::EOF()

     ::ReadLine()

     IF nCount == nLine ;  EXIT  ;  ENDIF

     nCount++
ENDDO

RETURN( nPos )


/*
**   ::Skip( [<nLines>] ) --> nPrevPos
**
**   Skips to line <nLine> from top. RETURNs previous position in file.
**
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD Skip( nLines ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL nCount := 0
LOCAL nPos   := FPOS( ::Handle )

DEFAULT nLines := 1

IF nLines  <= 0        // don't accept < 0

   RETURN nPos

ENDIF

WHILE !::EOF()

     IF nCount == nLines  ;  EXIT  ;  ENDIF

     ::ReadLine()
     nCount++
ENDDO
RETURN( nPos )


/*
**   ::MaxPages( <nPageSize> ) --> nMaxPages
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD MaxPages( nPageSize ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
DEFAULT nPageSize := ::nPageSize
RETURN( ::Size() / nPageSize )



/*
**   ::PrevPage( [<nBytes>] ) --> cPage
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD PrevPage( nBytes ) CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

DEFAULT nBytes := 1024

IF nBytes <= 0
   RETURN( "" )
ENDIF

IF !::BOF()
     FSEEK( ::Handle, -nBytes, FS_RELATIVE)
     ::cPage := FREADSTR( ::Handle, nBytes )
     FSEEK( ::Handle, -nBytes, FS_RELATIVE)
     ::nPage--
ENDIF

RETURN( ::cPage )



/*
**   ::NextPage( [<nBytes>] ) --> cPage
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD NextPage( nBytes )    CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

DEFAULT nBytes := 1024

IF nBytes <= 0
   RETURN( "" )
ENDIF

IF !::EOF()
     ::cPage := FREADSTR( ::Handle, nBytes )
     ::nPage++
ENDIF

RETURN( ::cPage )


/*
**   ::PrevLine( [<nBytes>] ) --> ::Buffer
*/
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD PrevLine( npBytes )   CLASS FileBase 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL fHandle := ::Handle
LOCAL nOrigPos := FPOS(fHandle), nMaxRead, nNewPos, ;
          lMoved, cBuff, nWhereCrLf, nPrev, cTemp

DEFAULT npBytes := 256

  IF nOrigPos == 0

    lMoved := .F.

  ELSE

    lMoved := .T.

    //  Check preceeding 2 chars for CR+LF
    FSeek(fHandle, -2, FS_RELATIVE)
    cTemp := Space(2)
    FRead(fHandle, @cTemp, 2)

    IF cTemp == CRLF()
      FSeek(fHandle, -2, FS_RELATIVE)
    ENDIF

    nMaxRead := MIN( npBytes, FPOS(fHandle))

    cBuff := Space(nMaxRead)
    nNewPos := FSeek(fHandle, -nMaxRead, FS_RELATIVE)
    FRead(fHandle, @cBuff, nMaxRead)
    nWhereCrLf := RAT( CRLF(), cBuff )
    IF nWhereCrLf == 0

      nPrev := nNewPos
      ::Buffer := cBuff

    ELSE

      nPrev    := nNewPos + nWhereCrLf + 1
      ::Buffer := SubStr(cBuff, nWhereCrLf + 2)

    ENDIF

    FSeek(fHandle, nPrev, FS_SET)

  ENDIF

RETURN IF( lMoved, ::Buffer, "" )


/*
                * * * * * UNCHECKED !!! * * * * *

METHOD PrevLine( nBytes )    CLASS FileBase 
LOCAL cRet, cPage := ""
LOCAL nAt   := 0
LOCAL nPos  := FPOS( ::Handle )
DEFAULT nBytes := 1024

IF nBytes <= 0
   RETURN( "" )
ENDIF

IF !( FPOS(::Handle) == 1)  // !BOF()
     FSEEK( ::Handle, -nBytes, FS_RELATIVE)       // position back
     cPage := FREADSTR( ::Handle, nBytes )        // read forward
     nAt := RAT( Chr(13), cPage )                 // find crlf()
     cRet := RIGHT( cPage, nAt+1)
     //FSEEK( ::Handle, -(nAt-1), FS_RELATIVE)
     FSEEK( ::Handle, nPos )
     FSEEK( ::Handle, -(nAT-1), 1 )
ENDIF

RETURN( cRet )
*/

