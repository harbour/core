/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File support Functions For hbdoc
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
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

#pragma -w2
#pragma linenumber=on

#include "hbclass.ch"

#include "common.ch"
#include "fileio.ch"

#define pBUFFER_LENGTH 4096

STATIC s_oFileBase

*+--------------------------------------------------------------------
*+
*+    Class FileBase
*+
*+--------------------------------------------------------------------
*+
CLASS FileBase FROM FileMan

   DATA nOpenMode   // Holds the value to use when opening the file
   DATA nCreateMode // Holds the value to use when creating the file
   DATA nDosHandle  // Holds the DOS file handle for this

   DATA nEndOfFile  // Holds the last byte value in the file
   DATA nSkipLength // This holds the default skpping length of 1
   DATA cName       // This holds the name of the file being worked on
   DATA nPosition   // This holds the position in the file at
   DATA lAtBottom   // This is a value to show if at bottom of file
   DATA lAtTop      // This is a value to show if at top of file

   METHOD new( cname )                  // This is the constructor for the file
   METHOD FOPEN()   // This opens the specified file
   METHOD closefile()                   // This closes the specified file
   METHOD fskip( n )                    // Moves the byte pointer within the file
   METHOD FWRITE( c )                   // Write passed data to file w/ len() == nSkipLenght
   METHOD retrieve()                    // Returns the contents of the file at current pos
   METHOD fgoTop()  // Move the byte pointer to the top of the file
   METHOD fgoBottom()                   // Move the byte pointer to the bottom of the file
   METHOD fgoto()   // Moves the byte pointer to a specific location
   METHOD create()
   message fappend METHOD fappendByte( cByte )
   message BuffGet METHOD BufferGet( lDirection )
   METHOD SKIP( nRecord )               // Moves the byte pointer within the file
   METHOD WRITE( cChar )                // Write passed data to file w/ len() == nSkipLenght
   METHOD goTop()   // Move the byte pointer to the top of the file
   METHOD goBottom()                    // Move the byte pointer to the bottom of the file
   METHOD GOTO( nValue )                // Moves the byte pointer to a specific location
   METHOD OPEN()
   message append METHOD appendLine( cline )

ENDCLASS

/* Method:  Init/New
   Params:  N/A
   Returns: Self
   Purpose: Constructor
*/
METHOD new( cName ) CLASS FileBase

   super:new()
   // first thing to do is check to see if there is a valid file

   ::nSkipLength := 1
   ::nOpenMode   := FO_READWRITE        // Mode for which to open the file
   ::nCreateMode := FC_NORMAL           // Mode for which to create the file

   ::cName := cName

   RETURN self

/* Method:  skip( <nRecords> )
   Params:  <nRecords>
   Returns: Self
   Purpose: This method moves the file's byte pointer <nRecords> position
            from the current location.  The actualy movement is determined
            on the value of ::nSkipLength which holds the skipping base.
            This class's purpose is to do one byte movements.
*/
METHOD fskip( nRecords ) CLASS FileBase

   DEFAULT nRecords TO 1

   IF ::noDosError() .AND. ::nDosHandle > 0
      FSEEK( ::nDosHandle, ( ::nSkipLength * nRecords ), FS_RELATIVE )
      ::nLastDosMessage := FERROR()
      ::nPosition       := FSEEK( ::nDosHandle, 0, FS_RELATIVE )
      DO CASE
         CASE ::nPosition == ::nEndOfFile
            ::lAtBottom := .T.
            ::lAtTop    := .F.
         CASE ::nPosition <= 1
            ::lAtTop    := .T.
            ::lAtBottom := .F.
         OTHERWISE
            ::lAtBottom := ::lAtTop := .F.
      ENDCASE
   ENDIF

   RETURN Self

/* Method:  gotop()
   Params:  N/A
   Returns: Self
   Purpose: Move the byte pointer to the top of the file
*/
METHOD fgotop() CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      ::nPosition       := FSEEK( ::nDosHandle, 0, FS_SET )
      ::nLastDosMessage := FERROR()
      ::lAtTop          := .T.
      ::lAtBottom       := .F.
   ENDIF

   RETURN Self

/* Method:  gobottom()
   Params:  N/A
   Returns: Self
   Purpose: Move hte byte pointer of the file to tbe bottom.
*/
METHOD fgoBottom() CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      ::nPosition       := FSEEK( ::nDosHandle, 0, FS_END )
      ::nLastDosMessage := FERROR()
      ::lAtTop          := .F.
      ::lAtBottom       := .T.
   ENDIF

   RETURN Self

/* Method:  closefile()
   Params:  N/A
   Returns: Self
   Purpose: To close the file
*/
METHOD closefile() CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      FCLOSE( ::nDosHandle )
      ::nLastDosMessage := FERROR()
      ::delItem( ::nDosHandle )
      ::lAtTop    := ::lAtBottom := .F.
      ::nPosition := 0
   ENDIF

   RETURN Self

/* Method:  retrieve
   Params:  N/A
   Returns: <cChar>
   Purpose: To return the contents of the file at the current position based
            on the length of ::nSkipLength.
*/
METHOD retrieve() CLASS FileBase

   LOCAL cReturn := ""  // as char
   LOCAL nMoved        // as int

   IF ::noDosError() .AND. ::nDosHandle > 0
      cReturn           := SPACE( ::nSkipLength )
      nMoved            := FREAD( ::nDosHandle, @cReturn, ::nSkipLength )
      ::nLastDosMessage := FERROR()
      FSEEK( ::nDosHandle, - ( nMoved ), FS_RELATIVE )      // Re-position the pointer
   ENDIF

   RETURN cReturn

/* Method:  write(<cChar>)
   Params:  <cChar>
   Returns: Self
   Purpose: To write out to the contents of the file the value in the
            parameter <cChar>.
*/
METHOD FWRITE( cChar ) CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      IF ISCHARACTER( cChar )
         FWRITE( ::nDosHandle, cChar, 1 )
         ::nLastDosMessage := FERROR()
         IF ::noDosError()
            FSEEK( ::nDosHandle, ::nPosition, FS_SET )      // Re-position the pointer
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/* Method:  goto(<nRecord>)
   Params:  <nRecord>       The record byte to move to
   Returns: Self
   Purpose: This method moves the byte marker to the <nRecord> position
            within the file.  It is also based on the value stored to the
            ::nSkipLength instance variable
*/
METHOD fgoto( nValue ) CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      IF ISNUMBER( nValue )
         IF nValue > 0 .AND. ;
                    ( nValue * ::nSkipLength ) <= ::nEndOfFile
            FSEEK( ::nDosHandle, ( nValue * ::nSkipLength ), FS_SET )
            ::nLastDosMessage := FERROR()
            ::nPosition       := FSEEK( ::nDosHandle, 0, FS_RELATIVE )
            DO CASE
               CASE ::nPosition == ::nEndOfFile
                  ::lAtBottom := .T.
                  ::lAtTop    := .F.
               CASE ::nPosition <= 1
                  ::lAtTop    := .T.
                  ::lAtBottom := .F.
               OTHERWISE
                  ::lAtBottom := ::lAtTop := .F.
            ENDCASE
         ENDIF
      ENDIF
   ENDIF

   RETURN ::nPosition

/* Method:  create()
   Params:  N/A
   Returns: Self
   Purpose: Creates the specified file with the proper access code
*/
METHOD Create() CLASS FileBase

   LOCAL nFile         // as int

   IF ::noDosError()
      nFile             := FCREATE( ::cName, ::nCreateMode )
      ::nLastDosMessage := FERROR()
      IF ::noDosError()                 // No Error
         FCLOSE( nFile )                // Close the file
         ::fopen()  // Re-open the file
      ENDIF
   ENDIF

   RETURN Self

/* Method:  open()
   Params:  N/A
   Returns: Self
   Purpose: Opens the file with the proper access code
*/
METHOD FOPEN() CLASS FileBase

   IF ::noDosError()
      ::nDosHandle :=::openfile( ::cName, ::nOpenMode )
      ::nEndOfFile := FSEEK( ::nDosHandle, 0, FS_END )
      ::nPosition  := FSEEK( ::nDosHandle, 0, FS_SET )
      ::lAtTop     := .T.
      ::lAtBottom  := .F.
   ENDIF

RETURN Self

METHOD fappendByte( cByte ) CLASS FileBase

   DEFAULT cByte TO ""

   IF !EMPTY( cByte )                   // Valid line
      IF ::noDosError() .AND. ::nDosHandle > 0              // No error
         FSEEK( ::nDosHandle, 0, FS_END )
         FWRITE( ::nDosHandle, cByte, 1 )
         ::nEndOfFile  := FSEEK( ::nDosHandle, 0, FS_END )
         ::nPosition   := FSEEK( ::nDosHandle, - ( LEN( cByte ) ), FS_END )
         ::nSkipLength := LEN( cByte )
         ::lAtBottom   := ::lAtTop := .F.
      ENDIF
   ENDIF

RETURN Self

METHOD OPEN() CLASS FileBase

   Self:nDosHandle := Self:openfile( ::cName, ::nOpenMode )
   ::nEndOfFile    := FSEEK( Self:nDosHandle, 0, FS_END )
   FSEEK( Self:nDosHandle, 0, FS_SET )
   ::nSkipLength := Self:Buffget()
   ::lAtTop      := .T.
   ::lAtBottom   := .F.
   ::nHan        := Self:nDosHandle
   RETURN Self

/* Method:  gotop()
   Params:  N/A
   Returns: Self
   Purpose: Move the byte pointer to the top of the file
*/
METHOD gotop() CLASS FileBase

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      ::fgotop()
      ::nSkipLength := Self:Buffget()
   ENDIF

   RETURN Self

/* Method:  gobottom()
   Params:  N/A
   Returns: Self
   Purpose: Move hte byte pointer of the file to tbe bottom.
*/
METHOD goBottom() CLASS FileBase

   LOCAL cBuffer       // as char
   LOCAL lWithCRLF := .F.               // as logical

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      ::fgobottom()
      // Now, back off from the end one line length and set the marker
      cBuffer := SPACE( pBUFFER_LENGTH )
      FSEEK( Self:nDosHandle, - ( pBUFFER_LENGTH ), FS_END )
      FREAD( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
      IF RIGHT( cBuffer, 2 ) == hb_OSNewLine()   // We need to remove this extra one!
         cBuffer   := LEFT( cBuffer, LEN( cBuffer ) - 2 )
         lWithCRLF := .T.
      ENDIF
      cBuffer       := SUBSTR( cBuffer, RAT( hb_OSNewLine(), cBuffer ) + 2 )
      ::nSkipLength := LEN( cBuffer ) + iif( lWithCRLF, 2, 0 )
      ::nposition   := FSEEK( Self:nDosHandle, - ( LEN( cBuffer ) ), FS_END )
      IF lWithCRLF
         ::nposition := FSEEK( Self:nDosHandle, - 2, FS_RELATIVE )
      ENDIF
   ENDIF

   RETURN Self

/* Method:  write(<cChar>)
   Params:  <cChar>
   Returns: Self
   Purpose: To write out to the contents of the file the value in the
            parameter <cChar>.
*/
METHOD WRITE( cChar ) CLASS FileBase

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      IF ISCHARACTER( cChar )
         IF cChar > Self:nSkipLength    // we are going to truncate for now...
            FWRITE( Self:nDosHandle, cChar, Self:nSkipLength )
         ELSE
            FWRITE( Self:nDosHandle, cChar, LEN( cChar ) )
         ENDIF
         FSEEK( Self:nDosHandle, ::nposition, FS_SET )
         Self:nLastDosMessage := FERROR()
         IF Self:noDosError()
            FSEEK( Self:nDosHandle, Self:nPosition, FS_SET )     // Re-position the pointer
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/* Method:  appendLine( <cLine )
   Params:  <cLine>         Character line to append
   Returns: Self
   Purpose: To append a blank CRLF delimited line at the end of the file.
            If <cLine> is not passed or if it an empty line with 0 bytes
            in length, the function will not operate.
*/
METHOD appendLine( cLine ) CLASS FileBase

   DEFAULT cLine TO ""

   IF LEN( cLine ) == 0                 // Valid line
      IF Self:noDosError() .AND. Self:nDosHandle > 0        // No error
         IF !( hb_OSNewLine() $ cLine )          // No CRLF, so add
            cLIne += hb_OSNewLine()
         ENDIF
         FSEEK( Self:nDosHandle, 0, FS_END )
         FWRITE( Self:nDosHandle, cLine )
         ::nEndOfFile  := FSEEK( Self:nDosHandle, 0, FS_END )
         ::nposition   := FSEEK( Self:nDosHandle, - ( LEN( cLine ) ), FS_END )
         ::nSkipLength := LEN( cLine )
         ::lAtBottom   := ::lAtTop := .F.
      ENDIF
   ENDIF

   RETURN Self

/* Method:  skip( <nRecords> )
   Params:  <nRecords>
   Returns: Self
   Purpose: This method moves the file's byte pointer <nRecords> position
            from the current location.  The actualy movement is determined
            on the value of Self:nSkipLength which holds the skipping base.
            This class's purpose is to do one byte movements.
*/

METHOD SKIP( nRecords ) CLASS FileBase

   LOCAL nCount := 0   // as int

   DEFAULT nRecords TO 1

   // Here, we have to start looking for CHR(13)+CHR(10) character
   // combinations.  Once found, then we have to set the super class
   // values appropriately

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      DO CASE
         CASE nRecords > 0              // It's positive movement
            WHILE nCount ++ != nRecords
               ::fskip()
               ::nSkipLength := Self:Buffget()
            ENDDO

         CASE nRecords < 0              // It's negative movement
            WHILE nCount -- != nRecords
               ::nSkipLength := Self:Buffget( .F. )
               ::fskip( - 1 )
            ENDDO

      ENDCASE
   ENDIF

   RETURN Self

/* Method:  goto(<nRecord>)
   Params:  <nRecord>       The record byte to move to
   Returns: Self
   Purpose: This method moves the byte marker to the <nRecord> position
            within the file.  It is also based on the value stored to the
            Self:nSkipLength instance variable
*/
METHOD GOTO( nValue ) CLASS FileBase

   LOCAL cLine     := ""                   // as char
   LOCAL nCount    := 0                    // as int
   LOCAL lContinue := .T.                // as logical
   LOCAL cBuffer       // as char

   DEFAULT nValue TO 0

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      IF ISNUMBER( nValue )
         IF nValue > 0                  // o.k. so far
            FSEEK( Self:nDosHandle, 0, FS_SET )                  // start at the top
            WHILE lContinue
               cBuffer   := SPACE( pBUFFER_LENGTH )
               lContinue := ( FREAD( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH ) == ;
                              pBUFFER_LENGTH )
               cBuffer := cLine + cBuffer
               WHILE hb_OSNewLine() $ cBuffer
                  IF ++ nCount == nValue
                     lContinue := .F.
                     EXIT
                  ENDIF
                  cBuffer := SUBSTR( cBuffer, AT( hb_OSNewLine(), cBuffer ) + 2 )
               ENDDO
               cLine := cBuffer
            ENDDO
            IF nCount == nValue         // We have a match
               FSEEK( Self:nDosHandle, - ( pBUFFER_LENGTH ), FS_RELATIVE )                // Back off from here
               ::nposition := FSEEK( Self:nDosHandle, ;
                                     ( pBUFFER_LENGTH - LEN( cBuffer ) ), ;
                                     FS_RELATIVE )                    // Move
               ::nSkipLength := Self:Buffget()
            ELSE
               FSEEK( Self:nDosHandle, ::nposition, FS_SET )
               nCount := 0
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN nCount

METHOD BufferGet( lForward ) CLASS FileBase

   LOCAL cBuffer       // as char
   LOCAL nLocation     // as int
   LOCAL nRead         // as int
   LOCAL lWithCRLF := .F.               // as logical

   DEFAULT lForward TO .T.

   IF !lForward

      nRead := FSEEK( Self:nDosHandle, ;
                      - ( iif( ::nposition < pBUFFER_LENGTH, ;
                      ::nposition, ;
                      pBUFFER_LENGTH ) ), ;
                      FS_RELATIVE )               // rewind backwards

      cBuffer := SPACE( ::nposition - nRead )
      FREAD( Self:nDosHandle, @cBuffer, ( ::nposition - nRead ) )

      IF RIGHT( cBuffer, 2 ) == hb_OSNewLine()   // with line already
         cBuffer   := LEFT( cBuffer, LEN( cBuffer ) - 2 )
         lWithCRLF := .T.
      ENDIF
      nLocation := LEN( cBuffer ) - ( RAT( hb_OSNewLine(), cBuffer ) )

   ELSE
      cBuffer := SPACE( pBUFFER_LENGTH )
      nRead   := FREAD( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
      FSEEK( Self:nDosHandle, - ( iif( nRead < pBUFFER_LENGTH, nRead, ;
             pBUFFER_LENGTH ) ), FS_RELATIVE )    // Rewind

      // Now, parse the string. and file

      nLocation := AT( hb_OSNewLine(), cBuffer )

      // Now, if there is NO CRLF in the buffer and if the value of the
      // number of bytes read is less than the buffer length, then we
      // have an end of file condition.
      IF nLocation == 0 .AND. ( nRead < pBUFFER_LENGTH )
         // If so, then set the appropriate flags accordingly.
         ::lAtBottom := .T.
         ::lAtTop    := .F.
      ENDIF
   ENDIF

   RETURN nLocation

#xtranslate DOSFILENAME(<c>) => substr( <c>, rat("\",<c>)+1 )

#define pDOS_HANDLE 1
#define pDOS_FILE   2
#define pDOS_PATH   3

*+--------------------------------------------------------------------
*+
*+    Class FileMan
*+
*+--------------------------------------------------------------------
*+
CLASS FileMan

   data aDosHandles // Holds an array of dos handles and names

   data nHan
   data nLastDosMessage                 // Holds the DOS error level of last operation

   METHOD addItem( nDos, cFile, cPath ) // Adds an item to the array of handles
   METHOD delItem( xitem )              // Deletes an item from the array of handles

   METHOD new()     // The constructor for this class
   METHOD closeAll()                    // Closes all of those files
   METHOD rewindAll()                   // Positions the file pointer for each file
   METHOD writeAll()                    // Performs hard-write of all

   METHOD getFileName( cfile )          // Obtains the name of the file based on ID
   METHOD getFileId( nid )              // Obtains the ID based on file name
   METHOD getFilePath( xItem )          // Obtains file path based on either ID or name

   METHOD noDosError()                  // Returns a logical true/false

   METHOD openfile( cFile, nMethod )    // Opens the specified file and sets error
   METHOD Buffget( ld ) virtual

ENDCLASS

/* Method:  Init/New
   Params:  N/A
   Returns: Self
   Purpose: Constructor
*/
METHOD new() CLASS FileMan

   IF ::aDosHandles == NIL              // has yet to be initialized
      ::aDosHandles := {}
   ENDIF

   IF ::nLastDosMessage == NIL          // has yet to be initialized
      ::nLastDosMessage := 0
   ENDIF

   RETURN Self

   // The following are global operations that need to be performed by all
   // files regardless of their format

/* Method:  ::closeAll()
   Params:  N/A
   Returns: Self
   Purpose: To go through the stack of opened file handles and close each
            file, one at a time.  Since this is a global operation, it
            will not check on the status of the error message on each
            pass but will at the start of the evaluation.

*/
METHOD closeAll() CLASS FileMan

   IF ::nLastDosMessage == 0
      AEVAL( ::aDosHandles, { | aFile | FCLOSE( aFile[ pDOS_HANDLE ] ) } )
   ENDIF

   RETURN Self

/* Method:  ::rewindAll()
   Params:  N/A
   Returns: Self
   Purpose: To go through the stack of opened file handles and places the
            file pointer to the top of each file, one at a time.  Since
            Since this is a global operation, it will not check on the
            status of the error message on each pass but will at the
            start of the evaluation.

*/
METHOD rewindAll() CLASS FileMan

   IF ::nLastDosMessage == 0
      AEVAL( ::aDosHandles, { | aFile | FSEEK( aFile[ pDOS_HANDLE ], 0, FS_SET ) } )
   ENDIF

   RETURN Self

/* Method:  ::writeAll()
   Params:  N/A
   Returns: Self
   Purpose: To go through the stack of opened file handles and writes each
            file, one at a time.  Since this is a global operation, it
            will not check on the status of the error message on each
            pass but will at the start of the evaluation.

*/
METHOD writeAll() CLASS FileMan

   IF ::nLastDosMessage == 0
      AEVAL( ::aDosHandles, { | aFile | FWRITE( aFile[ pDOS_HANDLE ], "", 0 ) } )
   ENDIF

   RETURN Self

/* Method:  ::getFileName( <nId> )
   Params:  <nId>           DOS File handle / ID
   Returns: <cName>         File name store with that ID handle
   Purpose: This method will return the file's name found
            within the table of this class.
*/
METHOD getFileName( nId ) CLASS FileMan                     // Obtains the name of the file based on ID

   LOCAL cName     := ""                   // as char
   LOCAL nPosition     // as int

   IF ::nLastDosMessage == 0
      IF ISNUMBER( nId )
         nPosition := ASCAN( ::aDosHandles, ;
                             { | aFile | nId == aFile[ pDOS_HANDLE ] } )
         IF nPosition != 0
            cName := ::aDosHandles[ nPosition, pDOS_FILE ]
         ENDIF
      ENDIF
   ENDIF

   RETURN cName

/* Method:  ::getFileId( <cName> )
   Params:  <cName>         File names used to store item to stack
   Returns: <nId>           DOS File handle or ID associated with name
   Purpose: This method will return the file's ID or DOS handle found
            within the table of this class.
*/
METHOD getFileId( cName ) CLASS FileMan                     // Obtains the ID based on file name

   LOCAL nId       := 0                    // as int
   LOCAL nPosition     // as int

   IF ::nLastDosMessage == 0
      IF ISCHARACTER( cName )
         nPosition := ASCAN( ::aDosHandles, ;
                             { | aFile | cName == aFile[ pDOS_FILE ] } )
         IF nPosition != 0
            nId := ::aDosHandles[ nPosition, pDOS_HANDLE ]
         ENDIF
      ENDIF
   ENDIF

   RETURN nId

/* Method:  ::getFilePath( <xItem> )
   Params:  <xItem>         DOS File handle / ID or stored file name
   Returns: <cPath>         Associated file path
   Purpose: This method will return the associated DOS path for either the
            given file name or DOS file handle / ID.  If there is no file
            path or if there is an error with the method, the return value
            will be a NULL character byte.
*/
METHOD getFilePath( xItem ) CLASS FileMan                   // Obtains file path based on either ID or name

   LOCAL cPath     := ""                   // as char
   LOCAL nPosition     // as int

   IF ::nLastDosMessage == 0
      DO CASE
         CASE ISCHARACTER( xItem )   // we've got the file name
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aFile | xItem == aFile[ pDOS_FILE ] } )
            IF nPosition != 0
               cPath := ::aDosHandles[ nPosition, pDOS_PATH ]
            ENDIF

         CASE ISNUMBER( xItem )      // we've got the file path
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aFile | xItem == aFile[ pDOS_HANDLE ] } )
            IF nPosition != 0
               cPath := ::aDosHandles[ nPosition, pDOS_PATH ]
            ENDIF

      ENDCASE
   ENDIF

   RETURN cPath

   // The following two methods are for the sole purpose of manipulating the
   // array of DOS file handles

/* Method:  ::addItem( <nDos>, <cFile> [, <cPath] )
   Params:  <nDos>          DOS file handle
            <cFile>         File name
            <cPath>         File path, defaults to ""
   Returns: self
   Purpose: This method will add the DOS file ID and name to the internal
            stack.  It will not work if either of the the first two
            parameters are not passed to the method OR if the value of
            ::nLastDosMessage is 0.  The return value of the method will
            be the object itself.
*/
METHOD addItem( nDos, cFile, cPath ) CLASS FileMan

   DEFAULT cPath TO ""

   IF ::nLastDosMessage == 0
      AADD( ::aDosHandles, { nDos, cFile, cPath } )
   ENDIF

   RETURN Self

/* Method:  ::delItem( <xItem> )
   Params:  <xItem>         DOS file handle or file name
   Returns: <lSuccess>      Success status of operation
   Purpose: To go through the stack of opened file handles and based on the
            parameter passed to the method, it will remove the file from
            the stack.  If <xItem> is a numeric, it will be assumed a
            valud DOS file handle.  If <xItem> is character, then it will
            be assumed the name of the file.  If <xItem> is neither numeric
            or character or if the value of ::nLastDosMessage is not 0,
            then the method will return a logical false (.F.) value;
            otherwise, a logical true (.T.) will be returned.
*/
METHOD delItem( xItem ) CLASS FileMan

   LOCAL nPosition     // as int
   LOCAL lSuccess  := .T.                // as logical

   // if xItem is N/Numeric, then seek on first element;
   // if xItem is C/Character, then seek on second element

   IF ::nLastDosMessage == 0            // No DOS error!
      DO CASE
         CASE ISNUMBER( xItem )         // It's a DOS file handle
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aItem | xItem == aItem[ pDOS_HANDLE ] } )
            IF nPosition == 0
               // Don't remove and set the return value of the function
               lSuccess := .F.
            ELSE
               // Since we have a position, remove from the table and keep the
               // default return value
               ADEL( ::aDosHandles, nPosition )
               ASIZE( ::aDosHandles, LEN( ::aDosHandles ) - 1 )
            ENDIF

         CASE ISCHARACTER( xItem )   // It's a file name
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aItem | xItem == aItem[ pDOS_FILE ] } )
            IF nPosition == 0
               // Don't remove and set the return value of the function
               lSuccess := .F.
            ELSE
               // Since we have a position, remove from the table and keep the
               // default return value
               ADEL( ::aDosHandles, nPosition )
               ASIZE( ::aDosHandles, LEN( ::aDosHandles ) - 1 )
            ENDIF

         OTHERWISE
            // Invalid data passed to method
            lSuccess := .F.

      ENDCASE
   ELSE
      lSuccess := .F.

   ENDIF

   RETURN lSuccess

/* Method:  noDosError()
   Params:  N/A
   Returns: <lNoError>
   Purpose: To return a logical true (.T.) if there is no existing error
            state within the system
*/
METHOD noDosError() CLASS FileMan

   RETURN ::nLastDosMessage == 0

/* Method:  open()
   Params:  N/A
   Returns: <nDosHandle>
   Purpose: This method acutally opens the file specified by the parameter
            <cFile> with the open mode of <nMethod>.  Each file object
            should carry this information locally and use this method only
            to update the internal table.

*/
METHOD openfile( cFile, nMethod ) CLASS FileMan

   LOCAL nFileHandle   // as int
   LOCAL cFilename     // as char
   LOCAL cPath         // as char

   DEFAULT nMethod TO 0

   nFileHandle := FOPEN( cFile, nMethod )                   // opens the file
   IF !EMPTY( FERROR() )                // There was an error in opening
      ::nLastDosMessage := FERROR()
      nFileHandle       := - 1
   ELSE
      cFileName := DOSFILENAME( cFile )
      cPath     := STRTRAN( cFile, cFileName, "" )
      ::addItem( nFileHandle, cFileName, cPath )
   ENDIF

   ::nHan := nFileHandle

   RETURN nFileHandle

/*****************************************************************/

/****
*   FT_FUSE( cFile, nMode ) ---> nHandle
*   Open a File
*/
FUNCTION FT_FUSE( cFile, nMode )

   IF cFile == NIL
      s_oFileBase:closefile()
   ELSE
      s_oFileBase := FileBase():new( cFile )
      IF nMode != NIL
         s_oFileBase:nOpenMode := nMode
      ENDIF
      s_oFileBase:open()
   ENDIF

   RETURN s_oFileBase:nHan

FUNCTION FT_FEOF()
   RETURN s_oFileBase:lAtBottom

FUNCTION FReadLn()
   RETURN s_oFileBase:retrieve()

PROCEDURE FT_FGotop()
   s_oFileBase:Gotop()
   RETURN

PROCEDURE FT_FSKIP( n )
   s_oFileBase:Skip( n )
   RETURN

PROCEDURE FT_MKDIR( cDir )

#ifdef HB_COMPAT_C53
   MakeDir( cDir )
#else
   HB_SYMBOL_UNUSED( cDir ) // TOFIX
#endif

   RETURN

FUNCTION StrPos( cBuffer )

   LOCAL x
   LOCAL cChar

   FOR x := 1 TO LEN( cBuffer )

      cChar := SUBSTR( cBuffer, x, 1 )

      IF cChar >= "A" .AND. cChar <= "Z" .OR. ;
         cChar >= "a" .AND. cChar <= "z" .OR. ;
         cChar >= "0" .AND. cChar <= "9" .OR. ;
         cChar $ "@-(|.*#/=?!<>+" .OR. ;
         cChar == '"'

         RETURN x
      ENDIF
   NEXT

   RETURN 0

FUNCTION GetNumberofTableItems( cBuffer )

   LOCAL cItem
   LOCAL nItem := 0

   cBuffer := ALLTRIM( cBuffer )

   DO WHILE AT( SPACE( 3 ), cBuffer ) > 0
      cItem := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
      IF AT( SPACE( 3 ), cBuffer ) == 0
         nItem++
      ELSE
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, "" ) )
         nItem++
      ENDIF
   ENDDO
   nItem++

   RETURN nItem

FUNCTION FREADline( nH, cB, nMaxLine )

   LOCAL cLine := SPACE( nMaxLine )
   LOCAL nSavePos
   LOCAL nEol
   LOCAL nNumRead

   cB       := ""
   nSavePos := FSEEK( nH, 0, FS_RELATIVE )
   nNumRead := FREAD( nH, @cLine, nMaxLine )
   IF ( nEol := AT( hb_OSNewLine(), SUBSTR( cLine, 1, nNumRead ) ) ) == 0
      cB := cLine
   ELSE
      cB := SUBSTR( cLine, 1, nEol - 1 )
      FSEEK( nH, nSavePos + Len( hb_OSNewLine() ) - 1, FS_SET )
   ENDIF

   RETURN nNumRead != 0
