/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FFILE1.PRG Filebase class for hbdoc
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "hbclass.ch"
#include 'common.ch'
#include '..\hbdoc\hbdocdef.ch'

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Class FileBase
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
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
   ::nOpenMode   := 2                   // Mode for which to open the file
   ::nCreateMode := 0                   // Mode for which to create the file

   ::cName := cName

   RETURN ( self )

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
      FSEEK( ::nDosHandle, ( ::nSkipLength * nRecords ), 1 )
      ::nLastDosMessage := FERROR()
      ::nPosition       := FSEEK( ::nDosHandle, 0, 1 )
      DO CASE
         CASE ::nPosition == ::nEndOfFile
            ::lAtBottom := pTRUE
            ::lAtTop    := pFALSE
         CASE ::nPosition <= 1
            ::lAtTop    := pTRUE
            ::lAtBottom := pFALSE
         OTHERWISE
            ::lAtBottom := ::lAtTop := pFALSE
      ENDCASE
   ENDIF

   RETURN ( self )

   /* Method:  gotop()
   Params:  N/A
   Returns: Self
   Purpose: Move the byte pointer to the top of the file
*/
METHOD fgotop() CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      ::nPosition       := FSEEK( ::nDosHandle, 0, 0 )
      ::nLastDosMessage := FERROR()
      ::lAtTop          := pTRUE
      ::lAtBottom       := pFALSE
   ENDIF

   RETURN ( self )

   /* Method:  gobottom()
   Params:  N/A
   Returns: Self
   Purpose: Move hte byte pointer of the file to tbe bottom.
*/
METHOD fgoBottom() CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      ::nPosition       := FSEEK( ::nDosHandle, 0, 2 )
      ::nLastDosMessage := FERROR()
      ::lAtTop          := pFALSE
      ::lAtBottom       := pTRUE
   ENDIF

   RETURN ( self )

   /* Method:  close()
   Params:  N/A
   Returns: Self
   Purpose: To close the file
*/
METHOD closefile() CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      FCLOSE( ::nDosHandle )
      ::nLastDosMessage := FERROR()
      ::delItem( ::nDosHandle )
      ::lAtTop    := ::lAtBottom := pFALSE
      ::nPosition := 0
   ENDIF

   RETURN ( self )

   /* Method:  retrieve
   Params:  N/A
   Returns: <cChar>
   Purpose: To return the contents of the file at the current position based
            on the length of ::nSkipLength.
*/
METHOD retrieve() CLASS FileBase

   LOCAL cReturn       // as char
   LOCAL nMoved        // as int

   IF ::noDosError() .AND. ::nDosHandle > 0
      cReturn           := SPACE( ::nSkipLength )
      nMoved            := FREAD( ::nDosHandle, @cReturn, ::nSkipLength )
      ::nLastDosMessage := FERROR()
      FSEEK( ::nDosHandle, - ( nMoved ), 1 )                // Re-position the pointer
   ENDIF

   RETURN ( cReturn )

   /* Method:  write(<cChar>)
   Params:  <cChar>
   Returns: Self
   Purpose: To write out to the contents of the file the value in the
            parameter <cChar>.
*/
METHOD FWRITE( cChar ) CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      IF cChar IS pCHARACTER
         FWRITE( ::nDosHandle, cChar, 1 )
         ::nLastDosMessage := FERROR()
         IF ::noDosError()
            FSEEK( ::nDosHandle, ::nPosition, 0 )           // Re-position the pointer
         ENDIF
      ENDIF
   ENDIF

   RETURN ( self )

   /* Method:  goto(<nRecord>)
   Params:  <nRecord>       The record byte to move to
   Returns: Self
   Purpose: This method moves the byte marker to the <nRecord> position
            within the file.  It is also based on the value stored to the
            ::nSkipLength instance variable
*/
METHOD fgoto( nValue ) CLASS FileBase

   IF ::noDosError() .AND. ::nDosHandle > 0
      IF nValue IS pNUMERIC
         IF nValue > 0 .AND. ;
                    ( nValue * ::nSkipLength ) <= ::nEndOfFile
            FSEEK( ::nDosHandle, ( nValue * ::nSkipLength ), 0 )
            ::nLastDosMessage := FERROR()
            ::nPosition       := FSEEK( ::nDosHandle, 0, 1 )
            DO CASE
               CASE ::nPosition == ::nEndOfFile
                  ::lAtBottom := pTRUE
                  ::lAtTop    := pFALSE
               CASE ::nPosition <= 1
                  ::lAtTop    := pTRUE
                  ::lAtBottom := pFALSE
               OTHERWISE
                  ::lAtBottom := ::lAtTop := pFALSE
            ENDCASE
         ENDIF
      ENDIF
   ENDIF

   RETURN ( ::nPosition )

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

   RETURN ( self )

   /* Method:  open()
   Params:  N/A
   Returns: Self
   Purpose: Opens the file with the proper access code
*/
METHOD FOPEN() CLASS FileBase

   IF ::noDosError()
      ::nDosHandle :=::openfile( ::cName, ::nOpenMode )
      ::nEndOfFile := FSEEK( ::nDosHandle, 0, 2 )
      ::nPosition  := FSEEK( ::nDosHandle, 0, 0 )
      ::lAtTop     := pTRUE
      ::lAtBottom  := pFALSE
   ENDIF

RETURN ( self )

METHOD fappendByte( cByte ) CLASS FileBase

   DEFAULT cByte TO ""

   IF !EMPTY( cByte )                   // Valid line
      IF ::noDosError() .AND. ::nDosHandle > 0              // No error
         FSEEK( ::nDosHandle, 0, 2 )
         FWRITE( ::nDosHandle, cByte, 1 )
         ::nEndOfFile  := FSEEK( ::nDosHandle, 0, 2 )
         ::nPosition   := FSEEK( ::nDosHandle, - ( LEN( cByte ) ), 2 )
         ::nSkipLength := LEN( cByte )
         ::lAtBottom   := ::lAtTop := pFALSE
      ENDIF
   ENDIF

RETURN ( self )

// End of File: FFile1.prg
METHOD OPEN() CLASS FileBase

   Self:nDosHandle := Self:openfile( ::cName, ::nOpenMode )
   ::nEndOfFile    := FSEEK( Self:nDosHandle, 0, 2 )
   FSEEK( Self:nDosHandle, 0, 0 )
   ::nSkipLength := Self:Buffget()
   ::lAtTop      := pTRUE
   ::lAtBottom   := pFALSE
   ::nHan        := Self:nDosHandle
   RETURN ( self )

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

   RETURN ( self )

   /* Method:  gobottom()
   Params:  N/A
   Returns: Self
   Purpose: Move hte byte pointer of the file to tbe bottom.
*/
METHOD goBottom() CLASS FileBase

   LOCAL cBuffer       // as char
   LOCAL lWithCRLF := pFALSE               // as logical

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      ::fgobottom()
      // Now, back off from the end one line length and set the marker
      cBuffer := SPACE( pBUFFER_LENGTH )
      FSEEK( Self:nDosHandle, - ( pBUFFER_LENGTH ), 2 )
      FREAD( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
      IF RIGHT( cBuffer, 2 ) == pCRLF   // We need to remove this extra one!
         cBuffer   := LEFT( cBuffer, LEN( cBuffer ) - 2 )
         lWithCRLF := pTRUE
      ENDIF
      cBuffer       := SUBSTR( cBuffer, RAT( pCRLF, cBuffer ) + 2 )
      ::nSkipLength := LEN( cBuffer ) + IF( lWithCRLF, 2, 0 )
      ::nposition   := FSEEK( Self:nDosHandle, - ( LEN( cBuffer ) ), 2 )
      IF lWithCRLF
         ::nposition := FSEEK( Self:nDosHandle, - 2, 1 )
      ENDIF
   ENDIF

   RETURN ( self )

   /* Method:  close()
   Params:  N/A
   Returns: Self
   Purpose: To close the file
*/
METHOD FCLOSE() CLASS FileBase

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      FCLOSE( Self:nDosHandle )
      Self:nLastDosMessage := FERROR()
      Self:delItem( Self:nDosHandle )
      Self:lAtTop    := Self:lAtBottom := pFALSE
      Self:nPosition := 0
   ENDIF

   RETURN ( self )

   /* Method:  write(<cChar>)
   Params:  <cChar>
   Returns: Self
   Purpose: To write out to the contents of the file the value in the
            parameter <cChar>.
*/
METHOD WRITE( cChar ) CLASS FileBase

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      IF cChar IS pCHARACTER
         IF cChar > Self:nSkipLength    // we are going to truncate for now...
            FWRITE( Self:nDosHandle, cChar, Self:nSkipLength )
         ELSE
            FWRITE( Self:nDosHandle, cChar, LEN( cChar ) )
         ENDIF
         FSEEK( Self:nDosHandle, ::nposition, 0 )
         Self:nLastDosMessage := FERROR()
         IF Self:noDosError()
            FSEEK( Self:nDosHandle, Self:nPosition, 0 )     // Re-position the pointer
         ENDIF
      ENDIF
   ENDIF

   RETURN ( self )

   /* Method:  getBuffer( <lDirection> )
   Params:  <lDirection>    Logical toggle for direction
   Returns: <nBytes>
   Purpose: To return the number of bytes either forward or backward from
            the present file pointer position in which the next CRLF char
            appears.  If <lDirection> is a logical false (.F.) value, them
            the operation will go in reverse order; otherwise, it will go
            in a forward direction.  The default value is a logical true
            (.T.) value.
*/
METHOD Buffget( lForward ) CLASS FileBase

   LOCAL cBuffer       // as char
   LOCAL nLocation     // as int
   LOCAL nRead         // as int
   LOCAL lWithCRLF := pFALSE               // as logical

   DEFAULT lForward TO pTRUE

   IF !lForward

      nRead := FSEEK( Self:nDosHandle, ;
                      - ( IF( ::nposition < pBUFFER_LENGTH, ;
                      ::nposition, ;
                      pBUFFER_LENGTH ) ), ;
                      1 )               // rewind backwards

      cBuffer := SPACE( ::nposition - nRead )
      FREAD( Self:nDosHandle, @cBuffer, ( ::nposition - nRead ) )

      IF RIGHT( cBuffer, 2 ) == pCRLF   // with line already
         cBuffer   := LEFT( cBuffer, LEN( cBuffer ) - 2 )
         lWithCRLF := pTRUE
      ENDIF
      nLocation := LEN( cBuffer ) - ( RAT( pCRLF, cBuffer ) )

   ELSE
      cBuffer := SPACE( pBUFFER_LENGTH )
      nRead   := FREAD( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
      FSEEK( Self:nDosHandle, - ( IF( nRead < pBUFFER_LENGTH, nRead, ;
             pBUFFER_LENGTH ) ), 1 )    // Rewind

      // Now, parse the string. and file

      nLocation := AT( pCRLF, cBuffer )

      // Now, if there is NO CRLF in the buffer and if the value of the
      // number of bytes read is less than the buffer length, then we
      // have an end of file condition.
      IF nLocation == 0 .AND. ( nRead < pBUFFER_LENGTH )
         // If so, then set the appropriate flags accordingly.
         ::lAtBottom := pTRUE
         ::lAtTop    := pFALSE
      ENDIF
   ENDIF

   RETURN ( nLocation )

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
         IF !( pCRLF $ cLine )          // No CRLF, so add
            cLIne += pCRLF
         ENDIF
         FSEEK( Self:nDosHandle, 0, 2 )
         FWRITE( Self:nDosHandle, cLine )
         ::nEndOfFile  := FSEEK( Self:nDosHandle, 0, 2 )
         ::nposition   := FSEEK( Self:nDosHandle, - ( LEN( cLine ) ), 2 )
         ::nSkipLength := LEN( cLine )
         ::lAtBottom   := ::lAtTop := pFALSE
      ENDIF
   ENDIF

   RETURN ( self )

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
               ::nSkipLength := Self:Buffget( pFALSE )
               ::fskip( - 1 )
            ENDDO

      ENDCASE
   ENDIF

   RETURN ( self )

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
   LOCAL lContinue := pTRUE                // as logical
   LOCAL cBuffer       // as char

   DEFAULT nValue TO 0

   IF Self:noDosError() .AND. Self:nDosHandle > 0
      IF nValue IS pNUMERIC
         IF nValue > 0                  // o.k. so far
            FSEEK( Self:nDosHandle, 0, 0 )                  // start at the top
            WHILE lContinue
               cBuffer   := SPACE( pBUFFER_LENGTH )
               lContinue := ( FREAD( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH ) == ;
                              pBUFFER_LENGTH )
               cBuffer := cLine + cBuffer
               WHILE pCRLF $ cBuffer
                  IF ++ nCount == nValue
                     lContinue := pFALSE
                     EXIT
                  ENDIF
                  cBuffer := SUBSTR( cBuffer, AT( pCRLF, cBuffer ) + 2 )
               ENDDO
               cLine := cBuffer
            ENDDO
            IF nCount == nValue         // We have a match
               FSEEK( Self:nDosHandle, - ( pBUFFER_LENGTH ), 1 )                // Back off from here
               ::nposition := FSEEK( Self:nDosHandle, ;
                                     ( pBUFFER_LENGTH - LEN( cBuffer ) ), ;
                                     1 )                    // Move
               ::nSkipLength := Self:Buffget()
            ELSE
               FSEEK( Self:nDosHandle, ::nposition, 0 )
               nCount := 0
            ENDIF
         ENDIF
      ENDIF
   ENDIF

RETURN ( nCount )

// End of File: FFile2.prg

METHOD BufferGet( lForward ) CLASS FileBase

   LOCAL cBuffer       // as char
   LOCAL nLocation     // as int
   LOCAL nRead         // as int
   LOCAL lWithCRLF := pFALSE               // as logical

   DEFAULT lForward TO pTRUE

   IF !lForward

      nRead := FSEEK( Self:nDosHandle, ;
                      - ( IF( ::nposition < pBUFFER_LENGTH, ;
                      ::nposition, ;
                      pBUFFER_LENGTH ) ), ;
                      1 )               // rewind backwards

      cBuffer := SPACE( ::nposition - nRead )
      FREAD( Self:nDosHandle, @cBuffer, ( ::nposition - nRead ) )

      IF RIGHT( cBuffer, 2 ) == pCRLF   // with line already
         cBuffer   := LEFT( cBuffer, LEN( cBuffer ) - 2 )
         lWithCRLF := pTRUE
      ENDIF
      nLocation := LEN( cBuffer ) - ( RAT( pCRLF, cBuffer ) )

   ELSE
      cBuffer := SPACE( pBUFFER_LENGTH )
      nRead   := FREAD( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
      FSEEK( Self:nDosHandle, - ( IF( nRead < pBUFFER_LENGTH, nRead, ;
             pBUFFER_LENGTH ) ), 1 )    // Rewind

      // Now, parse the string. and file

      nLocation := AT( pCRLF, cBuffer )

      // Now, if there is NO CRLF in the buffer and if the value of the
      // number of bytes read is less than the buffer length, then we
      // have an end of file condition.
      IF nLocation == 0 .AND. ( nRead < pBUFFER_LENGTH )
         // If so, then set the appropriate flags accordingly.
         ::lAtBottom := pTRUE
         ::lAtTop    := pFALSE
      ENDIF
   ENDIF

RETURN ( nLocation )

*+ EOF: FFILE1.PRG
