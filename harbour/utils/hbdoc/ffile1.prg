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
#include 'hbdocdef.ch'
class FileBase from FileMan



    DATA nOpenMode        // Holds the value to use when opening the file
    DATA nCreateMode      // Holds the value to use when creating the file
    DATA nDosHandle       // Holds the DOS file handle for this



    DATA nEndOfFile       // Holds the last byte value in the file
    DATA nSkipLength      // This holds the default skpping length of 1
    DATA cName            // This holds the name of the file being worked on
    DATA nPosition      // This holds the position in the file at
    DATA lAtBottom      // This is a value to show if at bottom of file
    DATA lAtTop         // This is a value to show if at top of file

    method new(cname)          // This is the constructor for the file
    method fopen()          // This opens the specified file
    method closefile()         // This closes the specified file
    method fskip(n)          // Moves the byte pointer within the file
    method fwrite(c)         // Write passed data to file w/ len() == nSkipLenght
    method retrieve()         // Returns the contents of the file at current pos
    method fgoTop()         // Move the byte pointer to the top of the file
    method fgoBottom()      // Move the byte pointer to the bottom of the file
    method fgoto()          // Moves the byte pointer to a specific location
    method create()
    message fappend method fappendByte(cByte)
    message BuffGet method BufferGet(lDirection)
    method skip(nRecord)          // Moves the byte pointer within the file
    method write(cChar)         // Write passed data to file w/ len() == nSkipLenght
    method goTop()         // Move the byte pointer to the top of the file
    method goBottom()     // Move the byte pointer to the bottom of the file
    method goto(nValue)          // Moves the byte pointer to a specific location
    method open()
    message append method appendLine(cline)

endclass

/* Method:  Init/New
   Params:  N/A
   Returns: Self
   Purpose: Constructor
*/
method new( cName ) class FileBase
    super:new()
  // first thing to do is check to see if there is a valid file

  ::nSkipLength   := 1
  ::nOpenMode     := 2   // Mode for which to open the file
  ::nCreateMode   := 0   // Mode for which to create the file

  ::cName := cName

  return( self )

/* Method:  skip( <nRecords> )
   Params:  <nRecords>
   Returns: Self
   Purpose: This method moves the file's byte pointer <nRecords> position
            from the current location.  The actualy movement is determined
            on the value of ::nSkipLength which holds the skipping base.
            This class's purpose is to do one byte movements.
*/
method fskip( nRecords )            class FileBase

  DEFAULT nRecords TO 1

  if ::noDosError() .and. ::nDosHandle > 0
    fseek( ::nDosHandle, (::nSkipLength * nRecords ), 1 )
    ::nLastDosMessage := ferror()
    ::nPosition := fseek( ::nDosHandle, 0, 1 )
    do case
    case ::nPosition == ::nEndOfFile
      ::lAtBottom := pTRUE
      ::lAtTop    := pFALSE
    case ::nPosition <= 1
      ::lAtTop    := pTRUE
      ::lAtBottom := pFALSE
    otherwise
      ::lAtBottom := ::lAtTop := pFALSE
    endcase
  endif

  return( self )

/* Method:  gotop()
   Params:  N/A
   Returns: Self
   Purpose: Move the byte pointer to the top of the file
*/
method fgotop()                     class FileBase

  if ::noDosError() .and. ::nDosHandle > 0
    ::nPosition := fseek( ::nDosHandle, 0, 0 )
    ::nLastDosMessage := ferror()
    ::lAtTop    := pTRUE
    ::lAtBottom := pFALSE
  endif

  return( self )

/* Method:  gobottom()
   Params:  N/A
   Returns: Self
   Purpose: Move hte byte pointer of the file to tbe bottom.
*/
method fgoBottom()                  class FileBase

  if ::noDosError() .and. ::nDosHandle > 0
    ::nPosition := fseek( ::nDosHandle, 0, 2 )
    ::nLastDosMessage := ferror()
    ::lAtTop    := pFALSE
    ::lAtBottom := pTRUE
  endif

  return( self )

/* Method:  close()
   Params:  N/A
   Returns: Self
   Purpose: To close the file
*/
method closefile()                     class FileBase

  if ::noDosError() .and. ::nDosHandle > 0
    fclose( ::nDosHandle )
    ::nLastDosMessage := ferror()
    ::delItem( ::nDosHandle )
    ::lAtTop := ::lAtBottom := pFALSE
    ::nPosition := 0
  endif

  return( self )

/* Method:  retrieve
   Params:  N/A
   Returns: <cChar>
   Purpose: To return the contents of the file at the current position based
            on the length of ::nSkipLength.
*/
method retrieve()                  class FileBase

  local cReturn // as char
  local nMoved  // as int

  if ::noDosError() .and. ::nDosHandle > 0
    cReturn := space( ::nSkipLength )
    nMoved := fread( ::nDosHandle, @cReturn, ::nSkipLength )
    ::nLastDosMessage := ferror()
    fseek( ::nDosHandle, -(nMoved), 1 )  // Re-position the pointer
  endif

  return( cReturn )

/* Method:  write(<cChar>)
   Params:  <cChar>
   Returns: Self
   Purpose: To write out to the contents of the file the value in the
            parameter <cChar>.
*/
method fwrite( cChar )              class FileBase

  if ::noDosError() .and. ::nDosHandle > 0
    IF cChar IS  pCHARACTER 
      fwrite( ::nDosHandle, cChar, 1 )
      ::nLastDosMessage := ferror()
      if ::noDosError()
        fseek( ::nDosHandle, ::nPosition, 0 )  // Re-position the pointer
      endif
    endif
  endif

  return( self )

/* Method:  goto(<nRecord>)
   Params:  <nRecord>       The record byte to move to
   Returns: Self
   Purpose: This method moves the byte marker to the <nRecord> position
            within the file.  It is also based on the value stored to the
            ::nSkipLength instance variable
*/
method fgoto( nValue )              class FileBase

  if ::noDosError() .and. ::nDosHandle > 0
    IF nValue IS  pNUMERIC
      if nValue > 0 .and. ;
         (nValue * ::nSkipLength) <= ::nEndOfFile
        fseek( ::nDosHandle, (nValue * ::nSkipLength), 0 )
        ::nLastDosMessage := ferror()
        ::nPosition := fseek( ::nDosHandle, 0, 1 )
        do case
        case ::nPosition == ::nEndOfFile
          ::lAtBottom := pTRUE
          ::lAtTop    := pFALSE
        case ::nPosition <= 1
          ::lAtTop    := pTRUE
          ::lAtBottom := pFALSE
        otherwise
          ::lAtBottom := ::lAtTop := pFALSE
        endcase
      endif
    endif
  endif

  return( ::nPosition )

/* Method:  create()
   Params:  N/A
   Returns: Self
   Purpose: Creates the specified file with the proper access code
*/
method Create()                    class FileBase

  local nFile // as int

  if ::noDosError()
    nFile := fcreate( ::cName, ::nCreateMode )
    ::nLastDosMessage := ferror()
    if ::noDosError()  // No Error
      fclose( nFile )        // Close the file
      ::fopen()               // Re-open the file
    endif
  endif

  return( self )

/* Method:  open()
   Params:  N/A
   Returns: Self
   Purpose: Opens the file with the proper access code
*/
method fopen()                      class FileBase

  if ::noDosError()
    ::nDosHandle := ::openfile( ::cName, ::nOpenMode )
    ::nEndOfFile := fseek( ::nDosHandle, 0, 2 )
    ::nPosition  := fseek( ::nDosHandle, 0, 0 )
    ::lAtTop     := pTRUE
    ::lAtBottom  := pFALSE
  endif

  return( self )

method fappendByte(cByte)           class FileBase

  DEFAULT cByte to ""

  if !empty( cByte )                                 // Valid line
    if ::noDosError() .and. ::nDosHandle > 0   // No error
      fseek(::nDosHandle, 0, 2 )
      fwrite(::nDosHandle, cByte, 1 )
      ::nEndOfFile  := fseek(::nDosHandle, 0, 2 )
      ::nPosition   := fseek(::nDosHandle, -(len(cByte)), 2 )
      ::nSkipLength := len( cByte )
      ::lAtBottom   := ::lAtTop := pFALSE
    endif
  endif

  return( self )

// End of File: FFile1.prg
method open() class FileBase 

  Self:nDosHandle        := Self:openfile(::cName, ::nOpenMode)
  ::nEndOfFile  := fseek(Self:nDosHandle, 0, 2)
  fseek(Self:nDosHandle, 0, 0)
  ::nSkipLength := Self:Buffget()
  ::lAtTop      := pTRUE
  ::lAtBottom   := pFALSE
  ::nHan:=  Self:nDosHandle
  return( self )

/* Method:  gotop()
   Params:  N/A
   Returns: Self
   Purpose: Move the byte pointer to the top of the file
*/
method gotop() class FileBase 

  if Self:noDosError() .and. Self:nDosHandle > 0
    ::fgotop()
    ::nSkipLength := Self:Buffget()
  endif

  return( self )

/* Method:  gobottom()
   Params:  N/A
   Returns: Self
   Purpose: Move hte byte pointer of the file to tbe bottom.
*/
method goBottom() class FileBase 

  local cBuffer            // as char
  local lWithCRLF := pFALSE// as logical

  if Self:noDosError() .and. Self:nDosHandle > 0
    ::fgobottom()
    // Now, back off from the end one line length and set the marker
    cBuffer := space( pBUFFER_LENGTH )
    fseek( Self:nDosHandle, -( pBUFFER_LENGTH ), 2 )
    fread( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
    if right(cBuffer,2) == pCRLF  // We need to remove this extra one!
      cBuffer := left(cBuffer, len(cBuffer)-2)
      lWithCRLF := pTRUE
    endif
    cBuffer := substr( cBuffer, rat( pCRLF, cBuffer )+2 )
    ::nSkipLength := len( cBuffer ) + if( lWithCRLF, 2, 0 )
    ::nposition := fseek( Self:nDosHandle, -(len(cBuffer)), 2 )
    if lWithCRLF
      ::nposition := fseek( Self:nDosHandle, -2, 1 )
    endif
  endif

  return( self )

/* Method:  close()
   Params:  N/A
   Returns: Self
   Purpose: To close the file
*/
method fclose()    class FileBase 

  if Self:noDosError() .and. Self:nDosHandle > 0
    fclose( Self:nDosHandle )
    Self:nLastDosMessage := ferror()
    Self:delItem( Self:nDosHandle )
    Self:lAtTop := Self:lAtBottom := pFALSE
    Self:nPosition := 0
  endif

  return( self )

/* Method:  write(<cChar>)
   Params:  <cChar>
   Returns: Self
   Purpose: To write out to the contents of the file the value in the
            parameter <cChar>.
*/
method write( cChar ) class FileBase 

  if Self:noDosError() .and. Self:nDosHandle > 0
    IF cChar IS  pCHARACTER
      if cChar > Self:nSkipLength   // we are going to truncate for now...
        fwrite( Self:nDosHandle, cChar, Self:nSkipLength )
      else
        fwrite( Self:nDosHandle, cChar, len(cChar) )
      endif
      fseek( Self:nDosHandle, ::nposition, 0 )
      Self:nLastDosMessage := ferror()
      if Self:noDosError()
        fseek( Self:nDosHandle, Self:nPosition, 0 )  // Re-position the pointer
      endif
    endif
  endif

  return( self )

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
method Buffget(lForward)   class FileBase 

  local cBuffer            // as char
  local nLocation          // as int
  local nRead              // as int
  local lWithCRLF := pFALSE// as logical

  DEFAULT lForward TO pTRUE

  if !lForward

    nRead   := fseek( Self:nDosHandle, ;
                      -(if( ::nposition < pBUFFER_LENGTH, ;
                            ::nposition, ;
                            pBUFFER_LENGTH )), ;
                      1 )  // rewind backwards

    cBuffer := space( ::nposition - nRead )
    fread( Self:nDosHandle, @cBuffer, (::nposition - nRead) )

    if right( cBuffer, 2 ) == pCRLF  // with line already
      cBuffer   := left( cBuffer, len(cBuffer)-2 )
      lWithCRLF := pTRUE
    endif
    nLocation := len(cBuffer) - (rat( pCRLF, cBuffer ))

  else
    cBuffer := space( pBUFFER_LENGTH )
    nRead := fread( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
    fseek( Self:nDosHandle, -( if( nRead < pBUFFER_LENGTH, nRead, ;
                            pBUFFER_LENGTH ) ), 1 )        // Rewind

    // Now, parse the string. and file

    nLocation := at( pCRLF, cBuffer )

    // Now, if there is NO CRLF in the buffer and if the value of the
    // number of bytes read is less than the buffer length, then we
    // have an end of file condition.
    if nLocation == 0 .and. ( nRead < pBUFFER_LENGTH )
      // If so, then set the appropriate flags accordingly.
      ::lAtBottom := pTRUE
      ::lAtTop    := pFALSE
    endif
  endif

  return( nLocation )

/* Method:  appendLine( <cLine )
   Params:  <cLine>         Character line to append
   Returns: Self
   Purpose: To append a blank CRLF delimited line at the end of the file.
            If <cLine> is not passed or if it an empty line with 0 bytes
            in length, the function will not operate.
*/
method appendLine( cLine ) class FileBase 

  DEFAULT cLine to ""

  if len( cLine ) == 0                              // Valid line
    if Self:noDosError() .and. Self:nDosHandle > 0   // No error
      if !(pCRLF $ cLine )                           // No CRLF, so add
        cLIne += pCRLF
      endif
      fseek(Self:nDosHandle, 0, 2 )
      fwrite(Self:nDosHandle, cLine )
      ::nEndOfFile  := fseek(Self:nDosHandle, 0, 2 )
      ::nposition   := fseek(Self:nDosHandle, -(len(cLine)), 2 )
      ::nSkipLength := len( cLine )
      ::lAtBottom   := ::lAtTop := pFALSE
    endif
  endif

  return( self )

/* Method:  skip( <nRecords> )
   Params:  <nRecords>
   Returns: Self
   Purpose: This method moves the file's byte pointer <nRecords> position
            from the current location.  The actualy movement is determined
            on the value of Self:nSkipLength which holds the skipping base.
            This class's purpose is to do one byte movements.
*/

method skip( nRecords )    class FileBase 

  local nCount := 0// as int

  DEFAULT nRecords TO 1

  // Here, we have to start looking for CHR(13)+CHR(10) character
  // combinations.  Once found, then we have to set the super class
  // values appropriately

  if Self:noDosError() .and. Self:nDosHandle > 0
    do case
    case nRecords > 0   // It's positive movement
      while nCount++ != nRecords
        ::fskip()
        ::nSkipLength := Self:Buffget()
      enddo

    case nRecords < 0   // It's negative movement
      while nCount-- != nRecords
        ::nSkipLength := Self:Buffget( pFALSE )
        ::fskip(-1)
      enddo

    endcase
  endif

  return( self )

/* Method:  goto(<nRecord>)
   Params:  <nRecord>       The record byte to move to
   Returns: Self
   Purpose: This method moves the byte marker to the <nRecord> position
            within the file.  It is also based on the value stored to the
            Self:nSkipLength instance variable
*/
method goto( nValue )      class FileBase 

  local cLine     := ""   // as char
  local nCount    := 0    // as int
  local lContinue := pTRUE// as logical
  local cBuffer           // as char

  DEFAULT nValue TO 0

  if Self:noDosError() .and. Self:nDosHandle > 0
    IF nValue IS  pNUMERIC
      if nValue > 0  // o.k. so far
        fseek( Self:nDosHandle, 0, 0 )  // start at the top
        while lContinue
          cBuffer := space( pBUFFER_LENGTH )
          lContinue := ( fread( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH ) == ;
                         pBUFFER_LENGTH )
          cBuffer := cLine + cBuffer
          while pCRLF $ cBuffer
            if ++nCount == nValue
              lContinue := pFALSE
              exit
            endif
            cBuffer := substr( cBuffer, at( pCRLF, cBuffer )+2 )
          enddo
          cLine := cBuffer
        enddo
        if nCount == nValue  // We have a match
          fseek( Self:nDosHandle, -( pBUFFER_LENGTH ), 1 )  // Back off from here
          ::nposition := fseek( Self:nDosHandle, ;
                                      (pBUFFER_LENGTH - len( cBuffer )), ;
                                      1 ) // Move
          ::nSkipLength := Self:Buffget()
        else
          fseek( Self:nDosHandle, ::nposition, 0 )
          nCount := 0
        endif
      endif
    endif
  endif

  return( nCount )

// End of File: FFile2.prg

method BufferGet(lForward)   Class FileBase

  local cBuffer            // as char
  local nLocation          // as int
  local nRead              // as int
  local lWithCRLF := pFALSE// as logical

  DEFAULT lForward TO pTRUE

  if !lForward

    nRead   := fseek( Self:nDosHandle, ;
                      -(if( ::nposition < pBUFFER_LENGTH, ;
                            ::nposition, ;
                            pBUFFER_LENGTH )), ;
                      1 )  // rewind backwards

    cBuffer := space( ::nposition - nRead )
    fread( Self:nDosHandle, @cBuffer, (::nposition - nRead) )

    if right( cBuffer, 2 ) == pCRLF  // with line already
      cBuffer   := left( cBuffer, len(cBuffer)-2 )
      lWithCRLF := pTRUE
    endif
    nLocation := len(cBuffer) - (rat( pCRLF, cBuffer ))

  else
    cBuffer := space( pBUFFER_LENGTH )
    nRead := fread( Self:nDosHandle, @cBuffer, pBUFFER_LENGTH )
    fseek( Self:nDosHandle, -( if( nRead < pBUFFER_LENGTH, nRead, ;
                            pBUFFER_LENGTH ) ), 1 )        // Rewind

    // Now, parse the string. and file

    nLocation := at( pCRLF, cBuffer )

    // Now, if there is NO CRLF in the buffer and if the value of the
    // number of bytes read is less than the buffer length, then we
    // have an end of file condition.
    if nLocation == 0 .and. ( nRead < pBUFFER_LENGTH )
      // If so, then set the appropriate flags accordingly.
      ::lAtBottom := pTRUE
      ::lAtTop    := pFALSE
    endif
  endif

  return( nLocation )

