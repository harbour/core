/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FCLASS.PRG Fileman class for hbdoc
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


class FileMan



     data aDosHandles  // Holds an array of dos handles and names

     data nHan
     data nLastDosMessage      // Holds the DOS error level of last operation

     method addItem(nDos, cFile , cPath)           // Adds an item to the array of handles
     method delItem(xitem)           // Deletes an item from the array of handles

    method new()              // The constructor for this class
    method closeAll()          // Closes all of those files
    method rewindAll()         // Positions the file pointer for each file
    method writeAll()          // Performs hard-write of all

     method getFileName(cfile)       // Obtains the name of the file based on ID
     method getFileId(nid)         // Obtains the ID based on file name
     method getFilePath(xItem)       // Obtains file path based on either ID or name

     method noDosError()        // Returns a logical true/false

     method openfile( cFile, nMethod )              // Opens the specified file and sets error
    method Buffget(ld) virtual

endclass

/* Method:  Init/New
   Params:  N/A
   Returns: Self
   Purpose: Constructor
*/

method new()  class FileMan

  if ::aDosHandles == NIL   // has yet to be initialized
    ::aDosHandles := {}
  endif

  if ::nLastDosMessage == NIL  // has yet to be initialized
    ::nLastDosMessage := 0
  endif

  return( self )

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

method closeAll()  class FileMan

  if ::nLastDosMessage == 0
    aeval( ::aDosHandles, {|aFile| fclose( aFile[pDOS_HANDLE] )} )
  endif

  return( self )

/* Method:  ::rewindAll()
   Params:  N/A
   Returns: Self
   Purpose: To go through the stack of opened file handles and places the
            file pointer to the top of each file, one at a time.  Since
            Since this is a global operation, it will not check on the
            status of the error message on each pass but will at the
            start of the evaluation.

*/
method rewindAll()  class FileMan

  if ::nLastDosMessage == 0
    aeval( ::aDosHandles, {|aFile| fseek( aFile[pDOS_HANDLE], 0, 0 )} )
  endif

  return( self )

/* Method:  ::writeAll()
   Params:  N/A
   Returns: Self
   Purpose: To go through the stack of opened file handles and writes each
            file, one at a time.  Since this is a global operation, it
            will not check on the status of the error message on each
            pass but will at the start of the evaluation.

*/
method writeAll()  class FileMan

  if ::nLastDosMessage == 0
    aeval( ::aDosHandles, {|aFile| fwrite( aFile[pDOS_HANDLE], "", 0 )} )
  endif

  return( self )

/* Method:  ::getFileName( <nId> )
   Params:  <nId>           DOS File handle / ID
   Returns: <cName>         File name store with that ID handle
   Purpose: This method will return the file's name found
            within the table of this class.
*/
method getFileName( nId )   class FileMan// Obtains the name of the file based on ID

  local cName := "" // as char
  local nPosition   // as int

  if ::nLastDosMessage == 0
    IF nId IS pNUMERIC
      nPosition := ascan( ::aDosHandles, ;
                          {|aFile| nId == aFile[pDOS_HANDLE]} )
      if nPosition != 0
        cName := ::aDosHandles[ nPosition, pDOS_FILE ]
      endif
    endif
  endif

  return( cName )

/* Method:  ::getFileId( <cName> )
   Params:  <cName>         File names used to store item to stack
   Returns: <nId>           DOS File handle or ID associated with name
   Purpose: This method will return the file's ID or DOS handle found
            within the table of this class.
*/
method getFileId( cName )    class FileMan// Obtains the ID based on file name

  local nId := 0  // as int
  local nPosition // as int

  if ::nLastDosMessage == 0
    IF cName IS pCHARACTER
      nPosition := ascan( ::aDosHandles, ;
                          {|aFile| cName == aFile[pDOS_FILE]} )
      if nPosition != 0
        nId := ::aDosHandles[ nPosition, pDOS_HANDLE ]
      endif
    endif
  endif

  return( nId )

/* Method:  ::getFilePath( <xItem> )
   Params:  <xItem>         DOS File handle / ID or stored file name
   Returns: <cPath>         Associated file path
   Purpose: This method will return the associated DOS path for either the
            given file name or DOS file handle / ID.  If there is no file
            path or if there is an error with the method, the return value
            will be a NULL character byte.
*/
method getFilePath( xItem )  class FileMan // Obtains file path based on either ID or name

  local cPath := "" // as char
  local nPosition   // as int

  if ::nLastDosMessage == 0
    do case
    case  (xItem IS pCHARACTER)  // we've got the file name
      nPosition := ascan( ::aDosHandles, ;
                          {|aFile| xItem == aFile[pDOS_FILE]} )
      if nPosition != 0
        cPath := ::aDosHandles[ nPosition, pDOS_PATH ]
      endif

    case (xItem IS pNUMERIC)    // we've got the file path
      nPosition := ascan( ::aDosHandles, ;
                          {|aFile| xItem == aFile[pDOS_HANDLE]} )
      if nPosition != 0
        cPath := ::aDosHandles[ nPosition, pDOS_PATH ]
      endif

    endcase
  endif

  return( cPath )

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
method addItem( nDos, cFile, cPath )  class FileMan

  DEFAULT cPath TO ""

  if ::nLastDosMessage == 0
    aadd( ::aDosHandles, {nDos, cFile, cPath} )
  endif

  return( self )

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

method delItem( xItem )  class FileMan

  local nPosition          // as int
  local lSuccess  := pTRUE // as logical

  // if xItem is N/Numeric, then seek on first element;
  // if xItem is C/Character, then seek on second element

  if ::nLastDosMessage == 0    // No DOS error!
    do case
    case (xItem IS pNUMERIC)   // It's a DOS file handle
      nPosition := ascan( ::aDosHandles, ;
                          {|aItem| xItem == aItem[pDOS_HANDLE]} )
      if nPosition == 0
        // Don't remove and set the return value of the function
        lSuccess := pFALSE
      else
        // Since we have a position, remove from the table and keep the
        // default return value
        adel( ::aDosHandles, nPosition )
        asize( ::aDosHandles, len( ::aDosHandles ) - 1 )
      endif

    case (xItem IS pCHARACTER ) // It's a file name
      nPosition := ascan( ::aDosHandles, ;
                          {|aItem| xItem == aItem[pDOS_FILE]} )
      if nPosition == 0
        // Don't remove and set the return value of the function
        lSuccess := pFALSE
      else
        // Since we have a position, remove from the table and keep the
        // default return value
        adel( ::aDosHandles, nPosition )
        asize( ::aDosHandles, len( ::aDosHandles ) - 1 )
      endif

    otherwise
      // Invalid data passed to method
      lSuccess := pFALSE

    endcase
  else
    lSuccess := pFALSE

  endif

  return( lSuccess )

/* Method:  noDosError()
   Params:  N/A
   Returns: <lNoError>
   Purpose: To return a logical true (.T.) if there is no existing error
            state within the system
*/

method noDosError()  class FileMan

  return( ::nLastDosMessage == 0 )

/* Method:  open()
   Params:  N/A
   Returns: <nDosHandle>
   Purpose: This method acutally opens the file specified by the parameter
            <cFile> with the open mode of <nMethod>.  Each file object
            should carry this information locally and use this method only
            to update the internal table.

*/
method openfile( cFile, nMethod )  class FileMan

  local nFileHandle // as int
  local cFilename   // as char
  local cPath       // as char

  DEFAULT nMethod TO 0

  nFileHandle := fopen(cFile, nMethod)  // opens the file
  if !empty( ferror() )                 // There was an error in opening
    ::nLastDosMessage := ferror()
    nFileHandle := -1
  else
    cFileName := DOSFILENAME( cFile )
    cPath     := strtran( cFile, cFileName, "" )
    ::addItem( nFileHandle, cFileName, cPath )
  endif
  ::nHan:=nFileHandle
  return( nFileHandle )

// End of File: FClass1.prg
