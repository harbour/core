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

 */

#include "hbclass.ch"
#include 'common.ch'
#include 'hbdocdef.ch'

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Class FileMan
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
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

   RETURN ( self )

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

   RETURN ( self )

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
      AEVAL( ::aDosHandles, { | aFile | FSEEK( aFile[ pDOS_HANDLE ], 0, 0 ) } )
   ENDIF

   RETURN ( self )

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

   RETURN ( self )

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
      IF nId IS pNUMERIC
         nPosition := ASCAN( ::aDosHandles, ;
                             { | aFile | nId == aFile[ pDOS_HANDLE ] } )
         IF nPosition != 0
            cName := ::aDosHandles[ nPosition, pDOS_FILE ]
         ENDIF
      ENDIF
   ENDIF

   RETURN ( cName )

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
      IF cName IS pCHARACTER
         nPosition := ASCAN( ::aDosHandles, ;
                             { | aFile | cName == aFile[ pDOS_FILE ] } )
         IF nPosition != 0
            nId := ::aDosHandles[ nPosition, pDOS_HANDLE ]
         ENDIF
      ENDIF
   ENDIF

   RETURN ( nId )

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
         CASE ( xItem IS pCHARACTER )   // we've got the file name
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aFile | xItem == aFile[ pDOS_FILE ] } )
            IF nPosition != 0
               cPath := ::aDosHandles[ nPosition, pDOS_PATH ]
            ENDIF

         CASE ( xItem IS pNUMERIC )     // we've got the file path
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aFile | xItem == aFile[ pDOS_HANDLE ] } )
            IF nPosition != 0
               cPath := ::aDosHandles[ nPosition, pDOS_PATH ]
            ENDIF

      ENDCASE
   ENDIF

   RETURN ( cPath )

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

   RETURN ( self )

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
   LOCAL lSuccess  := pTRUE                // as logical

   // if xItem is N/Numeric, then seek on first element;
   // if xItem is C/Character, then seek on second element

   IF ::nLastDosMessage == 0            // No DOS error!
      DO CASE
         CASE ( xItem IS pNUMERIC )     // It's a DOS file handle
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aItem | xItem == aItem[ pDOS_HANDLE ] } )
            IF nPosition == 0
               // Don't remove and set the return value of the function
               lSuccess := pFALSE
            ELSE
               // Since we have a position, remove from the table and keep the
               // default return value
               ADEL( ::aDosHandles, nPosition )
               ASIZE( ::aDosHandles, LEN( ::aDosHandles ) - 1 )
            ENDIF

         CASE ( xItem IS pCHARACTER )   // It's a file name
            nPosition := ASCAN( ::aDosHandles, ;
                                { | aItem | xItem == aItem[ pDOS_FILE ] } )
            IF nPosition == 0
               // Don't remove and set the return value of the function
               lSuccess := pFALSE
            ELSE
               // Since we have a position, remove from the table and keep the
               // default return value
               ADEL( ::aDosHandles, nPosition )
               ASIZE( ::aDosHandles, LEN( ::aDosHandles ) - 1 )
            ENDIF

         OTHERWISE
            // Invalid data passed to method
            lSuccess := pFALSE

      ENDCASE
   ELSE
      lSuccess := pFALSE

   ENDIF

   RETURN ( lSuccess )

   /* Method:  noDosError()
   Params:  N/A
   Returns: <lNoError>
   Purpose: To return a logical true (.T.) if there is no existing error
            state within the system
*/

METHOD noDosError() CLASS FileMan

   RETURN ( ::nLastDosMessage == 0 )

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
RETURN ( nFileHandle )

// End of File: FClass1.prg

*+ EOF: FCLASS1.PRG
