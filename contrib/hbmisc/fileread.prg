/* Harbour Project source code
   A class that reads a file one line at a time
   http://harbour-project.org/
   Donated to the public domain on 2001-04-03 by David G. Holm <dholm@jsd-llc.com>
 */

#include "hbclass.ch"

#include "fileio.ch"

#define oF_ERROR_MIN          1
#define oF_CREATE_OBJECT      1
#define oF_OPEN_FILE          2
#define oF_READ_FILE          3
#define oF_CLOSE_FILE         4
#define oF_ERROR_MAX          4
#define oF_DEFAULT_READ_SIZE  4096

CREATE CLASS TFileRead

   VAR cFile                   // The filename
   VAR nHan                    // The open file handle
   VAR lEOF                    // The end of file reached flag
   VAR nError                  // The current file error code
   VAR nLastOp                 // The last operation done (for error messages)
   VAR cBuffer                 // The readahead buffer
   VAR nReadSize               // How much to add to the readahead buffer on each read from the file

   METHOD New( cFile, nSize )  // Create a new class instance
   METHOD Open( nMode )        // Open the file for reading
   METHOD Close()              // Close the file when done
   METHOD ReadLine()           // Read a line from the file
   METHOD Name()               // Retunrs the file name
   METHOD IsOpen()             // Returns .T. if file is open
   METHOD MoreToRead()         // Returns .T. if more to be read
   METHOD Error()              // Returns .T. if error occurred
   METHOD ErrorNo()            // Returns current error code
   METHOD ErrorMsg( cText )    // Returns formatted error message

   PROTECTED:

   METHOD EOL_pos()

END CLASS

METHOD New( cFile, nSize ) CLASS TFileRead

   IF nSize == NIL .OR. nSize < 1
      // The readahead size can be set to as little as 1 byte, or as much as
      // 65535 bytes, but venturing out of bounds forces the default size.
      nSize := oF_DEFAULT_READ_SIZE
   ENDIF

   ::cFile     := cFile             // Save the file name
   ::nHan      := F_ERROR           // It's not open yet
   ::lEOF      := .T.               // So it must be at EOF
   ::nError    := 0                 // But there haven't been any errors
   ::nLastOp   := oF_CREATE_OBJECT  // Because we just created the class
   ::cBuffer   := ""                // and nothing has been read yet
   ::nReadSize := nSize             // But will be in this size chunks

   RETURN Self

METHOD Open( nMode ) CLASS TFileRead

   IF ::nHan == F_ERROR
      // Only open the file if it isn't already open.
      IF nMode == NIL
         nMode := FO_READ + FO_SHARED   // Default to shared read-only mode
      ENDIF
      ::nLastOp := oF_OPEN_FILE
      ::nHan := FOpen( ::cFile, nMode )   // Try to open the file
      IF ::nHan == F_ERROR
         ::nError := FError()       // It didn't work
         ::lEOF   := .T.            // So force EOF
      ELSE
         ::nError := 0              // It worked
         ::lEOF   := .F.            // So clear EOF
      ENDIF
   ELSE
      // The file is already open, so rewind to the beginning.
      IF FSeek( ::nHan, 0 ) == 0
         ::lEOF := .F.              // Definitely not at EOF
      ELSE
         ::nError := FError()       // Save error code if not at BOF
      ENDIF
      ::cBuffer := ""               // Clear the readahead buffer
   ENDIF

   RETURN Self

METHOD ReadLine() CLASS TFileRead

   LOCAL cLine := ""
   LOCAL nPos

   ::nLastOp := oF_READ_FILE

   IF ::nHan == F_ERROR
      ::nError := F_ERROR           // Set unknown error if file not open
   ELSE
      // Is there a whole line in the readahead buffer?
      nPos := ::EOL_pos()
      DO WHILE ( nPos <= 0 .OR. nPos > Len( ::cBuffer ) - 3 ) .AND. ! ::lEOF
         // Either no or maybe, but there is possibly more to be read.
         // Maybe means that we found either a CR or an LF, but we don't
         // have enough characters to discriminate between the three types
         // of end of line conditions that the class recognizes (see below).
         cLine := FReadStr( ::nHan, ::nReadSize )
         IF Empty( cLine )
            // There was nothing more to be read. Why? (Error or EOF.)
            ::nError := FError()
            IF ::nError == 0
               // Because the file is at EOF.
               ::lEOF := .T.
            ENDIF
         ELSE
            // Add what was read to the readahead buffer.
            ::cBuffer += cLine
         ENDIF
         // Is there a whole line in the readahead buffer yet?
         nPos := ::EOL_pos()
      ENDDO
      // Is there a whole line in the readahead buffer?
      IF nPos <= 0
         // No, which means that there is nothing left in the file either, so
         // return the entire buffer contents as the last line in the file.
         cLine := ::cBuffer
         ::cBuffer := ""
      ELSE
         // Yes. Is there anything in the line?
         IF nPos > 1
            // Yes, so return the contents.
            cLine := Left( ::cBuffer, nPos - 1 )
         ELSE
            // No, so return an empty string.
            cLine := ""
         ENDIF
         // Deal with multiple possible end of line conditions.
         DO CASE
         CASE SubStr( ::cBuffer, nPos, 3 ) == Chr( 13 ) + Chr( 13 ) + Chr( 10 )
            // It's a messed up DOS newline (such as that created by a program
            // that uses "\r\n" as newline when writing to a text mode file,
            // which causes the '\n' to expand to "\r\n", giving "\r\r\n").
            nPos += 3
         CASE SubStr( ::cBuffer, nPos, 2 ) == Chr( 13 ) + Chr( 10 )
            // It's a standard DOS newline
            nPos += 2
         OTHERWISE
            // It's probably a Mac or Unix newline
            nPos++
         ENDCASE
         ::cBuffer := SubStr( ::cBuffer, nPos )
      ENDIF
   ENDIF

   RETURN cLine

METHOD EOL_pos() CLASS TFileRead

   LOCAL nCRpos, nLFpos, nPos

   // Look for both CR and LF in the file read buffer.
   nCRpos := At( Chr( 13 ), ::cBuffer )
   nLFpos := At( Chr( 10 ), ::cBuffer )
   DO CASE
   CASE nCRpos == 0
      // If there's no CR, use the LF position.
      nPos := nLFpos
   CASE nLFpos == 0
      // If there's no LF, use the CR position.
      nPos := nCRpos
   OTHERWISE
      // If there's both a CR and an LF, use the position of the first one.
      nPos := Min( nCRpos, nLFpos )
   ENDCASE

   RETURN nPos

METHOD Close() CLASS TFileRead

   ::nLastOp := oF_CLOSE_FILE
   ::lEOF := .T.
   // Is the file already closed.
   IF ::nHan == F_ERROR
      // Yes, so indicate an unknown error.
      ::nError := F_ERROR
   ELSE
      // No, so close it already!
      FClose( ::nHan )
      ::nError := FError()
      ::nHan   := F_ERROR           // The file is no longer open
      ::lEOF   := .T.               // So force an EOF condition
   ENDIF

   RETURN Self

METHOD Name() CLASS TFileRead

   // Returns the filename associated with this class instance.

   RETURN ::cFile

METHOD IsOpen() CLASS TFileRead

   // Returns .T. if the file is open.

   RETURN ::nHan != F_ERROR

METHOD MoreToRead() CLASS TFileRead

   // Returns .T. if there is more to be read from either the file or the
   // readahead buffer. Only when both are exhausted is there no more to read.

   RETURN ! ::lEOF .OR. ! Empty( ::cBuffer )

METHOD Error() CLASS TFileRead

   // Returns .T. if an error was recorded.

   RETURN ::nError != 0

METHOD ErrorNo() CLASS TFileRead

   // Returns the last error code that was recorded.

   RETURN ::nError

METHOD ErrorMsg( cText ) CLASS TFileRead

   STATIC sc_cAction := { "on", "creating object for", "opening", "reading from", "closing" }

   LOCAL cMessage, nTemp

   // Has an error been recorded?
   IF ::nError == 0
      // No, so report that.
      cMessage := "No errors have been recorded for " + ::cFile
   ELSE
      // Yes, so format a nice error message, while avoiding a bounds error.
      IF ::nLastOp < oF_ERROR_MIN .OR. ::nLastOp > oF_ERROR_MAX
         nTemp := 1
      ELSE
         nTemp := ::nLastOp + 1
      ENDIF
      cMessage := iif( Empty( cText ), "", cText ) + "Error " + hb_ntos( ::nError ) + " " + sc_cAction[ nTemp ] + " " + ::cFile
   ENDIF

   RETURN cMessage
