/* Donated to the public domain on 2001-04-03 by David G. Holm <dholm@jsd-llc.com> */

/* A class that reads a file one line at a time */

#include "hbclass.ch"

#include "fileio.ch"

#define O_F_ERROR_MIN           1
#define O_F_CREATE_OBJECT       1
#define O_F_OPEN_FILE           2
#define O_F_READ_FILE           3
#define O_F_CLOSE_FILE          4
#define O_F_ERROR_MAX           4
#define O_F_DEFAULT_READ_SIZE   4096

CREATE CLASS TFileRead

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

   VAR cFile                             // The filename
   VAR hFile                             // The open file handle
   VAR lEOF      INIT .T.                // The end of file reached flag
   VAR nError    INIT 0                  // The current file error code
   VAR nLastOp   INIT O_F_CREATE_OBJECT  // The last operation done (for error messages)
   VAR cBuffer   INIT ""                 // The readahead buffer
   VAR nReadSize                         // How much to add to the readahead buffer on each read from the file

   METHOD EOL_pos()

ENDCLASS

METHOD New( cFile, nSize ) CLASS TFileRead

   IF ! HB_ISNUMERIC( nSize ) .OR. nSize < 1
      // The readahead size can be set to as little as 1 byte, or as much as
      // 65535 bytes, but venturing out of bounds forces the default size.
      nSize := O_F_DEFAULT_READ_SIZE
   ENDIF

   ::cFile     := cFile              // Save the file name
   ::nReadSize := nSize              // But will be in this size chunks

   RETURN Self

METHOD Open( nMode ) CLASS TFileRead

   IF ::hFile == NIL
      // Only open the file if it isn't already open.
      ::nLastOp := O_F_OPEN_FILE
      IF ( ::hFile := hb_vfOpen( ::cFile, hb_defaultValue( nMode, FO_READ + FO_SHARED ) ) ) == NIL
         ::nError := FError()       // It didn't work
         ::lEOF   := .T.            // So force EOF
      ELSE
         ::nError := 0              // It worked
         ::lEOF   := .F.            // So clear EOF
      ENDIF
   ELSE
      // The file is already open, so rewind to the beginning.
      IF hb_vfSeek( ::hFile, 0 ) == 0
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
   LOCAL nRead

   ::nLastOp := O_F_READ_FILE

   IF ::hFile == NIL
      ::nError := NIL          // Set unknown error if file not open
   ELSE
      // Is there a whole line in the readahead buffer?
      nPos := ::EOL_pos()
      cLine := Space( ::nReadSize )
      DO WHILE ( nPos <= 0 .OR. nPos > Len( ::cBuffer ) - 3 ) .AND. ! ::lEOF
         // Either no or maybe, but there is possibly more to be read.
         // Maybe means that we found either a CR or an LF, but we don't
         // have enough characters to discriminate between the three types
         // of end of line conditions that the class recognizes (see below).
         IF ( nRead := hb_vfRead( ::hFile, @cLine, hb_BLen( cLine ) ) ) <= 0
            // There was nothing more to be read. Why? (Error or EOF.)
            ::nError := FError()
            IF ::nError == 0
               // Because the file is at EOF.
               ::lEOF := .T.
            ENDIF
         ELSE
            // Add what was read to the readahead buffer.
            ::cBuffer += hb_BLeft( cLine, nRead )
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

   // Look for both CR and LF in the file read buffer.
   LOCAL nCRpos := At( Chr( 13 ), ::cBuffer )
   LOCAL nLFpos := At( Chr( 10 ), ::cBuffer )
   LOCAL nPos

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

   ::nLastOp := O_F_CLOSE_FILE
   ::lEOF := .T.
   // Is the file already closed.
   IF ::hFile == NIL
      // Yes, so indicate an unknown error.
      ::nError := NIL
   ELSE
      // No, so close it already!
      hb_vfClose( ::hFile )
      ::nError := FError()
      ::hFile  := NIL               // The file is no longer open
      ::lEOF   := .T.               // So force an EOF condition
   ENDIF

   RETURN Self

// Returns the filename associated with this class instance.
METHOD Name() CLASS TFileRead
   RETURN ::cFile

// Returns .T. if the file is open.
METHOD IsOpen() CLASS TFileRead
   RETURN ::hFile != NIL

// Returns .T. if there is more to be read from either the file or the
// readahead buffer. Only when both are exhausted is there no more to read.
METHOD MoreToRead() CLASS TFileRead
   RETURN ! ::lEOF .OR. ! HB_ISNULL( ::cBuffer )

// Returns .T. if an error was recorded.
METHOD Error() CLASS TFileRead
   RETURN ::nError != 0

// Returns the last error code that was recorded.
METHOD ErrorNo() CLASS TFileRead
   RETURN ::nError

METHOD ErrorMsg( cText ) CLASS TFileRead

   STATIC sc_cAction := { ;
      "on", ;
      "creating object for", ;
      "opening", ;
      "reading from", ;
      "closing" }

   LOCAL cMessage, nTemp

   // Has an error been recorded?
   IF ::nError == 0
      // No, so report that.
      cMessage := "No errors have been recorded for " + ::cFile
   ELSE
      // Yes, so format a nice error message, while avoiding a bounds error.
      IF ::nLastOp < O_F_ERROR_MIN .OR. ::nLastOp > O_F_ERROR_MAX
         nTemp := 1
      ELSE
         nTemp := ::nLastOp + 1
      ENDIF
      cMessage := hb_defaultValue( cText, "" ) + "Error " + hb_ntos( ::nError ) + " " + sc_cAction[ nTemp ] + " " + ::cFile
   ENDIF

   RETURN cMessage
