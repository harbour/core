/* 
 * $Id$
 */

/*
 * Harbour Project source code:
 * A class that reads a file one line at a time
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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
 * ChangeLog:
 *
 * V 1.1    David G. Holm               Committed to CVS.
 * V 1.0    David G. Holm               Initial version.
 *
 */

#include "fileio.ch"

#define oF_ERROR_MIN          1
#define oF_CREATE_OBJECT      1
#define oF_OPEN_FILE          2
#define oF_READ_FILE          3
#define oF_CLOSE_FILE         4
#define oF_ERROR_MAX          4
#define oF_DEFAULT_READ_SIZE 4096

/*  $DOC$
 *  $FUNCNAME$
 *      TFileRead()
 *  $CATEGORY$
 *      Harbour Tools
 *  $ONELINER$
 *      Read a file one line at a time
 *  $SYNTAX$
 *      oFile := TFileRead():New( <cFileName> [, <nReadSize> ] )
 *  $ARGUMENTS$
 *      cFileName is the required name of the file to be read.
 *      nReadSize is the optional size to use when reading from the file.
 *      The default value is 4096 and the allowed range is 1 through 65535.
 *      Any value outside of this range causes the default value to be used.
 *  $RETURNS$
 *      An instance of the File Reader class
 *  $DESCRIPTION$
 *      TFileRead() is used to access a file one line at a time. You must
 *      specify the name of the file when an instance of the class is created.
 *      The class data should be considered private to the class.
 *      The class methods are as follows:
 *         New()              Creates a new instance of the TFileRead class.
 *         Open([<nFlags>])   Opens the file for reading. The optional nFlags
 *                            parameter can use any of the FOPEN() flags from
 *                            fileio.ch. The default is FO_READ + FO_SHARED.
 *                            Calling this method when the file is already
 *                            open causes the next ReadLine() to start over
 *                            from the beginning of the file.
 *         Close()            Closes the file.
 *         ReadLine()         Returns one line from the file, stripping the
 *                            newline characters. The following sequences are
 *                            treated as one newline: 1) CR CR LF; 2) CR LF;
 *                            3) LF; and 4) CR. Note: LF CR is 2 newlines.
 *         Name()             Returns the name of the file.
 *         IsOpen()           Returns .T. if the file is open.
 *         MoreToRead()       Returns .T. if there are more lines to be read
 *                            (think of it as an inverse EOF function).
 *         Error()            Returns .T. if an error has occurred.
 *         ErrorNo()          Returns the current error code.
 *         ErrorMsg([<cPre>]) Returns a formatted error message.
 *  $EXAMPLES$
 *      #ifdef __HARBOUR__
 *       #define NEW_LINE CHR( 10 )
 *      #else
 *       #define NEW_LINE CHR( 13 ) + CHR( 10 )
 *      #endif
 *      #include "fileio.ch"
 *      
 *      PROCEDURE Main( cFile )
 *      LOCAL oFile := TFileRead():New( cFile )
 *      
 *         oFile:Open()
 *         IF oFile:Error()
 *            QOUT( oFile:ErrorMsg( "FileRead: " ) )
 *         ELSE
 *            WHILE oFile:MoreToRead()
 *               OUTSTD( oFile:ReadLine() )
 *               OUTSTD( NEW_LINE )
 *            END WHILE
 *            oFile:Close()
 *         END IF
 *      QUIT
 *  $TESTS$
 *      See Examples
 *  $STATUS$
 *      C
 *  $COMPLIANCE$
 *      This is a new Harbour Tools class
 *  $SEEALSO$
 *      TClass
 *  $END$
 */

FUNCTION TFileRead()
STATIC s_oClass

   IF s_oClass == NIL
      s_oClass := TClass():New( "TFile" )  // New class
      s_oClass:AddClassData( "cFile" )     // The filename
      s_oClass:AddClassData( "nHan" )      // The open file handle
      s_oClass:AddClassData( "lEOF" )      // The end of file reached flag
      s_oClass:AddClassData( "nError" )    // The current file error code
      s_oClass:AddClassData( "nLastOp" )   // The last operation done (for error messages)
      s_oClass:AddClassData( "cBuffer" )   // The readahead buffer
      s_oClass:AddClassData( "nReadSize" ) // How much to add to the readahead buffer on
                                         // each read from the file

      s_oClass:AddMethod( "New",        @f_new() )       // Create a new class instance
      s_oClass:AddMethod( "Open",       @f_open() )      // Open the file for reading
      s_oClass:AddMethod( "Close",      @f_close() )     // Close the file when done
      s_oClass:AddMethod( "ReadLine",   @f_read() )      // Read a line from the file
      s_oClass:AddMethod( "Name",       @f_name() )      // Retunrs the file name
      s_oClass:AddMethod( "IsOpen",     @f_is_open() )   // Returns .T. if file is open
      s_oClass:AddMethod( "MoreToRead", @f_more() )      // Returns .T. if more to be read
      s_oClass:AddMethod( "Error",      @f_error() )     // Returns .T. if error occurred
      s_oClass:AddMethod( "ErrorNo",    @f_error_no() )  // Returns current error code
      s_oClass:AddMethod( "ErrorMsg",   @f_error_msg() ) // Returns formatted error message
      s_oClass:Create()
   END IF

RETURN s_oClass:Instance()

STATIC FUNCTION f_new( cFile, nSize )
LOCAL oSelf := Qself()
   IF nSize == NIL .OR. nSize < 1
      // The readahead size can be set to as little as 1 byte, or as much as
      // 65535 bytes, but venturing out of bounds forces the default size.
      nSize := oF_DEFAULT_READ_SIZE
   END IF
   oSelf:cFile     := cFile             // Save the file name
   oSelf:nHan      := -1                // It's not open yet
   oSelf:lEOF      := .T.               // So it must be at EOF
   oSelf:nError    := 0                 // But there haven't been any errors
   oSelf:nLastOp   := oF_CREATE_OBJECT  // Because we just created the class
   oSelf:cBuffer   := ""                // and nothing has been read yet
   oSelf:nReadSize := nSize             // But will be in this size chunks
RETURN oSelf

STATIC FUNCTION f_open( nMode )
LOCAL oSelf := Qself()
   IF oSelf:nHan == -1
      // Only open the file if it isn't already open.
      IF nMode == NIL
         nMode := FO_READ + FO_SHARED   // Default to shared read-only mode
      END IF
      oSelf:nLastOp := oF_OPEN_FILE
      oSelf:nHan := FOPEN( oSelf:cFile, nMode )   // Try to open the file
      IF oSelf:nHan == -1
         oSelf:nError := FERROR()       // It didn't work
         oSelf:lEOF   := .T.            // So force EOF
      ELSE
         oSelf:nError := 0              // It worked
         oSelf:lEOF   := .F.            // So clear EOF
      END IF
   ELSE
      // The file is already open, so rewind to the beginning.
      IF FSEEK( oSelf:nHan, 0 ) == 0
         oSelf:lEOF := .F.              // Definitely not at EOF
      ELSE
         oSelf:nError := FERROR()       // Save error code if not at BOF
      END IF
      oSelf:cBuffer := ""               // Clear the readahead buffer
   END IF
RETURN oSelf

STATIC FUNCTION f_read()
LOCAL oSelf := Qself()
LOCAL cLine := ""
LOCAL nPos
   oSelf:nLastOp := oF_READ_FILE
   IF oSelf:nHan == -1
      oSelf:nError := -1                // Set unknown error if file not open
   ELSE
      // Is there a whole line in the readahead buffer?
      nPos := f_EOL_pos( oSelf )
      WHILE ( nPos <= 0 .OR. nPos > LEN( oSelf:cBuffer ) - 3 ) .AND. !oSelf:lEOF
         // Either no or maybe, but there is possibly more to be read.
         // Maybe means that we found either a CR or an LF, but we don't
         // have enough characters to discriminate between the three types
         // of end of line conditions that the class recognizes (see below).
         cLine := FREADSTR( oSelf:nHan, oSelf:nReadSize )
         IF EMPTY( cLine )
            // There was nothing more to be read. Why? (Error or EOF.)
            oSelf:nError := FERROR()
            IF oSelf:nError == 0
               // Because the file is at EOF.
               oSelf:lEOF := .T.
            END IF
         ELSE
            // Add what was read to the readahead buffer.
            oSelf:cBuffer += cLine
            cLine := ""
         END IF
         // Is there a whole line in the readahead buffer yet?
         nPos := f_EOL_pos( oSelf )
      END WHILE
      // Is there a whole line in the readahead buffer?
      IF nPos <= 0
         // No, which means that there is nothing left in the file either, so
         // return the entire buffer contents as the last line in the file.
         cLine := oSelf:cBuffer
         oSelf:cBuffer := ""
      ELSE
         // Yes. Is there anything in the line?
         IF nPos > 1
            // Yes, so return the contents.
            cLine := LEFT( oSelf:cBuffer, nPos - 1 )
         ELSE
            // No, so return an empty string.
            cLine := ""
         END IF
         // Deal with multiple possible end of line conditions.
         DO CASE
            CASE SUBSTR( oSelf:cBuffer, nPos, 3 ) == CHR( 13 ) + CHR( 13 ) + CHR( 10 )
               // It's a messed up DOS newline (such as that created by a program
               // that uses "\r\n" as newline when writing to a text mode file,
               // which causes the '\n' to expand to "\r\n", giving "\r\r\n").
               nPos += 3
            CASE SUBSTR( oSelf:cBuffer, nPos, 2 ) == CHR( 13 ) + CHR( 10 )
               // It's a standard DOS newline
               nPos += 2
            OTHERWISE
               // It's probably a Mac or Unix newline
               nPos++
         END CASE
         oSelf:cBuffer := SUBSTR( oSelf:cBuffer, nPos )
      END IF
   END IF
RETURN cLine

STATIC FUNCTION f_EOL_pos( oFile )
LOCAL nCRpos, nLFpos, nPos
   // Look for both CR and LF in the file read buffer.
   nCRpos := AT( CHR( 13 ), oFile:cBuffer )
   nLFpos := AT( CHR( 10 ), oFile:cBuffer )
   DO CASE
      CASE nCRpos == 0
         // If there's no CR, use the LF position.
         nPos := nLFpos
      CASE nLFpos == 0
         // If there's no LF, use the CR position.
         nPos := nCRpos
      OTHERWISE
         // If there's both a CR and an LF, use the position of the first one.
         nPos := MIN( nCRpos, nLFpos )
   END CASE
RETURN nPos

STATIC FUNCTION f_close()
LOCAL oSelf := Qself()
   oSelf:nLastOp := oF_CLOSE_FILE
   oSelf:lEOF := .T.
   // Is the file already closed.
   IF oSelf:nHan == -1
      // Yes, so indicate an unknown error.
      oSelf:nError := -1
   ELSE
      // No, so close it already!
      FCLOSE( oSelf:nHan )
      oSelf:nError := FERROR()
      oSelf:nHan   := -1                // The file is no longer open
      oSelf:lEOF   := .T.               // So force an EOF condition
   END IF
RETURN oSelf

STATIC FUNCTION f_name()
LOCAL oSelf := Qself()
  // Returns the filename associated with this class instance.
RETURN oSelf:cFile

STATIC FUNCTION f_is_open()
LOCAL oSelf := Qself()
  // Returns .T. if the file is open.
RETURN oSelf:nHan != -1

STATIC FUNCTION f_more()
LOCAL oSelf := Qself()
  // Returns .T. if there is more to be read from either the file or the
  // readahead buffer. Only when both are exhausted is there no more to read.
RETURN !oSelf:lEOF .OR. !EMPTY( oSelf:cBuffer )

STATIC FUNCTION f_error()
LOCAL oSelf := Qself()
  // Retunrs .T. if an error was recorded.
RETURN oSelf:nError != 0

STATIC FUNCTION f_error_no()
LOCAL oSelf := Qself()
  // Returns the last error code that was recorded.
RETURN oSelf:nError

STATIC FUNCTION f_error_msg( cText )
STATIC s_cAction := {"on", "creating object for", "opening", "reading from", "closing"}
LOCAL oSelf := Qself()
LOCAL cMessage, nTemp
   // Has an error been recorded?
   IF oSelf:nError == 0
      // No, so report that.
      cMessage := "No errors have been recorded for " + oSelf:cFile
   ELSE
      // Yes, so format a nice error message, while avoiding a bounds error.
      IF oSelf:nLastOp < oF_ERROR_MIN .OR. oSelf:nLastOp > oF_ERROR_MAX
         nTemp := 1
      ELSE
         nTemp := oSelf:nLastOp + 1
      END IF
      cMessage := IF( EMPTY( cText ), "", cText ) + "Error " + ALLTRIM( STR( oSelf:nError ) ) + " " + s_cAction[ nTemp ] + " " + oSelf:cFile
   END IF
RETURN cMessage
