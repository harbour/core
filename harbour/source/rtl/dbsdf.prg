/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Copies the contents of a database to an SDF text file.
 * Appends the contents of an SDF text file to a database.
 *
 * Copyright 2001-2002 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    function __dbSDF() replaced by the new one which uses
 *    SDF RDD I've just created
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

#include "hbcommon.ch"
#include "fileio.ch"
#include "error.ch"
#include "set.ch"

HB_FILE_VER( "$Id$" )

REQUEST SDF

PROCEDURE __dbSDF( lExport, cFile, aFields, bFor, bWhile, nNext, nRecord, lRest )

   IF lExport
      __dbCopy( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, "SDF" )
   ELSE
      __dbApp( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, "SDF" )
   ENDIF

RETURN


#ifdef __DBSDF_OLD_CODE__

#define SkipEOL( handle )   FSEEK( handle, nEOLSize, FS_RELATIVE )
#define AppendEOL( handle ) FWRITE( handle, HB_OSNewLine() )
#define AppendEOF( handle ) FWRITE( handle, CHR( 26 ) )

PROCEDURE __dbSDF( lExport, cFile, aFields, bFor, bWhile, nNext, nRecord, lRest )
   LOCAL index, handle, cFileName := cFile, nStart, nCount, oErr, nFileLen, aStruct
   LOCAL cFileEOL:=chr(13)+chr(10)       // EOL defaults to windows
   LOCAL nEOLSize:=2                     // EOL size default to windows

   // Process the file name argument.
   index := RAT( ".", cFileName )
   IF index > 0
      // The file name might include a file extension.
      IF RAT( "/", cFileName ) > index ;
      .OR. RAT( "\", cFileName ) > index
         // No, the file extension is in a directory name.
         index := 0
      END IF
   END IF
   IF index <= 0
      // No file name extension, so provide the default.
      cFileName += ".txt"
   END IF

   // Determine where to start and how many records to process.
   IF nRecord != NIL
      // The RECORD clause has the highest priority.
      nStart := nRecord
      nCount := 1
   ELSEIF nNext != NIL
      // The NEXT clause has the next highest priority.
      nStart := -1
      nCount := nNext
   ELSEIF bWhile != NIL .OR. lRest
      // The WHILE and REST clauses have equal priority.
      nStart := -1
      nCount := -1
   ELSE
      // Followed by the FOR clause or the ALL clause.
      nStart := 0
      nCount := -1
   END IF
   IF EMPTY( bFor )
      // This simplifies the test that determines whether or not to
      // use (i.e., import or export) any given processed record.
      bFor := {||.T.}
   END IF

   IF lExport
      // COPY TO SDF
      handle := FCREATE( cFileName )
      IF handle == -1
         oErr := ErrorNew()
         oErr:severity := ES_ERROR
         oErr:genCode := EG_CREATE
         oErr:subSystem := "SDF"
         oErr:subCode := 1002
         oErr:description := HB_LANGERRMSG( oErr:genCode )
         oErr:canRetry := .T.
         oErr:canDefault := .T.
         oErr:fileName := cFileName
         oErr:osCode := FERROR()
         Eval(ErrorBlock(), oErr)
      ELSE
         IF nStart > -1
            // Only reposition if a starting record was specified or implied.
            IF nStart == 0
               GO TOP
            ELSE
               GO (nStart)
            END IF
         END IF
         IF EMPTY( bWhile )
            // This simplifies the looping logic.
            bWhile := {||.T.}
         END IF
         // Process the records to copy SDF.
         WHILE EVAL( bWhile ) .AND. ( nCount == -1 .OR. nCount > 0 ) ;
         .AND. !BOF() .AND. !EOF()
            IF EVAL( bFor )
               IF EMPTY( aFields )
                  // Process all fields.
                  FOR index := 1 TO FCOUNT()
                     ExportFixed( handle, FIELDGET( index ) )
                  NEXT index
               ELSE
                  // Process the specified fields.
                  FOR index := 1 TO LEN( aFields )
                     ExportFixed( handle, FIELDGET( FIELDPOS( aFields[ index ] ) ) )
                  NEXT index
               END IF
               // Set up for the start of the next record.
               AppendEOL( handle )
            END IF
            IF nCount != -1
               nCount--
            END IF
            SKIP
         END WHILE
         IF SET(_SET_EOF)
            AppendEOF( handle )
         END IF
         FCLOSE( handle )
      END IF
   ELSE
      // APPEND FROM SDF
      handle := FOPEN( cFileName )
      IF handle == -1
         oErr := ErrorNew()
         oErr:severity := ES_ERROR
         oErr:genCode := EG_OPEN
         oErr:subSystem := "SDF"
         oErr:subCode := 1001
         oErr:description := HB_LANGERRMSG( oErr:genCode )
         oErr:canRetry := .T.
         oErr:canDefault := .T.
         oErr:fileName := cFileName
         oErr:osCode := FERROR()
         Eval(ErrorBlock(), oErr)
      ELSE
         cFileEOL:=FindEOL(handle)
         nEOLSize:=len(cFileEOL)
         IF EMPTY( bWhile )
            // This simplifies the looping logic.
            bWhile := {||.T.}
         END IF
         nFileLen := FSEEK( handle,0,FS_END )
         
         FSEEK( handle,0 )
         aStruct := DBSTRUCT()
         WHILE FSEEK( handle,0,FS_RELATIVE ) + 1 < nFileLen
            APPEND BLANK
            IF EMPTY( aFields )
               // Process all fields.
               FOR index := 1 TO FCOUNT()
                  IF !ImportFixed( handle,index,aStruct, cFileEOL )
                     EXIT
                  ENDIF
               NEXT index
            ELSE
               // Process the specified fields.
               FOR index := 1 TO LEN( aFields )
                  IF !ImportFixed( handle,FIELDPOS( aFields[ index ] ),aStruct, cFileEOL )
                     EXIT
                  ENDIF
               NEXT index
            END IF
            // Set up for the start of the next record.
            SkipEOL( handle )
            
         END WHILE
         FCLOSE( handle )
      END IF
   END IF
RETURN

STATIC FUNCTION ExportFixed( handle, xField )
   DO CASE
      CASE VALTYPE( xField ) == "C"
         FWRITE( handle, xField )
      CASE VALTYPE( xField ) == "D"
         FWRITE( handle, DTOS( xField ) )
      CASE VALTYPE( xField ) == "L"
         FWRITE( handle, iif( xField, "T", "F" ) )
      CASE VALTYPE( xField ) == "N"
         FWRITE( handle, STR( xField ) )
      OTHERWISE
         RETURN .F.
   END CASE
RETURN .T.

STATIC FUNCTION ImportFixed( handle, index, aStruct, cFileEOL )
   LOCAL cBuffer := Space(aStruct[ index,3 ]), pos, res := .T., nRead
   LOCAL vres

   nRead := FREAD( handle, @cBuffer, aStruct[ index,3 ] )
   IF ( pos := At( cFileEOL,cBuffer ) ) > 0 .AND. pos <= nRead
      res := .F.
      FSEEK( handle, -( nRead - pos + 1 ), FS_RELATIVE )
      IF pos > 1
         cBuffer := Left( cBuffer,pos-1 )
      ELSE
         RETURN res
      ENDIF
   ENDIF

   DO CASE
      CASE aStruct[ index,2 ] == "D"
         vres := HB_STOD( cBuffer )
      CASE aStruct[ index,2 ] == "L"
         vres := iif( cBuffer == "T",.T.,.F. )
      CASE aStruct[ index,2 ] == "N"
         vres := VAL( cBuffer )
      OTHERWISE
         vres := cBuffer
   END CASE
   FIELDPUT( index, vres )
RETURN res

STATIC FUNCTION FindEOL(fh)
  LOCAL nBufSize:=512
  LOCAL cBuffer:=space(nBufSize)
  LOCAL nF:=0,c:='',cResult:=''
  fseek(fh,0,FS_SET)
  do while fread(fh,@cBuffer,nBufSize) > 0
    do while nF <= nBufSize
      c:=substr(cBuffer,nF,1)
        IF c==chr(10)  // Unix type EOL
          cResult := chr(10)
        ELSEIF c==chr(13)  // MSWIN or MAC
          if substr(cBuffer,nF+1,1)==chr(10)
            cResult := (chr(13)+chr(10))
          else
            cResult := chr(13)
          endif  
        ENDIF
      if len(cResult)>0
        fseek(fh,0,FS_SET)
        RETURN cResult
      endif
      ++nF
    enddo
  enddo
  fseek(fh,0,FS_SET)
  if len(cResult)==0
    // TODO: Report an error - can't find EOL
  endif  
RETURN cResult

#endif /* __DBSDF_OLD_CODE__ */
