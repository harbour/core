/*
 * Harbour Project source code:
 * Copies the contents of a database to a delimited text file.
 *
 * Copyright 2001 David G. Holm <dholm@jsd-llc.com>
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

#include <fileio.ch>

/*  $DOC$
 *  $FUNCNAME$
 *      __dbDelim()
 *  $CATEGORY$
 *      Conversion
 *  $ONELINER$
 *      Copies the contents of a database to a delimited text file.
 *  $SYNTAX$
 *      __dbDelim( <lFlag1>, <xcFile>, [<xcDelim>], [<aFields>],
 *      [<bFor>], [<bWhile>], [<nNext>], [<?>], <lFlag2>  ) --> NIL
 *  $ARGUMENTS$
 *      <lFlag1>
 *      <xcFile> The name of the text file to create. If a file
 *      extension is not specified, ".txt" is used by default.
 *      <xcDelim> The character(s) to use as character field delimiters
 *      or "BLANK" (not case sensitive), which eliminates the character
 *      field delimiters and sets the field separator to a single space.
 *      <aFields> A list of field names to limit the copy to. If not
 *      specified or if empty, then all fields are copied.
 *      <bFor> A block expression containing a for expression to use to
 *      limit the records that will be copied, starting from the first
 *      record, unless a <bWhile> or <nNext> argument is included.
 *      <bWhile> A block expression containing a while expression to use
 *      to limit the records that will be copied, starting from the current
 *      record.
 *      <nNext> The number of consecutive records to copy, starting from
 *      the current record.
 *      <?>
 *      <lFlag2>
 *  $RETURNS$
 *      NIL
 *  $DESCRIPTION$
 *      __dbDelim() copies the selected contents of a database table to
 *      a delimited text file. The default selection is all fields from
 *      all records.
 *  $EXAMPLES$
 *      // Copy delinquent accounts into a delimited text file.
 *
 *      USE ACCOUNTS NEW
 *      COPY TO overdue FOR !EMPTY( accounts->duedate ) ;
 *      .AND. DATE() - accounts->duedate > 30
 *  $TESTS$
 *      
 *  $STATUS$
 *      I
 *  $COMPLIANCE$
 *      __dbDelim() is intended to be fully compliant with CA-Clipper's
 *      function of the same name and is the underlying implementation
 *      of the COPY TO command.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *
 *  $SEEALSO$
 *      __dbSDF()
 *  $END$
 */

FUNCTION __dbDelim( lFlag1, cFile, cDelimArg, aFields, bFor, bWhile, nNext, arg8, lFlag2 )
///   LOCAL cDateFormat := SET( _SET_DATEFORMAT, "YYYYMMDD" )
   LOCAL index, handle, lWriteSep := .F., cFileName := cFile
   LOCAL cSeparator := ",", cDelimLeft := CHR( 34 ), cDelimRight := cDelimLeft
   IF !EMPTY( cDelimArg )
      IF UPPER( cDelimArg ) == "BLANK"
         cDelimLeft := cDelimRight := ""
         cSeparator := " "
      ELSE
         cDelimLeft := LEFT( cDelimArg, 1 )
         IF LEN( cDelimArg ) > 1
            cDelimRight := SUBSTR( cDelimArg, 2, 1 )
         ELSE
            cDelimRight := cDelimLeft
         END IF
      END IF
   END IF

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
   handle := FCREATE( cFileName )
   IF handle == -1
   ELSE
      IF EMPTY( bWhile ) .AND. EMPTY( nNext )
         // Start from the first record, unless a WHILE condition
         // or a NEXT count is specified.
         GO TOP
      END IF
      IF EMPTY( bFor )
         bFor := {||.T.}
      END IF
      IF EMPTY( bWhile )
         bWhile := {||!EOF()}
      END IF
      IF nNext == NIL
         // If NEXT count not specified, indicate no next count.
         // The EMPTY test can't be used here, because NEXT 0
         // is a valid scope that must not copy any records.
         nNext := -1
      END IF
      WHILE EVAL( bWhile ) .AND. ( nNext == -1 .OR. nNext > 0 )
         IF EVAL( bFor )
            IF EMPTY( aFields )
               FOR index := 1 TO FCOUNT()
                  IF lWriteSep
                     AppendSep( handle, cSeparator )
                  END IF
                  lWriteSep := Export( handle, FIELDGET( index ), cDelimLeft, cDelimRight )
               NEXT index
            ELSE
               FOR index := 1 TO LEN( aFields )
                  IF lWriteSep
                     AppendSep( handle, cSeparator )
                  END IF
                  lWriteSep := Export( handle, FIELDGET( FIELDPOS( aFields[ index ] ) ), cDelimLeft, cDelimRight )
               NEXT index
            END IF
            AppendEOL( handle )
            lWriteSep := .F.
            IF nNext != -1
               nNext--
            END IF
         END IF
         SKIP
      END WHILE
      AppendEOF( handle )
      FCLOSE( handle )
   END IF
///   SET( _SET_DATEFORMAT, cDateFormat )
RETURN NIL

STATIC FUNCTION Export( handle, xField, cDelimLeft, cDelimRight )
   LOCAL cText := "", lWrite := .F.
   DO CASE
      CASE VALTYPE( xField ) == "C"
         cText := cDelimLeft + TRIM( xField ) + cDelimRight
         lWrite := .T.
      CASE VALTYPE( xField ) == "D"
         cText := DTOS( xField )
         lWrite := .T.
      CASE VALTYPE( xField ) == "L"
         cText := IF( xField, "T", "F" )
         lWrite := .T.
      CASE VALTYPE( xField ) == "N"
         cText := LTRIM( STR( xField ) )
         lWrite := .T.
   END CASE
   FWRITE( handle, cText )
RETURN lWrite

STATIC FUNCTION AppendEOL( handle )
   STATIC cEOL := CHR( 13 ) + CHR( 10 )
RETURN FWRITE( handle, cEOL )

STATIC FUNCTION AppendEOF( handle )
   STATIC cEOF := CHR( 26 )
RETURN FWRITE( handle, cEOF )

STATIC FUNCTION AppendSep( handle, cSep )
RETURN FWRITE( handle, cSep )
