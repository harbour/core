/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Copies the contents of a database to a delimited text file.
 * Appends the contents of a delimited text file to a database.
 *
 * Copyright 2001-2002 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
 * APPEND FROM code submitted by Marco Braida <marcobra@elart.it>
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

HB_FILE_VER( "$Id$" )

#define AppendEOL( handle )       FWRITE( handle, CHR( 13 ) + CHR( 10 ) )
#define AppendEOF( handle )       FWRITE( handle, CHR( 26 ) )
#define AppendSep( handle, cSep ) FWRITE( handle, cSep )

PROCEDURE __dbDelim( lExport, cFile, cDelimArg, aFields, bFor, bWhile, nNext, nRecord, lRest )
   LOCAL index, handle, lWriteSep, cFileName := cFile, nStart, nCount, oErr
   LOCAL cSeparator := ",", cDelim := CHR( 34 )
//------------------
local Pos:=0
local nPosFl:=0
local nDimBuff:=65535
local cByte :=""
local lunghezze:={}
local eol:=chr(13)+chr(10)
local contacamp:=0
local primariga:=.t.
local offset:=0
local rig:=0
local cont_r:=""
local Lfinefile:=.f.
local nFileLen
local cCharEol:=HB_OSNewLine()
local nPosLasteol
local lcisonoeol
//------------------
   // Process the delimiter argument.
   IF !EMPTY( cDelimArg )
      IF UPPER( cDelimArg ) == "BLANK"
         cDelim := ""
         cSeparator := " "
      ELSE
         cDelim := LEFT( cDelimArg, 1 )
      END IF
   END IF

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
      // COPY TO DELIMITED
      handle := FCREATE( cFileName )
      IF handle == -1
         oErr := ErrorNew()
         oErr:severity := ES_ERROR
         oErr:genCode := EG_CREATE
         oErr:subSystem := "DELIM"
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
         // Set up for the start of the first record.
         lWriteSep := .F.
         // Process the records to copy delimited.
         WHILE EVAL( bWhile ) .AND. ( nCount == -1 .OR. nCount > 0 ) ;
         .AND. !BOF() .AND. !EOF()
            IF EVAL( bFor )
               IF EMPTY( aFields )
                  // Process all fields.
                  FOR index := 1 TO FCOUNT()
                     IF lWriteSep
                        AppendSep( handle, cSeparator )
                     END IF
                     lWriteSep := ExportVar( handle, FIELDGET( index ), cDelim )
                  NEXT index
               ELSE
                  // Process the specified fields.
                  FOR index := 1 TO LEN( aFields )
                     IF lWriteSep
                        AppendSep( handle, cSeparator )
                     END IF
                     lWriteSep := ExportVar( handle, FIELDGET( FIELDPOS( aFields[ index ] ) ), cDelim )
                  NEXT index
               END IF
               // Set up for the start of the next record.
               AppendEOL( handle )
               lWriteSep := .F.
            END IF
            IF nCount != -1
               nCount--
            END IF
            SKIP
         END WHILE
         AppendEOF( handle )
         FCLOSE( handle )
      END IF
   ELSE
      // APPEND FROM DELIMITED
      handle := FOPEN( cFileName )
      IF handle == -1
         oErr := ErrorNew()
         oErr:severity := ES_ERROR
         oErr:genCode := EG_OPEN
         oErr:subSystem := "DELIM"
         oErr:subCode := 1001
         oErr:description := HB_LANGERRMSG( oErr:genCode )
         oErr:canRetry := .T.
         oErr:canDefault := .T.
         oErr:fileName := cFileName
         oErr:osCode := FERROR()
         Eval(ErrorBlock(), oErr)
      ELSE
         IF EMPTY( bWhile )
             // This simplifies the looping logic.
             bWhile := {||.T.}
         ENDIF 
         // ---------------------------------------
         // Please fill with the other test here
         // Marco Braida 2002
         // marcobra@elart.it
         // ---------------------------------------

         nFileLen:=FSEEK(handle,0,FS_END)
	 nDimBuff:=min(nFileLen,nDimBuff)
	 cByte:=space(nDimBuff)
	 FSEEK(handle,0)
//	 cCharEol:=chr(13)
	 nPosLastEol:=0
         do while .not. lFineFile
	    fseek(handle,nPoslastEol,FS_SET)   // forward the pointer
            //we must not go after the eof
            if nPosLastEol + nDimBuff > nFileLen
               // change the buffer size
               nDimBuff:=nFileLen-nPosLastEol
               cByte:=space(nDimBuff)
               Lfinefile:=.t. 
             endif
             // fill the buffer
             cByte:=space(nDimBuff)
             fread(handle,@cByte,nDimBuff)
             //we test the last position of the last eol +1 in this buffer
             nPoslastEol+=rat(cCharEol,cByte)+1
             //do this if in the buffer there are eol char
             lcisonoeol:=.t.
             do while lcisonoeol
                // the position of the first eol
                nposfl:=at(cCharEol,cByte) 
                lcisonoeol:=(nPosfl>0)
                if lcisonoeol
                   // cut the row
                   Pos:=1
                   cont_r:=substr(cByte,Pos,nposfl-Pos)
                   appendtodb(cont_r,cSeparator)
                   // skipping the line feed and now we are on a good char
                   pos:=nposfl+2
                   cont_r:=""
                   //cut the row  
                   cByte:=substr(cByte,Pos)
                endif     
              enddo
         enddo
         FCLOSE( handle )
      END IF

   END IF
RETURN

STATIC FUNCTION ExportVar( handle, xField, cDelim )
   DO CASE
      CASE VALTYPE( xField ) == "C"
         FWRITE( handle, cDelim + TRIM( xField ) + cDelim )
      CASE VALTYPE( xField ) == "D"
         FWRITE( handle, DTOS( xField ) )
      CASE VALTYPE( xField ) == "L"
         FWRITE( handle, iif( xField, "T", "F" ) )
      CASE VALTYPE( xField ) == "N"
         FWRITE( handle, LTRIM( STR( xField ) ) )
      OTHERWISE
         RETURN .F.
   END CASE
RETURN .T.


STATIC FUNCTION appendtodb(row,cDelim)
local lenrow:=len(row)
local aStruct:=DBSTRUCT()
local aMyVal:={}
local ii:=1
local npos:=0, nPosNext:=0
local nDBFFields
local cBuffer, cUPbuffer
local vRes
//if there is one field only there is no Delim and i put...
row:=row+cDelim
nPos:=at(cDelim,row)
aadd( aMyval,substr(row,1,nPos-1) )
do while .t.
    nPosNext:=at(cDelim,row,npos+2)
    if nPosNext=0
       exit
    endif
    aadd( aMyVal,substr(row,npos+1,nPosnext-npos-1) )
    nPos:=nPosnext    
    if nPos>lenrow
       exit
    endif 
enddo
nDBFfields:=min(len(aMyVal),len(aStruct))
append blank

for ii:=1 to nDBFfields
   cBuffer:=strtran(aMyval[ii],'"','')
   DO CASE
      CASE aStruct[ ii,2 ] == "D"
         vRes := HB_STOD( cBuffer )
      CASE aStruct[ ii,2 ] == "L"
         cUPbuffer:=upper(cBuffer)
         vRes := iif( cUPBuffer == "T" .or. cUPBuffer== "1" .or. cUPBuffer=="Y",.T.,.F. )
      CASE aStruct[ ii,2 ] == "N"
         vRes := VAL( cBuffer )
      OTHERWISE
         vRes := cBuffer
   END CASE

   FIELDPUT(ii,vRes)
next
return .T. 
