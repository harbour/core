/*
 * Harbour Project source code:
 * Converts a .dbf file into a Postgres table
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * (The original file was ported from mysql and changed by Rodrigo Moreno rodrigo_moreno@yahoo.com)
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#require "hbpgsql"

#include "fileio.ch"
#include "inkey.ch"

#include "hbextcdp.ch"

PROCEDURE Main()

   LOCAL cTok
   LOCAL cHostName
   LOCAL cUser
   LOCAL cPassword
   LOCAL cDatabase := "postgres", cTable, cFile
   LOCAL i
   LOCAL lCreateTable := .F.
   LOCAL oServer, oTable, oRecord
   LOCAL cField
   LOCAL sType
   LOCAL dType
   LOCAL cValue
   LOCAL nCommit := 100
   LOCAL nHandle
   LOCAL nCount := 0
   LOCAL nRecno := 0
   LOCAL lTruncate := .F.
   LOCAL lUseTrans := .F.
   LOCAL cPath := "public"

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   SET DELETED ON

   /* Scan parameters and setup workings */
   FOR i := 1 TO PCount()

      cTok := hb_PValue( i )

      DO CASE
      CASE cTok == "-h" ; cHostName := hb_PValue( ++i )
      CASE cTok == "-d" ; cDatabase := hb_PValue( ++i )
      CASE cTok == "-t" ; cTable := AllTrim( hb_PValue( ++i ) )
      CASE cTok == "-f" ; cFile := hb_PValue( ++i )
      CASE cTok == "-u" ; cUser := hb_PValue( ++i )
      CASE cTok == "-p" ; cPassword := hb_PValue( ++i )
      CASE cTok == "-c" ; lCreateTable := .T.
      CASE cTok == "-x" ; lTruncate := .T.
      CASE cTok == "-s" ; lUseTrans := .T.
      CASE cTok == "-m" ; nCommit := Val( hb_PValue( ++i ) )
      CASE cTok == "-r" ; nRecno := Val( hb_PValue( ++i ) )
      CASE cTok == "-e" ; cPath := hb_PValue( ++i )
      CASE cTok == "-cp" ; Set( _SET_DBCODEPAGE, hb_PValue( ++i ) )
      OTHERWISE
         help()
         RETURN
      ENDCASE
   NEXT

   IF Empty( cTable ) .OR. Empty( cFile )
      help()
      RETURN
   ENDIF

   // create log file
   IF ( nHandle := FCreate( cTable + ".log" ) ) == F_ERROR
      ? "Cannot create log file"
      RETURN
   ENDIF

   USE ( cFile ) SHARED READONLY

   oServer := TPQServer():New( cHostName, cDatabase, cUser, cPassword, NIL, cPath )
   IF oServer:NetErr()
      ? oServer:ErrorMsg()
      RETURN
   ENDIF

   oServer:lallCols := .F.

   IF lCreateTable
      IF oServer:TableExists( cTable )
         oServer:DeleteTable( cTable )
         IF oServer:NetErr()
            ? oServer:ErrorMsg()
            FWrite( nHandle, "Error: " + oServer:ErrorMsg() + hb_eol() )
            FClose( nHandle )
            RETURN
         ENDIF
      ENDIF
      oServer:CreateTable( cTable, dbStruct() )

      IF oServer:NetErr()
         ? oServer:ErrorMsg()
         FWrite( nHandle, "Error: " + oServer:ErrorMsg() + hb_eol() )
         FClose( nHandle )
         RETURN
      ENDIF
   ENDIF

   IF lTruncate
      oServer:Execute( "truncate table " + cTable )
      IF oServer:NetErr()
         ? oServer:ErrorMsg()
         FWrite( nHandle, "Error: " + oServer:ErrorMsg() + hb_eol() )
         FClose( nHandle )
         RETURN
      ENDIF
   ENDIF

   oTable := oServer:Query( "SELECT * FROM " + cTable + " LIMIT 1" )
   IF oTable:NetErr()
      ? oTable:ErrorMsg()
      FWrite( nHandle, "Error: " + oTable:ErrorMsg() + hb_eol() )
      FClose( nHandle )
      RETURN
   ENDIF

   IF lUseTrans
      oServer:StartTransaction()
   ENDIF

   FWrite( nHandle, "Start: " + Time() + hb_eol() )

   ? "Start:", Time()
   ?

   IF nRecno != 0
      dbGoto( nRecno )
   ENDIF

   DO WHILE ! Eof() .AND. Inkey() != K_ESC .AND. ( nRecno == 0 .OR. nRecno == RecNo() )
      oRecord := oTable:GetBlankRow()

      FOR i := 1 TO oTable:FCount()
         cField := Lower( oTable:FieldName( i ) )
         sType := hb_FieldType( FieldPos( cField ) )
         dType := oRecord:FieldType( i )
         cValue := FieldGet( FieldPos( cField ) )

         IF cValue != NIL
            IF dType != sType
               DO CASE
               CASE dType == "C" .AND. sType == "N" ; cValue := hb_ntos( cValue )
               CASE dType == "C" .AND. sType == "D" ; cValue := DToC( cValue )
               CASE dType == "C" .AND. sType == "L" ; cValue := iif( cValue, "S", "N" )
               CASE dType == "N" .AND. sType == "C" ; cValue := Val( cValue )
               CASE dType == "N" .AND. sType == "D" ; cValue := Val( DToS( cValue ) )
               CASE dType == "N" .AND. sType == "L" ; cValue := iif( cValue, 1, 0 )
               CASE dType == "D" .AND. sType == "C" ; cValue := CToD( cValue )
               CASE dType == "D" .AND. sType == "N" ; cValue := hb_SToD( hb_ntos( cValue ) )
               CASE dType == "L" .AND. sType == "N" ; cValue := ! Empty( cValue )
               CASE dType == "L" .AND. sType == "C" ; cValue := iif( AllTrim( cValue ) $ "YySs1", .T., .F. )
               ENDCASE
            ENDIF

            IF cValue != NIL
               IF oRecord:FieldType( i ) == "C" .OR. oRecord:FieldType( i ) == "M"
                  oRecord:FieldPut( i, hb_StrToUTF8( cValue ) )
               ELSE
                  oRecord:FieldPut( i, cValue )
               ENDIF
            ENDIF
         ENDIF
      NEXT

      oTable:Append( oRecord )

      IF oTable:NetErr()
         ?
         ? "Error Record:", RecNo(), Left( oTable:ErrorMsg(), 70 )
         ?
         FWrite( nHandle, "Error at record: " + hb_ntos( RecNo() ) + " Description: " + oTable:ErrorMsg() + hb_eol() )
      ELSE
         nCount++
      ENDIF

      dbSkip()

      IF ( nCount % nCommit ) == 0
         DevPos( Row(), 1 )
         DevOut( "imported recs: " + hb_ntos( nCount ) )

         IF lUseTrans
            oServer:commit()
            oServer:StartTransaction()
         ENDIF
      ENDIF
   ENDDO

   IF ( nCount % nCommit ) != 0 .AND. lUseTrans
      oServer:commit()
   ENDIF

   FWrite( nHandle, "End: " + Time() + ", records in dbf: " + hb_ntos( RecNo() ) + ", imported recs: " + hb_ntos( nCount ) + hb_eol() )

   ? "End:", Time()

   FClose( nHandle )

   dbCloseAll()

   oTable:Destroy()
   oServer:Destroy()

   RETURN

STATIC PROCEDURE Help()

   ? "dbf2pg - dbf file to PostgreSQL table conversion utility"
   ? "-h hostname"
   ? "-u user"
   ? "-p password"
   ? "-d name of database to use (default: postgres)"
   ? "-t name of table to add records to (required)"
   ? "-c delete existing table and create a new one"
   ? "-f name of .dbf file to import (required)"
   ? "-x truncate table before append records"
   ? "-s use transaction"
   ? "-m commit interval"
   ? "-r insert only record number"
   ? "-e search path"
   ?

   RETURN
