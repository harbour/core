/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * dbf2pg.prg - converts a .dbf file into a Postgres table
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

#require "hbpgsql"

#include "inkey.ch"
#include "fileio.ch"

#include "hbextcdp.ch"

PROCEDURE Main( ... )

   LOCAL cTok
   LOCAL cHostName := "localhost"
   LOCAL cUser := "postgres"
   LOCAL cPassWord := ""
   LOCAL cDataBase, cTable, cFile
   LOCAL aDbfStruct, i
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

   SET CENTURY ON
   SET DATE ANSI
   SET DELETE ON

   rddSetDefault( "DBFDBT" )

   IF PCount() < 6
      help()
      QUIT
   ENDIF

   i := 1
   /* Scan parameters and setup workings */
   DO WHILE i <= PCount()

      cTok := hb_PValue( i++ )

      DO CASE
      CASE cTok == "-h"
         cHostName := hb_PValue( i++ )

      CASE cTok == "-d"
         cDataBase := hb_PValue( i++ )

      CASE cTok == "-t"
         cTable := hb_PValue( i++ )

      CASE cTok == "-f"
         cFile := hb_PValue( i++ )

      CASE cTok == "-u"
         cUser := hb_PValue( i++ )

      CASE cTok == "-p"
         cPassWord := hb_PValue( i++ )

      CASE cTok == "-c"
         lCreateTable := .T.

      CASE cTok == "-x"
         lTruncate := .T.

      CASE cTok == "-s"
         lUseTrans := .T.

      CASE cTok == "-m"
         nCommit := Val( hb_PValue( i++ ) )

      CASE cTok == "-r"
         nRecno := Val( hb_PValue( i++ ) )

      CASE cTok == "-e"
         cPath := hb_PValue( i++ )

      CASE cTok == "-cp"
         hb_cdpSelect( hb_PValue( i++ ) )

      OTHERWISE
         help()
         QUIT
      ENDCASE
   ENDDO

   // create log file
   IF ( nHandle := FCreate( RTrim( cTable ) + ".log" ) ) == F_ERROR
      ? "Cannot create log file"
      QUIT
   ENDIF

   USE ( cFile ) SHARED
   aDbfStruct := DBStruct()

   oServer := TPQServer():New( cHostName, cDatabase, cUser, cPassWord, NIL, cPath )
   IF oServer:NetErr()
      ? oServer:ErrorMsg()
      QUIT
   ENDIF

   oServer:lallCols := .F.

   IF lCreateTable
      IF oServer:TableExists( cTable )
         oServer:DeleteTable( cTable )
         IF oServer:NetErr()
            ? oServer:ErrorMsg()
            FWrite( nHandle, "Error: " + oServer:ErrorMsg() + hb_eol() )
            FClose( nHandle )
            QUIT
         ENDIF
      ENDIF
      oServer:CreateTable( cTable, aDbfStruct )

      IF oServer:NetErr()
         ? oServer:ErrorMsg()
         FWrite( nHandle, "Error: " + oServer:ErrorMsg() + hb_eol() )
         FClose( nHandle )
         QUIT
      ENDIF
   ENDIF

   IF lTruncate
      oServer:Execute( "truncate table " + cTable )
      IF oServer:NetErr()
         ? oServer:ErrorMsg()
         FWrite( nHandle, "Error: " + oServer:ErrorMsg() + hb_eol() )
         FClose( nHandle )
         QUIT
      ENDIF
   ENDIF

   oTable := oServer:Query( "SELECT * FROM " + cTable + " LIMIT 1" )
   IF oTable:NetErr()
      Alert( oTable:ErrorMsg() )
      FWrite( nHandle, "Error: " + oTable:ErrorMsg() + hb_eol() )
      FClose( nHandle )
      QUIT
   ENDIF

   IF lUseTrans
      oServer:StartTransaction()
   ENDIF

   FWrite( nHandle, "Start: " + Time() + hb_eol() )

   ? "Start: ", Time()
   ?

   IF ! Empty( nRecno )
      dbGoto( nRecno )
   ENDIF

   DO WHILE ! Eof() .AND. Inkey() != K_ESC .AND. ( Empty( nRecno ) .OR. nRecno == RecNo() )
      oRecord := oTable:GetBlankRow()

      FOR i := 1 TO oTable:FCount()
         cField := Lower( oTable:FieldName( i ) )
         sType := FieldType( FieldPos( cField ) )
         dType := oRecord:Fieldtype( i )
         cValue := FieldGet( FieldPos( cField ) )

         IF cValue != NIL
            IF dType != sType
               IF dType == "C" .AND. sType == "N"
                 cValue := Str( cValue )

               ELSEIF dType == "C" .AND. sType == "D"
                 cValue := DToC( cValue )

               ELSEIF dType == "C" .AND. sType == "L"
                 cValue := iif( cValue, "S", "N" )

               ELSEIF dType == "N" .AND. sType == "C"
                 cValue := Val( cValue )

               ELSEIF dType == "N" .AND. sType == "D"
                 cValue := Val( DToS( cValue ) )

               ELSEIF dType == "N" .AND. sType == "L"
                 cValue := iif( cValue, 1, 0 )

               ELSEIF dType == "D" .AND. sType == "C"
                 cValue := CToD( cValue )

               ELSEIF dType == "D" .AND. sType == "N"
                 cValue := SToD( Str( cValue ) )

               ELSEIF dType == "L" .AND. sType == "N"
                 cValue := ! Empty( cValue )

               ELSEIF dType == "L" .AND. sType == "C"
                 cValue := iif( AllTrim( cValue ) $ "YySs1", .T., .F. )

               ENDIF
            ENDIF

            IF cValue != NIL
               IF oRecord:Fieldtype( i ) == "C" .OR. oRecord:Fieldtype( i ) == "M"
                  oRecord:FieldPut( i, hb_StrToUTF8( cValue ) )
               ELSE
                  oRecord:FieldPut( i, cValue )
               ENDIF
            ENDIF
         ENDIF
      NEXT

      oTable:Append(oRecord)

      IF oTable:NetErr()
         ?
         ? "Error Record: ", RecNo(), Left( oTable:ErrorMsg(), 70 )
         ?
         FWrite( nHandle, "Error at record: " + hb_ntos( RecNo() ) + " Description: " + oTable:ErrorMsg() + hb_eol() )
      ELSE
         nCount++
      ENDIF

      dbSkip()

      IF ( nCount % nCommit ) == 0
         DevPos( Row(), 1 )
         DevOut( "imported recs: " + Str( nCount ) )

         IF lUseTrans
            oServer:commit()
            oServer:StartTransaction()
         ENDIF
      ENDIF
   ENDDO

   IF ( nCount % nCommit ) != 0
      IF lUseTrans
         oServer:commit()
      ENDIF
   ENDIF

   FWrite( nHandle, "End: " + Time() + ", records in dbf: " + hb_ntos( RecNo() ) + ", imported recs: " + hb_ntos( nCount ) + hb_eol() )

   ? "End: ", Time()
   ?

   FClose( nHandle )

   CLOSE ALL
   oTable:Destroy()
   oServer:Destroy()

   RETURN

PROCEDURE Help()

   ? "dbf2pg - dbf file to PostgreSQL table conversion utility"
   ? "-h hostname (default: localhost)"
   ? "-u user (default: root)"
   ? "-p password (default no password)"
   ? "-d name of database to use"
   ? "-t name of table to add records to"
   ? "-c delete existing table and create a new one"
   ? "-f name of .dbf file to import"
   ? "-x truncate table before append records"
   ? "-s use transaction"
   ? "-m commit interval"
   ? "-r insert only record number"
   ? "-e search path"

   ? ""

   RETURN
