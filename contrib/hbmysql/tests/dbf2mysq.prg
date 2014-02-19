/*
 * Harbour Project source code:
 * Converts a .dbf file into a MySQL table
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

#require "hbmysql"

#include "inkey.ch"

PROCEDURE Main()

   LOCAL cTok
   LOCAL cHostName := "localhost"
   LOCAL cUser := "root"
   LOCAL cPassword := ""
   LOCAL cDatabase, cTable, cFile
   LOCAL i
   LOCAL lCreateTable := .F.
   LOCAL oServer, oTable, oRecord

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   // At present time (2000-10-23) DBFCDX is default RDD and DBFNTX is
   // now DBF (I mean the one able to handle .dbt-s :-))
   rddSetDefault( "DBF" )

   // Scan parameters and setup workings
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
      OTHERWISE
         help()
         RETURN
      ENDCASE
   NEXT

   IF Empty( cTable ) .OR. Empty( cFile ) .OR. Empty( cDatabase )
      help()
      RETURN
   ENDIF

   USE ( cFile ) SHARED READONLY

   oServer := TMySQLServer():New( cHostName, cUser, cPassword )
   IF oServer:NetErr()
      ? oServer:Error()
      RETURN
   ENDIF

   oServer:SelectDB( cDatabase )
   IF oServer:NetErr()
      ? oServer:Error()
      RETURN
   ENDIF

   IF lCreateTable
      IF hb_AScan( oServer:ListTables(), cTable,,, .T. ) > 0
         oServer:DeleteTable( cTable )
         IF oServer:NetErr()
            ? oServer:Error()
            RETURN
         ENDIF
      ENDIF
      oServer:CreateTable( cTable, dbStruct() )
      IF oServer:NetErr()
         ? oServer:Error()
         RETURN
      ENDIF
   ENDIF

   // Initialize MySQL table
   oTable := oServer:Query( "SELECT * FROM " + cTable + " LIMIT 1" )
   IF oTable:NetErr()
      ? oTable:Error()
      RETURN
   ENDIF

   DO WHILE ! Eof() .AND. Inkey() != K_ESC

      oRecord := oTable:GetBlankRow()

      FOR i := 1 TO FCount()
         oRecord:FieldPut( i, FieldGet( i ) )
      NEXT

      oTable:Append( oRecord )
      IF oTable:NetErr()
         ? oTable:Error()
      ENDIF

      dbSkip()

      IF ( RecNo() % 100 ) == 0
         DevPos( Row(), 1 )
         DevOut( "imported recs: " + hb_ntos( RecNo() ) )
      ENDIF
   ENDDO

   dbCloseArea()

   oTable:Destroy()
   oServer:Destroy()

   RETURN

STATIC PROCEDURE Help()

   ? "dbf2mysq - dbf file to MySQL table conversion utility"
   ? "-h hostname (default: localhost)"
   ? "-u user (default: root)"
   ? "-p password (default no password)"
   ? "-d name of database to use (required)"
   ? "-t name of table to add records to (required)"
   ? "-c delete existing table and create a new one"
   ? "-f name of .dbf file to import (required)"
   ? "all parameters but -h -u -p -c are mandatory"
   ?

   RETURN
