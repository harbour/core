/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MySQL DBMS test program
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

#require "hbmysql"

#include "dbstruct.ch"

PROCEDURE Main( cArg )

   LOCAL oServer, oQuery2, oRow, aStru
   LOCAL oQuery

   SET CENTURY ON

   oServer := TMySQLServer():New( "localhost", "root", "" )
   IF oServer:NetErr()
      Alert( oServer:Error() )
   ENDIF

   oServer:SelectDB( "ims" )
#if 0
   oQuery := oServer:Query( "SELECT * from maga limit 10" )
   oRow := oQuery:GetRow()
#endif

   dbUseArea( .T.,, cArg, "wn", .F. )

   IF ! oServer:DeleteTable( "test" )
      Alert( oServer:Error() )
   ENDIF

   aStru := dbStruct()
   IF oServer:CreateTable( "test", aStru )
      Alert( "test created successfully" )
   ELSE
      Alert( oServer:Error() )
   ENDIF

   oQuery := oServer:Query( "SELECT C111, C116, C134 from maga limit 10" )
#if 0
   oRow := oQuery:GetRow()
#endif

   oServer:Destroy()

   DO WHILE ! wn->( Eof() )

      oQuery2 := oServer:Query( "SELECT * from test where CODF='" + wn->CODF + "' and CODP='" + wn->CODP + "'" )

      IF oQuery2:LastRec() > 0

         ? "found "

         oRow := oQuery2:GetRow()

         oRow:FieldPut( oRow:FieldPos( "GIACENZA" ), oRow:FieldGet( oRow:FieldPos( "GIACENZA" ) ) + wn->GIACENZA )
         oRow:FieldPut( oRow:FieldPos( "ACQGR" ), oRow:FieldGet( oRow:FieldPos( "ACQGR" ) ) + wn->ACQGR )
         oRow:FieldPut( oRow:FieldPos( "ACQDI" ), oRow:FieldGet( oRow:FieldPos( "ACQDI" ) ) + wn->ACQDI )

         IF ! oQuery2:Update( oRow )
            Alert( oQuery2:Error() )
         ENDIF
      ELSE
         ? wn->CODF + " " + wn->CODP

         oRow := oQuery:GetBlankRow()

         oRow:FieldPut( oRow:FieldPos( "CODF" ), wn->CODF )
         oRow:FieldPut( oRow:FieldPos( "CODP" ), wn->CODP )
         oRow:FieldPut( oRow:FieldPos( "GIACENZA" ), wn->GIACENZA )
         oRow:FieldPut( oRow:FieldPos( "DATA" ), wn->DATA + 365 * 100 )
         oRow:FieldPut( oRow:FieldPos( "ACQGR" ), wn->ACQGR )
         oRow:FieldPut( oRow:FieldPos( "ACQDI" ), wn->ACQDI )

         IF ! oQuery:Append( oRow )
            Alert( oQuery:Error() )
         ENDIF
      ENDIF

      wn->( dbSkip() )

   ENDDO

   wn->( dbCloseArea() )

   RETURN
