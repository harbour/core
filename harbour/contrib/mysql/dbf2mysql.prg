/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * dbf2mysql.prg - converts a .dbf file into a MySQL table
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

#include "inkey.ch"

procedure main(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)

   local cTok, nTok := 1
   local cHostName := "localhost"
   local cUser := "root"
   local cPassWord := ""
   local cDataBase, cTable, cFile
   local aDbfStruct, i
   local lCreateTable := .F.
   local oServer, oTable, oRecord

   SET CENTURY ON
   SET EPOCH TO 1960

   // At present time (23/10/00) DBFCDX is default RDD and DBFNTX is
   // now DBF (I mean the one able to handle .DBTs :-))
   rddSetDefault( "DBF" )

   if PCount() < 6
      help()
      quit
   endif

   i := 1
   // Scan parameters and setup workings
   while (i <= PCount())

      cTok := hb_PValue(i++)

      do case
      case cTok == "-h"
         cHostName := hb_PValue(i++)

      case cTok == "-d"
         cDataBase := hb_PValue(i++)

      case cTok == "-t"
         cTable := hb_PValue(i++)

      case cTok == "-f"
         cFile := hb_PValue(i++)

      case cTok == "-u"
         cUser := hb_PValue(i++)

      case cTok == "-p"
         cPassWord := hb_PValue(i++)

      case cTok == "-c"
         lCreateTable := .T.

      otherwise
         help()
         quit
      endcase
   enddo

   dbUseArea(.T.,, cFile, "dbffile",, .T.)
   aDbfStruct := dbffile->(dbStruct())

   oServer := TMySQLServer():New(cHostName, cUser, cPassWord)
   if oServer:NetErr()
      ? oServer:Error()
      quit
   endif

   oServer:SelectDB(cDataBase)
   if oServer:NetErr()
      ? oServer:Error()
      quit
   endif

   if lCreateTable
      if Ascan(oServer:ListTables(), cTable) > 0
         oServer:DeleteTable(cTable)
         if oServer:NetErr()
            ? oServer:Error()
            quit
         endif
      endif
      oServer:CreateTable(cTable, aDbfStruct)
      if oServer:NetErr()
         ? oServer:Error()
         quit
      endif
   endif

   // Initialize MySQL table
   oTable := oServer:Query("SELECT * FROM " + cTable + " LIMIT 1")
   if oTable:NetErr()
      Alert(oTable:Error())
      quit
   endif

   while !dbffile->(eof()) .AND. Inkey() <> K_ESC

      oRecord := oTable:GetBlankRow()

      for i := 1 to dbffile->(FCount())
         oRecord:FieldPut(i, dbffile->(FieldGet(i)))
      next

      oTable:Append(oRecord)
      if oTable:NetErr()
         Alert(oTable:Error())
      endif

      dbffile->(dbSkip())

      DevPos(Row(), 1)
      if (dbffile->(RecNo()) % 100) == 0
         DevOut("imported recs: " + Str(dbffile->(RecNo())))
      endif
   enddo

   dbffile->(dbCloseArea())
   oTable:Destroy()
   oServer:Destroy()
return


procedure Help()

   ? "dbf2MySQL - dbf file to MySQL table conversion utility"
   ? "-h hostname (default: localhost)"
   ? "-u user (default: root)"
   ? "-p password (default no password)"
   ? "-d name of database to use"
   ? "-t name of table to add records to"
   ? "-c delete existing table and create a new one"
   ? "-f name of .dbf file to import"
   ? "all parameters but -h -u -p -c are mandatory"
   ? ""

return
