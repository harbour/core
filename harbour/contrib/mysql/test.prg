/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MySQL DBMS test program
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

#include "dbstruct.ch"

procedure main(cArg)

   local oServer, oQuery, oQuery2, oRow, i, aStru

   SET CENTURY ON
   SET EPOCH TO 1960

   oServer := TMySQLServer():New("localhost", "root", "")
   if oServer:NetErr()
      Alert(oServer:Error())
   endif

   oServer:SelectDB("ims")
   oQuery:=oServer:Query("SELECT * from maga limit 10")
   oRow := oQuery:GetRow()

   dbUseArea(.T.,, cArg, "wn", .F.)

   if !oServer:DeleteTable("test")
      Alert(oServer:Error())
   endif

   aStru := dbStruct()
   if oServer:CreateTable("test", aStru)
      Alert("test created successfully")
   else
      Alert(oServer:Error())
   endif

   oQuery:=oServer:Query("SELECT C111, C116, C134 from maga limit 10")
   oRow := oQuery:GetRow()

   oServer:Destroy()

   while !wn->(eof())

      oQuery2 := oServer:Query("SELECT * from test where CODF='" + wn->CODF + "' and CODP='" + wn->CODP + "'")

      if oQuery2:LastRec() > 0

         ? "found "

         oRow := oQuery2:GetRow()

         oRow:FieldPut(oRow:FieldPos("GIACENZA"), oRow:FieldGet(oRow:FieldPos("GIACENZA")) + wn->GIACENZA)
         oRow:FieldPut(oRow:FieldPos("ACQGR"), oRow:FieldGet(oRow:FieldPos("ACQGR")) + wn->ACQGR)
         oRow:FieldPut(oRow:FieldPos("ACQDI"), oRow:FieldGet(oRow:FieldPos("ACQDI")) + wn->ACQDI)

         if !oQuery2:Update(oRow)
            Alert(oQuery2:Error())
         endif

      else

         ? wn->CODF + " " + wn->CODP

         oRow := oQuery:GetBlankRow()

         oRow:FieldPut(oRow:FieldPos("CODF"), wn->CODF)
         oRow:FieldPut(oRow:FieldPos("CODP"), wn->CODP)
         oRow:FieldPut(oRow:FieldPos("GIACENZA"), wn->GIACENZA)
         oRow:FieldPut(oRow:FieldPos("DATA"), wn->DATA + 365 * 100)
         oRow:FieldPut(oRow:FieldPos("ACQGR"), wn->ACQGR)
         oRow:FieldPut(oRow:FieldPos("ACQDI"), wn->ACQDI)

         if !oQuery:Append(oRow)
            Alert(oQuery:Error())
         endif

      endif

      wn->(dbSkip())

   enddo


   wn->(dbCloseArea())

return

