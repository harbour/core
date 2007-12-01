/*
 *
 * $Id$
 *
 * Harbour Project source code:
 * dbf2pg.prg - converts a .dbf file into a Postgres table
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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
 * The Original file was ported from Mysql and changed by Rodrigo Moreno rodrigo_moreno@yahoo.com
 *
 */

#include "inkey.ch"
#include "common.ch"

#define CRLF chr(13) + chr(10)

procedure main(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)

   local cTok, nTok := 1
   local cHostName := "localhost"
   local cUser := "postgres"
   local cPassWord := ""
   local cDataBase, cTable, cFile
   local aDbfStruct, i
   local lCreateTable := .F.
   local oServer, oTable, oRecord
   Local cField
   Local sType
   Local dType
   Local cValue
   Local nCommit := 100
   Local nHandle
   Local nCount := 0
   Local nRecno := 0
   Local lTruncate := .F.
   Local lUseTrans := .F.
   Local cPath := 'public'

   SET CENTURY ON
   SET EPOCH TO 1960
   SET DELETE ON
   SET DATE FORMAT "dd/mm/yyyy"

   rddSetDefault( "DBFDBT" )

   if PCount() < 6
      help()
      quit
   endif

   i := 1
   // Scan parameters and setup workings
   while (i <= PCount())
      
      cTok := PValue(i++)

      do case
      case cTok == "-h"
         cHostName := PValue(i++)

      case cTok == "-d"
         cDataBase := PValue(i++)

      case cTok == "-t"
         cTable := PValue(i++)

      case cTok == "-f"
         cFile := PValue(i++)

      case cTok == "-u"
         cUser := PValue(i++)

      case cTok == "-p"
         cPassWord := PValue(i++)

      case cTok == "-c"
         lCreateTable := .T.

      case cTok == "-x"
         lTruncate := .T.

      case cTok == "-s"
         lUseTrans := .T.
         
      case cTok == "-m"
         nCommit := val(PValue(i++))

      case cTok == "-r"
         nRecno := val(PValue(i++))

      case cTok == "-e"
         cPath := PValue(i++)

      otherwise
         help()
         quit
      endcase
   enddo

   // create log file
   if (nHandle := FCreate(Trim(cTable) + '.log')) == -1
        ? 'Cannot create log file'
        quit
   endif        

   USE (cFile) SHARED
   aDbfStruct := DBStruct()

   oServer := TPQServer():New(cHostName, cDatabase, cUser, cPassWord, nil, cPath)
   if oServer:NetErr()
      ? oServer:Error()
      quit
   endif
   
   oServer:lallCols := .F.

   if lCreateTable
      if oServer:TableExists(cTable)
         oServer:DeleteTable(cTable)
         if oServer:NetErr()
            ? oServer:Error()
            FWrite( nHandle, "Error: " + oServer:Error() + CRLF )
            FClose( nHandle )
            quit
         endif
      endif
      oServer:CreateTable(cTable, aDbfStruct)

      if oServer:NetErr()
         ? oServer:Error()
         FWrite( nHandle, "Error: " + oServer:Error() + CRLF )
         FClose( nHandle )
         quit
      endif
   endif
   
   if lTruncate
        oServer:Execute('truncate table ' + cTable)        
        if oServer:NetErr()
            ? oServer:Error()
            FWrite( nHandle, "Error: " + oServer:Error() + CRLF )
            FClose( nHandle )
            quit
        endif        
   endif

   oTable := oServer:Query("SELECT * FROM " + cTable + " LIMIT 1")
   if oTable:NetErr()
      Alert(oTable:Error())
      FWrite( nHandle, "Error: " + oTable:Error() + CRLF )
      FClose( nHandle )
      quit
   endif

   if lUseTrans
      oServer:StartTransaction()
   endif      
   
   FWrite( nHandle, "Start: " + time() + CRLF )

   ? "Start: ", time()
   ? 
   
   if ! Empty(nRecno)
      dbgoto(nRecno)
   endif      

   while ! eof() .and. Inkey() <> K_ESC .and. (empty(nRecno) .or. nRecno == recno())
      oRecord := oTable:GetBlankRow()
      
      for i := 1 to oTable:FCount()
         cField := lower(oTable:FieldName(i))
         sType := fieldtype(fieldpos(cField))
         dType := oRecord:Fieldtype(i)
         cValue := fieldget(fieldpos(cField))
         
         if ! ISNIL(cValue)
            if dType != sType           
               if dType == 'C' .and. sType == 'N'
                 cValue := Str(cValue)
                 
               elseif dType == 'C' .and. sType == 'D'
                 cValue := DtoC(cValue)
            
               elseif dType == 'C' .and. sType == 'L'
                 cValue := IIF( cValue, "S", "N" )
                 
               elseif dType == 'N' .and. sType == 'C'
                 cValue := val(cValue)
                 
               elseif dType == 'N' .and. sType == 'D'
                 cValue := Val(DtoS(cValue))
            
               elseif dType == 'N' .and. sType == 'L'
                 cValue := IIF( cValue, 1, 0 )
                 
               elseif dType == 'D' .and. sType == 'C'
                 cValue := CtoD(cValue)
                 
               elseif dType == 'D' .and. sType == 'N'
                 cValue := StoD(Str(cValue))
                 
               elseif dType == 'L' .and. sType == 'N'
                 cValue := ! Empty(cValue)

               elseif dType == 'L' .and. sType == 'C'
                 cValue := IIF( alltrim(cValue) $ "YySs1", .T., .F. )                                                  
            
               end
            end            
            
            if ! ISNIL(cValue)
                if oRecord:Fieldtype(i) == 'C' .or. oRecord:Fieldtype(i) == 'M'                    
                    oRecord:FieldPut(i, hb_oemtoansi(cValue))
                else                    
                    oRecord:FieldPut(i, cValue)
                endif                                        
            endif
         endif
      next

      oTable:Append(oRecord)
      
      if oTable:NetErr()
         ?
         ? "Error Record: ", recno(), left(oTable:Error(),70)
         ? 
         FWrite( nHandle, "Error at record: " + Str(recno()) + " Description: " + oTable:Error() + CRLF )         
      else
         nCount++         
      endif

      dbSkip()

      if (nCount % nCommit) == 0
         DevPos(Row(), 1)
         DevOut("imported recs: " + Str(nCount))         
         
         if lUseTrans
            oServer:commit()
            oServer:StartTransaction()
         endif            
      endif
   enddo
   
   if (nCount % nCommit) != 0
        if lUseTrans
            oServer:commit()
        endif            
   endif        

   FWrite( nHandle, "End: " +  time() + ", records in dbf: " + ltrim(str(recno())) + ", imported recs: " + ltrim(str(nCount)) + CRLF )

   ? "End: ", time()
   ? 

   FClose( nHandle )

   close all
   oTable:Destroy()
   oServer:Destroy()
return


procedure Help()

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

return
