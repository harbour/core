/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Tzipfile Class
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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


#include 'hbclass.ch'
#include 'common.ch'

CLASS tZipFile
data cFile as CHARACTER    init ""
Data aFiles as ARRAY init {}
Data aList  as ARRAY init {}
Data bBlock  as CODEBLOCK        init {||NIL}
Data lOverwrite  as LOGICAL    init .f.
Data nCompMethod as NUMERIC    init 8
Method New(cFile,nMethod,bBlock,lCompress,lOverwrite)
Method DoCompress()
Method DoUnzip()
method AddFile(cFile)
Method GetNumberofFiles()
Method GetList()
Method ShowList()
ENDCLASS
Method New(cFile,nMethod,bBlock,lCompress,lOverwrite) CLASS tZipFile
default lCompress to .t.
Default lOverWrite to .T.
if lCompress
    Default nMethod to 8
Endif
::cFile       := cFile
::nCompMethod := nMethod
::lOverWrite  := lOverWrite
::aFiles      := {}
::bBlock := bBlock
return Self
Method AddFile(cFile) CLASS tZipFile
if cFile != NIL
    aadd(::aFiles,cFile)
endif
return Self

Method GetList() CLASS tZipFile
 ::aList := {}
::aList  := HB_GETFILESINZIP(::cFile)
return   Self
Method ShowList() CLASS tZipFile
local x
For x:=1 to len(::aList)
    qout(::aList[x])
next
return Self

Method GetNumberofFiles()  CLASS tZipFile
Local nFiles
nFiles:=HB_GETUNZIPFILE(::cFile)
return nFiles

Method doCompress()  CLASS tZipFile
if HB_ZIPFILE(::cFile,::aFiles, ::nCompMethod,::bBlock,::lOverWrite)
   Return .T.
endif
Return .F.

Method DoUnzip()  CLASS tZipFile
IF HB_UNZIPFILE(::cFile,::bBlock,1)
  Return .t.
endif
return .f.
