/*
 * $Id$
 */

//*******************************************************************
// ralter.prg: a RALTER oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

#define _RALTER_PRG_
#define _IMPLEMENT_ONEW_

#include "ralter.och"

//*******************************************************************
implement oinit(id,str,file,line,pos)
   super:oinit(id,str,file,line,pos)
   this:tokenList :={}
return this

//*******************************************************************
implement getStr()
local i
local str
    
   if (!this:id==TKID_RALTER)
      return super:getStr()
   endif
   str:="["
   for i:=1 to len(this:tokenList)
      str+=TOKEN.(this:tokenList[i]):getStr()
   end for
          
   str+="]"
   if (this:isError())
      str+=", Error: "+this:errorStr()+guessedEol()
   endif
   
return str

//*******************************************************************
   



