/*
 * $Id$
 */

//*******************************************************************
// rsmmarkr.prg: Az RSMMARKR oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

// #include "token.och"

//*******************************************************************
#define _RSMMARKR_PRG_
#define _IMPLEMENT_ONEW_

#include "rsmmarkr.och"

//*******************************************************************
implement oinit(id,str,file,line,pos)
   super:oinit(id,str,file,line,pos)
   this:wordList :={}
return this

//*******************************************************************
implement getStr()
local str,i
    
   if (!this:id==TKID_RESTRICTED_MATCH_MARKER)
      return super:getStr()
   endif
   str:=""
   for i:=1 to len(this:wordList)
      if (i>1)
         str+=","
      endif
      str+=this:wordList[i]
   end for
         
   str:="<"+if(this:str==nil,"",this:str)+":"+str+">"
   if (this:isError())
      str+=", Error: "+this:errorStr()+guessedEol()
   endif
   
return str

//*******************************************************************
   



