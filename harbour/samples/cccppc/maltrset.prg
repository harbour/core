/*
 * $Id$
 */

//*******************************************************************
// maltrset.prg: a MALTRSET oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

#define _MALTRSET_PRG_
#define _IMPLEMENT_ONEW_

#include "maltrset.och"

//*******************************************************************
implement oinit(id,str,file,line,pos)
   super:oinit(id,str,file,line,pos)
   this:alterset :={}
return this

//*******************************************************************
implement addAlter(tokenList)
   aadd(this:alterset,tokenList)
return nil

//*******************************************************************
implement getStr()
local str,i,j,alter
    
   if (!this:id==TKID_MALTERSET)
      return super:getStr()
   endif
   str:=""
   for i:=1 to len(this:alterset)
      str+="["
      alter:=this:alterset[i]
      for j:=1 to len(alter)
         str+=TOKEN.(alter[j]):getStr()
      end for
      str+="]"
   end for
      
   if (this:isError())
      str+=", Error: "+this:errorStr()+guessedEol()
   endif
   
return str
//*******************************************************************
   



