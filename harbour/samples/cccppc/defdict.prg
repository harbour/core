/*
 * $Id$
 */

//*******************************************************************
// defdict.prg: #define sz¢t r
// 1999, Csisz r Levente

//*******************************************************************
#include "ctoken.ch"
#include "objgen.ch"

//*******************************************************************
#include "token.och"
#include "edefdict.och"

//*******************************************************************

#define _DEFDICT_PRG_
// #define _IMPLEMENT_ONEW_

#include "defdict.och"


//*******************************************************************
implement oinit()

   super:oinit()
   
   this:dict:={}
return nil

//*******************************************************************
implement add(edefdict)
local w

   
   if (nil!=(w:=this:atKey(EDEFDICT.edefdict:name)))
      return w
   endif
   
   aadd(this:dict,edefdict)
return nil

//*******************************************************************
implement atIdx(name)
local i

   for i:=1 to len(this:dict)
      if (name==EDEFDICT.(this:dict[i]):name)
         return i
      endif
   end for
return 0

//*******************************************************************
implement atKey(name)
local i

   if (0!=(i:=this:atIdx(name)))
      return this:dict[i]
   endif
return nil   

//*******************************************************************
implement delKey(name)
local i

   if (0!=(i:=this:atIdx(name)))
      adel(this:dict,i)
      asize(this:dict,len(this:dict)-1)
   endif
return nil   

//*******************************************************************
implement printStr(printBlock)
local i,w
local str:=""

   for i:=1 to len(this:dict)
      w:=toStr(i)+": "+EDEFDICT.(this:dict[i]):printStr()
      if (printBlock!=nil)
         eval(printBlock,w)
      else
         str+=w+newline()
      endif
   end for
return str

//*******************************************************************

