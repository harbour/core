/*
 * $Id$
 */

//*******************************************************************
// reader.prg: A reader oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "tbuffer.och"

//*******************************************************************
#define _READER_PRG_
#define _IMPLEMENT_ONEW_

#include "reader.och"

//*******************************************************************

implement oinit(name,errorStream)
   super:oinit()
   this:name:=name
   this:tbuffer:=C.TBUFFER:onew()
   this:errorStream:=errorStream
return this

//*******************************************************************
implement read()
local w

   if (nil!=(w:=TBUFFER.(this:tbuffer):get()))
      return w
   endif
return this:readItem()
   
//*******************************************************************
implement unread(item)
   TBUFFER.(this:tbuffer):unget(item)
return nil
   
//*******************************************************************
implement readTBuffer()
return TBUFFER.(this:tbuffer):get()
   
//*******************************************************************
implement arrayTBuffer()
local i,r:={}

   for i:=1 to TBUFFER.(this:tBuffer):bItemNumber
      aadd(r,TBUFFER.(this:tBuffer):getBItem(i))
   end for
   
return r

//*******************************************************************
implement isError(l)

   if (empty(this:errorStream))
      return .f.
   endif

   if (empty(l))
      return .t.
   endif

return len(this:errorStream)>l

//*******************************************************************
implement addError(prsErr)
   if (this:errorStream!=nil)
      aadd(this:errorStream,prsErr)
   endif
return nil

//*******************************************************************
#ifdef OLD
implement errorStr()
local errStr

   if (!this:isError())
      return nil
   endif
   
   errStr:=""
   if (!empty(this:error[1]))
      errStr+=this:error[1]+": "
   endif
   errStr+=this:error[3]+", errCode: "+toStr(this:error[2])
return errStr
#endif
   
//*******************************************************************

