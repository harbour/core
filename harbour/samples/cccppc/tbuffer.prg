/*
 * $Id$
 */

//*******************************************************************
// tbuffer.prg: A tbuffer oszt ly implement ci¢ja.
// 1999, Csisz r Levente

#include "objgen.ch"

#define _TBUFFER_PRG_
#define _IMPLEMENT_ONEW_

#include "tbuffer.och"

//*******************************************************************
#define TBID_CHARARRAY "C"
#define TBID_NORMAL    "N"

//*******************************************************************
#define TB_MAXSGET     100

//*******************************************************************
implement oinit()
   super:oinit()
   this:buffer:={}
return this

//*******************************************************************
implement put(item)
local e

   if (item==nil)
      return item
   endif
   
   if (!valtype(item)=="C")
      aadd(this:buffer,{TBID_NORMAL,item})
      return item
   endif
   if (len(this:buffer)<=0)
      aadd(this:buffer,{TBID_CHARARRAY,item,1})
      return item
   endif
   e:=atail(this:buffer)
   if (e[1]==TBID_NORMAL)
      aadd(this:buffer,{TBID_CHARARRAY,item,1})
      return item
   endif
   e[2]:=e[2]+item
return item
      
//*******************************************************************

//*******************************************************************
implement unget(item)
local e

   if (item==nil)
      return item
   endif
   
   if (!valtype(item)=="C")
      aunget(this:buffer,{TBID_NORMAL,item})
      return item
   endif
   if (len(this:buffer)<=0)
      aunget(this:buffer,{TBID_CHARARRAY,item,1})
      return item
   endif
   e:=this:buffer[1]
   if (e[1]==TBID_NORMAL)
      aunget(this:buffer,{TBID_CHARARRAY,item,1})
      return item
   endif
   if (e[3]>1)
      e[2]:=substr(e[2],e[3])
      e[3]:=1
   endif
   e[2]:=item+e[2]
return item
      
//*******************************************************************
implement get()
local e,item

   if (len(this:buffer)<=0)
      return nil
   endif
   
   e:=this:buffer[1]
   
   if (e[1]==TBID_NORMAL)
      return aread(this:buffer)[2]
   endif
   
   if (e[3]>len(e[2]))
      aread(this:buffer)
      return this:get()
   endif
   
   item:=substr(e[2],e[3],1)
   
   e[3]:=e[3]+1
   
   if (e[3]>len(e[2]))
      aread(this:buffer)
      return item
   endif
   
   if (e[3]>TB_MAXSGET)
      e[2]:=substr(e[2],e[3])
      e[3]:=1
   endif
   
return item

//*******************************************************************
implement unput()
local e,item

   if (len(this:buffer)<=0)
      return nil
   endif
   
   e:=atail(this:buffer)
   
   if (e[1]==TBID_NORMAL)
      return apop(this:buffer)[2]
   endif
   
   if (e[3]>len(e[2]))
      adrop(this:buffer)
      return this:unput()
   endif
   
   item:=right(e[2],1)
   e[2]:=substr(e[2],1,len(e[2])-1)
return item

//*******************************************************************
implement clear()
   this:buffer:={}
return nil

//*******************************************************************
implement bItemNumber()
return len(this:buffer)
   
//*******************************************************************
implement getBItem(i)
local e

   e:=this:buffer[i]
   if (e[1]==TBID_NORMAL)
      return e[2]
   endif
return substr(e[2],e[3])
   
//*******************************************************************
implement appendBuffer(aTBuffer)
   aappend(this:buffer,CTHIS.aTBuffer:buffer)
return this

//*******************************************************************
// cimplement oinitclass()
//    superclass:oinitclass()
// return class
//*******************************************************************


