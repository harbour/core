/*
 * $Id$
 */

//*******************************************************************
// lreader.prg: Az lreader oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

//*******************************************************************
#include "debug.ch"

//*******************************************************************
#include "objgen.ch"

#define _LREADER_PRG_
#define _IMPLEMENT_ONEW_

#include "lreader.och"

//*******************************************************************

implement oinit(BOSItem,EOSItem,name,errorStream)
   super:oinit(name,errorStream)
   this:readerStack:={}
   this:BOSItem:=BOSItem
   this:EOSItem:=EOSItem
return this

//*******************************************************************
implement readItem()
// Mj.: Ha azt a koncepci¢t v†lasztjuk, hogy a getFile(),getLine(),
// getPos() a legfelsì reader-nek van †tir†ny°tva, akkor a read()-et
// Çs az unread()-et is felÅl kell defini†lni.
local r,t,w

   if (len(this:readerStack)<=0)
      return nil
   endif
   
   r:=alast(this:readerStack)
   if (r[2])
      r[2]:=.f.
      // this:name:=READER.r[1]:name
      if (nil!=(w:=this:getBOSItem()))
         PDEBUG(outerr("BOS: ",w,crlf()))
         return w
      endif
   endif   

   r:=r[1]
   if (nil==(t:=READER.r:read()))
      // if (READER.r:isError())
      //    this:error:=READER.r:error
      //    return nil
      // endif
      w:=this:getEOSItem()
      this:dropReader()
      // apop(this:readerStack)
      if (!empty(this:readerStack))
         atail(this:readerStack)[2]:=.t.
      endif
      if (nil!=w)
         PDEBUG(outerr("EOS: ",w,newline()))
         return w
      endif                          
      // Ez azÇrt kell, hogy amikor egy reader £jra akt°v lesz, akkor
      // jîjjîn egy BOS.
      return this:read()
   endif
   
return t 
   
//*******************************************************************
implement pushReader(aReader)
   aadd(this:readerStack,{aReader,.t.})
return nil

//*******************************************************************
implement topReader()
   if (len(this:readerStack)<=0)
      return nil
   endif
return atail(this:readerStack)[1]
   
//*******************************************************************
implement popReader()
   if (len(this:readerStack)<=0)
      return nil
   endif
return apop(this:readerStack)[1]

//*******************************************************************
implement dropReader()
local w

   if (len(this:readerStack)<=0)
      return nil
   endif
   w:=apop(this:readerStack)[1] // A segÇdv†ltoz¢ kell, mert az 
                                // objektum helyÇn †ll¢ kifejezÇst 
                                // kÇtszer ÇrtÇkeli ki!
   READER.w:destruct()
return nil

//*******************************************************************
implement isEmpty()
return len(this:readerStack)<=0
   
//*******************************************************************
implement getBOSItem()
local b

   if (this:BOSItem==nil)
      return nil
   endif
   
   b:=aclone(this:BOSItem)
   b[2]:=this:getFile()
   b[3]:=this:getLine()
   b[4]:=this:getPos()
   b[5]:=len(this:readerStack)
return b

//*******************************************************************
implement getEOSItem()
local e

   if (this:EOSItem==nil)
      return nil
   endif
   
   e:=aclone(this:EOSItem)
   e[2]:=this:getFile()
   e[3]:=this:getLine()
   e[4]:=this:getPos()
   e[5]:=len(this:readerStack)
   
return e

//*******************************************************************
implement getFile()
   if (!empty(this:readerStack))
      return CREADER.(atail(this:readerStack)[1]):getFile()
   endif
return this:name
   

//*******************************************************************
implement getLine()
   if (!empty(this:readerStack))
      return CREADER.(atail(this:readerStack)[1]):getLine()
   endif
return this:line

//*******************************************************************
implement getPos()
   if (!empty(this:readerStack))
      return CREADER.(atail(this:readerStack)[1]):getPos()
   endif
return this:pos

//*******************************************************************
   

