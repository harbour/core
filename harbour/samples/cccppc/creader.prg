/*
 * $Id$
 */

//*******************************************************************
// creader.prg: A creader oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

#include "objgen.ch"

#define _CREADER_PRG_
#define _IMPLEMENT_ONEW_

#include "creader.och"
#include "ctoken.ch"

//*******************************************************************
#include "cr_lf.ch"

//*******************************************************************
implement read() // Itt ez kell Çs nem a readItem()
local c,c2,w

   c:=super:read()
   if (valtype(c)=="C")
      if (c==CHAR_CR)
         c2:=super:read()
         if (valtype(c2)=="C" .and. c2==CHAR_LF)
            w:=aclone(CTK_EOL)
            w[2]:=c+c2
            w[3]:=this:pos
            c:=w
            this:line++
            this:pos:=1
         else
            super:unread(c2)
         endif
      elseif (c==CHAR_LF)
         w:=aclone(CTK_EOL)
         w[2]:=c
         w[3]:=this:pos
         c:=w
         this:line++
         this:pos:=1
      elseif (this:pos!=0) // Ez kell!!!! Ezzel jelzi, hogy a sorra 
                           // visszalÇptek, Çs °gy a poz°ci¢ nem
                           // ismert.
         this:pos++
      endif
   endif
return c

//*******************************************************************
implement unread(item)
// A line-t Çs a poz°ci¢t vissza†ll°tja, az EOS-t Çs a BOS-t viszont 
// nem figyeli. (Ezt az lreader-nek kellene megtennie, ha 
// szÅksÇges.)
local str
/*
   if (valtype(item)=="C")
      if (CHAR_LF$item)
         // Itt sor sz†ml†l¢t ugyan vissza lehet †ll°tani, de a
         // poz°ci¢t nem, mert ahhoz tudni kellene, milyen hossz£
         // volt az elìzì sor.
         // Mj.: Ez lehetsÇges lenne, ha a CR_LF-be belek¢doln†nk a
         //      sor hosszt. Ekkor persze token lenne Çs nem dupla 
         //      karakter.
         this:pos:=0
         this:line-=xnumat(CHAR_LF,item)
      elseif (this:pos!=0)
         this:pos-=len(item)
      endif
   endif
*/
   if (valtype(item)=="A")
      if (item[1]==CTKID_EOL)
         str:=item[2]
         this:pos:=item[3]
         this:line--
      else
         str:=item
      endif
   else
      str:=item
   endif
return super:unread(str)

//*******************************************************************
implement getFile()
return this:name

//*******************************************************************
implement getLine()
return this:line

//*******************************************************************
implement getPos()
return this:pos


//*******************************************************************
implement oinit(name,errorStream)
   super:oinit(name,errorStream)
   
   this:line:=1
   this:pos:=1
return this

//*******************************************************************

