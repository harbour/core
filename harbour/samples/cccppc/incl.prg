/*
 * $Id$
 */

//*******************************************************************
// incl.prg: Az INCL oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

//*******************************************************************
#include "debug.ch"

//*******************************************************************
#include "objgen.ch"

#include "lreader.och"
#include "freader.och"

#define _INCL_PRG_
#define _IMPLEMENT_ONEW_

#include "incl.och"

//*******************************************************************

implement oinit(lreader,includeList,maxInclDeep)
   super:oinit()
   this:lreader:=lreader
   this:includeList:=includeList
   this:maxInclDeep:=maxInclDeep
return this

//*******************************************************************
implement openIncludeFile(filename)
// Ret: 0: sikerÅlt, 1: nyit†si hiba, 2: nem tal†lhat¢, 3: t£l sok
//      egym†sba skatuly†z†s.
// Ha a name tartalmaz path-t, akkor megnÇzi a kurrens direktoriban 
// is, egyÇbkÇnt pedig csak az includeList-ben.

local fr,l

   if (this:maxInclDeep>0 .and.;
       len(LREADER.(this:lreader):readerStack)>=this:maxInclDeep)
      return 3
   endif
      
   // ågy kÇtszer is jîn hibaÅzenet, egyszer az freader-tìl, egyszer
   // pedig a hparser-tìl, de ez nem baj.
   // Mj.: Ha csak egy Åzenetet akarunk, akkor az freader 
   //      hibaÅzeneteit Çrtelmezni kell, Çs hparser(include) 
   //      hibaÅzenetekkÇ alak°tani.
   fr:=C.FREADER:onew(LREADER.(this:lreader):errorStream)
   
   l:=len(FREADER.fr:errorStream)
   
   if (!dirFName(filename)=="")
      FREADER.fr:dOpen({""},filename)
      if (FREADER.fr:isError(l))
         // Nem sikerÅlt megnyitni.
         return 1
      endif
   endif
   
   if (!FREADER.fr:isOpen())
      FREADER.fr:dOpen(this:includeList,filename)
   endif
   
   if (FREADER.fr:isError(l))
      // Nem sikerÅlt megnyitni.
      return 1
   endif

   if (!FREADER.fr:isOpen())
      return 2
   endif

   // Itt kell befñzni a l†ncba.
      
   LREADER.(this:lreader):pushReader(fr)

   PDEBUG(outstd("include: "+filename))

return 0
//*******************************************************************





