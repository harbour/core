/*
 * $Id$
 */

//*******************************************************************
// prtreepr.prg: A PRTREEPR oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

//*******************************************************************
#include "objgen.ch"

#include "prtree.och"

//*******************************************************************
#define _PRTREEPR_PRG_
#define _IMPLEMENT_ONEW_

#include "prtreepr.och"

//*******************************************************************
implement oinit(eprtree)
           
   super:oinit()
   this:eprtree:=eprtree
   this:start()
   
return this
   
//*******************************************************************
implement start()

   if (this:eprtree!=nil)
      this:subtree      :=PRTREE.(this:eprtree):tree
   else
      this:subtree      :={}
   endif
   this:result          :=nil
   this:itemArray       :={}
   this:wordLen         :=0
   this:lastStopWordLen :=0
   // this:lastStopResult  :=nil  // Ez igaz†b¢l nem kell, elÇg a result is.

return nil

//*******************************************************************
implement put(item)
/*
  Egy item-el tov†bb megy az elemzì f†ban.
  Az itemeket gyñjti az itemArray-ban.
  
  Ret: nil, ha nincs vÇge.
       .t., ha vÇge van Çs elfogadta,
       .f., ha vÇge van Çs nem fogadta el.
*/

local tptree,nRds
local str,lastStop,i

   aadd(this:itemArray,item)

   if (0==(i:=ascan(this:subtree,{|x| x[1]==item})))
      // Nincs tov†bb, viszont a lastStop-ban van olyan token,
      // ami illeszkedik ennek az elejÇhez.
      this:wordLen:=this:lastStopWordLen
      // this:result:=this:lastStopResult
      return this:wordLen!=0
   endif
   
   // Van ilyen item, megyÅnk lejjebb a f†ban
   this:wordLen++
   this:subtree:=this:subtree[i][2]
   // Elìszîr megnÇzzÅk, hogy a vÇgÇre ÇrtÅnk-e.
   if (len(this:subtree)==0)
      // Hiba, vagy Åres a fa, vissza a lastStop-hoz.
      this:wordLen:=this:lastStopWordLen
      // this:result:=this:lastStopResult
      return this:wordLen!=0
   endif
   if (this:subtree[1][1]==nil)
      // Itt meg lehet †llni, pl. ha a kîvetkezì keresÇs nem hoz
      // eredmÇnyt.
      if (len(this:subtree)==1)
         // Nem lehet tov†bb menni, de visszalÇpni sem kell.
         // id:=this:subtree[1][2]
         this:result:=this:subtree[1][2]
         return this:wordLen!=0
      endif
      // Lehet mÇg tov†bb, de ha elakad, akkor ez j¢!
      this:lastStopWordLen:=this:wordLen
      // this:lastStopResult:=this:subtree[1][2]
      this:result:=this:subtree[1][2]
   endif

   // MÇg nincs eredmÇny, megyÅnk tov†bb.
   
return nil

//*******************************************************************



