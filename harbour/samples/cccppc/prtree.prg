/*
 * $Id$
 */

//*******************************************************************
// prtree.prg: A PRTREE oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#define _PRTREE_PRG_
// #define _IMPLEMENT_ONEW_

#include "prtree.och"

//*******************************************************************
implement oinit()
           
   super:oinit()
   this:nWords:=0
   this:tree:={}
   
return this
   

//*******************************************************************
static function addTPTree(tPTree,ctId,ctArray,ctI)
local t,i,node

   if (len(ctArray)<ctI)
      // Itt a v‚ge.
      node:={nil,ctId}
      if (len(tPTree)==0)
         aadd(tPTree,node)
         return nil
      endif
      if (tpTree[1][1]==nil)
         // M r van egy termin lisunk ugyanilyen tartalommal.
         return "addTPTree: M r van ilyen token a f ban: "+toExprStr(ctArray)
      endif
      aunget(tPTree,node)
      return nil
   endif
   t:=ctArray[ctI++]
   for i:=1 to len(tPTree)
      if (tPTree[i][1]==t)
         return addTPTree(tPTree[i][2],ctId,ctArray,ctI)
      endif      
   end for
   // Hozz  kell venni.
   node:={t,{}}
   aadd(tPTree,node)
return addTPTree(node[2],ctId,ctArray,ctI)

//*******************************************************************

implement addWord(result,itemArray)
local w

   if (nil==(w:=addTPTree(this:tree,result,itemArray,1)))
      this:nWords++
   endif
return w

//*******************************************************************


