/*
 * $Id$
 */

//*******************************************************************
// mmarker.prg: az MMARKER oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

#define _MMARKER_PRG_
#define _IMPLEMENT_ONEW_

#include "mmarker.och"

//*******************************************************************
implement oinit(id,str,file,line,pos)
   super:oinit(id,str,file,line,pos)
   this:mNum :=nil
return this

//*******************************************************************
implement getName()
return this:str

   
//*******************************************************************
cimplement isMatchMarker(aToken)
// Meg llp¡tja, hogy az aToken egy match marker-e.
// Ret: .t., ha igezn, .f., ha nem.
static idMatchMarkers:={;
   TKID_REGULAR_MATCH_MARKER     ,;
   TKID_WILD_MATCH_MARKER        ,;
   TKID_EXT_EXPR_MATCH_MARKER    ,;
   TKID_LIST_MATCH_MARKER        ,;
   TKID_RESTRICTED_MATCH_MARKER  ;
} 

return 0!=ascan(idMatchMarkers,TOKEN.aTOken:id)   

//*******************************************************************


