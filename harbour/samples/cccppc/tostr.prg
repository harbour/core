/*
 * $Id$
 */

//*******************************************************************
// tostr.prg: rt‚k -> string konverzi¢.
// 1998, Csisz r Levente

/*
   1998.05.,21, Csisz r Levente
   
      - Kiv‚ve a cslutils.prg -b“l.
   
*/

//*******************************************************************
function toStr(val)
// A val-t stringg‚ alak¡tja
local type:=valType(val)
   if (type=="N")
      return allTrim(str(val))
   elseif (type=="D")
      return dtoc(val)
   elseif (type=="L")
      return if(val,"T","F")
   elseif (type=="C" .or. type=="M")
      return val
   elseif (val==nil)
      return "nil"
   endif
   alert("toStr: Nem konvert lhat¢ t¡pus: '"+type+"'")
return ""

//*******************************************************************

