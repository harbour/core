/*
 * $Id$
 */

//*******************************************************************
// felbont.prg: Egy string felbont†sa elv†laszt¢ karakterek mentÇn.
// 1999, Csisz†r Levente

//*******************************************************************
function felbont(str,elvalasztok)
/*
 Az str-t felbontja az 'elvalasztok' stringben levì karaktereknÇl, Çs
 ad egy tîmbît, amiben az elemek vannak.
 Ha az 'elvalasztok' nincs megadva, akkor " "+chr(9)-t tÇtelez fel.
 Az Åreseket kiveszi.
 Mj.: A chr(9) a tab karakter.
*/

local t,e,i,pos,elem

   if (elvalasztok==nil)
      elvalasztok:=" "+chr(9)
   elseif (len(elvalasztok)<=0)
      if (str=="")
         return {}
      else
         return {str}
      endif
   endif

   e:=left(elvalasztok,1)
   for i:=2 to len(elvalasztok)
      str:=strtran(str,substr(elvalasztok,i,1),e)
   end for      
   
   t:={}
   while(0<len(str))
      if (0==(pos:=at(e,str)))
         elem:=str
         str:=""
      elseif (pos==1)
         elem:=""
         str:=substr(str,2)
      else
         elem:=substr(str,1,pos-1)
         str:=substr(str,pos+1)
      endif
      if (!elem=="")
         aadd(t,elem)
      endif
   end while

return t

//*******************************************************************

#ifdef OLD   
local wlist:={}, n:=0, i,sep1,welem

    if(sep==NIL)
        sep:=" "+chr(9)
    end

    sep1:=left(sep,1)
    for i:=2 to len(sep)
       txt:=strtran(txt,substr(sep,i,1),sep1)
    next

    while( n<len(txt) )
        txt:=substr(txt,n+1)
    
        if( (i:=at(sep1,txt))==0 )
            wElem:=txt
            n:=len(txt)
        elseif(i==1)
            wElem:=""
            n:=1
        else
            wElem:=substr(txt,1,i-1)
            n:=i
        end
        if !(wElem=="")
           aadd(wlist,wElem)
        endif
    end
    return wlist
#endif

************************************************************************
