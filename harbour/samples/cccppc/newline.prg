/*
 * $Id$
 */

//*******************************************************************
// newline.prg: —j sor karakter.
// 1999, Csisz r Levente

//*******************************************************************
function newline()
static nl
#ifdef _UNIX_
   if (nl==nil)
      nl:=chr(10)
   endif
#else
   if (nl==nil)
      nl:=chr(13)+chr(10)
   endif
#endif
return nl

//*******************************************************************

