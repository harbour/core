//
// $Id$
//

//*******************************************************************
// edefdict.cls: #define sz¢t r elem
// 1999, Csisz r Levente

class EDEFDICT OBJECT

   attrib name
   attrib params
   attrib body   // {[tokenArray|paramNumber],...}
   attrib deffilelinepos

   nmethod change(paramValues)
   // A paramValues tartalmazza a param‚terek ‚rt‚keit.
   
   nmethod printStr()
   // Stringk‚nt adja meg az elemet #define n‚v(params) body
   // form ban.
   
   rmethod oinit(name,params,body,deffile,defline,defpos)


metaclass

   rmethod onew(name,params,body,deffile,defline,defpos)

