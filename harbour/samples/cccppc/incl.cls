//
// $Id$
//

//*******************************************************************
// incl.cls: Include direktorik nyilv ntart sa, £j readerek nyit sa
//           az include fil‚kre.
// 1999, Csisz r Levente

class INCL OBJECT
                        
   attrib lreader            // Ebben vannak a nyitott include fil‚k.

   attrib includeList        // A direktorik, amiben keresni kell
                             // az input fil‚ket.
                             
   attrib maxInclDeep        // Legfeljebb ennyi lehet egym sba 
                             // skatuly zva.
                             // 0: Nincs limit.
                             
   nmethod openIncludeFile(filename)
   
   rmethod oinit(lreader,includeList,maxInclDeep)
 
metaclass

   rmethod onew(lreader,includeList,maxInclDeep)






