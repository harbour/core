//
// $Id$
//

//*******************************************************************
// mmarker.cls : Match marker token oszt ly.
// 1999, Csisz r Levente

// Match markereket t rol¢ token.

class MMARKER TOKEN

   attrib mNum  // A match marker sorsz ma egy makr¢ defin¡ci¢ban.
     
   attrib nextToken // A regular match markerek olyanok, hogy az 
                    // elemz“ el“re n‚z a k”vetkez“ tokenhez.
                    // Ezt a tokent t rolja itt. Ha nil, akkor
                    // nincs ilyen token.
   rmethod oinit(id,str,file,line,pos)
   // Inicializ lja az objektumot. Az str lesz a match marker neve.
   
   nmethod getName()
   
metaclass

   rmethod onew(id,str,file,line,pos)


   nmethod isMatchMarker(aToken)
   // Meg llp¡tja, hogy az aToken egy match marker-e.
   // Ret: .t., ha igezn, .f., ha nem.

