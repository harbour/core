//
// $Id$
//

//*******************************************************************
// maltrset.cls : Match alternative set token oszt ly.
// 1999, Csisz r Levente

// A bal oldalon el“fordul¢ altenat¡va halmazokat t rol¢ token.
// Az alternat¡v kat egy-egy token t”mb jelk‚pezi, ezeknek a t”mbje
// lesz egy alternat¡va halmaz.

class MALTRSET MMARKER

   attrib alterset     // Token list kat tartalmaz, minden lista
                       // egy-egy alternat¡va.


   nmethod addAlter(tokenList)
   // L‚trehoz egy £j alternat¡v t, amit a tokenList reprezent l.
   
   rmethod getStr()
   
   rmethod oinit(id,str,file,line,pos)
   // Inicializ lja az objektumot.
   
metaclass

   rmethod onew(id,str,file,line,pos)


