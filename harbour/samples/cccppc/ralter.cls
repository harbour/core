//
// $Id$
//

//*******************************************************************
// ralter.cls : Result alternative token oszt ly.
// 1999, Csisz r Levente

// A jobb oldalon el“fordul¢ altenat¡v kat t rol¢ token.
// Az alternat¡v kat egy-egy token t”mb jelk‚pezi, egy ilyen t”mb
// van a tokenList-ben.

class RALTER RMARKER

   attrib tokenList    // Az alternat¡v hoz tartoz¢ tokenek list ja.

   rmethod getStr()

   rmethod oinit(id,str,file,line,pos)
   // Inicializ lja az objektumot.
   
metaclass

   rmethod onew(id,str,file,line,pos)


