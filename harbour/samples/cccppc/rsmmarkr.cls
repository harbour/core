//
// $Id$
//

//*******************************************************************
// rsmmarkr.cls : Restricted match marker token oszt ly.
// 1999, Csisz r Levente

// Egy restricted match markert implement l.
// A v laszthat¢ nevek a wordList t”mbben vannak string-k‚nt.

class RSMMARKR MMARKER

   attrib wordList // Neveket tartalmaz (stringk‚nt), amiket 
                   // v lasztani lehet.

   rmethod getStr()
   
   rmethod oinit(id,str,file,line,pos)
   // Inicializ lja az objektumot.
   
metaclass

   rmethod onew(id,str,file,line,pos)


