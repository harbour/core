//
// $Id$
//

//*******************************************************************
// sreader.cls: Stringb“l karaktereket olvas¢ oszt ly. A CREADER-t“l 
//              ”r”k”l.
// 1999, Csisz r Levente

class SREADER CREADER

   attrib str     // A string, amib“l olvasunk,

   attrib istr    // A poz¡ci¢ a stringben.
      
   rmethod readItem()
   // Olvas egy karaktert. 
   // Ret: A karakter, ha OK, nil, ha v‚ge van a streamnek, vagy ha
   //      hiba volt.
   
   rmethod oinit(str,name,errorStream)
   // Inicializ lja az objektumot.
   
metaclass

   rmethod onew(str,name,errorStream)


