//
// $Id$
//

//*******************************************************************
// creader.cls: èltal†nos 'streambìl karaktert' olvas¢ oszt†ly.
// 1999, Csisz†r Levente

// Ebbìl îrîkîl a filÇ Çs a string olvas¢ oszt†ly.
// A val¢di lesz†rmazottaknak a readItem()-et kell implement†lniuk.

class CREADER READER
           
   attrib line
   attrib pos  // 0, ha a poz°ci¢ ismeretlen.
   
   rmethod read()
   // A super:read()-al olvassa a stream-et. 
   // Az egym†s ut†n kîvetkezì CR Çs LF karaktereket egy CR_LF (kettì
   // hossz£) karakterrÇ alak°tja.
   // Sz†molja a sorokat Çs nyilv†ntartja a poz°ci¢t.
   
   rmethod unread(item)
   // Visszateszi az item-et az inputra.
   // Vissza sz†molja a sorokat. ójsor esetÇn a poz°ci¢t null†zza.

   nmethod getFile()
   nmethod getLine()
   nmethod getPos()

   rmethod oinit(name,errorStream)
   // Inicializ†lja az objektumot.
   
metaclass

   rmethod onew(name,errorStream)
                    
