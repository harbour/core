//
// $Id$
//

//*******************************************************************
// tkstr.cls: String token reprezent ci¢. 
// 1999, Csisz r Levente

class TKSTR TOKEN

   attrib kezdo        // Az eredeti kezd“.
   attrib zaro         // Ha hi nyzik, akkor ""

   rmethod getStr()
   
   rmethod oinit(id,str,file,line,pos,kezdo,zaro)
   // Inicializ lja az objektumot.
   
metaclass

   rmethod onew(id,str,file,line,pos,kezdo,zaro)

   rmethod copyFromToken(t,id,str,kezdo,zaro)

