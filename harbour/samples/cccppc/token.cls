//
// $Id$
//

//*******************************************************************
// token.cls: Token reprezent†ci¢.
// 1999, Csisz†r Levente

class TOKEN OBJECT

   attrib id           // A token azonos°t¢ja.
   attrib str          // A token tartalma. Ez jîtt az inputr¢l.
   attrib file         // A file (reader) neve, ahol a tokent 
                       // tal†lta.
                       
   attrib line         // A sor, ahol a token volt a stream-ben.
   attrib pos          // A poz°ci¢ a soron belÅl, ahol a token volt
                       // a stream-ben.
   attrib classify     // Az ekvivalencia oszt†lya. Ez egy string.
   attrib error        // Ha a token hib†s volt (pl befejezetlen 
                       // string etc), akkor a hiba k¢dja.
   attrib eqtype
   
   nmethod isError()
   // Igaz, ha a token hib†s.
   
   nmethod errorStr()
   // Nil, ha nem volt hiba, egy string, ami a hiba le°r†s†t
   // tartalmazza, ha volt hiba.

   nmethod printToStr()
   // Ad egy stringet, ami a token olvashat¢ form†ban.
   
   nmethod getStr()
   // A token str-jÇt adja string form†ban, ha hiba volt, akkor
   // a hibak¢dot is hozz†teszi szîvegesen.
   
   nmethod copyToken(id,str)
   // KÇsz°t egy olyan tokent, ami ennek a m†solata, csak az id-je
   // Çs az str-je m†s, Çs az error nil.
   
   rmethod oinit(id,str,file,line,pos)
   // Inicializ†lja az objektumot.
   
metaclass

   rmethod onew(id,str,file,line,pos)

   nmethod onewError(id,str,file,line,pos,error)
                       
   attrib errStrArray
   
   rmethod oinitclass()

   nmethod copyFromToken(t,id,str)

   attrib equivClass

   nmethod findClassify(str)

