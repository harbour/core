//
// $Id$
//

//*******************************************************************
// parser.cls: ltal nos elemz“ oszt ly.
// 1999, Csisz r Levente


// Az inputr¢l olvasott 'item'-eket egy parserBuffer-ben t rolja,
// ‚s az 'item' attributumba is beolvassa.
// A pufferb“l az inputra vissza tud tenni 'item'-eket.
// A puffer karakter r‚sz‚t meg tudja adni stringk‚nt.
 

class PARSER TREADER

   attrib item   
   attrib parserBuffer
         
   rmethod readItem()
   // Olvas egy elemet. 
   // Ret: Az elem, ha OK, nil, ha v‚ge van a streamnek, vagy ha
   //      hiba volt.

   nmethod putParserBuffer(anItem)
   // Hozz ad egy elemet a puffer v‚g‚hez Az elem lehet speci lis is.

   nmethod getParserBuffer()
   // Kivesz egy elemet a puffer elej‚r“l.

   nmethod unputParserBuffer()
   // Kivesz egy elemet a puffer v‚g‚r“l.

   nmethod ungetParserBuffer(anItem)
   // Betesz egy elemet a puffer elej‚re.

   nmethod clearParserBuffer()
   // T”rli a parserBuffer()-t.
   
   nmethod rds()
   // Olvas egy karatert a readInput()-al a lineBuf-ba.
   // Az olvasott ‚rt‚ket a buf-ba is beleteszi, ha az nem az eof.
   
   nmethod unrds(n)
   // A buf utols¢ n elem‚t visszateszi az inputra. Az item-et t”rli.
   // Azt adja vissza, hogy h ny elemet sikerlt visszatenni.
   // Ha az n nil, akkor egyet tesz vissza.
      
   nmethod strParserBuffer()
   // Megadja a parserBuffer tartalm t stringk‚nt.
   // A nem stringeket eldobja.
   
   nmethod arrayParserBuffer()
   // Ad egy t”mb”t, amiben a parserBuffer tartalma van.
   
   rmethod oinit(inputReader,name,errorStream)
   
metaclass
   
   rmethod onew(inputReader,name,errorStream)
   

