//
// $Id$
//

//*******************************************************************
// freader.cls: Fil‚b“l karaktereket olvas¢ oszt ly. A CREADER-t“l 
// ”r”k”l.
// 1999, Csisz r Levente

class FREADER CREADER

   attrib path    // Az olvasott fil‚ path-a. (Az fil‚ neve a 
                  // name-ben van.) Mj.: A n‚vben is lehet path.
   attrib fid     // Az olvasott fil‚ azonos¡t¢ja. (Ha nyitva van)
   
   attrib buffer  // A fil‚ olvas sakor a buffer. Ha nil, akkor 
                  // EOF-on vagyunk.
   attrib ibuffer // A fil‚ olvas sakor a bufferben a poz¡ci¢.
   
   nmethod pathName()
   // Megadja a teljes fil‚nevet (az el‚r‚si £ttal egytt).
   // Ha a fileName abszol£t n‚v ('/' kezd“dik vagy (dos-on)
   // drive n‚v van benne), akkor a path-t nem teszi el‚.
    
   nmethod open(path,fileName)
   // Megnyitja az adott fil‚t az adott path-al.
   // Mj.: A fileName-ben is lehet path.
   // Ret: this, ha sikerlt a megnyit s, nil, ha nem.
   //      Hiba eset‚n a this:error-t be ll¡tja.
   
   nmethod close()
   // Lez rja a fil‚t. Ha a nincs nyitva, nem csin l semmit.
   
   nmethod dOpen(pathArray,fileName)
   // V‚gigpr¢b lgatja a path-okat, ‚s amelyikben megtal lja a 
   // fil‚t, akkor abban megpr¢b lja megnyitni.
   // Mj.: A fileName-ben is lehet path.
   // Ret: this, ha sikerlt a megnyit s, nil, ha nem.
   //      Hiba eset‚n a this:error-t be ll¡tja.

   nmethod isOpen()
   // Igaz, ha a fil‚ nyitva van.
      
   rmethod readItem()
   // Olvas egy karaktert. 
   // Ret: A karakter, ha OK, nil, ha v‚ge van a streamnek, vagy ha
   //      hiba volt.
   
   rmethod destruct() 
   // T”rli az objektumhoz kapcsol¢d¢ kls“ eszk”z”ket. (Pl. lez rja
   // a fil‚t etc.)
                          
   rmethod oinit(errorStream)
   // Inicializ lja az objektumot.
   
metaclass

   rmethod onew(errorStream)

   rmethod oinitclass()

