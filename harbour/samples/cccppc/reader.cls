//
// $Id$
//

//*******************************************************************
// reader.cls: èltal†nos stream olvas¢ oszt†ly. Ebbìl îrîkîlnek a 
//             karakter Çs a token olvas¢ oszt†lyok.
// 1999, Csisz†r Levente

class READER OBJECT

   attrib name    // Az olvasott stream azonos°t¢ja (a 
                  // hibajelzÇsekhez)
   attrib errorStream   // Ebbe a tîmbbe °rja a hibaÅzeneteket.
                        // prsError objektumok vannak benne.
   
   attrib tbuffer // Ez egy TBUFFER, ide teszi be az unread()-el
                  // visszatett itemeket.
   
   nmethod read()
   // Olvas egy 'elemet'. Az elem a nil kivÇtelÇvel b†rmilyen
   // objektum lehet. Ha az unget puffer nem Åres, akkor onnan
   // olvassa, ha Åres, akkor pedig a readItem()-el olvas egyet
   // a stream-bìl.
   // Ret: Az elem, ha OK, nil, ha vÇge van a streamnek, vagy ha 
   //      hiba volt. Hiba esetÇn be†ll°tja a this:error-t.
   
   nmethod unread(item)
   // Visszatesz az readerbe egy elemet.
   
   nmethod readTBuffer()
   // A tbuffer-bìl olvas. Ha a tbuffer Åres, akkor nil
   
   nmethod arrayTBuffer()
   // Ad egy tîmbît, amiben a tbuffer elemi vannak.
   
   abstract nmethod readItem()
   // Belsì mñvelet, az val¢di aloszt†lyoknak implement†lni kell.
   // Olvas egy 'elemet' kîzvetlenÅl a stream-bìl az unget puffer
   // megkerÅlÇsÇvel. Az elem a nil kivÇtelÇvel b†rmilyen
   // objektum lehet. 
   // Ret: Az elem, ha OK, nil, ha vÇge van a streamnek, vagy ha 
   //      hiba volt. Hiba esetÇn be†ll°tja a this:error-t.
   
   
   nmethod isError(l)
   // Igaz, ha volt hiba.
   // l: Az errorStream elìzì hossza. Ha meg van adva, akkor akkor
   //    ad igazat, ha az errorStream hossza nagyobb, mint az l.
   
   nmethod addError(prsErr)
   // Hozz†ad egy prsErr objektumot az errorStream-hez, ha az nem nil
   
   // nmethod errorStr()
   // Nil, ha nem volt hiba, egy string, ami a hiba le°r†s†t
   // tartalmazza, ha volt hiba.

   abstract nmethod destruct{retnil}()
   // Tîrli az objektumhoz kapcsol¢d¢ kÅlsì eszkîzîket. (Pl. lez†rja
   // a filÇt etc.)
                          
   rmethod oinit(name,errorStream)
   // Inicializ†lja az objektumot.
   
metaclass

   rmethod onew(name,errorStream)
                    

