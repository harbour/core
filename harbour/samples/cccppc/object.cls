//
// $Id$
//

class OBJECT

   nmethod oinit{ob_oinit}()        
   // Inicializ l egy (tetsz“leges) objektumot.

   nmethod getClass{ob_getClass}()
   // Megadja az objektum oszt ly t.

   nmethod isClass{ob_isClass}()    
   // Az objektum oszt ly-e vagy sem.

   nmethod getAttrib{ob_getAttrib}(attribId) 
   // Ha az attribId string, akkor az adott nev– attrib£tum
   // ‚rt‚k‚t adja. Ha sz m, akkor az adott index– attributum
   // ‚rt‚k‚t adja.
  
   nmethod setAttrib{ob_setAttrib}(attribId,value)
   // Ha az attribId string, akkor az adott nev– attrib£tum
   // ‚rt‚k‚t  ll¡tja be. Ha sz m, akkor az adott index–
   // attributum ‚rt‚k‚t  ll¡tja be.
   // Mindig a 'value'-val t‚r vissza.
  
  