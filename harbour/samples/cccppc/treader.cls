//
// $Id$
//

//*******************************************************************
// treader.cls: èltal†nos 'token' olvas¢ oszt†ly. 
// 1999, Csisz†r Levente

// Meg kell adni neki egy reader-t
// amibìl olvas. A reader adhat tokent is Çs karaktert is.
// Az olvasott karakterekbìl Çs tokenekbìl kÇsz°t egy token sorozatot
// ami reprezent†lja az eredeti input folyamot.

class TREADER READER
                        
   attrib inputReader
   nmethod readInput()       // Olvas egy elemet az inputReader-rìl.
   nmethod unReadInput(item) // Visszatesz egy elemet az inputReader
                             // inputj†ra.

   nmethod addInputReader(reader)
   
   rmethod oinit(inputReader,name,errorStream)
 
metaclass

   rmethod onew(inputReader,name,errorStream)
