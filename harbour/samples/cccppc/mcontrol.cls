//
// $Id$
//

//*******************************************************************
// mcontrol.cls: Ez egy olyan eszk”z, ami az inputra £j 
//               'changer'-eket tud ind¡tani.
// 1999, Csisz r Levente


class MCONTROL TREADER

   attrib defdict
   attrib xtrdict
   attrib ujsor
   attrib trPrsAlg
   
   rmethod readItem()
   // Olvas egy elemet. Ha az elem megfelel bizonyos felt‚teleknek
   // akkor ind¡t r  egy 'changer'-t.
   // Ret: Az elem, ha OK, nil, ha v‚ge van a streamnek, vagy ha
   //      hiba volt.

   rmethod oinit(inputReader,name,defdict,xtrdict,errorStream,trPrsAlg)
   
metaclass
   
   rmethod onew(inputReader,name,defdict,xtrdict,errorStream,trPrsAlg)
   
   

