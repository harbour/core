/*
 * $Id$
 */

#ifndef _OBJGEN_DEF_
#define _OBJGEN_DEF_
   #xtranslate (OBJGEN.(<o>):<m>)[:=<x>] => ;
                  eval(<o>\[1\]\[<m>\],<o>[,<x>])
   #xtranslate (OBJGEN.(<o>):<m>)([<p,...>])[:=<x>] => ;
                   eval(<o>\[1\]\[<m>\],<o>[,<p>][,<x>])

   #xtranslate (OBJGENM.(<methodArray>).(<o>):<m>)[:=<x>] => ;
                   eval(<methodArray>\[<m>\],<o>[,<x>])

   #xtranslate (OBJGENM.(<methodArray>).(<o>):<m>)([<p,...>])[:=<x>] => ;
                   eval(<methodArray>\[<m>\],<o>[,<p>][,<x>])

   #xtranslate (PKGGEN.(<id>).(<o>):<m>)[:=<x>] => ;
                   eval(<o>\[2\]\[<id>\]\[<m>\],<o>[,<x>])
   #xtranslate (PKGGEN.(<id>).(<o>):<m>)([<p,...>])[:=<x>] => ;
                   eval(<o>\[2\]\[<id>\]\[<m>\],<o>[,<p>][,<x>])

   #xtranslate (PKGGENM.(<methodArray>).(<o>):<m>)[:=<x>] => ;
                  eval(<methodArray>\[<m>\],<o>[,<x>])
   #xtranslate (PKGGENM.(<methodArray>).(<o>):<m>)([<p,...>])[:=<x>] => ;
                  eval(<methodArray>\[<m>\],<o>[,<p>][,<x>])
#endif

