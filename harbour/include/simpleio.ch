/*
 * $Id$
 */

#ifndef _SIMPLEIO_CH
#define _SIMPLEIO_CH

#command ?  [ <xList,...> ]          => ;
                        ( OutStd( Chr(13) + Chr(10) ) [, OutStd(<xList>)] )

#command ?? [ <xList,...> ]          => ;
                        OutStd( <xList> )

#command ACCEPT TO <idVar>           => ;
                        <idVar> := StrTran( FReadStr(0, 256), Chr(13) + Chr(10) )

#command ACCEPT <cPrompt> TO <idVar> => ;
                        ? <cPrompt> ; ACCEPT TO <idVar>

#endif /* _SIMPLEIO_CH */
