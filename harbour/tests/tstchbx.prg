/*
 * $Id$
 */

#ifdef __HARBOUR__
#command @ <row>, <col> GET <var>                                       ;
                        CHECKBOX                                        ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [CAPTION <caption>]                             ;
                        [MESSAGE <message>]                             ;
                        [COLOR <color>]                                 ;
                        [FOCUS <fblock>]                                ;
                        [STATE <sblock>]                                ;
                        [STYLE <style>]                                 ;
                        [SEND <msg>]                                    ;
                        [GUISEND <guimsg>]                              ;
                                                                        ;
      => SetPos( <row>, <col> )                                         ;
       ; AAdd( GetList,                                                 ;
              _GET_( <var>, <(var)>, NIL, <{valid}>, <{when}> ) )       ;
       ; ATail(GetList):Control := _CheckBox_( <var>, <caption>,        ;
                        <message>, <color>, <{fblock}>, <{sblock}>,     ;
                        <style> )                           ;
       ; ATail(GetList):reader  := { | a, b, c, d |                     ;
                                    GuiReader( a, b, c, d ) }           ;
      [; ATail(GetList):<msg>]                                          ;
      [; ATail(GetList):Control:<guimsg>]                               ;
       ; ATail(GetList):Control:Display()
#endif
function Main
Local lx :=.f.
local ly :=.f.
cls
Setcolor('w/b+,w/b,w+/b,w/b+,w/b+,w/b+')
@ 2,3 Say "Married"
@ 2,12 Get lx CHECKBOX color 'w/b+,w/b,w+/r,w/g+'
@ 3,3 Say "Single"
@ 3,12 Get ly CHECKBOX color 'w/b+,w/b,w+/r,w/g+'
read
? "Is the Person Married",if(lx," Yes ", " No ")
? "Is the Person Single",if(ly," Yes ", " No ")
return Nil
