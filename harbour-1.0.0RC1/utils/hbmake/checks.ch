/***
* Checks.ch
*
* Definition of @ ... GET Check box command.
*/

#define CHECK_BOX "X"

#command @ <row>, <col> GET <var> CHECKBOX <cStr> [when <when>]                   ;
                                                                     ;
      =>                                                             ;
         SetPos(<row>, <col>)                                        ;
         ; Aadd(GetList,                                             ;
                CheckGetNew({|x| iif(x == NIL, <var>, <var> := x) }, ;
                     <(var)>, <cStr>,<{when}>))
