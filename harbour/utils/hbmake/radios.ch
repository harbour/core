/***
* Radios.ch
*
* Header file for Radio button gets.
*/

// #define K_SPACE 32
#define RADIO_BUTTON chr(111) 

#command @ <row>, <col> GET <var>                                ;
                        RADIO <radios,...> [valid <valid>]       ;
                                                                 ;
      =>                                                         ;
         SetPos(<row>, <col>)                                    ;
         ; RadioGets({|x| iif(x == NIL, <var>, <var> := x) },    ;
                     <(var)>, <radios>, GetList,<{valid}>)                 ;
         ; DrawRadios(GetList, Atail(GetList))
