// Harbour Get System sample

#define bSETGET(x) { | u | If( PCount()==0, x, x:= u ) }

#command @ <row>, <col> GET <var>                                       ;
                        [PICTURE <pic>]                                 ;
                        [VALID <valid>]                                 ;
                        [WHEN <when>]                                   ;
                        [SEND <msg>]                                    ;
                                                                        ;
      => SetPos( <row>, <col> )                                         ;
       ; AAdd(                                                          ;
           GetList,                                                     ;
           _GET_( <var>, <"var">, <pic>, <{valid}>, <{when}>, bSETGET(<var>) ):display();
             )                                                          ;
      [; ATail(GetList):<msg>]

function Main()

   local cName := "Harbour     "
   local cWish := "Power   "
   local GetList := {}

   CLS

   @ 2, 2 SAY "Enter your name:" GET cName
   @ 4, 2 SAY "Enter your wish:" GET cWish
   READ

   ? cName
   ? cWish

return nil

