//NOTEST
/*
 * $Id$
*/
//DO NOT RUN THIS PROGRAM - ITS PURPOSE IS THE SYNTAX CHECK ONLY!

EXTERNAL __case, __begin
STATIC nExt, bEgin, bReak, cAse, do, wHile, wIth, eXit

Function Main()

//just to prevent any disaster if someone will want to run it
  IF( .T. )
    RETURN nil
  ENDIF

//
//DANGER!!
//One line comments that ends with '**/' doesn't work!!!
//
//

//   NEXT(nExt)	//in Clipper: NEXT does not match FOR
//   BEGIN( bEgin +';' + ;;   //////Clipper doesn't like more then one ';'
//          ";" ; /* ;;;;;Clipper doesn't like this comment after ';' ;;;;;;; */
//        )
   BREAK_(bReak)
//   CASE(case)	//it is not possible to call case() in this context
   case :=CASE( CASE )	//this is valid

   //DO is reserved function name in Clipper!
   DO_( nExt+bEgin/bReak-do*wHile%cAse $ wIth )
//   WHILE( while ) //it is not possible to call while() in this context

   ELSE_()      //ELSE is reserved word for *real*
   ELSEIF_()    //ELSEIF is reserved word for *real*
   ENDIF_()     //see above
   ENDCASE_()   //see above
   ENDDO_()     //see above

   END_()    //END? surprise :) it works in some cases!

   EXIT( eXit )

RETURN nil

/*================================================================
* * * * * ** Checking for NEXT
*/
FUNCTION NeXT( next_next/*next next*/ )
Local nExt, nExt7, nExtNEXT

    For NExt := 1 To 10

       OutStd( nExT ) // Actually this needs to use str()

   Next /*next*/ nExt     //next

   FOR nExt = 1 TO nExt
     QOut( nExt )
   NEXT

   FOR nExt :=1 TO 10
     QOut( nExt )
   NEXT ; ////////// ;
   NEXT

   nExt := 10 //next

//   nExt++	/*in Clipper: NEXT does not match FOR*/
   nExt :=nExt++
   --nExt	/*next*/
   nExt7 :=7
   nExtNEXT := nExt	//next

//   nExt[ 1 ] :=nExt	//in Clipper: NEXT does not match FOR
//   nExt[ nExt ] := next[ next ]
   nExt :=next[ next ]

//   next->next :=next->next + next->next	//NEXT does not match FOR
   next :=next->next
   ( next )->( next() )

  IF( nExt > nExt+4 )
    nExt =nExt +nExt * 5
  ELSEIF( nExt > 0 )
    nExt = nExt * 6 + nExt
  ELSE
    nExt +=5
  ENDIF

RETURN( nExt * /*next*/ nExt )

/*===================================================================
* Checking for BEGIN
**/
FUNCTION BEGIN( BEGIN_BEGIN )
LOCAL bEgin
LOCAL xbEgin
LOCAL bEginBEGIN
//LOCAL bEgin0, /* BEGIN OF BEGIN */ ;    /* in Clipper: Incomplete statement */
//	bEgin1
LOCAL xbEginBEGIN := 100

BEGIN SEQUENCE
    bEgin0 :=0
    FOR bEgin:=1 TO 10
      QOUT( bEgin )
      xbEgin :=bEgin
      bEginBEGIN :=xbEgin * 10
      bEgin0 +=bEginBEGIN
      --xbEginBEGIN
      bEgin++
      --bEgin
      ( begin )->( begin () )
    NEXT bEgin

//    begin->begin :=begin->begin +; /************************/
//	begin->begin
    bEgin :=BEGIN( xbegin +bEgin +bEginBEGIN -bEgin0 * xbEginBEGIN ) +;
            bEgin[ bEgin ]

END SEQUENCE

BEGIN /* BEGIN */ SEQU

    BEGIN_BEGIN :=bEgin//begin

END /* BEGIN */ SEQUENC

RETURN bEgin ^ 2

/*====================================================================
* Test for BREAK and BEGIN/RECOVER sequence
**/
FUNCTION BREAK_( break_break )
LOCAL bReak:=0

  ++bReak
  IF( bReak = 0 )
    Break /*break*/ ( nil )
    BREAK   /* break to beggining */
    Break(0)	//in Clipper: syntax error: ')'
  ENDIF

  Break /*** break ***/ ;
; //////////////////////////////////////////////////
; //What
; //the
; //comment
; //like
; //it
; //is
; //doing
; //in
; //place
; //like
; //this?
; //Do
; //you
; //use
; //it
; //often?
;///////////////////////////////////////////////////
  (0)	//in Clipper: incomplete statement or unbalanced delimiters

begin sequence
  FOR bReak:=1 To 10
    QOut( bReak * bReak )
    break->break :=1
    ( break )->( break(0) )
  NEXT bReak
  break->break :=break->break +break->break
  BREAK
//  break :=break[ 2 ] //not allowed in Clipper
  bReak :=IIF( bReak=1, BREAK(0), BREAK(bReak) )
recover USING bReak
  BREAK( Break( break(0) ) )
end

BREAK
RETURN bReak( bReak )

/*====================================================================
* Test for CASE/DO CASE
*/
FUNCTION CASE( case_ )
LOCAL case

  FOR case:=1 TO 15
    QUOT( case )
    QOUT( case * ; ////
case )
  NEXT case

//  case[ case ] :=case	//in Clipper: Case is not immediatelly within DO CASE
//  case[ 2 ] :=2	//in Clipper: the same as above - Harbour compiles both
//  case :=case[ 2 ]
  case =case + case - case
  case :={|case| case( case )}
  case :={|| case }
//  case :={|case| case[ 4 ]}

  DO /* case */ CASE
  CASE 1
    case =case ** case
  CASE 2+case
    case =case +1
  CASE case++
//    case--	//sorry -Clipper also doesn't compile this line
//    case++	//sorry -Clipper also doesn't compile this line
  CASE ++case
    ++case
  CASE ;
    case( case )
  CASE CASE
  CASE !CASE
  CASE -CASE
  CASE +CASE
/*
*
*
*/
  CASE( CASE )
    case =case != case
  CASE( CASE )	//new CASE or function call? :)
  CASE case->case
    case->case :=case->case +1
    ( case )->( case() )
  OTHERWISE
    case *=case
  ENDCASE

RETURN case

/*====================================================================
* Test for DO CASE / DO WHILE / DO / WITH
*/
FUNCTION DO_( do )
LOCAL case
LOCAL while
LOCAL with

  do case
  case do
  case case
  endcase

  do/***do***/case/**/
  case do
  endcase

  DO CASE
  ENDCASE

  while .t.
  enddo

  do ;
   while do
   do :=1
  enddo

  do/***do***/while do/***do do do, da da da***/
   do :=do
  enddo

  do :={|do| do}
  do while do
    do()
    do->do :=do()
    do++
    do--
  enddo

  do while while
    ++do
  enddo

  do++
  do +=do
  do->do :=do->do +1
  ( do )->( do() )
  do[ 1 ] :=do
////////  do[ do ] :=do[ do ][ do ]

  DO do WITH do
  DO do WITH do()
  DO do

  DO while;
   while
   while :=while()
  ENDDO

  while while
//   while++	//incomplete statement or unbalanced delimiter
//   while--	//incomplete statement or unbalanced delimiter
   --while
   while +=while
   while->while :=while() +while->while
  enddo

//  while[ 1 ] :=while
//  while[ 2 ] +=2
//  while( while )
//  while( while[1] )
//  while[ while ] :=while[ 1 ]	//in Clipper: syntax error ' 1 '
  while :={|while| while}

  while while++ < 10
    ( while )->( while() )
  enddo

//  while while[1]	//in Clipper: syntax error ' 1 '
//  enddo

  DO /* ****  */;
;
;
 while


  DO while WITH
   do :=with + while
  ENDDO

  do WITH;
;
;

  with()

  DO while WITH while
  DO while WITH while()
  DO while WITH with
  DO do WITH with
  DO do WITH with()

  with++
  ++with
  with +=with
  with->with :=with() +with->with +1
  ( with )->( with() )
  with :={|with| with}
//  with :=with[ 1 ]	//Syntax error ' 1 '
//  with[ with ] :=1	//Syntax error ' WITH '
//  with[ with ][ with ] =with[ with ][ with ]

  DO with WITH with
  DO with WITH with()
  DO with WITH do
  DO with WITH while
  DO with WITH do()
  DO with WITH while()

RETURN DO


FUNCTION WHILE( while )
RETURN while

FUNCTION With( with )
RETURN with

/*====================================================================
* Test for END
*/
FUNCTION END_(  )
LOCAL end, while

//  end()	//in Clipper: ENDIF does not match IF
  end :=end()

  ++end
//  end++		//in Clipper ENDIF does not match IF
  end :=end++

//  end->end +=1		//in Clipper; ENDIF does not match IF
  end :=end->end

  DO end WITH end
  DO end WITH end++

//  end->( end() )	//in Clipper: ENDIF does not match IF
  ( end )->( end() )

  DO WHILE !end
    --while
  END

  DO WHILE end
    ++while
  END

  IF end
  END

  DO CASE
  CASE end
  END

//  end[ 1 ] :=1		//in Clipper: ENDIF does not match IF
//  end[ end ] :=2
  end :=end[ end ]
  end =end[ end ]
  end +=end
  end ^=end
  end %=end

BEGIN SEQU
  end :={| end | end }
END

RETURN end

FUNCTION end( end )
RETURN end * end

/*====================================================================
* Test for EXIT
*/
EXIT FUNCTION EXIT( exit )
//LOCAL exit

  exit++
  ++exit

  exit[ 1 ] :=1
  exit :=exit[ 1 ]
  exit->exit :=exit->exit +1
  ( exit )->exit :=exit
  DO exit WITH exit

  exit()
  exit( exit )

//  EXIT	//EXIT statement with no loop in sight

  FOR exit:=1 TO 10
    exit++
    DO exit
    EXIT
  NEXT

  WHILE !exit
    exit +=1
    EXIT
  ENDDO

  DO CASE
  CASE exit
//    EXIT	//EXIT statement with no loop in sight
  CASE !exit
    exit()
  ENDCASE

RETURN exit

