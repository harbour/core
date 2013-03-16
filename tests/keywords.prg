//NOTEST

//DO NOT RUN THIS PROGRAM - ITS PURPOSE IS THE SYNTAX CHECK ONLY!

/* NOTE:
 * Harbour compiled with the Flex version of lexical scanner is
 * designed to stay Clipper compatible in keywords usage.
 * Simplex version is extending their usage in some places.
 * Use HB_CLIPPER_COMPATIBLE to check compilation in full compatibility mode
 */
//#define HB_CLIPPER_COMPATIBLE

#include "keywords.ch"  //INCLUDE test

EXTERNAL __case, __begin
STATIC nExt, bEgin, bReak, cAse, do, wHile, wIth, eXit, eXternal, fIeld
STATIC for, in, include, init, loop, local, using, static, return, recover

FUNCTION NoTestMain()

//just to prevent any disaster if someone will want to run it
  IF .T.
    RETURN nil
  ENDIF

//
//DANGER!!
//One line comments that ends with '**/' doesn't work!!!
//
//

//   NEXT(nExt) //in Clipper: NEXT does not match FOR
//   BEGIN( bEgin +';' + ;;   //////Clipper doesn't like more then one ';'
//          ";" ; /* ;;;;;Clipper doesn't like this comment after ';' ;;;;;;; */
//        )
   BREAK_(bReak)
//   CASE(case) //it is not possible to call case() in this context
   case :=CASE( CASE ) //this is valid

   //DO is reserved function name in Clipper!
   DO( nExt+bEgin/bReak-do*wHile%cAse $ wIth )

   WHILE( while ) //it is not possible to call while() in this context
   END

   ELSE_()      //ELSE is reserved word for *real*
   ELSEIF_()    //ELSEIF is reserved word for *real*
   ENDIF_()     //see above
   ENDCASE_()   //see above
   ENDDO_()     //see above

   END_()    //END? surprise :) it works in some cases!

   EXIT( eXit )

   EXTERNAL( eXternal )

   _FIELD( fIeld )

   for :=FOR( for )

   in( in )

   include( include )

   Init( init )

   LOCAL( local )

   LOOP( loop )

   USING( using )

   STATIC( STATIC )

   return :=return( return )

   RECOVER( recover )

RETURN( return )

/*================================================================
* * * * * ** Checking for NEXT
*/
FUNCTION NeXT( next_next/*next next*/ )

Local nExt, nExt7, nExtNEXT

    For NExt := 1 To 10

       OutStd( nExT )

   Next /*next*/ nExt     //next

   FOR nExt := 1 TO nExt
     QOut( nExt )
   NEXT

   FOR nExt :=1 TO 10
     QOut( nExt )
   NEXT ; ////////// ;
   NEXT

   nExt := 10 //next

   //nExt++  // TODO: PP steals the ++ in Clipper: NEXT does not match FOR
   nExt :=nExt++
   --nExt     /*next*/
   nExt7 :=7
   nExtNEXT := nExt   //next

   //nExt[ 1 ] :=nExt   // TODO: PP steals the [... in Clipper: NEXT does not match FOR
   //nExt[ nExt ] := next[ next ]
   nExt :=next[ next ]

#ifndef HB_CLIPPER_COMPATIBLE
   next->next :=next->next + next->next //NEXT does not match FOR in Clipper and Harbour(Flex)
#endif
   next :=next->next
   ( next )->( next() )

  IF( nExt > nExt+4 )
    nExt := nExt +nExt * 5
  ELSEIF( nExt > 0 )
    nExt := nExt * 6 + nExt
  ELSE
    nExt +=5
  ENDIF

RETURN( nExt * /*next*/ nExt )

/*===================================================================
* Checking for BEGIN
**/
FUNCTION BEGIN( BEGIN_BEGIN )
LOCAL bEgin
LOCAL bEgin0, /* BEGIN OF BEGIN */ ;    /* in Clipper: Incomplete statement */
bEgin1

BEGIN SEQUENCE
    bEgin :=0
    FOR bEgin:=1 TO 10
      QOUT( bEgin )
      bEgin :=bEgin[ 1 ]
      bEgin[ 1 ] :=bEgin
      bEgin :=bEgin * 10
      bEgin++
      --bEgin
      ( begin )->( begin () )
    NEXT bEgin

    bEgin :=begin->begin

   begin->begin :=begin->begin +; /************************/
begin->begin
    bEgin :=BEGIN( begin +bEgin ) +;
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
  IF bReak == 0
    Break /*break*/ ( nil )
    BREAK   /* break to beggining */
    Break(0)   //in Clipper: syntax error: ')'
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
  (0)  //in Clipper: incomplete statement or unbalanced delimiters

begin sequence
  FOR bReak:=1 To 10
    QOut( bReak * bReak )
    break->break :=1
    ( break )->( break(0) )
  NEXT bReak
  break->break :=break->break +break->break
  BREAK
  break :=break[ 2 ] //not allowed in Clipper
  bReak :=iif( bReak==1, BREAK(0), BREAK(bReak) )
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

 do case
  case[ case ] == "case"
  case[ 2 ] == "2"
 endcase

  case :=case[ 2 ]
  case =case + case - case
  case :={|case| case( case )}
  case :={|| case }
  case :={|case| case[ 4 ]}

  DO /* case */ CASE
  CASE 1
    case =case ** case
  CASE 2+case
    case =case +1
  CASE case++
#ifndef HB_CLIPPER_COMPATIBLE
    case--   //sorry -Clipper & Harbour(flex) doesn't compile this line - but SimpLex does
    case++   //sorry -Clipper & harbour(flex) doesn't compile this line - but SimpLex does
#endif
    ( case++ )
    ( case-- )
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
  CASE( CASE ) //new CASE or function call? :) - CASE supercedes case as identifier when syntax is valid.
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
  do[ do ] :=do[ do ][ do ]

  DO do WITH do
  DO do WITH do()
  DO do

  DO while;
  while
    while :=while()
  ENDDO

  while while
#ifndef HB_CLIPPER_COMPATIBLE
    while++   //Clipper & harbour(flex) incomplete statement or unbalanced delimiter
    while--   //Clipper & harbour(flex) incomplete statement or unbalanced delimiter
#endif
    ( while++ )
    ( while-- )
    --while
    while +=while
    while->while :=while() +while->while
  enddo

  do->do :=while->while

  while[ 1 ] ==while
     while[ 2 ] $ "2"
       while( while )
         while( while[1] )
           while[ while ] == while[ 1 ]

             while :={|while| while}

           enddo
         enddo
       enddo

       while while++ < 10
          ( while )->( while() )
       enddo

     enddo
  enddo


  while while[1]   //in Clipper: syntax error ' 1 '
  enddo

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
  with :=with[ 1 ] //Syntax error ' 1 '
  with[ with ] :=1 //Syntax error ' WITH '
  with[ with ][ with ] =with[ with ][ with ]

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

  //end()   TODO: PP steals the () //in Clipper: ENDIF does not match IF
  end :=end()

  ++end
  //end++  TODO: PP steals the ++    //in Clipper ENDIF does not match IF
  ( end++ )
  ( end-- )
  end :=end++

#ifndef HB_CLIPPER_COMPATIBLE
  end->end +=1     //in Clipper & Harbour(flex): ENDIF does not match IF
#endif
  end :=end->end

  DO end WITH end
  DO end WITH end++

#ifndef HB_CLIPPER_COMPATIBLE
  end->( end() )   //in Clipper & harbour(flex): ENDIF does not match IF
#endif
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

  //end[ 1 ] :=1     //TODO: PP steals the [...  in Clipper: ENDIF does not match IF
  //end[ end ] :=2
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

  exit++
  ++exit

  exit[ 1 ] :=1
  exit :=exit[ 1 ]
  exit->exit :=exit->exit +1
  ( exit )->exit :=exit
  DO exit WITH exit

  exit()
  exit( exit )

//  EXIT    //EXIT statement with no loop in sight

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
//    EXIT     //EXIT statement with no loop in sight
  CASE !exit
    exit()
  ENDCASE

RETURN exit


/*====================================================================
* Test for EXTERNAL
*/
FUNCTION EXTERN( extern )
LOCAL external
LOCAL exte
EXTE exte
EXTER exter
EXTERN extern
EXTERNA externa
EXTERNAL external

  extern++
  ++extern
  extern[ 1 ] :=1
  extern[ 1 ] :=extern
  external[ external ][ external ] :=external
  extern()
  externa()
  external()
  external->external :=external->external +1
  ( external )->external :=(external)->( external( external[ external ] ) )

  DO external
  DO external WITH external
  DO exte WITH exte
  DO WHILE external
    external :=!external
  ENDDO

RETURN extern

/*====================================================================
* Test for FIELD
*/
FUNCTION _FIELD( _field )  //FIELD is reserved function name (FIELDNAME)
FIELD field
FIEL fiel

  field++
  ++field

  field[ 1 ] :=field
  field[ field ] :=field[ field ]

  field->field :=field
  field->field :=field->field

  FIELD( FIEL(0) )

#ifndef HB_CLIPPER_COMPATIBLE
  DO field  //Incorrect number of arguments
  DO field WITH field     //field cannot be passed by a reference
#endif
  WHILE field
    fiel :=field +1
  ENDDO

RETURN field


/*====================================================================
* Test for FOR
*/
FUNCTION FOR( for )

  for++
  --for

  FOR for:=1 TO for+10
    for +=1
  NEXT for

  for :=for[ for ]
  for[ 1 ] :=1
  for[ for ] :=for
  for[ for ][ for ] :=for [ for ]

#ifndef HB_CLIPPER_COMPATIBLE
  for( for( for ) )   //in Clipper: incomplete statement or unbalanced delimiters
  for( 0 )      //in Clipper: incomplete statement or unbalanced delimiters
  for()      //syntax error ')'
#endif
  for :=for()
  for :=for( for( for ) )

  for :={|for| for}
  EVAL( {|for| for}, for )

  for->for :=for
  for :=for->for
  for->for :=for->for
  ( for )->for :=( for )->for

  DO for
  DO for WITH for
  DO WHILE for
    for ^=2
  ENDDO

RETURN for

/*====================================================================
* Test for IN
*/
FUNCTION IN( _in )
FIELD begin IN begin
FIELD break IN break
FIELD case IN case
FIELD do IN do
FIELD for IN for
FIELD in IN in
FIELD next IN next
FIELD while IN while
FIELD end IN end
FIELD with IN with
FIELD exit IN exit
FIELD external IN external
FIELD field IN field

  IN( in )
  in++
  --in
  in->in :=in
  in[ 1 ] :=1
  in[ in ] :=in[ in ]
  ( in )->in :=in

  in :={|in| in}
  EVAL( {|in| in}, in )

  DO in
#ifndef HB_CLIPPER_COMPATIBLE
  DO in WITH in       //IN cannot be passed by a reference
#endif

RETURN in

/*====================================================================
* Test for INCLUDE
*/
FUNCTION include( include )

  FOR include:=1 TO 10
    include++
    --include
  NEXT include

  WHILE include
  ENDDO

  include[ 1 ] :=1
  include := include[ include ]

  include :={|include| include}
  EVAL( {|include| include}, include )

  DO include
  DO include WITH include

  include->include :=1
  include->include :=include->include +1
  ( include )->include :=INCLUDE( include->include )

RETURN include

/*====================================================================
* Test for INIT
*/
INIT fUNCTION Init( init )

  FOR init:=1 TO 10
    init++
    --init
  NEXT init

  WHILE init
    init :=!init
  END

  init[ 1 ] :=1
  init :=init[ init ]

  init->init :=init->init +1
  ( init )->init :=INIT( init[ init->init ] )

  init :={|init| init}
  EVAL( {|init| init}, init )

  DO INIT WITH init
  DO INIT

RETURN init


/*====================================================================
* Test for LOCAL
*/
FUNCTION local( _local )
LOCAL local

  FOR local:=1 TO 10
    local++
    --local
  NEXT local

  local :={|local| local}
  EVAL( {|local| local}, local )

  WHILE local
  ENDDO

  local[ 1 ] :=1
  local :=local[ local ]

  local->local :=local->local
  ( local )->local :=LOCAL( local[ local->local ] )

  DO local
  DO local WITH local

RETURN local

/*====================================================================
* Test for LOOP
*/
FUNCTION loop( loop )

  FOR loop:=1 TO 10
    loop++
    --loop
    QOut( loop )
    IF( loop == 5 )
      Qout( "LOOP to begginig" )
      loop
    ENDIF
    IF( loop == 9 )
      Qout( "EXIT from FOR statement" )
      EXIT
    ENDIF
  NEXT loop
  Qout( "After ", loop, "loops" )


//  LOOP

  loop :={|loop| loop}
  EVAL( {|loop| loop}, loop )

  loop =1
  WHILE loop <= 10
    Qout( loop )
    IF( loop == 5 )
        Qout( "LOOP to 7" )
        loop :=7
        LOOP
    ENDIF
    IF( loop == 9 )
      Qout( "EXIT form while" )
      EXIT
    ENDIF
    ++loop
  ENDDO
  Qout( "After ", loop, " loops" )

  loop[ 1 ] :=1
  loop :=loop[ loop ]

  loop->loop :=loop->loop
  ( loop )->loop :=loop( loop[ loop->loop ] )

  DO loop
  DO loop WITH loop

RETURN loop


/*====================================================================
* Test for USING
*/
FUNCTION using
LOCAL using
PRIVATE &using

  EVAL( using )

  FOR using:=1 TO 10
    ? using
    BREAK using
  NEXT

  DO WHILE using > 0
    ++using
    using--
  ENDDO

  BEGIN SEQUENCE
    ? USING
  RECOVER USIN using
    ? using
  END

RETURN using


/*====================================================================
* Test for STATIC
*/
FUNCTION STATIC
STAT stat
STATI stati
STATIC static
PRIVATE &STATIC

  EVAL( STATIC )

  FOR static:=1 TO 10
    ? static
    static( static )
    BREAK static
  NEXT

  IF static
     BREAK stat
  ENDIF

  DO WHILE static
    ++static
    static -=2
    break stat
  ENDDO

  static++
  static--
  ( static++ )
  ( static-- )
  BEGIN SEQUENCE
    ? static
  RECOVER USIN static
    ? static
  END

RETURN static


/*====================================================================
* Test for RETURN
*/
FUNCTION RETURN
STAT return
LOCAL Self
PRIVATE &return

  EVAL( return )

  FOR return:=1 TO 10
    ? return
    return ( return )
    BREAK return
  NEXT

  IF return
     RETU return
  ENDIF

  return := return( return )
  return += return
  return-=return
  return->return ^= 3
  return:return()

  return::return
  return ::return
  return-1
  return -1
  return+2
  return +2
#ifndef HB_CLIPPER_COMPATIBLE
  return++    //Clipper fails on this
  return--    //Clipper fails on this
#endif
  ( return++ )
  ( return-- )
  ++return
  --return
  return!return
  return &return

  DO WHILE return
    ++return
    return -=2
    break return
  ENDDO

  BEGIN SEQUENCE
    ? return
  RECOVER USIN return
    ? return
  END

RETURN( return ) + 2


/*====================================================================
* Test for RECOVER
*/
FUNCTION RECOVER
STAT RECOVER
PRIVATE &RECOVER

  EVAL( RECOVER )

  FOR RECOVER:=1 TO 10
    ? RECOVER
    RECOVER ( RECOVER )
    BREAK RECOVER
  NEXT

  IF RECOVER
     RETU RECOVER
  ENDIF

  RECOVER := RECOVER( RECOVER )

  DO WHILE RECOVER
    ++RECOVER
    RECOVER -=2
    break RECOVER
  ENDDO

  RECOVER++
  RECOVER--
  ( RECOVER++ )
  ( RECOVER-- )

  BEGIN SEQUENCE
    ? RECOVER
  RECOVER
    ? RECOVER
  END

  BEGIN SEQUENCE
    ? RECOVER
  RECOVER USIN RECOVER
    ? RECOVER
  END

RETURN( RECOVER ) +2


/*====================================================================
* Test for OTHERWISE
*/
FUNCTION OTHERWISE
STAT OTHERWISE
PRIVATE &OTHERWISE

  EVAL( OTHERWISE )

  FOR OTHERWISE:=1 TO 10
    ? OTHERWISE
    OTHERWISE ( OTHERWISE )
    BREAK OTHERWISE
  NEXT

  IF OTHERWISE
     RETU OTHERWISE
  ENDIF

  OTHERWISE := OTHERWISE( OTHERWISE )

  OTHERWISE++

  DO WHILE OTHERWISE
    ++OTHERWISE
    OTHERWISE -=2
    break OTHERWISE
  ENDDO

  DO CASE
  CASE OTHERWISE
    ? OTHERWISE
  CASE !OTHERWISE
    ? OTHERWISE
  OTHE
    ? OTHERWISE +1
//  OTHER           //Mayhem in CASE
    ? OTHERWISE +2
  END


RETURN( OTHERWISE ) +2

/* Testing macro compilation */
PROCEDURE MACRO()
MEMVAR s
PRIVATE s

 s := &s
 s := &s.
 s := &s.1
 s := &s.123
 s := &s._
 s := &s._____
 s := &s._1
 s := &s.1_bo
 s := _&s
 s := _&s.
 s := _&s.1
 s := _&s.123
 s := _&s._
 s := _&s._____
 s := _&s._1
 s := _&s.1_bo
 s := x_&s
 s := x_&s_1
 s := x_&s._1
 s := x_&s._123
 s := x_&s.123
 s := x_&s.2_
 s := x_&s.2_x

RETURN
