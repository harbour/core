//DO NOT RUN THIS PROGRAM - ITS PURPOSE IS THE SYNTAX CHECK ONLY!

STATIC nExt, bEgin, bReak

Function Main()

   NEXT(nExt)
   BEGIN( bEgin +';' + ;   //////
          ";" ; /* ;;;;;just trying to be smart;;;;;;; */
        )
   BREAK_(bReak)

RETURN nil

/*================================================================
************************************************ Checking for NEXT
*/
FUNCTION NeXT( next_next/*next next*/ )
Local nExt, nExt7, nExtNEXT

    For NExt := 1 To 10

       OutStd( nExT ) // Actually this needs to use str()

   Next /*next*/ nExt     //next
//NEXT

   nExt := 10 //next

   nExt++	/*n_ext*/	//---
   --nExt	/*next*/
   nExt7 :=7
   nExtNEXT := nExt	//next

   nExt[ nExt ] := NEXT( nExt[nExt+nExt] * nExt *(nExt+nExt*5) )

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
*/
FUNCTION BEGIN( BEGIN_BEGIN )
//LOCAL bEgin, xbEgin , bEginBEGIN , bEgin0, xbEginBEGIN:=100
LOCAL bEgin
LOCAL xbEgin
LOCAL bEginBEGIN
LOCAL bEgin0, /* BEGIN OF BEGIN */ ;    /* begin */
	bEgin1
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
    NEXT bEgin

    bEgin :=BEGIN( xbegin +bEgin +bEginBEGIN -bEgin0 * xbEginBEGIN ) +;
            bEgin[ bEgin ]

END SEQUENCE

BEGIN /* BEGIN */ SEQU

    BEGIN_BEGIN :=bEgin//begin

END /* BEGIN */ SEQUENC

RETURN bEgin ^ 2

/*====================================================================
* Test for BREAK and BEGIN/RECOVER sequence
*/
FUNCTION BREAK_( break_break )
LOCAL bReak:=0

  ++bReak
  IF( bReak = 0 )
    Break /*break*/ ( nil )
    BREAK   /* break to beggining */
    Break()	//this line is not valid in Clipper: syntax error: ')'
  ENDIF

  Break /* break in break */ ;;;     
; /////////////
  ()

begin sequence
  FOR bReak:=1 To 10
    QOut( bReak * bReak )
  NEXT bReak
  BREAK
  bReak :=IIF( bReak=1, BREAK(0), BREAK(bReak) )
recover USING bReak
  BREAK( Break( break() ) )
end

BREAK
RETURN bReak[ bReak ]
