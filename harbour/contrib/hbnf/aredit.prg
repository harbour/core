/*
 * $Id$
 */

/*
 * Author....: James J. Orlowski, M.D.
 * CIS ID....: 72707,601
 *
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:56   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   12 Jun 1991 00:42:38   GLENN
 * A referee suggested changing the documentation such that the return value
 * is shown as "xElement" rather than "cElement" because the function
 * can return different types.
 *
 *    Rev 1.0   07 Jun 1991 23:03:24   GLENN
 * Initial revision.
 *
 *
 */

/*

    Some notes:

       The tbmethods section is a short cut from Spence's book instead
       of using the longer DO CASE method.

       Jim Gale showed me the basic array browser and Robert DiFalco
       showed me the improved  skipblock in public messages on Nanforum.

       I added the functionality of the "Edit Get" code block
       (ie bGetFunc), TestGet() demo, and the add/delete rows.

*/

#include "inkey.ch"

// Default heading, column, footer separators
#define DEF_HSEP    hb_UTF8ToStrBox( "═╤═" )
#define DEF_CSEP    hb_UTF8ToStrBox( " │ " )
#define DEF_FSEP    hb_UTF8ToStrBox( "═╧═" )

// Default info for tb_methods section
#define KEY_ELEM 1
#define BLK_ELEM 2

// ANYTYPE[]   ar        - Array to browse
// NUMERIC     nElem     - Element In Array
// CHARACTER[] aHeadings - Array of Headings for each column
// BLOCK[]     aBlocks   - Array containing code block for each column.
// CODE BLOCK  bGetFunc  - Code Block For Special Get Processing
//  NOTE: When evaluated a code block is passed the array element to
//          be edited

FUNCTION FT_ArEdit( nTop, nLeft, nBot, nRight, ;
      ar, nElem, aHeadings, aBlocks, bGetFunc )

   LOCAL exit_requested, nKey, meth_no
   LOCAL cSaveWin, i, b, column
   LOCAL nDim, cType, cVal
   LOCAL tb_methods := ;
      { ;
      { K_DOWN,       {| b | b:down() } }, ;
      { K_UP,         {| b | b:up() } }, ;
      { K_PGDN,       {| b | b:pagedown() } }, ;
      { K_PGUP,       {| b | b:pageup() } }, ;
      { K_CTRL_PGUP,  {| b | b:gotop() } }, ;
      { K_CTRL_PGDN,  {| b | b:gobottom() } }, ;
      { K_RIGHT,      {| b | b:Right() } }, ;
      { K_LEFT,       {| b | b:Left() } }, ;
      { K_HOME,       {| b | b:home() } }, ;
      { K_END,        {| b | b:end() } }, ;
      { K_CTRL_LEFT,  {| b | b:panleft() } }, ;
      { K_CTRL_RIGHT, {| b | b:panright() } }, ;
      { K_CTRL_HOME,  {| b | b:panhome() } }, ;
      { K_CTRL_END,   {| b | b:panend() } } ;
      }

   cSaveWin := SaveScreen( nTop, nLeft, nBot, nRight )
   @ nTop, nLeft TO nBot, nRight

   b := TBrowseNew( nTop + 1, nLeft + 1, nBot - 1, nRight - 1 )
   b:headsep := DEF_HSEP
   b:colsep  := DEF_CSEP
   b:footsep := DEF_FSEP

   b:gotopblock    := {|| nElem := 1 }
   b:gobottomblock := {|| nElem := Len( ar[ 1 ] ) }

   // skipblock originally coded by Robert DiFalco
   b:SkipBlock     := {| nSkip, nStart | nStart := nElem, ;
      nElem := Max( 1, Min( Len( ar[ 1 ] ), nElem + nSkip ) ), ;
      nElem - nStart }

   FOR i := 1 TO Len( aBlocks )
      column := TBColumnNew( aHeadings[ i ], aBlocks[ i ] )
      b:addcolumn( column )
   NEXT

   exit_requested := .F.
   DO WHILE ! exit_requested

      DO WHILE NextKey() == 0 .AND. !b:stabilize()
      ENDDO

      nKey := Inkey( 0 )

      meth_no := AScan( tb_methods, {| elem | nKey == elem[ KEY_ELEM ] } )
      IF meth_no != 0
         Eval( tb_methods[ meth_no, BLK_ELEM ], b )
      ELSE
         DO CASE
         CASE nKey == K_F7
            FOR nDim := 1 TO Len( ar )
               hb_ADel( ar[ nDim ], nElem, .T. )
            NEXT
            b:refreshAll()

         CASE nKey == K_F8
            FOR nDim := 1 TO Len( ar )
               // check valtype of current element before AINS()
               cType := ValType( ar[ nDim, nElem ] )
               cVal  := ar[ nDim, nElem ]
               hb_AIns( ar[ nDim ], nElem,, .T. )
               IF cType == "C"
                  ar[ nDim, nElem ] := Space( Len( cVal ) )
               ELSEIF cType == "N"
                  ar[ nDim, nElem ] := 0
               ELSEIF cType == "L"
                  ar[ nDim, nElem ] := .F.
               ELSEIF cType == "D"
                  ar[ nDim, nElem ] := SToD()
               ENDIF
            NEXT
            b:refreshAll()

         CASE nKey == K_ESC
            exit_requested := .T.

            // Other exception handling ...
         CASE HB_ISBLOCK( bGetFunc )
            IF nKey != K_ENTER
               // want last key to be part of GET edit so KEYBOARD it
               hb_keyPut( LastKey() )
            ENDIF
            Eval( bGetFunc, b, ar, b:colPos, nElem )
            // after get move to next field
            hb_keyPut( iif( b:colPos < b:colCount, K_RIGHT, { K_HOME, K_DOWN } ) )

            // Placing K_ENTER here below Edit Block (i.e. bGetFunc)
            // defaults K_ENTER to Edit when bGetFunc Is Present
            // BUT if no bGetFunc, then K_ENTER selects element to return
         CASE nKey == K_ENTER
            exit_requested := .T.

         ENDCASE
      ENDIF
   ENDDO
   RestScreen( nTop, nLeft, nBot, nRight, cSaveWin )

   // if no bGetFunc then ESC returns 0, otherwise return value of last element
   // TOFIX: ValType() never returns NIL
   RETURN iif( ValType( bGetFunc ) == NIL .AND. nKey == K_ESC, ;
      0, ar[ b:colPos, nElem ] )
