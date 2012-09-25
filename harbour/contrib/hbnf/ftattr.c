/*
 * $Id$
 */

/*
 * File......: saveatt.asm
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.2   03 Oct 1992 14:35:14   GLENN
 *  Ted Means made modifications to support use of dispBegin()/dispEnd().
 *
 *     Rev 1.1   15 Aug 1991 23:07:58   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.0   12 Jun 1991 01:30:20   GLENN
 *  Initial revision.
 *

 */

/* This is the original FT_SAVEATT() code
   IDEAL

   Public   FT_SaveAtt

   Extrn    __ParNI:Far
   Extrn    __RetCLen:Far
   Extrn    __xGrab:Far
   Extrn    __xFree:Far
   Extrn    __gtSave:Far

   nTop     EQU       Word Ptr BP - 2
   nLeft    EQU       Word Ptr BP - 4
   nBottom  EQU       Word Ptr BP - 6
   nRight   EQU       Word Ptr BP - 8
   nAttr    EQU       Byte Ptr BP - 10
   nBufLen  EQU       Word Ptr BP - 12

   cBuffer  EQU       DWord Ptr BP - 16
   nBufOfs  EQU       Word Ptr BP - 16
   nBufSeg  EQU       Word Ptr BP - 14

   Segment  _NanFor   Word      Public    "CODE"
         Assume    CS:_NanFor

   Proc     FT_SaveAtt          Far

         Push      BP                        ; Save BP
         Mov       BP,SP                     ; Set up stack reference
         Sub       SP,16                     ; Allocate locals

         Mov       CX,4                      ; Set param count
   @@Coord: Push      CX                        ; Put on stack
         Call      __ParNI                   ; Retrieve param
         Pop       CX                        ; Get count back
         Push      AX                        ; Put value on stack
         Loop      @@Coord                   ; Get next value

         Pop       [nTop]                    ; Store top coordinate
         Pop       [nLeft]                   ; Store left coordinate
         Pop       [nBottom]                 ; Store bottom coordinate
         Pop       [nRight]                  ; Store right coordinate

         Mov       AX,[nBottom]              ; Load bottom coordinate
         Sub       AX,[nTop]                 ; Subtract top
         Inc       AX                        ; Calc length

         Mov       CX,[nRight]               ; Load right coordinate
         Sub       CX,[nLeft]                ; Subtract left
         Inc       CX                        ; Calc width
         Mul       CX                        ; Multiply length by width
         SHL       AX,1                      ; Calc buffer size
         Mov       [nBufLen],AX              ; Store buffer size

   @@Alloc: Push      AX                        ; Put size on stack
         Call      __xGrab                   ; Allocate memory
         Add       SP,2                      ; Realign stack
         Mov       [nBufSeg],DX              ; Store segment
         Mov       [nBufOfs],AX              ; Store offset

         Push      DX                        ; Load parameters for __gtSave
         Push      AX                        ; onto stack
         Push      [nRight]
         Push      [nBottom]
         Push      [nLeft]
         Push      [nTop]
         Call      __gtSave                  ; Grab screen image

         Push      DS                        ; Save required registers
         Push      SI
         Push      DI

         Mov       DS,[nBufSeg]              ; Load pointer to buffer
         Mov       SI,[nBufOfs]              ; into DS:SI

         Push      DS                        ; Duplicate pointer in ES:DI
         Push      SI
         Pop       DI
         Pop       ES
         Inc       SI                        ; Start with attribute byte

         Mov       CX,[nBufLen]              ; Load buffer length
         SHR       CX,1                      ; Divide by two
   @@Attr:  Lodsw                               ; Grab a screen word
         Stosb                               ; Store attribute only
         Loop      @@Attr                    ; Do next

         Pop       DI                        ; Restore registers
         Pop       SI
         Pop       DS

   Done:    Mov       AX,[nBufLen]              ; Load buffer length
         SHR       AX,1                      ; Divide by 2
         Push      AX                        ; Put length on stack
         Push      [nBufSeg]                 ; Put segment on stack
         Push      [nBufOfs]                 ; Put offset on stack
         Call      __RetClen                 ; Return attribute string
         Call      __xFree                   ; Free memory
         Mov       SP,BP                     ; Realign stack
         Pop       BP                        ; Restore BP
         Ret
   Endp     FT_SaveAtt
   Ends     _NanFor
   End
 */

#include "hbapi.h"
#include "hbapigt.h"

/* This is the New one Rewriten in C */

HB_FUNC( FT_SAVEATT )
{
   int      iTop     = hb_parni( 1 );  /* Defaults to zero on bad type */
   int      iLeft    = hb_parni( 2 );  /* Defaults to zero on bad type */
   int      iMaxRow  = hb_gtMaxRow();
   int      iMaxCol  = hb_gtMaxCol();
   int      iBottom  = hb_parnidef( 3, iMaxRow );
   int      iRight   = hb_parnidef( 4, iMaxRow );

   HB_SIZE  ulSize;
   char *   pBuffer;
   char *   pAttrib;

   if( iTop < 0 )
      iTop = 0;
   if( iLeft < 0 )
      iLeft = 0;
   if( iBottom > iMaxRow )
      iBottom = iMaxRow;
   if( iRight > iMaxCol )
      iRight = iMaxCol;

   if( iTop <= iBottom && iLeft <= iRight )
   {
      ulSize   = ( iBottom - iTop + 1 ) * ( iRight - iLeft + 1 );
      pBuffer  = pAttrib = ( char * ) hb_xgrab( ulSize + 1 );
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            int         iColor;
            HB_BYTE     bAttr;
            HB_USHORT   usChar;
            hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
            *pBuffer++ = ( char ) iColor;
            ++iCol;
         }
         ++iTop;
      }
      hb_retclen_buffer( pAttrib, ulSize );
   }
   else
      hb_retc_null();
}

/*
 * File......: restatt.asm
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.2   03 Oct 1992 14:33:46   GLENN
 *  Ted Means made modifications so these functions will work with
 *  dispBegin() and dispEnd().
 *
 *     Rev 1.1   15 Aug 1991 23:08:02   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.0   12 Jun 1991 01:30:14   GLENN
 *  Initial revision.
 *

 */

/* This is the Original FT_RESTATT() code
   IDEAL

   Public   FT_RestAtt

   Extrn    __ParNI:Far
   Extrn    __ParC:Far
   Extrn    __XGrab:Far
   Extrn    __XFree:Far
   Extrn    __gtSave:Far
   Extrn    __gtRest:Far

   nTop     EQU       Word Ptr BP - 2
   nLeft    EQU       Word Ptr BP - 4
   nBottom  EQU       Word Ptr BP - 6
   nRight   EQU       Word Ptr BP - 8
   nAttr    EQU       Byte Ptr BP - 10
   nBufLen  EQU       Word Ptr BP - 12

   cBuffer  EQU       DWord Ptr BP - 16
   nBufOfs  EQU       Word Ptr BP - 16
   nBufSeg  EQU       Word Ptr BP - 14

   Segment  _NanFor   Word      Public    "CODE"
         Assume    CS:_NanFor

   Proc     FT_RestAtt          Far

         Push      BP                        ; Save BP
         Mov       BP,SP                     ; Set up stack reference
         Sub       SP,16                     ; Allocate locals

         Mov       CX,4                      ; Set param count
   @@Coord: Push      CX                        ; Put on stack
         Call      __ParNI                   ; Retrieve param
         Pop       CX                        ; Get count back
         Push      AX                        ; Put value on stack
         Loop      @@Coord                   ; Get next value

         Pop       [nTop]                    ; Store top coordinate
         Pop       [nLeft]                   ; Store left coordinate
         Pop       [nBottom]                 ; Store bottom coordinate
         Pop       [nRight]                  ; Store right coordinate

         Mov       AX,[nBottom]              ; Load bottom coordinate
         Sub       AX,[nTop]                 ; Subtract top
         Inc       AX                        ; Calc length

         Mov       CX,[nRight]               ; Load right coordinate
         Sub       CX,[nLeft]                ; Subtract left
         Inc       CX                        ; Calc width
         Mul       CX                        ; Multiply length by width
         SHL       AX,1                      ; Calc buffer size
         Mov       [nBufLen],AX              ; Store buffer size

   @@Alloc: Push      AX                        ; Put size on stack
         Call      __xGrab                   ; Allocate memory
         Add       SP,2                      ; Realign stack
         Mov       [nBufSeg],DX              ; Store segment
         Mov       [nBufOfs],AX              ; Store offset

         Push      DX                        ; Load parameters for __gtSave
         Push      AX                        ; onto stack
         Push      [nRight]
         Push      [nBottom]
         Push      [nLeft]
         Push      [nTop]
         Call      __gtSave                  ; Grab screen image

         Push      DS                        ; Save required registers
         Push      SI
         Push      DI

         Mov       AX,5                      ; Specify 5th param
         Push      AX                        ; Put on stack
         Call      __ParC                    ; Get pointer to attr string
         Add       SP,2                      ; Realign stack

         Mov       DS,DX                     ; Load pointer to string
         Mov       SI,AX                     ; into DS:SI
         Mov       ES,[nBufSeg]              ; Load pointer to buffer
         Mov       DI,[nBufOfs]              ; into ES:DI
         Mov       CX,[nBufLen]              ; Load buffer length
         SHR       CX,1                      ; Divide by two

   @@Attr:  Inc       DI                        ; Point DI to attribute
         Lodsb                               ; Grab an attribute byte
         Stosb                               ; Store attribute
         Loop      @@Attr                    ; Do next

         Pop       DI                        ; Restore registers
         Pop       SI
         Pop       DS
         Call      __gtRest                  ; Restore screen image

   Done:    Push      [nBufSeg]                 ; Put segment on stack
         Push      [nBufOfs]                 ; Put offset on stack
         Call      __xFree                   ; Free memory
         Mov       SP,BP                     ; Realign stack
         Pop       BP                        ; Restore BP
         Ret
   Endp     FT_RestAtt
   Ends     _NanFor
   End
 */

/* This is the New one Rewriten in C */

HB_FUNC( FT_RESTATT )
{
   HB_SIZE ulLen = hb_parclen( 5 );

   if( ulLen )
   {
      int            iTop     = hb_parni( 1 );  /* Defaults to zero on bad type */
      int            iLeft    = hb_parni( 2 );  /* Defaults to zero on bad type */
      int            iMaxRow  = hb_gtMaxRow();
      int            iMaxCol  = hb_gtMaxCol();
      int            iBottom  = hb_parnidef( 3, iMaxRow );
      int            iRight   = hb_parnidef( 4, iMaxCol );
      const char *   pAttrib  = hb_parc( 5 );

      if( iTop < 0 )
         iTop = 0;
      if( iLeft < 0 )
         iLeft = 0;
      if( iBottom > iMaxRow )
         iBottom = iMaxRow;
      if( iRight > iMaxCol )
         iRight = iMaxCol;

      if( iTop <= iBottom && iLeft <= iRight )
      {
         while( ulLen && iTop <= iBottom )
         {
            int iCol = iLeft;
            while( ulLen && iCol <= iRight )
            {
               int         iColor;
               HB_BYTE     bAttr;
               HB_USHORT   usChar;
               hb_gtGetChar( iTop, iCol, &iColor, &bAttr, &usChar );
               iColor = ( HB_UCHAR ) *pAttrib++;
               hb_gtPutChar( iTop, iCol, iColor, bAttr, usChar );
               ++iCol;
               --ulLen;
            }
            ++iTop;
         }
      }
   }
}
