/*
 * $Id$
 */

/*
 * File......: vertmenu.prg
 * Author....: Greg Lief
 * CIS ID....: 72460,1760
 *
 * This function is an original work by Mr. Grump and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:04:48   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   01 Apr 1991 01:02:26   GLENN
 * Nanforum Toolkit
 *
 */

#include "box.ch"

// test code
#ifdef FT_TEST

PROCEDURE Main()
LOCAL MAINMENU := ;
    { { "DATA ENTRY", "ENTER DATA",         { || FT_MENU2(datamenu)  } }, ;
      { "Reports",    "Hard copy",          { || FT_MENU2(repmenu)   } }, ;
      { "Maintenance","Reindex files, etc.",{ || FT_MENU2(maintmenu) } }, ;
      { "Quit", "See ya later" } }

local datamenu := { { "Customers", , { || cust() } }   , ;
                    { "Invoices",  , { || inv() } }    , ;
                    { "Vendors",   , { || vendors() } }, ;
                    { "Exit", "Return to Main Menu" } }

local repmenu :=  { { "Customer List", , { || custrep() } }  , ;
                    { "Past Due",      , { || pastdue() } }  , ;
                    { "Weekly Sales",  , { || weeksales() } }, ;
                    { "Monthly P&L",   , { || monthpl() } }  , ;
                    { "Vendor List",   , { || vendorrep() } }, ;
                    { "Exit", "Return to Main Menu" } }

local maintmenu := { { "Reindex",  "Rebuild index files", { || re_ntx() } } , ;
                     { "Backup",   "Backup data files"  , { || backup() } } , ;
                     { "Compress", "Compress data files", { || compress()} }, ;
                     { "Exit", "Return to Main Menu" } }

FT_MENU2(mainmenu)
return

/* stub functions to avoid missing symbols */
static function cust
static function inv
static function vendors
static function custrep
static function pastdue
static function weeksales
static function monthpl
static function vendorrep
static function re_ntx
static function backup
static function compress

#endif

/*
   FT_MENU2(): display vertical menu
*/

FUNCTION ft_menu2( aMenuInfo, cColors )

LOCAL nChoice     := 1                       ,;
      nOptions    := Len( aMenuInfo )        ,;
      nMaxwidth   := 0                       ,;
      nLeft                                  ,;
      x                                      ,;
      cOldscreen                             ,;
      nTop                                   ,;
      lOldwrap    := Set( _SET_WRAP, .T. )   ,;
      lOldcenter  := Set( _SET_MCENTER, .T. ),;
      lOldmessrow := Set( _SET_MESSAGE )     ,;
      cOldcolor   := Set( _SET_COLOR )

IF cColors # NIL
   Set( _SET_COLOR, cColors )
ENDIF

/* if no message row has been established, use bottom row */
IF lOldmessrow == 0
   Set( _SET_MESSAGE, Maxrow() )
ENDIF

/* determine longest menu option */
Aeval( aMenuInfo, { | ele | nMaxwidth := max( nMaxwidth, len( ele[1] ) ) } )

/* establish top and left box coordinates */
nLeft := ( ( Maxcol() + 1 ) - nMaxwidth ) / 2
nTop  := ( ( Maxrow() + 1 ) - ( nOptions + 2 ) ) / 2

DO WHILE nChoice != 0 .AND. nChoice != nOptions

   cOldscreen := Savescreen( nTop, nLeft - 1, nTop + nOptions + 1, nLeft + nMaxwidth )

   @ nTop, nLeft - 1, nTop + nOptions + 1, nLeft + nMaxwidth BOX B_SINGLE + ' '
   Devpos( nTop, nLeft )
   FOR x := 1 to Len( aMenuInfo )
      IF Len( aMenuInfo[x] ) > 1 .AND. aMenuInfo[x,2] != NIL
         @ Row() + 1, nLeft PROMPT Padr( aMenuInfo[x, 1], nMaxwidth ) ;
                            MESSAGE aMenuInfo[x,2]
      ELSE
         @ Row() + 1, nLeft PROMPT Padr( aMenuInfo[x,1], nMaxwidth )
      ENDIF
   NEXT

   MENU TO nChoice

   Restscreen( nTop, nLeft - 1, nTop + nOptions + 1, nLeft + nMaxwidth, cOldscreen )

   /* execute action block attached to this option if there is one */
   IF nChoice > 0 .AND. Len(  aMenuInfo[ nChoice ]  ) == 3
      Eval(  aMenuInfo[nChoice,3]  )
   ENDIF

ENDDO

/* restore previous message and wrap settings */
Set( _SET_MESSAGE, lOldmessrow )
Set( _SET_MCENTER, lOldcenter )
Set( _SET_WRAP,    lOldwrap )
Set( _SET_COLOR,   cOldcolor )

RETURN NIL
