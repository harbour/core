/*
 * File......: VERTMENU.PRG
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

/*  $DOC$
 *  $FUNCNAME$
 *     FT_MENU2()
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Vertical lightbar menu
 *  $SYNTAX$
 *     FT_MENU2( <aMenuarray> [, <cColors> ] ) -> NIL
 *  $ARGUMENTS$
 *     <aMenuarray> is an array of menu options, messages, and action
 *      blocks.
 *
 *     Each element in this array is a nested array with the structure:
 *
 *        element[x, 1] = menu option
 *        element[x, 2] = message to be displayed when option is highlighted
 *        element[x, 3] = code block to be executed when option is selected
 *
 *     <cColors> is a string containing colors for the prompts, in the same
 *     format as that returned by Set( _SET_COLOR ).  If not supplied,
 *     colors default to the current color setting.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     This function greatly simplifies the process of displaying light-bar
 *     menus.  All prompts are padded out with spaces so they are the same
 *     length, a box is drawn around the prompts, the box is automatically
 *     centered on the screen, and the underlying screen is restored after
 *     a menu selection has been made.
 *
 *     Additionally, because you can tie action blocks to each menu
 *     option, you can save on a lot of DO CASE or IF..ELSEIF code in your
 *     main program.  See the test code for a succinct demonstration.
 *  $EXAMPLES$
 *      LOCAL mainmenu := ;
 *          { { "Data Entry", "Enter data",   { || FT_MENU2(datamenu)  } }, ;
 *            { "Reports",    "Hard copy",    { || FT_MENU2(repmenu)   } }, ;
 *            { "Maintenance","Reindex files",{ || FT_MENU2(maintmenu) } }, ;
 *            { "Quit", "See ya later" } }
 *      FT_MENU2(mainmenu)
 *  $END$
 */

#include "box.ch"

// test code
#ifdef FT_TEST

FUNCTION MAIN
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
return nil

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

* end of file: vertmenu.prg                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
