/*
 * $Id$
 */

#include <extend.h>
#include <init.h>

#ifdef __IBMCPP__
  #define INCL_DOSMISC
#endif

#include <errorapi.h>
#include <version.h>

#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(__MSC__) || defined(_MSC_VER) || defined(__DJGPP__)
  #include <dos.h>
  #include <stdlib.h>
#endif

#if defined(__GNUC__) && !defined(__DJGPP__)
  #include <sys/utsname.h>
#endif

#ifdef __WATCOMC__
  #include <i86.h>
  #if defined(__386__) && !defined(__WINDOWS_386__)
    #define INT_86 int386
  #else
    #define INT_86 int86
  #endif
#else
  #if defined(__EMX__)
    #define INT_86 _int86
  #else
    #define INT_86 int86
  #endif
#endif

HARBOUR HB___RUN(void);
HARBOUR HB_GETENV(void);
HARBOUR HB_OS(void);
HARBOUR HB_VERSION(void);

static SYMBOL symbols[] = {
{ "__RUN"  , FS_PUBLIC, HB___RUN  , 0 },
{ "GETENV" , FS_PUBLIC, HB_GETENV , 0 },
{ "OS"     , FS_PUBLIC, HB_OS     , 0 },
{ "VERSION", FS_PUBLIC, HB_VERSION, 0 }
};

HB_INIT_SYMBOLS( Environ__InitSymbols );
/*
void Environ__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}
*/

HARBOUR HB_OS(void)
{
   int hb_osmajor = -1, hb_osminor = -1, hb_osletter = -1;
   char * hb_os = 0;
   char version [128];
#ifdef __IBMCPP__

   unsigned long aulQSV [QSV_MAX] = {0};
   APIRET rc= DosQuerySysInfo (1L, QSV_MAX, (PVOID) aulQSV, sizeof (ULONG) * QSV_MAX);
   if (!rc)
   {
      hb_osmajor  = aulQSV [QSV_VERSION_MAJOR] / 10;
      hb_osminor  = aulQSV [QSV_VERSION_MINOR];
      hb_osletter = (aulQSV [QSV_VERSION_REVISION] > 0 && aulQSV [QSV_VERSION_REVISION] < 26) ? '@' + aulQSV [QSV_VERSION_REVISION] : 0;
   }
   hb_os = "OS/2";

#else

#if defined(__GNUC__) && !defined(__DJGPP__)

   struct utsname un;

   uname( &un );

   sprintf( version, "%s %s", un.sysname, un.release );

   /* DAP: Currently, OS() is coded with the expection that the operating
      system plays the major/minor version number game. Not all operating
      systems work like that. So, we do a little bit of mucking around
      because we've managed to work out version[] on our own. */

   hb_os      = "";
   hb_osmajor = -2;

#else

/* TODO: add MSVC support but MSVC cannot detect any OS except Windows! */
#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(__MSC__)

#if defined(_Windows) || defined(_WIN32)
   /* TODO: Determine the Windows type (32s/95/98/NT) and version */
   hb_os = "Windows";
#else
   /* detect OS/2 */
   if(_osmajor >= 10)
   {
      hb_os = "OS/2";
      if (_osmajor == 20 && _osminor > 20)
      {
         hb_osmajor = _osminor / 10;
         hb_osminor = _osminor % 10;
      }
      else
      {
         hb_osmajor  = _osmajor / 10;
         hb_osminor  = _osminor;
      }
      hb_osletter = 0;
   }
   else
   {
#if defined(__MSC__)
      if (_osmode == _WIN_MODE)
      {
         hb_os = "Windows";
         hb_osmajor  = _osmajor;
         hb_osminor  = _osminor
         hb_osletter = 0;
      }
#else
      /* detect Windows */
      _AX = 0x160A;
      geninterrupt(0x2F);
      if(_AX == 0)
      {
         hb_osmajor  = _BX / 256;
         hb_osminor  = _BX % 256;
         hb_osletter = 0;
         hb_os = "Windows 95/98";
      }
#endif /* __MSC__ */
      else
      {
         hb_os = "DOS";
         hb_osmajor  = _osmajor;
         hb_osminor  = _osminor;
         hb_osletter = 0;
      }
   }

#endif /* defined(_Windows) */

#else

   union REGS regs;

   /* detect OS/2 */
   regs.h.ah = 0x30;

   INT_86( 0x21, &regs, &regs );

   if(regs.h.al >= 10)
   {
      hb_os = "OS/2";
      if (regs.h.al == 20 && regs.h.ah > 20)
      {
         hb_osmajor = regs.h.ah / 10;
         hb_osminor = regs.h.ah % 10;
      }
      else
      {
         hb_osmajor  = regs.h.al / 10;
         hb_osminor  = regs.h.ah;
      }
      hb_osletter = 0;
   }
   else
   {
      hb_osmajor  = _osmajor;
      hb_osminor  = _osminor;
      regs.w.ax = 0x160A;

      INT_86( 0x2F, &regs, &regs );

      if( regs.w.ax == 0)
      {
         hb_os = "Windows";
         hb_osmajor  = regs.w.bx / 256;
         hb_osminor  = regs.w.bx % 256;
         hb_osletter = 0;
      }
      else
      {
         hb_os = "DOS";
         hb_osletter = 0;
      }
   }
#endif /* __TURBOC__ or __BORLANDC__ or __MSC__ */

   /* TODO: detect other OSes */

#endif /* __GNUC__ */
#endif /* __IBMCPP__ */

   if (!hb_os) strcpy (version, "Unknown");
   else if (hb_osmajor == -1) strcpy (version, hb_os);
   else if (hb_osmajor == -2 ) { /* NOP */ }
   else sprintf (version, "%s %d.%02d%c", hb_os, hb_osmajor, hb_osminor, hb_osletter);
   hb_retc (version);
}

HARBOUR HB_VERSION(void)
{
   char hb_ver[ 80 ];
   sprintf( hb_ver, "Harbour %d.%d%s Intl. (Build %d)  (%04d.%02d.%02d)",
      hb_major, hb_minor, hb_revision, hb_build, hb_year, hb_month, hb_day );
   hb_retc( hb_ver );
}

HARBOUR HB_GETENV(void)
{
   if( hb_pcount() == 1 )
   {
      char *szName = hb_parc(1);
      long lName = hb_parclen(1);

      while( lName && szName[lName - 1] == '=' )
      {
         /* strip the '=' or else it will clear the variable! */
         szName[lName - 1] = 0;
         lName--;
      }
      if( lName )
      {
         char *Value = getenv(szName);
         hb_retc(Value? Value: "");
      }
      else
         hb_retc("");
   }
   else
      hb_retc("");
}

/*
 * $FunctionName$
 *    __RUN
 * $Syntax$
 *    __RUN( <cCommand> )
 * $Argument$
 *    <cCommand> Command to execute
 * $Description$
 *    This command runs an external program. Please make sure that you have
 *    enough free memory to be able to run the external program.
 *    Do not use it to run Terminate and Stay Resident programs (in case of DOS)
 *    since it cause several problems
 * $Examples$
 *    __Run( "edit "+cMyTextFile )      // Runs an external editor
 *    __Run( "command" )                // Gives a DOS shell (DOS only)
 * $Files$
 *    source/rtl/environ.c
 *    Run an external program
 * $See also$
 *    ErrorLevel() ??   // TO DO : Is this correct ?
 */
HARBOUR HB___RUN( void )
{
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__)
   if( hb_pcount() == 1 )                         /* Parameter passed         */
   {
      system( hb_parc( 1 ) );
   }
#endif
}
