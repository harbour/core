/*
 * $Id$
 */

#ifdef __IBMCPP__
  #define INCL_DOSMISC
#endif

#include <extend.h>

#if defined(__TURBOC__) || defined(__BORLANDC__)
  #include <dos.h>
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

HARBOUR OS()
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

#ifdef __GNUC__

#else

/* TODO: add MSVC support but MSVC cannot detect any OS except Windows! */
#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(__MSC__)

#if defined(_Windows)
   hb_os = "Windows 95/98";
   _AX = 0x160A;
   geninterrupt(0x2F);
   if(_AX == 0)
   {
      hb_osmajor  = _BX / 256;
      hb_osminor  = _BX % 256;
      hb_osletter = 0;
   }
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
   else sprintf (version, "%s %d.%02d%c", hb_os, hb_osmajor, hb_osminor, hb_osletter);
   _retc (version);
}

HARBOUR VERSION()
{
   _retc( "Harbour alpha version" );
}

HARBOUR GETENV()
{
   if( _pcount() == 1 )
   {
      char *szName = _parc(1);
      long lName = _parclen(1);

      while( lName && szName[lName - 1] == '=' )
      {
         /* strip the '=' or else it will clear the variable! */
         szName[lName - 1] = 0;
         lName--;
      }
      if( lName )
      {
         char *Value = getenv(szName);
         _retc(Value? Value: "");
      }
      else
         _retc("");
   }
   else
      _retc("");
}
