/*
 * $Id$
 */

#include <extend.h>

HARBOUR OS()
{
/* TODO: add MSVC support but MSVC cannot detect any OS except Windows! */
#if defined(__TURBOC__) || defined(__BORLANDC__)

  /* detect OS/2 */
  _AX = 0x2B01;
  _CX = 0x4445;
  _DX = 0x5351;
  _AH = 0x30;
  geninterrupt(0x21);
  if(_AL >= 10)
  {
     _retc("OS/2");
  }

  /* detect Windows */
  /* TODO: get Windows version (major, minor) */
  _AX = 0x160A;
  geninterrupt(0x2F);
  if(_AX == 0)
  {
     _retc("Windows");
  }

#else

  union REGS regs;

  /* detect OS/2 */
  regs.h.ah = 0x30;

#if defined(__WATCOMC__) && defined(__386__)

  int386(0x21, &regs, &regs);

#else

#if defined(__EMX__)

  _int86(0x21, &regs, &regs);

#else

  int86(0x21, &regs, &regs);

#endif /* __EMX__ */

  if(regs.h.al >= 10)
  {
     _retc("OS/2");
  }

  /* TODO: get Windows version (major, minor) */
  /* detect Windows */
  regs.w.ax = 0x160A;

#if defined(__WATCOMC__) && defined(__386__)

  int386(0x2F, &regs, &regs);

#else

#if defined(__EMX__)

  _int86(0x2F, &regs, &regs);

#else

  int86(0x2F, &regs, &regs);

#endif /* EMX */

  if(regs.x.ax == 0)
  {
     _retc("Windows");
  }

#endif /* WATCOMC */

#endif /* __TURBOC__ or __BORLANDC__ */

  /* fall through to MS-DOS */
  /* TODO: detect other OSes */
  /* TODO: detect MS-DOS version */
  _retc("MS-DOS");
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

