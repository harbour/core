/* Rewritten in 2014 by Viktor Szakats (vszakats.net/harbour) and kept in the
   public domain.
   This is an original work by Glenn Scott and is placed in the public domain.

      Rev 1.4   17 Oct 1992 16:28:22   GLENN
   Leo cleaned up documentation blocks.

      Rev 1.3   08 Oct 1992 01:37:34   GLENN
   Added ft_NWSemUnlock() to complement ft_NWSemLock().  Modified
   the calling procedure for ft_NWSemLock() but it shouldn't break any
   existing code, although I doubt anyone's using it.


      Rev 1.2   17 Aug 1991 16:11:46   GLENN
   Oops, I forgot to comment out some test code.

      Rev 1.1   15 Aug 1991 23:05:34   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.0   28 Jun 1991 00:44:14   GLENN
   Initial revision.
 */

/* Semaphore Package for Novell NetWare */

#include "hbapi.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

static int _ft_nwsemopen( const char * szName, long nInitVal, HB_U32 * pnHandle, int * pnOpenCnt )
{
#if defined( HB_OS_DOS )
   {
      HB_BYTE buffer[ 1 + 128 ];

      union REGS regs;
      struct SREGS sregs;

      hb_strncpy( ( char * ) ( buffer + 1 ), szName, sizeof( buffer ) - 2 );
      buffer[ 0 ] = ( HB_BYTE ) strlen( ( char * ) ( buffer + 1 ) );

      regs.h.ah = 0xC5;
      regs.h.al = 0;
      regs.HB_XREGS.dx = FP_OFF( buffer );
      sregs.ds = FP_SEG( buffer );
      regs.HB_XREGS.cx = ( HB_U16 ) nInitVal;
      HB_DOS_INT86X( 0x21, &regs, &regs, &sregs );

      *pnHandle = ( ( ( HB_U32 ) regs.HB_XREGS.dx ) << 16 ) || ( HB_U32 ) regs.HB_XREGS.cx;
      *pnOpenCnt = regs.h.bl;

      return ( HB_BYTE ) regs.h.al;
   }
#else  /* TODO */
   HB_SYMBOL_UNUSED( szName );
   HB_SYMBOL_UNUSED( nInitVal );
   *pnHandle = *pnOpenCnt = 0;
   return 1;
#endif
}

HB_FUNC( FT_NWSEMOPEN )
{
   HB_U32 nHandle;
   int nOpenCnt;

   hb_retni( _ft_nwsemopen( hb_parcx( 1 ), hb_parnl( 2 ), &nHandle, &nOpenCnt ) );

   hb_stornint( nHandle, 3 );
   hb_storni( nOpenCnt, 4 );
}

HB_FUNC( FT_NWSEMEX )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;

      HB_U32 nHandle = ( HB_U32 ) hb_parnint( 1 );

      regs.h.ah = 0xC5;
      regs.h.al = 1;
      regs.HB_XREGS.cx = HB_LOWORD( nHandle );
      regs.HB_XREGS.dx = HB_HIWORD( nHandle );
      HB_DOS_INT86( 0x21, &regs, &regs );

      hb_stornl( regs.HB_XREGS.cx, 2 );
      hb_storni( regs.h.dl, 3 );

      hb_retni( ( HB_BYTE ) regs.h.al );
   }
#else  /* TODO */
   hb_stornl( 0, 2 );
   hb_storni( 0, 3 );
   hb_retni( 1 );
#endif
}

#define WAIT_SEMAPHORE    2
#define SIGNAL_SEMAPHORE  3
#define CLOSE_SEMAPHORE   4

static int _ft_nwsemfunc( HB_BYTE nMode, HB_U32 nHandle, HB_UINT nTimeOut )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;

      regs.h.ah = 0xC5;
      regs.h.al = nMode;
      regs.HB_XREGS.cx = HB_LOWORD( nHandle );
      regs.HB_XREGS.dx = HB_HIWORD( nHandle );
#if defined( __DJGPP__ )
      regs.HB_XREGS.bp = ( HB_U16 ) nTimeOut;
#else
      HB_SYMBOL_UNUSED( nTimeOut );
#endif
      HB_DOS_INT86( 0x21, &regs, &regs );

      return ( HB_BYTE ) regs.h.al;
   }
#else  /* TODO */
   HB_SYMBOL_UNUSED( nMode );
   HB_SYMBOL_UNUSED( nHandle );
   HB_SYMBOL_UNUSED( nTimeOut );
   return 1;
#endif
}

HB_FUNC( FT_NWSEMWAIT )
{
   hb_retni( _ft_nwsemfunc( WAIT_SEMAPHORE, ( HB_U32 ) hb_parnint( 1 ), ( HB_UINT ) hb_parni( 2 ) ) );
}

HB_FUNC( FT_NWSEMSIG )
{
   hb_retni( _ft_nwsemfunc( SIGNAL_SEMAPHORE, ( HB_U32 ) hb_parnint( 1 ), 0 ) );
}

HB_FUNC( FT_NWSEMCLOSE )
{
   hb_retni( _ft_nwsemfunc( CLOSE_SEMAPHORE, ( HB_U32 ) hb_parnint( 1 ), 0 ) );
}

HB_FUNC( FT_NWSEMLOCK )
{
   HB_U32 nHandle = ( HB_U32 ) hb_parnint( 2 );
   int nOpenCnt;

   if( _ft_nwsemopen( hb_parcx( 1 ), 0, &nHandle, &nOpenCnt ) == 0 && nOpenCnt != 1 )
   {
      _ft_nwsemfunc( CLOSE_SEMAPHORE, nHandle, 0 );
      nHandle = 0;
      hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_TRUE );

   hb_stornint( nHandle, 2 );
}

HB_FUNC( FT_NWSEMUNLOCK )
{
   hb_retl( _ft_nwsemfunc( CLOSE_SEMAPHORE, ( HB_U32 ) hb_parnint( 1 ), 0 ) == 0 );
}
