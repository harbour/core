#ifndef HARBOUR_OK
#define HARBOUR_OK	0	// ES_WHOCARES
#endif


typedef ULONG	 ERRCODE;
typedef unsigned char UCHAR;
typedef void *		HANDLE;

typedef BOOL *		BOOLP;

#ifdef __XPP__
#define ITEM			ContainerHandle
#define PCOUNT( paramList )	PCOUNT( paramList )
#define HARBOUR 		void
#define HARBOUR_ParamList	void *
#define paramList		paramList
#define XBONLY( x )		x
//	xbReleaseC( cFieldName )
#else

#define PCOUNT( paramList )	hb_parinfo(0)

#ifndef PCOUNT
#define PCOUNT			hb_parinfo(0)
#endif

#ifdef __GNUC__
//define HARBOUR		 void __attribute__ ((stdcall))
//already defined in ./include/
#else
#define HARBOUR 		void
#endif

#define HARBOUR_ParamList	void
#define paramList
#define XBONLY( x )
#define xbReleaseC( cFieldName )
#endif

#ifndef __XPP__

#define _PARC( paramlist, n )	hb_parc( n )
#define _PARNL( paramList, n )	hb_parnl( n )
#define _PARL( paramList, n )	hb_parl( n )
#define _PARTYPE( paramList, n) hb_parinfo( n )
#define _RETNL( paramList, n)	hb_retnl( n )
#define _RETL( paramList, n)	hb_retl( n )
#define _RETC( paramList, s)	hb_retc( s )
#define _RET(  paramlist    )	hb_ret()
#define _ITEMNEW( x )		hb_itemNew( x )
#define _ITEMRELEASE( item )	hb_itemRelease( item )
#define _ITEMRETURN( paramList, item )	   hb_itemReturn( item )
#define _ITEMPARAM( paramList, n )   hb_itemParam( n )
#define _ITEMPUTNL( item, nLong )    hb_itemPutNL( item, nLong)
#define _ITEMPUTC(  item, cStr	)    hb_itemPutC(  item, cStr )
#define _ITEMGETC(  item )	     hb_itemGetC(  item )
#else

#define _PARC( paramlist, n )	_xbParC( paramList, n)
#define _PARNL( paramlist, n )	_parnl( paramList, n )
#define _PARL( paramlist, n )	_parl( paramList, n )
#define _PARTYPE( paramList, n) _partype( paramList, n )
#define _RETNL( paramlist, n )	_retnl( paramList, n )
#define _RETL( paramlist, n )	_retl( paramList, n )
#define _RETC( paramlist, s )	_retc( paramList, s )
#define _RET(  paramlist    )	_ret(  paramList )
#define _ITEMNEW( x )		_conNew( x )
#define _ITEMRELEASE( item )	_conRelease( item )
#define _ITEMRETURN( paramList, x )	   _conReturn( paramList, x )
#define _ITEMPARAM( paramList, n )   _conParam( paramList, n, &pbRef);
#define _ITEMPUTNL( item, nLong )    _conPutNL( item, nLong)
#define _ITEMPUTC(  item, cStr	)    _conPutC(	item, cStr )
#define _ITEMGETC(  item )	     _xbGetC(	item )


#include <windows.h>
#include "xppdef.h"
#include "xppcon.h"

#endif



#ifdef DEBUG
void logmsg( char *s, ...);

#ifdef __XPP__
#define TRACEENTRY( p1, p2)	traceentry( p1, p2)
#define TRACEEXIT(  p1, p2)	traceexit(  p1, p2)
void traceentry( UCHAR *entrypoint, HARBOUR_ParamList paramList);
void traceexit(  UCHAR *entrypoint, HARBOUR_ParamList paramList);
void tracecall(  UCHAR *entrypoint, HARBOUR_ParamList paramList);
#else
#define TRACEENTRY( p1, p2)	traceentry( p1 )
#define TRACEEXIT(  p1, p2)	traceexit(  p1 )
void traceentry( UCHAR *entrypoint );
void traceexit(  UCHAR *entrypoint );
void tracecall(  UCHAR *entrypoint );
#endif

#define DEBUGTRACE1( x1 )				  logmsg( x1 )
#define DEBUGTRACE2( x1, x2 )				  logmsg( x1, x2 )
#define DEBUGTRACE3( x1, x2, x3)			  logmsg( x1, x2, x3)
#define DEBUGTRACE4( x1, x2, x3, x4)			  logmsg( x1, x2, x3, x4)
#define DEBUGTRACE5( x1, x2, x3, x4, x5)		  logmsg( x1, x2, x3, x4, x5)
#define DEBUGTRACE6( x1, x2, x3, x4, x5, x6)		  logmsg( x1, x2, x3, x4, x5, x6)
#define DEBUGTRACE7( x1, x2, x3, x4, x5, x6, x7)	  logmsg( x1, x2, x3, x4, x5, x6, x7)
#define DEBUGTRACE8( x1, x2, x3, x4, x5, x6, x7, x8)	  logmsg( x1, x2, x3, x4, x5, x6, x7, x8)
#define DEBUGTRACE9( x1, x2, x3, x4, x5, x6, x7, x8, x9 ) logmsg( x1, x2, x3, x4, x5, x6, x7, x8, x9)
#define DEBUGTRACE10(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 ) logmsg( x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
#define DEBUGTRACEX	  tracecall
#else

void logmsg( char *s, ...);
#define TRACEENTRY( p1, p2)
#define TRACEEXIT(  p1, p2)

#define DEBUGTRACE1( x1 )
#define DEBUGTRACE2( x1, x2 )
#define DEBUGTRACE3( x1, x2, x3)
#define DEBUGTRACE4( x1, x2, x3, x4)
#define DEBUGTRACE5( x1, x2, x3, x4, x5)
#define DEBUGTRACE6( x1, x2, x3, x4, x5, x6)
#define DEBUGTRACE7( x1, x2, x3, x4, x5, x6, x7)
#define DEBUGTRACE8( x1, x2, x3, x4, x5, x6, x7, x8)
#define DEBUGTRACE9( x1, x2, x3, x4, x5, x6, x7, x8, x9 )
#define DEBUGTRACE10(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10 )
#define DEBUGTRACEX( x, y )
#endif
