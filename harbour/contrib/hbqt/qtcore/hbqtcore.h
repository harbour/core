/*
 * $Id$
 */

#ifndef __HBQTCORE_H
#define __HBQTCORE_H

#include "hbqt.h"

HB_EXTERN_BEGIN

extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_HBQEvents );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_HBQSlots );
extern HB_EXPORT HBQT_GC_FUNC( hbqt_gcRelease_HBQString );

extern HB_EXPORT void * hbqt_gcAllocate_HBQEvents( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_HBQSlots( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_HBQString( void * pObj, bool bNew );

HB_EXTERN_END

#define hbqt_par_HBQEvents( n )                                 ( ( HBQEvents                                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQSlots( n )                                  ( ( HBQSlots                                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQString( n )                                 ( ( HBQString                                   * ) hbqt_gcpointer( n ) )

#define HBQT_TYPE_QSize                                         ( ( HB_U32 ) 0x5E28750F )

#endif /* __HBQTCORE_H */
