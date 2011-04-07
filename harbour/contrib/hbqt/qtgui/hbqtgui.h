/*
 * $Id$
 */

#ifndef __HBQTGUI_H
#define __HBQTGUI_H

#include "hbqt.h"

HB_EXTERN_BEGIN

extern HB_EXPORT void * hbqt_gcAllocate_QAction( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QIcon( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QPixmap( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QMenu( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QPalette( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QWidget( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QActionGroup( void * pObj, bool bNew );
extern HB_EXPORT void * hbqt_gcAllocate_QLayout( void * pObj, bool bNew );

HB_EXTERN_END

#define hbqt_par_QAction( n )                                   ( ( QAction                                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QContextMenuEvent( n )                         ( ( QContextMenuEvent                           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPalette( n )                                  ( ( QPalette                                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRegion( n )                                   ( ( QRegion                                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPainter( n )                                  ( ( QPainter                                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPixmap( n )                                   ( ( QPixmap                                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPrinter( n )                                  ( ( QPrinter                                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWidget( n )                                   ( ( QWidget                                     * ) hbqt_gcpointer( n ) )

#define HBQT_TYPE_QBrush                                        ( ( HB_U32 ) 0x94319C70 )
#define HBQT_TYPE_QColor                                        ( ( HB_U32 ) 0x03C0065A )
#define HBQT_TYPE_QFont                                         ( ( HB_U32 ) 0xF6037D8A )
#define HBQT_TYPE_QIcon                                         ( ( HB_U32 ) 0x43035C83 )
#define HBQT_TYPE_QGradient                                     ( ( HB_U32 ) 0x7F4BEC23 )
#define HBQT_TYPE_QMatrix                                       ( ( HB_U32 ) 0xAA3E35B7 )
#define HBQT_TYPE_QImage                                        ( ( HB_U32 ) 0xA0AB4AEC )
#define HBQT_TYPE_QBitmap                                       ( ( HB_U32 ) 0xBA602AD4 )
#define HBQT_TYPE_QBoxLayout                                    ( ( HB_U32 ) 0xA184A755 )

#ifndef HBQT_TYPE_QPixmap
#define HBQT_TYPE_QPixmap                                       ( ( HB_U32 ) 0xBEA836FC )
#endif

#endif /* __HBQTGUI_H */
