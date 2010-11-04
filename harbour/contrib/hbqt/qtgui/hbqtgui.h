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

#define HBQT_TYPE_QBitmap                                       0x210679C1
#define HBQT_TYPE_QBoxLayout                                    0x8E2E0CAF
#define HBQT_TYPE_QBrush                                        0x96A039B0
#define HBQT_TYPE_QColor                                        0x0151A39A
#define HBQT_TYPE_QFont                                         0x0A35B749
#ifndef HBQT_TYPE_QIcon
#define HBQT_TYPE_QIcon                                         0xBF359640
#endif
#ifndef HBQT_TYPE_QPixmap
#define HBQT_TYPE_QPixmap                                       0x25CE65E9
#endif

#endif /* __HBQTGUI_H */
