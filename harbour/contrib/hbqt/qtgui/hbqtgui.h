/*
 * $Id$
 */

#ifndef __HBQTGUI_H
#define __HBQTGUI_H

#include "hbqt.h"

#define hbqt_par_QAction( n )                                   ( ( QAction                                     * ) hbqt_par_ptr( n ) )
#define hbqt_par_QContextMenuEvent( n )                         ( ( QContextMenuEvent                           * ) hbqt_par_ptr( n ) )
#define hbqt_par_QPalette( n )                                  ( ( QPalette                                    * ) hbqt_par_ptr( n ) )
#define hbqt_par_QRegion( n )                                   ( ( QRegion                                     * ) hbqt_par_ptr( n ) )
#define hbqt_par_QPainter( n )                                  ( ( QPainter                                    * ) hbqt_par_ptr( n ) )
#define hbqt_par_QPixmap( n )                                   ( ( QPixmap                                     * ) hbqt_par_ptr( n ) )
#define hbqt_par_QPrinter( n )                                  ( ( QPrinter                                    * ) hbqt_par_ptr( n ) )
#define hbqt_par_QWidget( n )                                   ( ( QWidget                                     * ) hbqt_par_ptr( n ) )

#endif /* __HBQTGUI_H */
