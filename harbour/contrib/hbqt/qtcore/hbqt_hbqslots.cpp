/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbapierr.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqslots.h"

#include <QtCore/QPointer>

/*----------------------------------------------------------------------*/
#ifdef __PRITPAL__

static bool connect_signal( QString signal, QObject * object, HBQSlots * t_slots )
{

   if( signal == ( QString ) "customContextMenuRequested(QPoint)"             ) return object->connect( object, SIGNAL( customContextMenuRequested( const QPoint & )                      ), t_slots, SLOT( customContextMenuRequested( const QPoint & )                       ), Qt::AutoConnection );
   if( signal == ( QString ) "clicked()"                                      ) return object->connect( object, SIGNAL( clicked()                                                         ), t_slots, SLOT( clicked()                                                          ), Qt::AutoConnection );
   if( signal == ( QString ) "returnPressed()"                                ) return object->connect( object, SIGNAL( returnPressed()                                                   ), t_slots, SLOT( returnPressed()                                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "triggered()"                                    ) return object->connect( object, SIGNAL( triggered()                                                       ), t_slots, SLOT( triggered()                                                        ), Qt::AutoConnection );
   if( signal == ( QString ) "hovered()"                                      ) return object->connect( object, SIGNAL( hovered()                                                         ), t_slots, SLOT( hovered()                                                          ), Qt::AutoConnection );
   if( signal == ( QString ) "viewportEntered()"                              ) return object->connect( object, SIGNAL( viewportEntered()                                                 ), t_slots, SLOT( viewportEntered()                                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "pressed()"                                      ) return object->connect( object, SIGNAL( pressed()                                                         ), t_slots, SLOT( pressed()                                                          ), Qt::AutoConnection );
   if( signal == ( QString ) "released()"                                     ) return object->connect( object, SIGNAL( released()                                                        ), t_slots, SLOT( released()                                                         ), Qt::AutoConnection );
   if( signal == ( QString ) "stateChanged(int)"                              ) return object->connect( object, SIGNAL( stateChanged( int )                                               ), t_slots, SLOT( stateChanged( int )                                                ), Qt::AutoConnection );
   if( signal == ( QString ) "activated(int)"                                 ) return object->connect( object, SIGNAL( activated( int )                                                  ), t_slots, SLOT( activated( int )                                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "currentIndexChanged(int)"                       ) return object->connect( object, SIGNAL( currentIndexChanged( int )                                        ), t_slots, SLOT( currentIndexChanged( int )                                         ), Qt::AutoConnection );
   if( signal == ( QString ) "highlighted(int)"                               ) return object->connect( object, SIGNAL( highlighted( int )                                                ), t_slots, SLOT( highlighted( int )                                                 ), Qt::AutoConnection );
   if( signal == ( QString ) "triggered(bool)"                                ) return object->connect( object, SIGNAL( triggered( bool )                                                 ), t_slots, SLOT( triggered( bool )                                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "clicked(QModelIndex)"                           ) return object->connect( object, SIGNAL( clicked( const QModelIndex & )                                    ), t_slots, SLOT( clicked( const QModelIndex & )                                     ), Qt::AutoConnection );
   if( signal == ( QString ) "doubleClicked(QModelIndex)"                     ) return object->connect( object, SIGNAL( doubleClicked( const QModelIndex & )                              ), t_slots, SLOT( doubleClicked( const QModelIndex & )                               ), Qt::AutoConnection );
   if( signal == ( QString ) "entered(QModelIndex)"                           ) return object->connect( object, SIGNAL( entered( const QModelIndex & )                                    ), t_slots, SLOT( entered( const QModelIndex & )                                     ), Qt::AutoConnection );
   if( signal == ( QString ) "hovered(action)"                                ) return object->connect( object, SIGNAL( hovered( QAction * )                                              ), t_slots, SLOT( hovered( QAction * )                                               ), Qt::AutoConnection );
   if( signal == ( QString ) "currentChanged(int)"                            ) return object->connect( object, SIGNAL( currentChanged( int )                                             ), t_slots, SLOT( currentChanged( int )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "actionTriggered(int)"                           ) return object->connect( object, SIGNAL( actionTriggered(int)                                              ), t_slots, SLOT( actionTriggered(int)                                               ), Qt::AutoConnection );
   if( signal == ( QString ) "rangeChanged(int,int)"                          ) return object->connect( object, SIGNAL( rangeChanged(int,int)                                             ), t_slots, SLOT( rangeChanged(int,int)                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "sliderMoved(int)"                               ) return object->connect( object, SIGNAL( sliderMoved(int)                                                  ), t_slots, SLOT( sliderMoved(int)                                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "sliderPressed()"                                ) return object->connect( object, SIGNAL( sliderPressed()                                                   ), t_slots, SLOT( sliderPressed()                                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "sliderReleased()"                               ) return object->connect( object, SIGNAL( sliderReleased()                                                  ), t_slots, SLOT( sliderReleased()                                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "valueChanged(int)"                              ) return object->connect( object, SIGNAL( valueChanged(int)                                                 ), t_slots, SLOT( valueChanged(int)                                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "cursorPositionChanged(int,int)"                 ) return object->connect( object, SIGNAL( cursorPositionChanged(int,int)                                    ), t_slots, SLOT( cursorPositionChanged(int,int)                                     ), Qt::AutoConnection );
   if( signal == ( QString ) "editingFinished()"                              ) return object->connect( object, SIGNAL( editingFinished()                                                 ), t_slots, SLOT( editingFinished()                                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "returnPressed()"                                ) return object->connect( object, SIGNAL( returnPressed()                                                   ), t_slots, SLOT( returnPressed()                                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "selectionChanged()"                             ) return object->connect( object, SIGNAL( selectionChanged()                                                ), t_slots, SLOT( selectionChanged()                                                 ), Qt::AutoConnection );
   if( signal == ( QString ) "textChanged(QString)"                           ) return object->connect( object, SIGNAL( textChanged( const QString & )                                    ), t_slots, SLOT( textChanged( const QString & )                                     ), Qt::AutoConnection );
   if( signal == ( QString ) "textEdited(QString)"                            ) return object->connect( object, SIGNAL( textEdited( const QString & )                                     ), t_slots, SLOT( textEdited( const QString & )                                      ), Qt::AutoConnection );
   /* QTreeWidget */
   if( signal == ( QString ) "itemCollapsed(QTWItem)"                         ) return object->connect( object, SIGNAL( itemCollapsed( QTreeWidgetItem * )                                ), t_slots, SLOT( itemCollapsed( QTreeWidgetItem * )                                 ), Qt::AutoConnection );
   if( signal == ( QString ) "itemExpanded(QTWItem)"                          ) return object->connect( object, SIGNAL( itemExpanded( QTreeWidgetItem * )                                 ), t_slots, SLOT( itemExpanded( QTreeWidgetItem * )                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "currentItemChanged(QTWItem,QTWItem)"            ) return object->connect( object, SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * )        ), t_slots, SLOT( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * )         ), Qt::AutoConnection );
   if( signal == ( QString ) "itemActivated(QTWItem,int)"                     ) return object->connect( object, SIGNAL( itemActivated( QTreeWidgetItem *, int )                           ), t_slots, SLOT( itemActivated( QTreeWidgetItem *, int )                            ), Qt::AutoConnection );
   if( signal == ( QString ) "itemChanged(QTWItem,int)"                       ) return object->connect( object, SIGNAL( itemChanged( QTreeWidgetItem *, int )                             ), t_slots, SLOT( itemChanged( QTreeWidgetItem *, int )                              ), Qt::AutoConnection );
   if( signal == ( QString ) "itemClicked(QTWItem,int)"                       ) return object->connect( object, SIGNAL( itemClicked( QTreeWidgetItem *, int )                             ), t_slots, SLOT( itemClicked( QTreeWidgetItem *, int )                              ), Qt::AutoConnection );
   if( signal == ( QString ) "itemDoubleClicked(QTWItem,int)"                 ) return object->connect( object, SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int )                       ), t_slots, SLOT( itemDoubleClicked( QTreeWidgetItem *, int )                        ), Qt::AutoConnection );
   if( signal == ( QString ) "itemEntered(QTWItem,int)"                       ) return object->connect( object, SIGNAL( itemEntered( QTreeWidgetItem *, int )                             ), t_slots, SLOT( itemEntered( QTreeWidgetItem *, int )                              ), Qt::AutoConnection );
   if( signal == ( QString ) "itemPressed(QTWItem,int)"                       ) return object->connect( object, SIGNAL( itemPressed( QTreeWidgetItem *, int )                             ), t_slots, SLOT( itemPressed( QTreeWidgetItem *, int )                              ), Qt::AutoConnection );
   if( signal == ( QString ) "itemSelectionChanged()"                         ) return object->connect( object, SIGNAL( itemSelectionChanged()                                            ), t_slots, SLOT( itemSelectionChanged()                                             ), Qt::AutoConnection );
   /* QListWidget */
   if( signal == ( QString ) "currentRowChanged(int)"                         ) return object->connect( object, SIGNAL( currentRowChanged( int )                                          ), t_slots, SLOT( currentRowChanged( int )                                           ), Qt::AutoConnection );
   if( signal == ( QString ) "currentTextChanged(QString)"                    ) return object->connect( object, SIGNAL( currentTextChanged( const QString & )                             ), t_slots, SLOT( currentTextChanged( const QString & )                              ), Qt::AutoConnection );
   if( signal == ( QString ) "currentItemChanged(QLWItem,QLWItem)"            ) return object->connect( object, SIGNAL( currentItemChanged( QListWidgetItem *, QListWidgetItem * )        ), t_slots, SLOT( currentItemChanged( QListWidgetItem *, QListWidgetItem * )         ), Qt::AutoConnection );
   if( signal == ( QString ) "itemActivated(QLWItem)"                         ) return object->connect( object, SIGNAL( itemActivated( QListWidgetItem * )                                ), t_slots, SLOT( itemActivated( QListWidgetItem * )                                 ), Qt::AutoConnection );
   if( signal == ( QString ) "itemChanged(QLWItem)"                           ) return object->connect( object, SIGNAL( itemChanged( QListWidgetItem * )                                  ), t_slots, SLOT( itemChanged( QListWidgetItem * )                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "itemClicked(QLWItem)"                           ) return object->connect( object, SIGNAL( itemClicked( QListWidgetItem * )                                  ), t_slots, SLOT( itemClicked( QListWidgetItem * )                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "itemDoubleClicked(QLWItem)"                     ) return object->connect( object, SIGNAL( itemDoubleClicked( QListWidgetItem * )                            ), t_slots, SLOT( itemDoubleClicked( QListWidgetItem * )                             ), Qt::AutoConnection );
   if( signal == ( QString ) "itemEntered(QLWItem)"                           ) return object->connect( object, SIGNAL( itemEntered( QListWidgetItem * )                                  ), t_slots, SLOT( itemEntered( QListWidgetItem * )                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "itemPressed(QLWItem)"                           ) return object->connect( object, SIGNAL( itemPressed( QListWidgetItem * )                                  ), t_slots, SLOT( itemPressed( QListWidgetItem * )                                   ), Qt::AutoConnection );
   /* QTableWidget */
   if( signal == ( QString ) "cellActivated(int,in)"                          ) return object->connect( object, SIGNAL( cellActivated( int, int )                                         ), t_slots, SLOT( cellActivated( int, int )                                          ), Qt::AutoConnection );
   if( signal == ( QString ) "cellChanged(int,int)"                           ) return object->connect( object, SIGNAL( cellChanged( int, int )                                           ), t_slots, SLOT( cellChanged( int, int )                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "cellClicked(int,int)"                           ) return object->connect( object, SIGNAL( cellClicked( int, int )                                           ), t_slots, SLOT( cellClicked( int, int )                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "cellDoubleClicked(int,int)"                     ) return object->connect( object, SIGNAL( cellDoubleClicked( int, int )                                     ), t_slots, SLOT( cellDoubleClicked( int, int )                                      ), Qt::AutoConnection );
   if( signal == ( QString ) "cellEntered(int,int)"                           ) return object->connect( object, SIGNAL( cellEntered( int, int )                                           ), t_slots, SLOT( cellEntered( int, int )                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "cellPressed(int,int)"                           ) return object->connect( object, SIGNAL( cellPressed( int, int )                                           ), t_slots, SLOT( cellPressed( int, int )                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "currentCellChanged(int,int,int,int)"            ) return object->connect( object, SIGNAL( currentCellChanged( int, int, int, int )                          ), t_slots, SLOT( currentCellChanged( int, int, int, int )                           ), Qt::AutoConnection );
   if( signal == ( QString ) "currentItemChanged(QTblWItem,QLWItem)"          ) return object->connect( object, SIGNAL( currentItemChanged( QTableWidgetItem *, QTableWidgetItem * )      ), t_slots, SLOT( currentItemChanged( QTableWidgetItem *, QTableWidgetItem * )       ), Qt::AutoConnection );
   if( signal == ( QString ) "itemActivated(QTblWItem)"                       ) return object->connect( object, SIGNAL( itemActivated( QTableWidgetItem * )                               ), t_slots, SLOT( itemActivated( QTableWidgetItem * )                                ), Qt::AutoConnection );
   if( signal == ( QString ) "itemChanged(QTblWItem)"                         ) return object->connect( object, SIGNAL( itemChanged( QTableWidgetItem * )                                 ), t_slots, SLOT( itemChanged( QTableWidgetItem * )                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "itemClicked(QTblWItem)"                         ) return object->connect( object, SIGNAL( itemClicked( QTableWidgetItem * )                                 ), t_slots, SLOT( itemClicked( QTableWidgetItem * )                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "itemDoubleClicked(QTblWItem)"                   ) return object->connect( object, SIGNAL( itemDoubleClicked( QTableWidgetItem * )                           ), t_slots, SLOT( itemDoubleClicked( QTableWidgetItem * )                            ), Qt::AutoConnection );
   if( signal == ( QString ) "itemEntered(QTblWItem)"                         ) return object->connect( object, SIGNAL( itemEntered( QTableWidgetItem * )                                 ), t_slots, SLOT( itemEntered( QTableWidgetItem * )                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "itemPressed(QTblWItem)"                         ) return object->connect( object, SIGNAL( itemPressed( QTableWidgetItem * )                                 ), t_slots, SLOT( itemPressed( QTableWidgetItem * )                                  ), Qt::AutoConnection );
   /* QDialog (s) QFontDialog, QFileDialog */
   if( signal == ( QString ) "currentFontChanged(QFont)"                      ) return object->connect( object, SIGNAL( currentFontChanged( const QFont & )                               ), t_slots, SLOT( currentFontChanged( const QFont & )                                ), Qt::AutoConnection );
   if( signal == ( QString ) "fontSelected(QFont)"                            ) return object->connect( object, SIGNAL( fontSelected( const QFont & )                                     ), t_slots, SLOT( fontSelected( const QFont & )                                      ), Qt::AutoConnection );
   if( signal == ( QString ) "accepted()"                                     ) return object->connect( object, SIGNAL( accepted()                                                        ), t_slots, SLOT( accepted()                                                         ), Qt::AutoConnection );
   if( signal == ( QString ) "finished(int)"                                  ) return object->connect( object, SIGNAL( finished( int )                                                   ), t_slots, SLOT( finished( int )                                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "rejected()"                                     ) return object->connect( object, SIGNAL( rejected()                                                        ), t_slots, SLOT( rejected()                                                         ), Qt::AutoConnection );
   if( signal == ( QString ) "currentChanged(QString)"                        ) return object->connect( object, SIGNAL( currentChanged( const QString & )                                 ), t_slots, SLOT( currentChanged( const QString & )                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "directoryEntered(QString)"                      ) return object->connect( object, SIGNAL( directoryEntered( const QString & )                               ), t_slots, SLOT( directoryEntered( const QString & )                                ), Qt::AutoConnection );
   if( signal == ( QString ) "fileSelected(QString)"                          ) return object->connect( object, SIGNAL( fileSelected( const QString & )                                   ), t_slots, SLOT( fileSelected( const QString & )                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "filesSelected(QStringList)"                     ) return object->connect( object, SIGNAL( filesSelected( const QStringList & )                              ), t_slots, SLOT( filesSelected( const QStringList & )                               ), Qt::AutoConnection );
   if( signal == ( QString ) "filterSelected(QString)"                        ) return object->connect( object, SIGNAL( filterSelected( const QString & )                                 ), t_slots, SLOT( filterSelected( const QString & )                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "accepted(QPrinter)"                             ) return object->connect( object, SIGNAL( accepted( QPrinter * )                                            ), t_slots, SLOT( accepted( QPrinter * )                                             ), Qt::AutoConnection );
   if( signal == ( QString ) "copyAvailable(bool)"                            ) return object->connect( object, SIGNAL( copyAvailable( bool )                                             ), t_slots, SLOT( copyAvailable( bool )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "currentCharFormatChanged(QTextCharFormat)"      ) return object->connect( object, SIGNAL( currentCharFormatChanged( const QTextCharFormat & )               ), t_slots, SLOT( currentCharFormatChanged( const QTextCharFormat & )                ), Qt::AutoConnection );
   if( signal == ( QString ) "cursorPositionChanged()"                        ) return object->connect( object, SIGNAL( cursorPositionChanged()                                           ), t_slots, SLOT( cursorPositionChanged()                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "redoAvailable(bool)"                            ) return object->connect( object, SIGNAL( redoAvailable( bool )                                             ), t_slots, SLOT( redoAvailable( bool )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "textChanged()"                                  ) return object->connect( object, SIGNAL( textChanged()                                                     ), t_slots, SLOT( textChanged()                                                      ), Qt::AutoConnection );
   if( signal == ( QString ) "undoAvailable(bool)"                            ) return object->connect( object, SIGNAL( undoAvailable( bool )                                             ), t_slots, SLOT( undoAvailable( bool )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "timeout()"                                      ) return object->connect( object, SIGNAL( timeout()                                                         ), t_slots, SLOT( timeout()                                                          ), Qt::AutoConnection );
   /* Generic purpose mechanism to receive key and mouse events off subclasses */
   if( signal == ( QString ) "geometriesChanged()"                            ) return object->connect( object, SIGNAL( geometriesChanged()                                               ), t_slots, SLOT( geometriesChanged()                                                ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionAutoResize(int,int)"                     ) return object->connect( object, SIGNAL( sectionAutoResize( int, QHeaderView::ResizeMode )                 ), t_slots, SLOT( sectionAutoResize( int, QHeaderView::ResizeMode )                  ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionClicked(int)"                            ) return object->connect( object, SIGNAL( sectionClicked( int )                                             ), t_slots, SLOT( sectionClicked( int )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionCountChanged(int,int)"                   ) return object->connect( object, SIGNAL( sectionCountChanged( int, int )                                   ), t_slots, SLOT( sectionCountChanged( int, int )                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionDoubleClicked(int)"                      ) return object->connect( object, SIGNAL( sectionDoubleClicked( int )                                       ), t_slots, SLOT( sectionDoubleClicked( int )                                        ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionEntered(int)"                            ) return object->connect( object, SIGNAL( sectionEntered( int )                                             ), t_slots, SLOT( sectionEntered( int )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionHandleDoubleClicked(int)"                ) return object->connect( object, SIGNAL( sectionHandleDoubleClicked( int )                                 ), t_slots, SLOT( sectionHandleDoubleClicked( int )                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionMoved(int,int,int)"                      ) return object->connect( object, SIGNAL( sectionMoved( int, int, int )                                     ), t_slots, SLOT( sectionMoved( int, int, int )                                      ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionPressed(int)"                            ) return object->connect( object, SIGNAL( sectionPressed( int )                                             ), t_slots, SLOT( sectionPressed( int )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "sectionResized(int,int,int)"                    ) return object->connect( object, SIGNAL( sectionResized( int, int, int )                                   ), t_slots, SLOT( sectionResized( int, int, int )                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "sortIndicatorChanged(int,int)"                  ) return object->connect( object, SIGNAL( sortIndicatorChanged( int, Qt::SortOrder )                        ), t_slots, SLOT( sortIndicatorChanged( int, Qt::SortOrder )                         ), Qt::AutoConnection );
   if( signal == ( QString ) "buttonClicked(int)"                             ) return object->connect( object, SIGNAL( buttonClicked( int )                                              ), t_slots, SLOT( buttonClicked( int )                                               ), Qt::AutoConnection );
   if( signal == ( QString ) "buttonPressed(int)"                             ) return object->connect( object, SIGNAL( buttonPressed( int )                                              ), t_slots, SLOT( buttonPressed( int )                                               ), Qt::AutoConnection );
   if( signal == ( QString ) "buttonReleased(int)"                            ) return object->connect( object, SIGNAL( buttonReleased( int )                                             ), t_slots, SLOT( buttonReleased( int )                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "linkActivated(QString)"                         ) return object->connect( object, SIGNAL( linkActivated( const QString & )                                  ), t_slots, SLOT( linkActivated( const QString & )                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "linkHovered(QString)"                           ) return object->connect( object, SIGNAL( linkHovered( const QString & )                                    ), t_slots, SLOT( linkHovered( const QString & )                                     ), Qt::AutoConnection );
   if( signal == ( QString ) "tabCloseRequested(int)"                         ) return object->connect( object, SIGNAL( tabCloseRequested( int )                                          ), t_slots, SLOT( tabCloseRequested( int )                                           ), Qt::AutoConnection );
   if( signal == ( QString ) "paintRequested(QPrinter)"                       ) return object->connect( object, SIGNAL( paintRequested( QPrinter * )                                      ), t_slots, SLOT( paintRequested( QPrinter * )                                       ), Qt::AutoConnection );
   /* QIODevice & QProcess */
   if( signal == ( QString ) "aboutToClose()"                                 ) return object->connect( object, SIGNAL( aboutToClose()                                                    ), t_slots, SLOT( aboutToClose()                                                     ), Qt::AutoConnection );
   if( signal == ( QString ) "bytesWritten(int)"                              ) return object->connect( object, SIGNAL( bytesWritten( qint64 )                                            ), t_slots, SLOT( bytesWritten( qint64 )                                             ), Qt::AutoConnection );
   if( signal == ( QString ) "readChannelFinished()"                          ) return object->connect( object, SIGNAL( readChannelFinished()                                             ), t_slots, SLOT( readChannelFinished()                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "readyRead()"                                    ) return object->connect( object, SIGNAL( readyRead()                                                       ), t_slots, SLOT( readyRead()                                                        ), Qt::AutoConnection );
   if( signal == ( QString ) "error(int)"                                     ) return object->connect( object, SIGNAL( error( int )                                                      ), t_slots, SLOT( error( int )                                                       ), Qt::AutoConnection );
   if( signal == ( QString ) "finished(int,int)"                              ) return object->connect( object, SIGNAL( finished( int, QProcess::ExitStatus )                             ), t_slots, SLOT( finished( int, QProcess::ExitStatus )                              ), Qt::AutoConnection );
   if( signal == ( QString ) "readyReadStandardError()"                       ) return object->connect( object, SIGNAL( readyReadStandardError()                                          ), t_slots, SLOT( readyReadStandardError()                                           ), Qt::AutoConnection );
   if( signal == ( QString ) "readyReadStandardOutput()"                      ) return object->connect( object, SIGNAL( readyReadStandardOutput()                                         ), t_slots, SLOT( readyReadStandardOutput()                                          ), Qt::AutoConnection );
   if( signal == ( QString ) "started()"                                      ) return object->connect( object, SIGNAL( started()                                                         ), t_slots, SLOT( started()                                                          ), Qt::AutoConnection );
   if( signal == ( QString ) "stateChanged(int)"                              ) return object->connect( object, SIGNAL( stateChanged( int )                                               ), t_slots, SLOT( stateChanged( int )                                                ), Qt::AutoConnection );
   /* QComboBox */
   if( signal == ( QString ) "activated(QString)"                             ) return object->connect( object, SIGNAL( activated( const QString & )                                      ), t_slots, SLOT( activated( const QString & )                                       ), Qt::AutoConnection );
   if( signal == ( QString ) "currentIndexChanged(QString)"                   ) return object->connect( object, SIGNAL( currentIndexChanged( const QString & )                            ), t_slots, SLOT( currentIndexChanged( const QString & )                             ), Qt::AutoConnection );
   if( signal == ( QString ) "editTextChanged(QString)"                       ) return object->connect( object, SIGNAL( editTextChanged( const QString & )                                ), t_slots, SLOT( editTextChanged( const QString & )                                 ), Qt::AutoConnection );
   if( signal == ( QString ) "highlighted(QString)"                           ) return object->connect( object, SIGNAL( highlighted( const QString & )                                    ), t_slots, SLOT( highlighted( const QString & )                                     ), Qt::AutoConnection );
   /* QTextDocument */
   if( signal == ( QString ) "blockCountChanged(int)"                         ) return object->connect( object, SIGNAL( blockCountChanged( int )                                          ), t_slots, SLOT( blockCountChanged( int )                                           ), Qt::AutoConnection );
   if( signal == ( QString ) "contentsChange(int,int,int)"                    ) return object->connect( object, SIGNAL( contentsChange( int, int, int )                                   ), t_slots, SLOT( contentsChange( int, int, int )                                    ), Qt::AutoConnection );
   if( signal == ( QString ) "contentsChanged()"                              ) return object->connect( object, SIGNAL( contentsChanged()                                                 ), t_slots, SLOT( contentsChanged()                                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "cursorPositionChanged(QTextCursor)"             ) return object->connect( object, SIGNAL( cursorPositionChanged( const QTextCursor & )                      ), t_slots, SLOT( cursorPositionChanged( const QTextCursor & )                       ), Qt::AutoConnection );
   if( signal == ( QString ) "documentLayoutChanged()"                        ) return object->connect( object, SIGNAL( documentLayoutChanged()                                           ), t_slots, SLOT( documentLayoutChanged()                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "modificationChanged(bool)"                      ) return object->connect( object, SIGNAL( modificationChanged( bool )                                       ), t_slots, SLOT( modificationChanged( bool )                                        ), Qt::AutoConnection );
   if( signal == ( QString ) "undoCommandAdded()"                             ) return object->connect( object, SIGNAL( undoCommandAdded()                                                ), t_slots, SLOT( undoCommandAdded()                                                 ), Qt::AutoConnection );
   /* QPlainTextEdit */
   if( signal == ( QString ) "updateRequest(QRect,int)"                       ) return object->connect( object, SIGNAL( updateRequest( const QRect &, int )                               ), t_slots, SLOT( updateRequest( const QRect &, int )                                ), Qt::AutoConnection );
   /* QItemSelectionModel */
   if( signal == ( QString ) "currentChanged(QModelIndex,QModelIndex)"        ) return object->connect( object, SIGNAL( currentChanged( const QModelIndex &,const QModelIndex & )         ), t_slots, SLOT( currentChanged( const QModelIndex &, const QModelIndex & )         ), Qt::AutoConnection );
   if( signal == ( QString ) "currentColumnChanged(QModelIndex,QModelIndex)"  ) return object->connect( object, SIGNAL( currentColumnChanged( const QModelIndex &,const QModelIndex & )   ), t_slots, SLOT( currentColumnChanged( const QModelIndex &, const QModelIndex & )   ), Qt::AutoConnection );
   if( signal == ( QString ) "currentRowChanged(QModelIndex,QModelIndex)"     ) return object->connect( object, SIGNAL( currentRowChanged( const QModelIndex &,const QModelIndex & )      ), t_slots, SLOT( currentRowChanged( const QModelIndex &, const QModelIndex & )      ), Qt::AutoConnection );
   if( signal == ( QString ) "selectionChanged(QItemSelection,QItemSelection)") return object->connect( object, SIGNAL( selectionChanged( const QItemSelection &, const QItemSelection & )), t_slots, SLOT( selectionChanged( const QItemSelection &, const QItemSelection &)  ), Qt::AutoConnection );
   /* QTextBrowser */
   if( signal == ( QString ) "anchorClicked(QUrl)"                            ) return object->connect( object, SIGNAL( anchorClicked( const QUrl & )                                     ), t_slots, SLOT( anchorClicked( const QUrl & )                                      ), Qt::AutoConnection );
   if( signal == ( QString ) "backwardAvailable(bool)"                        ) return object->connect( object, SIGNAL( backwardAvailable( bool )                                         ), t_slots, SLOT( backwardAvailable( bool )                                          ), Qt::AutoConnection );
   if( signal == ( QString ) "forwardAvailable(bool)"                         ) return object->connect( object, SIGNAL( forwardAvailable( bool )                                          ), t_slots, SLOT( forwardAvailable( bool )                                           ), Qt::AutoConnection );
   if( signal == ( QString ) "highlighted(QUrl)"                              ) return object->connect( object, SIGNAL( highlighted( const QUrl & )                                       ), t_slots, SLOT( highlighted( const QUrl & )                                        ), Qt::AutoConnection );
   if( signal == ( QString ) "historyChanged()"                               ) return object->connect( object, SIGNAL( historyChanged()                                                  ), t_slots, SLOT( historyChanged()                                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "sourceChanged(QUrl)"                            ) return object->connect( object, SIGNAL( sourceChanged( const QUrl & )                                     ), t_slots, SLOT( sourceChanged( const QUrl & )                                      ), Qt::AutoConnection );
   /* QDockWidget */
   if( signal == ( QString ) "allowedAreasChanged(int)"                       ) return object->connect( object, SIGNAL( allowedAreasChanged( Qt::DockWidgetAreas )                        ), t_slots, SLOT( allowedAreasChanged( Qt::DockWidgetAreas )                         ), Qt::AutoConnection );
   if( signal == ( QString ) "dockLocationChanged(int)"                       ) return object->connect( object, SIGNAL( dockLocationChanged( Qt::DockWidgetArea )                         ), t_slots, SLOT( dockLocationChanged( Qt::DockWidgetArea )                          ), Qt::AutoConnection );
   if( signal == ( QString ) "featuresChanged(int)"                           ) return object->connect( object, SIGNAL( featuresChanged( QDockWidget::DockWidgetFeatures )                ), t_slots, SLOT( featuresChanged( QDockWidget::DockWidgetFeatures )                 ), Qt::AutoConnection );
   if( signal == ( QString ) "topLevelChanged(bool)"                          ) return object->connect( object, SIGNAL( topLevelChanged( bool )                                           ), t_slots, SLOT( topLevelChanged( bool )                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "visibilityChanged(bool)"                        ) return object->connect( object, SIGNAL( visibilityChanged( bool )                                         ), t_slots, SLOT( visibilityChanged( bool )                                          ), Qt::AutoConnection );
   /* QCompleter */
   if( signal == ( QString ) "activated(QModelIndex)"                         ) return object->connect( object, SIGNAL( activated( const QModelIndex & )                                  ), t_slots, SLOT( activated( const QModelIndex & )                                   ), Qt::AutoConnection );
   if( signal == ( QString ) "highlighted(QModelIndex)"                       ) return object->connect( object, SIGNAL( highlighted( const QModelIndex & )                                ), t_slots, SLOT( highlighted( const QModelIndex & )                                 ), Qt::AutoConnection );
   /* QAbstractButton */
   if( signal == ( QString ) "toggled(bool)"                                  ) return object->connect( object, SIGNAL( toggled( bool )                                                   ), t_slots, SLOT( toggled( bool )                                                    ), Qt::AutoConnection );
   /* QSystemTrayIcon */
   if( signal == ( QString ) "activated(QSystemTrayIcon::ActivationReason)"   ) return object->connect( object, SIGNAL( activated( QSystemTrayIcon::ActivationReason )                    ), t_slots, SLOT( activated( QSystemTrayIcon::ActivationReason )                     ), Qt::AutoConnection );
   /* QMdiArea */
   if( signal == ( QString ) "subWindowActivated(QMdiSubWindow)"              ) return object->connect( object, SIGNAL( subWindowActivated( QMdiSubWindow * )                             ), t_slots, SLOT( subWindowActivated( QMdiSubWindow * )                              ), Qt::AutoConnection );
   /* QMdiSubWindow */
   if( signal == ( QString ) "aboutToActivate()"                              ) return object->connect( object, SIGNAL( aboutToActivate()                                                 ), t_slots, SLOT( aboutToActivate()                                                  ), Qt::AutoConnection );
   if( signal == ( QString ) "windowStateChanged(Qt::WindowStates,Qt::WindowStates)" ) return object->connect( object, SIGNAL( windowStateChanged( Qt::WindowStates, Qt::WindowStates )   ), t_slots, SLOT( windowStateChanged( Qt::WindowStates, Qt::WindowStates )           ), Qt::AutoConnection );
   /* QAbstractItemDelegate */
   if( signal == ( QString ) "closeEditor(QWidget,int)"                       ) return object->connect( object, SIGNAL( closeEditor( QWidget *, QAbstractItemDelegate::EndEditHint )      ), t_slots, SLOT( closeEditor( QWidget *, QAbstractItemDelegate::EndEditHint )       ), Qt::AutoConnection );
   if( signal == ( QString ) "commitData(QWidget)"                            ) return object->connect( object, SIGNAL( commitData( QWidget * )                                           ), t_slots, SLOT( commitData( QWidget * )                                            ), Qt::AutoConnection );
   if( signal == ( QString ) "sizeHintChanged(QModelIndex)"                   ) return object->connect( object, SIGNAL( sizeHintChanged( const QModelIndex & )                            ), t_slots, SLOT( sizeHintChanged( const QModelIndex & )                             ), Qt::AutoConnection );
   /* QGraphicsScene */
   if( signal == ( QString ) "sceneRectChanged(QRectF)"                       ) return object->connect( object, SIGNAL( sceneRectChanged( const QRectF & )                                ), t_slots, SLOT( sizeHintChanged( const QModelIndex & )                             ), Qt::AutoConnection );
   /* QDateTimeEdit */
   if( signal == ( QString ) "dateChanged(QDate)"                             ) return object->connect( object, SIGNAL( dateChanged( const QDate & )                                      ), t_slots, SLOT( dateChanged( const QDate & )                                       ), Qt::AutoConnection );
   if( signal == ( QString ) "dateTimeChanged(QDateTime)"                     ) return object->connect( object, SIGNAL( dateTimeChanged( const QDateTime & )                              ), t_slots, SLOT( dateTimeChanged( const QDateTime & )                               ), Qt::AutoConnection );
   if( signal == ( QString ) "timeChanged(QTime)"                             ) return object->connect( object, SIGNAL( timeChanged( const QTime & )                                      ), t_slots, SLOT( timeChanged( const QTime & )                                       ), Qt::AutoConnection );
   /* QApplication */
// if( signal == ( QString ) "commitDataRequest(QSessionManager)"             ) return object->connect( object, SIGNAL( commitDataRequest( QSessionManager & )                            ), t_slots, SLOT( commitDataRequest( QSessionManager & )                             ), Qt::AutoConnection );
   if( signal == ( QString ) "focusChanged(QWidget,QWidget)"                  ) return object->connect( object, SIGNAL( focusChanged( QWidget *, QWidget * )                              ), t_slots, SLOT( focusChanged( QWidget *, QWidget * )                               ), Qt::AutoConnection );
   if( signal == ( QString ) "fontDatabaseChanged()"                          ) return object->connect( object, SIGNAL( fontDatabaseChanged()                                             ), t_slots, SLOT( fontDatabaseChanged()                                              ), Qt::AutoConnection );
   if( signal == ( QString ) "lastWindowClosed()"                             ) return object->connect( object, SIGNAL( lastWindowClosed()                                                ), t_slots, SLOT( lastWindowClosed()                                                 ), Qt::AutoConnection );
// if( signal == ( QString ) "saveStateRequest(QSessionManager)"              ) return object->connect( object, SIGNAL( saveStateRequest( QSessionManager & )                             ), t_slots, SLOT( saveStateRequest( QSessionManager & )                              ), Qt::AutoConnection );
   /* New */
   return false;
}

static bool disconnect_signal( QObject * object, const char * signal )
{
   if( signal == ( QString ) "customContextMenuRequested(QPoint)"             ) return object->disconnect( SIGNAL( customContextMenuRequested( const QPoint & )                      ) );
   if( signal == ( QString ) "clicked()"                                      ) return object->disconnect( SIGNAL( clicked()                                                         ) );
   if( signal == ( QString ) "returnPressed()"                                ) return object->disconnect( SIGNAL( returnPressed()                                                   ) );
   if( signal == ( QString ) "triggered()"                                    ) return object->disconnect( SIGNAL( triggered()                                                       ) );
   if( signal == ( QString ) "hovered()"                                      ) return object->disconnect( SIGNAL( hovered()                                                         ) );
   if( signal == ( QString ) "viewportEntered()"                              ) return object->disconnect( SIGNAL( viewportEntered()                                                 ) );
   if( signal == ( QString ) "pressed()"                                      ) return object->disconnect( SIGNAL( pressed()                                                         ) );
   if( signal == ( QString ) "released()"                                     ) return object->disconnect( SIGNAL( released()                                                        ) );
   if( signal == ( QString ) "stateChanged(int)"                              ) return object->disconnect( SIGNAL( stateChanged( int )                                               ) );
   if( signal == ( QString ) "activated(int)"                                 ) return object->disconnect( SIGNAL( activated( int )                                                  ) );
   if( signal == ( QString ) "currentIndexChanged(int)"                       ) return object->disconnect( SIGNAL( currentIndexChanged( int )                                        ) );
   if( signal == ( QString ) "highlighted(int)"                               ) return object->disconnect( SIGNAL( highlighted( int )                                                ) );
   if( signal == ( QString ) "triggered(bool)"                                ) return object->disconnect( SIGNAL( triggered( bool )                                                 ) );
   if( signal == ( QString ) "clicked(QModelIndex)"                           ) return object->disconnect( SIGNAL( clicked( const QModelIndex & )                                    ) );
   if( signal == ( QString ) "doubleClicked(QModelIndex)"                     ) return object->disconnect( SIGNAL( doubleClicked( const QModelIndex & )                              ) );
   if( signal == ( QString ) "entered(QModelIndex)"                           ) return object->disconnect( SIGNAL( entered( const QModelIndex & )                                    ) );
   if( signal == ( QString ) "hovered(action)"                                ) return object->disconnect( SIGNAL( hovered( QAction * )                                              ) );
   if( signal == ( QString ) "currentChanged(int)"                            ) return object->disconnect( SIGNAL( currentChanged( int )                                             ) );
   if( signal == ( QString ) "actionTriggered(int)"                           ) return object->disconnect( SIGNAL( actionTriggered(int)                                              ) );
   if( signal == ( QString ) "rangeChanged(int,int)"                          ) return object->disconnect( SIGNAL( rangeChanged(int,int)                                             ) );
   if( signal == ( QString ) "sliderMoved(int)"                               ) return object->disconnect( SIGNAL( sliderMoved(int)                                                  ) );
   if( signal == ( QString ) "sliderPressed()"                                ) return object->disconnect( SIGNAL( sliderPressed()                                                   ) );
   if( signal == ( QString ) "sliderReleased()"                               ) return object->disconnect( SIGNAL( sliderReleased()                                                  ) );
   if( signal == ( QString ) "valueChanged(int)"                              ) return object->disconnect( SIGNAL( valueChanged(int)                                                 ) );
   if( signal == ( QString ) "cursorPositionChanged(int,int)"                 ) return object->disconnect( SIGNAL( cursorPositionChanged(int,int)                                    ) );
   if( signal == ( QString ) "editingFinished()"                              ) return object->disconnect( SIGNAL( editingFinished()                                                 ) );
   if( signal == ( QString ) "returnPressed()"                                ) return object->disconnect( SIGNAL( returnPressed()                                                   ) );
   if( signal == ( QString ) "selectionChanged()"                             ) return object->disconnect( SIGNAL( selectionChanged()                                                ) );
   if( signal == ( QString ) "textChanged(QString)"                           ) return object->disconnect( SIGNAL( textChanged( const QString &)                                     ) );
   if( signal == ( QString ) "textEdited(QString)"                            ) return object->disconnect( SIGNAL( textEdited( const QString &)                                      ) );
   /* QTreeWidget */
   if( signal == ( QString ) "itemExpanded(QTWItem)"                          ) return object->disconnect( SIGNAL( itemExpanded( QTreeWidgetItem * )                                 ) );
   if( signal == ( QString ) "itemCollapsed(QTWItem)"                         ) return object->disconnect( SIGNAL( itemCollapsed( QTreeWidgetItem * )                                ) );
   if( signal == ( QString ) "currentItemChanged(QTWItem,QTWItem)"            ) return object->disconnect( SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * )        ) );
   if( signal == ( QString ) "itemActivated(QTWItem,int)"                     ) return object->disconnect( SIGNAL( itemActivated( QTreeWidgetItem *, int )                           ) );
   if( signal == ( QString ) "itemChanged(QTWItem,int)"                       ) return object->disconnect( SIGNAL( itemChanged( QTreeWidgetItem *, int )                             ) );
   if( signal == ( QString ) "itemClicked(QTWItem,int)"                       ) return object->disconnect( SIGNAL( itemClicked( QTreeWidgetItem *, int )                             ) );
   if( signal == ( QString ) "itemDoubleClicked(QTWItem,int)"                 ) return object->disconnect( SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int )                       ) );
   if( signal == ( QString ) "itemEntered(QTWItem,int)"                       ) return object->disconnect( SIGNAL( itemEntered( QTreeWidgetItem *, int )                             ) );
   if( signal == ( QString ) "itemPressed(QTWItem,int)"                       ) return object->disconnect( SIGNAL( itemPressed( QTreeWidgetItem *, int )                             ) );
   if( signal == ( QString ) "itemSelectionChanged()"                         ) return object->disconnect( SIGNAL( itemSelectionChanged()                                            ) );
   /* QTableWidget */
   if( signal == ( QString ) "cellActivated(int,in)"                          ) return object->disconnect( SIGNAL( cellActivated( int, int )                                         ) );
   if( signal == ( QString ) "cellChanged(int,int)"                           ) return object->disconnect( SIGNAL( cellChanged( int, int )                                           ) );
   if( signal == ( QString ) "cellClicked(int,int)"                           ) return object->disconnect( SIGNAL( cellClicked( int, int )                                           ) );
   if( signal == ( QString ) "cellDoubleClicked(int,int)"                     ) return object->disconnect( SIGNAL( cellDoubleClicked( int, int )                                     ) );
   if( signal == ( QString ) "cellEntered(int,int)"                           ) return object->disconnect( SIGNAL( cellEntered( int, int )                                           ) );
   if( signal == ( QString ) "cellPressed(int,int)"                           ) return object->disconnect( SIGNAL( cellPressed( int, int )                                           ) );
   if( signal == ( QString ) "currentCellChanged(int,int,int,int)"            ) return object->disconnect( SIGNAL( currentCellChanged( int, int, int, int )                          ) );
   if( signal == ( QString ) "currentItemChanged(QTblWItem)"                  ) return object->disconnect( SIGNAL( currentItemChanged( QTableWidgetItem *, QTableWidgetItem * )      ) );
   if( signal == ( QString ) "itemActivated(QTblWItem)"                       ) return object->disconnect( SIGNAL( itemActivated( QTableWidgetItem * )                               ) );
   if( signal == ( QString ) "itemChanged(QTblWItem)"                         ) return object->disconnect( SIGNAL( itemChanged( QTableWidgetItem * )                                 ) );
   if( signal == ( QString ) "itemClicked(QTblWItem)"                         ) return object->disconnect( SIGNAL( itemClicked( QTableWidgetItem * )                                 ) );
   if( signal == ( QString ) "itemDoubleClicked(QTblWItem)"                   ) return object->disconnect( SIGNAL( itemDoubleClicked( QTableWidgetItem * )                           ) );
   if( signal == ( QString ) "itemEntered(QTblWItem)"                         ) return object->disconnect( SIGNAL( itemEntered( QTableWidgetItem * )                                 ) );
   if( signal == ( QString ) "itemPressed(QTblWItem)"                         ) return object->disconnect( SIGNAL( itemPressed( QTableWidgetItem * )                                 ) );
   /* */
   if( signal == ( QString ) "iconChanged()"                                  ) return object->disconnect( SIGNAL( iconChanged()                                                     ) );
   if( signal == ( QString ) "titleChanged(QString)"                          ) return object->disconnect( SIGNAL( titleChanged( const QString & )                                   ) );
   if( signal == ( QString ) "urlChanged(QUrl)"                               ) return object->disconnect( SIGNAL( urlChanged( const QUrl & )                                        ) );
   if( signal == ( QString ) "currentFontChanged(QFont)"                      ) return object->disconnect( SIGNAL( currentFontChanged( const QFont & )                               ) );
   if( signal == ( QString ) "fontSelected(QFont)"                            ) return object->disconnect( SIGNAL( fontSelected( const QFont & )                                     ) );
   if( signal == ( QString ) "accepted()"                                     ) return object->disconnect( SIGNAL( accepted()                                                        ) );
   if( signal == ( QString ) "finished(int)"                                  ) return object->disconnect( SIGNAL( finished( int )                                                   ) );
   if( signal == ( QString ) "rejected()"                                     ) return object->disconnect( SIGNAL( rejected()                                                        ) );
   if( signal == ( QString ) "currentChanged(QString)"                        ) return object->disconnect( SIGNAL( currentChanged( const QString & )                                 ) );
   if( signal == ( QString ) "directoryEntered(QString)"                      ) return object->disconnect( SIGNAL( directoryEntered( const QString & )                               ) );
   if( signal == ( QString ) "fileSelected(QString)"                          ) return object->disconnect( SIGNAL( fileSelected( const QString & )                                   ) );
   if( signal == ( QString ) "filesSelected(QStringList)"                     ) return object->disconnect( SIGNAL( filesSelected( const QStringList & )                              ) );
   if( signal == ( QString ) "filterSelected(QString)"                        ) return object->disconnect( SIGNAL( filterSelected( const QString & )                                 ) );
   if( signal == ( QString ) "accepted(QPrinter)"                             ) return object->disconnect( SIGNAL( accepted( QPrinter * )                                            ) );
   if( signal == ( QString ) "copyAvailable(bool)"                            ) return object->disconnect( SIGNAL( copyAvailable( bool )                                             ) );
   if( signal == ( QString ) "currentCharFormatChanged(QTextCharFormat)"      ) return object->disconnect( SIGNAL( currentCharFormatChanged( const QTextCharFormat & )               ) );
   if( signal == ( QString ) "cursorPositionChanged()"                        ) return object->disconnect( SIGNAL( cursorPositionChanged()                                           ) );
   if( signal == ( QString ) "redoAvailable(bool)"                            ) return object->disconnect( SIGNAL( redoAvailable( bool )                                             ) );
   if( signal == ( QString ) "textChanged()"                                  ) return object->disconnect( SIGNAL( textChanged()                                                     ) );
   if( signal == ( QString ) "undoAvailable(bool)"                            ) return object->disconnect( SIGNAL( undoAvailable( bool )                                             ) );
   if( signal == ( QString ) "timeout()"                                      ) return object->disconnect( SIGNAL( timeout()                                                         ) );
   if( signal == ( QString ) "geometriesChanged()"                            ) return object->disconnect( SIGNAL( geometriesChanged()                                               ) );
   if( signal == ( QString ) "sectionAutoResize(int,int)"                     ) return object->disconnect( SIGNAL( sectionAutoResize( int, QHeaderView::ResizeMode )                 ) );
   if( signal == ( QString ) "sectionClicked(int)"                            ) return object->disconnect( SIGNAL( sectionClicked( int )                                             ) );
   if( signal == ( QString ) "sectionCountChanged(int,int)"                   ) return object->disconnect( SIGNAL( sectionCountChanged( int, int )                                   ) );
   if( signal == ( QString ) "sectionDoubleClicked(int)"                      ) return object->disconnect( SIGNAL( sectionDoubleClicked( int )                                       ) );
   if( signal == ( QString ) "sectionEntered(int)"                            ) return object->disconnect( SIGNAL( sectionEntered( int )                                             ) );
   if( signal == ( QString ) "sectionHandleDoubleClicked(int)"                ) return object->disconnect( SIGNAL( sectionHandleDoubleClicked( int )                                 ) );
   if( signal == ( QString ) "sectionMoved(int,int,int)"                      ) return object->disconnect( SIGNAL( sectionMoved( int, int, int )                                     ) );
   if( signal == ( QString ) "sectionPressed(int)"                            ) return object->disconnect( SIGNAL( sectionPressed( int )                                             ) );
   if( signal == ( QString ) "sectionResized(int,int,int)"                    ) return object->disconnect( SIGNAL( sectionResized( int, int, int )                                   ) );
   if( signal == ( QString ) "sortIndicatorChanged(int,int)"                  ) return object->disconnect( SIGNAL( sortIndicatorChanged( int, Qt::SortOrder )                        ) );
   if( signal == ( QString ) "buttonClicked(int)"                             ) return object->disconnect( SIGNAL( buttonClicked( int )                                              ) );
   if( signal == ( QString ) "buttonPressed(int)"                             ) return object->disconnect( SIGNAL( buttonPressed( int )                                              ) );
   if( signal == ( QString ) "buttonReleased(int)"                            ) return object->disconnect( SIGNAL( buttonReleased( int )                                             ) );
   if( signal == ( QString ) "linkActivated(QString)"                         ) return object->disconnect( SIGNAL( linkActivated( const QString & )                                  ) );
   if( signal == ( QString ) "linkHovered(QString)"                           ) return object->disconnect( SIGNAL( linkHovered( const QString & )                                    ) );
   if( signal == ( QString ) "tabCloseRequested(int)"                         ) return object->disconnect( SIGNAL( tabCloseRequested( int )                                          ) );
   if( signal == ( QString ) "paintRequested(QPrinter)"                       ) return object->disconnect( SIGNAL( paintRequested( QPrinter * )                                      ) );
   /* QIODevice & QProcess */
   if( signal == ( QString ) "aboutToClose()"                                 ) return object->disconnect( SIGNAL( aboutToClose()                                                    ) );
   if( signal == ( QString ) "bytesWritten(int)"                              ) return object->disconnect( SIGNAL( bytesWritten( qint64 )                                            ) );
   if( signal == ( QString ) "readChannelFinished()"                          ) return object->disconnect( SIGNAL( readChannelFinished()                                             ) );
   if( signal == ( QString ) "readyRead()"                                    ) return object->disconnect( SIGNAL( readyRead()                                                       ) );
   if( signal == ( QString ) "error(int)"                                     ) return object->disconnect( SIGNAL( error( int )                                                      ) );
   if( signal == ( QString ) "finished(int,int)"                              ) return object->disconnect( SIGNAL( finished( int, QProcess::ExitStatus )                             ) );
   if( signal == ( QString ) "readyReadStandardError()"                       ) return object->disconnect( SIGNAL( readyReadStandardError()                                          ) );
   if( signal == ( QString ) "readyReadStandardOutput()"                      ) return object->disconnect( SIGNAL( readyReadStandardOutput()                                         ) );
   if( signal == ( QString ) "started()"                                      ) return object->disconnect( SIGNAL( started()                                                         ) );
   if( signal == ( QString ) "stateChanged(int)"                              ) return object->disconnect( SIGNAL( stateChanged( int )                                               ) );
   /* QComboBox */
   if( signal == ( QString ) "activated(QString)"                             ) return object->disconnect( SIGNAL( activated( const QString & )                                      ) );
   if( signal == ( QString ) "currentIndexChanged(QString)"                   ) return object->disconnect( SIGNAL( currentIndexChanged( const QString & )                            ) );
   if( signal == ( QString ) "editTextChanged(QString)"                       ) return object->disconnect( SIGNAL( editTextChanged( const QString & )                                ) );
   if( signal == ( QString ) "highlighted(QString)"                           ) return object->disconnect( SIGNAL( highlighted( const QString & )                                    ) );
   /* QTextDocument */
   if( signal == ( QString ) "blockCountChanged(int)"                         ) return object->disconnect( SIGNAL( blockCountChanged( int )                                          ) );
   if( signal == ( QString ) "contentsChange(int,int,int)"                    ) return object->disconnect( SIGNAL( contentsChange( int, int, int )                                   ) );
   if( signal == ( QString ) "contentsChanged()"                              ) return object->disconnect( SIGNAL( contentsChanged()                                                 ) );
   if( signal == ( QString ) "cursorPositionChanged(QTextCursor)"             ) return object->disconnect( SIGNAL( cursorPositionChanged( const QTextCursor & )                      ) );
   if( signal == ( QString ) "documentLayoutChanged()"                        ) return object->disconnect( SIGNAL( documentLayoutChanged()                                           ) );
   if( signal == ( QString ) "modificationChanged(bool)"                      ) return object->disconnect( SIGNAL( modificationChanged( bool )                                       ) );
   if( signal == ( QString ) "undoCommandAdded()"                             ) return object->disconnect( SIGNAL( undoCommandAdded()                                                ) );
   if( signal == ( QString ) "updateRequest(QRect,int)"                       ) return object->disconnect( SIGNAL( updateRequest( const QRect &, int )                               ) );
   /* QItemSelectionModel */
   if( signal == ( QString ) "currentChanged(QModelIndex,QModelIndex)"        ) return object->disconnect( SIGNAL( currentChanged( const QModelIndex &,const QModelIndex & )         ) );
   if( signal == ( QString ) "currentColumnChanged(QModelIndex,QModelIndex)"  ) return object->disconnect( SIGNAL( currentColumnChanged( const QModelIndex &,const QModelIndex & )   ) );
   if( signal == ( QString ) "currentRowChanged(QModelIndex,QModelIndex)"     ) return object->disconnect( SIGNAL( currentRowChanged( const QModelIndex &,const QModelIndex & )      ) );
   if( signal == ( QString ) "selectionChanged(QItemSelection,QItemSelection)") return object->disconnect( SIGNAL( selectionChanged( const QItemSelection &, const QItemSelection & )) );
   /* QListWidget */
   if( signal == ( QString ) "currentItemChanged(QLWItem,QLWItem)"            ) return object->disconnect( SIGNAL( currentItemChanged( QListWidgetItem *, QListWidgetItem * )        ) );
   if( signal == ( QString ) "currentRowChanged(int)"                         ) return object->disconnect( SIGNAL( currentRowChanged( int )                                          ) );
   if( signal == ( QString ) "currentTextChanged(QString)"                    ) return object->disconnect( SIGNAL( currentTextChanged( const QString & )                             ) );
   if( signal == ( QString ) "itemActivated(QLWItem)"                         ) return object->disconnect( SIGNAL( itemActivated( QListWidgetItem * )                                ) );
   if( signal == ( QString ) "itemChanged(QLWItem)"                           ) return object->disconnect( SIGNAL( itemChanged( QListWidgetItem * )                                  ) );
   if( signal == ( QString ) "itemClicked(QLWItem)"                           ) return object->disconnect( SIGNAL( itemClicked( QListWidgetItem * )                                  ) );
   if( signal == ( QString ) "itemDoubleClicked(QLWItem)"                     ) return object->disconnect( SIGNAL( itemDoubleClicked( QListWidgetItem * )                            ) );
   if( signal == ( QString ) "itemEntered(QLWItem)"                           ) return object->disconnect( SIGNAL( itemEntered( QListWidgetItem * )                                  ) );
   if( signal == ( QString ) "itemPressed(QLWItem)"                           ) return object->disconnect( SIGNAL( itemPressed( QListWidgetItem * )                                  ) );
   /* QTextBrowser */
   if( signal == ( QString ) "anchorClicked(QUrl)"                            ) return object->disconnect( SIGNAL( anchorClicked( const QUrl & )                                     ) );
   if( signal == ( QString ) "backwardAvailable(bool)"                        ) return object->disconnect( SIGNAL( backwardAvailable( bool )                                         ) );
   if( signal == ( QString ) "forwardAvailable(bool)"                         ) return object->disconnect( SIGNAL( forwardAvailable( bool )                                          ) );
   if( signal == ( QString ) "highlighted(QUrl)"                              ) return object->disconnect( SIGNAL( highlighted( const QUrl & )                                       ) );
   if( signal == ( QString ) "historyChanged()"                               ) return object->disconnect( SIGNAL( historyChanged()                                                  ) );
   if( signal == ( QString ) "sourceChanged(QUrl)"                            ) return object->disconnect( SIGNAL( sourceChanged( const QUrl & )                                     ) );
   /* QDockWidget */
   if( signal == ( QString ) "allowedAreasChanged(int)"                       ) return object->disconnect( SIGNAL( allowedAreasChanged( Qt::DockWidgetAreas )                        ) );
   if( signal == ( QString ) "dockLocationChanged(int)"                       ) return object->disconnect( SIGNAL( dockLocationChanged( Qt::DockWidgetArea )                         ) );
   if( signal == ( QString ) "featuresChanged(int)"                           ) return object->disconnect( SIGNAL( featuresChanged( QDockWidget::DockWidgetFeatures )                ) );
   if( signal == ( QString ) "topLevelChanged(bool)"                          ) return object->disconnect( SIGNAL( topLevelChanged( bool )                                           ) );
   if( signal == ( QString ) "visibilityChanged(bool)"                        ) return object->disconnect( SIGNAL( visibilityChanged( bool )                                         ) );
   /* QCompleter */
   if( signal == ( QString ) "activated(QModelIndex)"                         ) return object->disconnect( SIGNAL( activated( const QModelIndex & )                                  ) );
   if( signal == ( QString ) "highlighted(QModelIndex)"                       ) return object->disconnect( SIGNAL( highlighted( const QModelIndex & )                                ) );
   /* QAbstractButton */
   if( signal == ( QString ) "toggled(bool)"                                  ) return object->disconnect( SIGNAL( toggled( bool )                                                   ) );
   /* QSystemTrayIcon */
   if( signal == ( QString ) "activated(QSystemTrayIcon::ActivationReason)"   ) return object->disconnect( SIGNAL( activated( QSystemTrayIcon::ActivationReason )                    ) );
   /* QMdiArea */
   if( signal == ( QString ) "subWindowActivated(QMdiSubWindow)"              ) return object->disconnect( SIGNAL( subWindowActivated( QMdiSubWindow * )                             ) );
   /* QMdiSubWindow */
   if( signal == ( QString ) "aboutToActivate()"                              ) return object->disconnect( SIGNAL( aboutToActivate()                                                 ) );
   if( signal == ( QString ) "windowStateChanged(Qt::WindowStates,Qt::WindowStates)" ) return object->disconnect( SIGNAL( windowStateChanged( Qt::WindowStates, Qt::WindowStates )   ) );
   /* QAbstractItemDelegate */
   if( signal == ( QString ) "closeEditor(QWidget,int)"                       ) return object->disconnect( SIGNAL( closeEditor( QWidget *, QAbstractItemDelegate::EndEditHint )      ) );
   if( signal == ( QString ) "commitData(QWidget)"                            ) return object->disconnect( SIGNAL( commitData( QWidget * )                                           ) );
   if( signal == ( QString ) "sizeHintChanged(QModelIndex)"                   ) return object->disconnect( SIGNAL( sizeHintChanged( const QModelIndex & )                            ) );
   /* QGraphicsScene */
   if( signal == ( QString ) "sceneRectChanged(QRectF)"                       ) return object->disconnect( SIGNAL( sceneRectChanged( const QRectF & )                                ) );
   /* QDateTimeEdit */
   if( signal == ( QString ) "dateChanged(QDate)"                             ) return object->disconnect( SIGNAL( dateChanged( const QDate & )                                      ) );
   if( signal == ( QString ) "dateTimeChanged(QDateTime)"                     ) return object->disconnect( SIGNAL( dateTimeChanged( const QDateTime & )                              ) );
   if( signal == ( QString ) "timeChanged(QTime)"                             ) return object->disconnect( SIGNAL( timeChanged( const QTime & )                                      ) );
   /* QApplication */
// if( signal == ( QString ) "commitDataRequest(QSessionManager)"             ) return object->disconnect( SIGNAL( commitDataRequest( QSessionManager & )                            ) );
   if( signal == ( QString ) "focusChanged(QWidget,QWidget)"                  ) return object->disconnect( SIGNAL( focusChanged( QWidget *, QWidget * )                              ) );
   if( signal == ( QString ) "fontDatabaseChanged()"                          ) return object->disconnect( SIGNAL( fontDatabaseChanged()                                             ) );
   if( signal == ( QString ) "lastWindowClosed()"                             ) return object->disconnect( SIGNAL( lastWindowClosed()                                                ) );
// if( signal == ( QString ) "saveStateRequest(QSessionManager)"              ) return object->disconnect( SIGNAL( saveStateRequest( QSessionManager & )                             ) );
   /* new */

   return false;
}

/*----------------------------------------------------------------------*/

static void hbqt_SlotsExec( HBQSlots * t_slots, QObject * object, const char * pszEvent )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 0 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecBool( HBQSlots * t_slots, QObject * object, const char * pszEvent, bool bBool )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pBool = hb_itemPutL( NULL, bBool );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, pBool );
         hb_itemRelease( pBool );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecInt( HBQSlots * t_slots, QObject * object, const char * pszEvent, int iValue )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutNI( NULL, iValue );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntInt( HBQSlots * t_slots, QObject * object, const char * pszEvent, int iValue1, int iValue2 )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pValue1, pValue2 );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntIntInt( HBQSlots * t_slots, QObject * object, const char * pszEvent, int iValue1, int iValue2, int iValue3 )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutNI( NULL, iValue3 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 3, pValue1, pValue2, pValue3 );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntIntIntInt( HBQSlots * t_slots, QObject * object, const char * pszEvent, int iValue1, int iValue2, int iValue3, int iValue4 )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutNI( NULL, iValue3 );
         PHB_ITEM pValue4 = hb_itemPutNI( NULL, iValue4 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 4, pValue1, pValue2, pValue3, pValue4 );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
         hb_itemRelease( pValue4 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecString( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QString & string )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pString = hb_itemPutC( NULL, string.toAscii().data() );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, pString );
         hb_itemRelease( pString );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecModel( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QModelIndex & index )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pState = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, pState );
         delete ( ( QModelIndex * ) hb_itemGetPtr( pState ) );
         hb_itemRelease( pState );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecModelModel( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QModelIndex & index1, const QModelIndex & index2 )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pState1 = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index1 ) );
         PHB_ITEM pState2 = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index2 ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pState1, pState2 );
         delete ( ( QModelIndex * ) hb_itemGetPtr( pState1 ) );
         delete ( ( QModelIndex * ) hb_itemGetPtr( pState2 ) );
         hb_itemRelease( pState1 );
         hb_itemRelease( pState2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecItemSelItemSel( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QItemSelection & index1, const QItemSelection & index2 )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pState1 = hb_itemPutPtr( NULL, ( QItemSelection * ) new QItemSelection( index1 ) );
         PHB_ITEM pState2 = hb_itemPutPtr( NULL, ( QItemSelection * ) new QItemSelection( index2 ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, pState1, pState2 );
         delete ( ( QItemSelection * ) hb_itemGetPtr( pState1 ) );
         delete ( ( QItemSelection * ) hb_itemGetPtr( pState2 ) );
         hb_itemRelease( pState1 );
         hb_itemRelease( pState2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecTextCharFormat( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QTextCharFormat & f )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTextCharFormat( f ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QTextCharFormat * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecFont( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QFont & font )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QFont( font ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QFont * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQTextCursor( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QTextCursor & cursor )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTextCursor( cursor ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QTextCursor * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecStringList( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QStringList & stringList )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QStringList( stringList ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QStringList * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointer( HBQSlots * t_slots, QObject * object, const char * pszEvent, void * p1 )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, pP1 );
         hb_itemRelease( pP1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointerInt( HBQSlots * t_slots, QObject * object, const char * pszEvent, void * pP1, int iInt )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, pP1 );
         PHB_ITEM p2 = hb_itemPutNI( NULL, iInt );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, p1, p2 );
         hb_itemRelease( p1 );
         hb_itemRelease( p2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointerPointer( HBQSlots * t_slots, QObject * object, const char * pszEvent, void * pP1, void * pP2 )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, pP1 );
         PHB_ITEM p2 = hb_itemPutPtr( NULL, pP2 );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, p1, p2 );
         hb_itemRelease( p1 );
         hb_itemRelease( p2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQRectInt( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QRect & r, int dy )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QRect( r ) );
         PHB_ITEM p2 = hb_itemPutNI( NULL, dy );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 2, p1, p2 );
         delete ( ( QRect * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_itemRelease( p2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQPoint( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QPoint & pos )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QPoint( pos ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QPoint * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQRectF( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QRectF & rect )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QRectF( rect ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QRectF * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQUrl( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QUrl & link )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, ( QUrl * ) new QUrl( link ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QUrl * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQDate( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QDate & date )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, ( QDate * ) new QDate( date ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QDate * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQDateTime( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QDateTime & datetime )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, ( QDateTime * ) new QDateTime( datetime ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QDateTime * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecQTime( HBQSlots * t_slots, QObject * object, const char * pszEvent, const QTime & time )
{
   if( object )
   {
      int i = object->property( pszEvent ).toInt();
      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM p1 = hb_itemPutPtr( NULL, ( QTime * ) new QTime( time ) );
         hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 1, p1 );
         delete ( ( QTime * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}


HBQSlots::HBQSlots( QObject* parent ) : QObject( parent )
{
}

HBQSlots::~HBQSlots()
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
//HB_TRACE( HB_TR_DEBUG, ( "......HBQSlots::~HBQSlots()...... [ Un-disConnected Slot ]" ) );
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   /* QUESTION: Should there be all remaining active slots disconnected at this point? */

   /*           Should be disconnected, but this is a responsibility of programmer as object is only known to the application */
   listBlock.clear();
}

bool HBQSlots::hbIsConnected( PHB_ITEM pObj, const char * slot )
{
   HB_SYMBOL_UNUSED( pObj );

   QObject * object = ( QObject * ) hbqt_pPtrFromObj( 1 );
   return isConnected( object, slot );
}

bool HBQSlots::isConnected( QObject * object, const char * slot )
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL && listObj[ i ] == object )
      {
         if( object->property( slot ).toInt() == i + 1 )
         {
            return true;
         }
      }
   }
   return false;
}

bool HBQSlots::hbConnect( PHB_ITEM pObj, const char * slot, PHB_ITEM bBlock )
{
   HB_SYMBOL_UNUSED( pObj   );
   HB_SYMBOL_UNUSED( bBlock );

   QObject * object = ( QObject * ) hbqt_pPtrFromObj( 1 );                   /* get sender    */

   if( object )
   {
      if( !isConnected( object, slot ) )
      {
HB_TRACE( HB_TR_DEBUG, ( "AAA 3 %s  %p", slot, object ) );
         bool bConnected = connect_signal( ( QString ) slot, object, this );
HB_TRACE( HB_TR_DEBUG, ( "AAA 4" ) );
         if( bConnected )
         {
            PHB_ITEM pBlock = hb_itemNew( bBlock );                        /* get codeblock */
HB_TRACE( HB_TR_DEBUG, ( "AAA 5" ) );
            listBlock << pBlock;
            listObj   << object;

            object->setProperty( slot, ( int ) listBlock.size() );

            return true;
         }
      }
   }
   return false;
}

bool HBQSlots::hbDisconnect( PHB_ITEM pObj, const char * signal )
{
   HB_SYMBOL_UNUSED( pObj );

   QObject * object = ( QObject* ) hbqt_pPtrFromObj( 1 );

   if( object )
   {
      int i = object->property( signal ).toInt();

      if( i > 0 && i <= listBlock.size() )
      {
         hb_itemRelease( listBlock.at( i - 1 ) );
         listBlock[ i - 1 ] = NULL;
         listObj[ i - 1 ] = NULL;

         bool bRet = disconnect_signal( object, signal );

         HB_TRACE( HB_TR_DEBUG, ( "      QT_SLOTS_DISCONNECT: %s    %s", bRet ? "YES" : "NO", signal ) );

         return bRet;
      }
   }
   return false;
}

bool HBQSlots::hbClear()
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   listBlock.clear();
   return true;
}
/* Generic Key and Mouse Events emitted by subclass objects */
void HBQSlots::customContextMenuRequested( const QPoint & pos )                                             { hbqt_SlotsExecQPoint(         this, qobject_cast<QObject *>( sender() ), "customContextMenuRequested(QPoint)", pos                         ); }
void HBQSlots::triggered( bool checked )                                                                    { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "triggered(bool)", checked                                        ); }
void HBQSlots::hovered( QAction * action )                                                                  { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "hovered(action)", action                                         ); }
void HBQSlots::clicked()                                                                                    { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "clicked()"                                                       ); }
void HBQSlots::returnPressed()                                                                              { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "returnPressed()"                                                 ); }
void HBQSlots::viewportEntered()                                                                            { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "viewportEntered()"                                               ); }
void HBQSlots::pressed()                                                                                    { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "pressed()"                                                       ); }
void HBQSlots::released()                                                                                   { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "released()"                                                      ); }
void HBQSlots::triggered()                                                                                  { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "triggered()"                                                     ); }
void HBQSlots::hovered()                                                                                    { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "hovered()"                                                       ); }
void HBQSlots::stateChanged( int state )                                                                    { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "stateChanged(int)", state                                        ); }
void HBQSlots::activated( int index )                                                                       { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "activated(int)", index                                           ); }
void HBQSlots::currentIndexChanged( int index )                                                             { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "currentIndexChanged(int)", index                                 ); }
void HBQSlots::currentChanged( int index )                                                                  { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "currentChanged(int)", index                                      ); }
void HBQSlots::highlighted( int index )                                                                     { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "highlighted(int)", index                                         ); }
void HBQSlots::clicked( const QModelIndex & index )                                                         { hbqt_SlotsExecModel(          this, qobject_cast<QObject *>( sender() ), "clicked(QModelIndex)", index                                     ); }
void HBQSlots::doubleClicked( const QModelIndex & index )                                                   { hbqt_SlotsExecModel(          this, qobject_cast<QObject *>( sender() ), "doubleClicked(QModelIndex)", index                               ); }
void HBQSlots::entered( const QModelIndex & index )                                                         { hbqt_SlotsExecModel(          this, qobject_cast<QObject *>( sender() ), "entered(QModelIndex)", index                                     ); }
void HBQSlots::actionTriggered( int action )                                                                { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "actionTriggered(int)", action                                    ); }
void HBQSlots::rangeChanged( int min, int max )                                                             { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "rangeChanged(int)", min, max                                     ); }
void HBQSlots::sliderMoved( int value )                                                                     { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "sliderMoved(int)", value                                         ); }
void HBQSlots::sliderPressed()                                                                              { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "sliderPressed()"                                                 ); }
void HBQSlots::sliderReleased()                                                                             { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "sliderReleased()"                                                ); }
void HBQSlots::valueChanged( int value )                                                                    { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "valueChanged(int)", value                                        ); }
void HBQSlots::cursorPositionChanged( int iOld, int iNew )                                                  { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "cursorPositionChanged(int,int)", iOld, iNew                      ); }
void HBQSlots::editingFinished()                                                                            { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "editingFinished()"                                               ); }
void HBQSlots::selectionChanged()                                                                           { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "selectionChanged()"                                              ); }
void HBQSlots::textChanged( const QString & text )                                                          { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "textChanged(QString)", text                                      ); }
void HBQSlots::textEdited( const QString & text )                                                           { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "textEdited(QString)", text                                       ); }
/*  QTreeWidget */
void HBQSlots::itemExpanded( QTreeWidgetItem * item )                                                       { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemExpanded(QTWItem)", item                                     ); }
void HBQSlots::itemCollapsed( QTreeWidgetItem * item )                                                      { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemCollapsed(QTWItem)", item                                    ); }
void HBQSlots::currentItemChanged( QTreeWidgetItem * current, QTreeWidgetItem * previous )                  { hbqt_SlotsExecPointerPointer( this, qobject_cast<QObject *>( sender() ), "currentItemChanged(QTWItem,QTWItem)", current, previous          ); }
void HBQSlots::itemActivated( QTreeWidgetItem * item, int column )                                          { hbqt_SlotsExecPointerInt(     this, qobject_cast<QObject *>( sender() ), "itemActivated(QTWItem,int)", item, column                        ); }
void HBQSlots::itemChanged( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     this, qobject_cast<QObject *>( sender() ), "itemChanged(QTWItem,int)", item, column                          ); }
void HBQSlots::itemClicked( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     this, qobject_cast<QObject *>( sender() ), "itemClicked(QTWItem,int)", item, column                          ); }
void HBQSlots::itemDoubleClicked( QTreeWidgetItem * item, int column )                                      { hbqt_SlotsExecPointerInt(     this, qobject_cast<QObject *>( sender() ), "itemDoubleClicked(QTWItem,int)", item, column                    ); }
void HBQSlots::itemEntered( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     this, qobject_cast<QObject *>( sender() ), "itemEntered(QTWItem,int)", item, column                          ); }
void HBQSlots::itemPressed( QTreeWidgetItem * item, int column )                                            { hbqt_SlotsExecPointerInt(     this, qobject_cast<QObject *>( sender() ), "itemPressed(QTWItem,int)", item, column                          ); }
void HBQSlots::itemSelectionChanged()                                                                       { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "itemSelectionChanged()"                                          ); }
/* QDialog (s)*/
void HBQSlots::currentFontChanged( const QFont & font )                                                     { hbqt_SlotsExecFont(           this, qobject_cast<QObject *>( sender() ), "currentFontChanged(QFont)", font                                 ); }
void HBQSlots::fontSelected( const QFont & font )                                                           { hbqt_SlotsExecFont(           this, qobject_cast<QObject *>( sender() ), "fontSelected(QFont)", font                                       ); }
void HBQSlots::accepted()                                                                                   { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "accepted()"                                                      ); }
void HBQSlots::finished( int result )                                                                       { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "finished(int)", result                                           ); }
void HBQSlots::rejected()                                                                                   { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "rejected()"                                                      ); }
void HBQSlots::currentChanged( const QString & path )                                                       { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "currentChanged(QString)", path                                   ); }
void HBQSlots::directoryEntered( const QString & directory )                                                { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "directoryEntered(QString)", directory                            ); }
void HBQSlots::fileSelected( const QString & file )                                                         { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "fileSelected(QString)", file                                     ); }
void HBQSlots::filesSelected( const QStringList & selected )                                                { hbqt_SlotsExecStringList(     this, qobject_cast<QObject *>( sender() ), "filesSelected(QStringList)", selected                            ); }
void HBQSlots::filterSelected( const QString & filter )                                                     { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "filterSelected(QString)", filter                                 ); }
/* QPrintDialog */
void HBQSlots::accepted( QPrinter * printer )                                                               { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "accepted(QPrinter)", printer                                     ); }
/* QTextEdit */
void HBQSlots::copyAvailable( bool yes )                                                                    { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "copyAvailable(bool)", yes                                        ); }
void HBQSlots::currentCharFormatChanged( const QTextCharFormat & f )                                        { hbqt_SlotsExecTextCharFormat( this, qobject_cast<QObject *>( sender() ), "currentCharFormatChanged(QTextCharFormat)", f                    ); }
void HBQSlots::cursorPositionChanged()                                                                      { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "cursorPositionChanged()"                                         ); }
void HBQSlots::redoAvailable( bool available )                                                              { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "redoAvailable(bool)", available                                  ); }
void HBQSlots::textChanged()                                                                                { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "textChanged()"                                                   ); }
void HBQSlots::undoAvailable( bool available )                                                              { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "undoAvailable(bool)", available                                  ); }
void HBQSlots::timeout()                                                                                    { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "timeout()"                                                       ); }
void HBQSlots::geometriesChanged()                                                                          { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "geometriesChanged()"                                             ); }
void HBQSlots::sectionAutoResize( int logicalIndex, QHeaderView::ResizeMode mode )                          { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "sectionAutoResize(int,int)", logicalIndex, mode                  ); }
void HBQSlots::sectionClicked( int logicalIndex )                                                           { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "sectionClicked(int)", logicalIndex                               ); }
void HBQSlots::sectionCountChanged( int oldCount, int newCount )                                            { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "sectionCountChanged(int,int)", oldCount, newCount                ); }
void HBQSlots::sectionDoubleClicked( int logicalIndex )                                                     { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "sectionDoubleClicked(int)", logicalIndex                         ); }
void HBQSlots::sectionEntered( int logicalIndex )                                                           { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "sectionEntered(int)", logicalIndex                               ); }
void HBQSlots::sectionHandleDoubleClicked( int logicalIndex )                                               { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "sectionHandleDoubleClicked(int)", logicalIndex                   ); }
void HBQSlots::sectionMoved( int logicalIndex, int oldVisualIndex, int newVisualIndex )                     { hbqt_SlotsExecIntIntInt(      this, qobject_cast<QObject *>( sender() ), "sectionMoved(int,int,int)", logicalIndex, oldVisualIndex, newVisualIndex ); }
void HBQSlots::sectionPressed( int logicalIndex )                                                           { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "sectionPressed(int)", logicalIndex                               ); }
void HBQSlots::sectionResized( int logicalIndex, int oldSize, int newSize )                                 { hbqt_SlotsExecIntIntInt(      this, qobject_cast<QObject *>( sender() ), "sectionResized(int,int,int)", logicalIndex, oldSize, newSize     ); }
void HBQSlots::sortIndicatorChanged( int logicalIndex, Qt::SortOrder order )                                { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "sortIndicatorChanged(int,int)", logicalIndex, order              ); }
void HBQSlots::buttonClicked( int id )                                                                      { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "buttonClicked(int)", id                                          ); }
void HBQSlots::buttonPressed( int id )                                                                      { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "buttonPressed(int)", id                                          ); }
void HBQSlots::buttonReleased( int id )                                                                     { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "buttonReleased(int)", id                                         ); }
void HBQSlots::linkActivated( const QString & link )                                                        { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "linkActivated(QString)", link                                    ); }
void HBQSlots::linkHovered( const QString & link )                                                          { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "linkHovered(QString)", link                                      ); }
void HBQSlots::tabCloseRequested( int index )                                                               { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "tabCloseRequested(int)", index                                   ); }
void HBQSlots::paintRequested( QPrinter * printer )                                                         { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "paintRequested(QPrinter)", printer                               ); }
/* QIODevice */
void HBQSlots::aboutToClose()                                                                               { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "aboutToClose()"                                                  ); }
void HBQSlots::bytesWritten( qint64 bytes )                                                                 { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "bytesWritten(int)", bytes                                        ); }
void HBQSlots::readChannelFinished()                                                                        { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "readChannelFinished()"                                           ); }
void HBQSlots::readyRead()                                                                                  { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "readyRead()"                                                     ); }
/* QProcess */
void HBQSlots::error( QProcess::ProcessError error )                                                        { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "error(error)", error                                             ); }
void HBQSlots::finished( int exitCode, QProcess::ExitStatus exitStatus )                                    { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "finished(int,int)", exitCode, exitStatus                         ); }
void HBQSlots::readyReadStandardError()                                                                     { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "readyReadStandardError()"                                        ); }
void HBQSlots::readyReadStandardOutput()                                                                    { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "readyReadStandardOutput()"                                       ); }
void HBQSlots::started()                                                                                    { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "started()"                                                       ); }
void HBQSlots::stateChanged( QProcess::ProcessState newState )                                              { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "stateChanged(int)", newState                                     ); }
/* QComboBox */
void HBQSlots::activated( const QString & text )                                                            { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "activated(QString)", text                                        ); }
void HBQSlots::currentIndexChanged( const QString & text )                                                  { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "currentIndexChanged(QString)", text                              ); }
void HBQSlots::editTextChanged( const QString & text )                                                      { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "editTextChanged(QString)", text                                  ); }
void HBQSlots::highlighted( const QString & text )                                                          { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "highlighted(QString)", text                                      ); }
/* QTextDocument */
void HBQSlots::blockCountChanged( int newBlockCount )                                                       { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "blockCountChanged(int)", newBlockCount                           ); }
void HBQSlots::contentsChange( int position, int charsRemoved, int charsAdded )                             { hbqt_SlotsExecIntIntInt(      this, qobject_cast<QObject *>( sender() ), "contentsChange(int,int,int)", position, charsRemoved, charsAdded ); }
void HBQSlots::contentsChanged()                                                                            { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "contentsChanged()"                                               ); }
void HBQSlots::cursorPositionChanged( const QTextCursor & cursor )                                          { hbqt_SlotsExecQTextCursor(    this, qobject_cast<QObject *>( sender() ), "cursorPositionChanged(QTextCursor)", cursor                      ); }
void HBQSlots::documentLayoutChanged()                                                                      { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "documentLayoutChanged()"                                         ); }
void HBQSlots::modificationChanged( bool changed )                                                          { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "modificationChanged(bool)", changed                              ); }
void HBQSlots::undoCommandAdded()                                                                           { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "undoCommandAdded()"                                              ); }
/* QPlainTextEdit */
void HBQSlots::updateRequest( const QRect & rect, int dy )                                                  { hbqt_SlotsExecQRectInt(       this, qobject_cast<QObject *>( sender() ), "updateRequest(QRect,int)", rect, dy                              ); }
/* QItemSelectionModel */
void HBQSlots::currentChanged( const QModelIndex & currentIndex, const QModelIndex & previousIndex )        { hbqt_SlotsExecModelModel(     this, qobject_cast<QObject *>( sender() ), "currentChanged(QModelIndex,QModelIndex)", currentIndex, previousIndex       ); }
void HBQSlots::currentColumnChanged( const QModelIndex & currentIndex, const QModelIndex & previousIndex )  { hbqt_SlotsExecModelModel(     this, qobject_cast<QObject *>( sender() ), "currentColumnChanged(QModelIndex,QModelIndex)", currentIndex, previousIndex ); }
void HBQSlots::currentRowChanged( const QModelIndex & currentIndex, const QModelIndex & previousIndex )     { hbqt_SlotsExecModelModel(     this, qobject_cast<QObject *>( sender() ), "currentRowChanged(QModelIndex,QModelIndex)", currentIndex, previousIndex    ); }
void HBQSlots::selectionChanged( const QItemSelection & selected, const QItemSelection & deselected )       { hbqt_SlotsExecItemSelItemSel( this, qobject_cast<QObject *>( sender() ), "selectionChanged(QItemSelection,QItemSelection)", selected, deselected      ); }
/* QListWidget */
void HBQSlots::currentRowChanged( int currentRow )                                                          { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "currentRowChanged(int)", currentRow                              ); }
void HBQSlots::currentTextChanged( const QString & currentText )                                            { hbqt_SlotsExecString(         this, qobject_cast<QObject *>( sender() ), "currentTextChanged(QString)", currentText                        ); }
void HBQSlots::currentItemChanged( QListWidgetItem * current, QListWidgetItem * previous )                  { hbqt_SlotsExecPointerPointer( this, qobject_cast<QObject *>( sender() ), "currentItemChanged(QLWItem,QLWItem)", current, previous          ); }
void HBQSlots::itemActivated( QListWidgetItem * item )                                                      { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemActivated(QLWItem)", item                                    ); }
void HBQSlots::itemChanged( QListWidgetItem * item )                                                        { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemChanged(QLWItem)", item                                      ); }
void HBQSlots::itemClicked( QListWidgetItem * item )                                                        { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemClicked(QLWItem)", item                                      ); }
void HBQSlots::itemDoubleClicked( QListWidgetItem * item )                                                  { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemDoubleClicked(QLWItem)", item                                ); }
void HBQSlots::itemEntered( QListWidgetItem * item )                                                        { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemEntered(QLWItem)", item                                      ); }
void HBQSlots::itemPressed( QListWidgetItem * item )                                                        { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemPressed(QLWItem)", item                                      ); }
/* QTableWidget */
void HBQSlots::cellActivated( int row, int column )                                                         { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "cellActivated(int,int)", row, column                             ); }
void HBQSlots::cellChanged( int row, int column )                                                           { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "cellChanged(int,int)", row, column                               ); }
void HBQSlots::cellClicked( int row, int column )                                                           { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "cellClicked(int,int)", row, column                               ); }
void HBQSlots::cellDoubleClicked( int row, int column )                                                     { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "cellDoubleClicked(int,int)", row, column                         ); }
void HBQSlots::cellEntered( int row, int column )                                                           { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "cellEntered(int,int)", row, column                               ); }
void HBQSlots::cellPressed( int row, int column )                                                           { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "cellEntered(int,int)", row, column                               ); }
void HBQSlots::currentCellChanged( int currentRow, int currentColumn, int previousRow, int previousColumn ) { hbqt_SlotsExecIntIntIntInt(   this, qobject_cast<QObject *>( sender() ), "currentCellChanged(int,int,int,int)", currentRow, currentColumn, previousRow, previousColumn ); }
void HBQSlots::currentItemChanged( QTableWidgetItem * current, QTableWidgetItem * previous )                { hbqt_SlotsExecPointerPointer( this, qobject_cast<QObject *>( sender() ), "currentItemChanged(QTblWItem,QLWItem)", current, previous        ); }
void HBQSlots::itemActivated( QTableWidgetItem * item )                                                     { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemActivated(QTblWItem)", item                                  ); }
void HBQSlots::itemChanged( QTableWidgetItem * item )                                                       { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemChanged(QTblWItem)", item                                    ); }
void HBQSlots::itemClicked( QTableWidgetItem * item )                                                       { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemClicked(QTblWItem)", item                                    ); }
void HBQSlots::itemDoubleClicked( QTableWidgetItem * item )                                                 { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemDoubleClicked(QTblWItem)", item                              ); }
void HBQSlots::itemEntered( QTableWidgetItem * item )                                                       { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemEntered(QTblWItem)", item                                    ); }
void HBQSlots::itemPressed( QTableWidgetItem * item )                                                       { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "itemPressed(QTblWItem)", item                                    ); }
/* QTextBrowser */
void HBQSlots::anchorClicked( const QUrl & link )                                                           { hbqt_SlotsExecQUrl(           this, qobject_cast<QObject *>( sender() ), "anchorClicked(QUrl)", link                                       ); }
void HBQSlots::backwardAvailable( bool available )                                                          { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "backwardAvailable(bool)", available                              ); }
void HBQSlots::forwardAvailable( bool available )                                                           { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "forwardAvailable(bool)", available                               ); }
void HBQSlots::highlighted( const QUrl & link )                                                             { hbqt_SlotsExecQUrl(           this, qobject_cast<QObject *>( sender() ), "highlighted(QUrl)", link                                         ); }
void HBQSlots::historyChanged()                                                                             { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "historyChanged()"                                                ); }
void HBQSlots::sourceChanged( const QUrl & src )                                                            { hbqt_SlotsExecQUrl(           this, qobject_cast<QObject *>( sender() ), "sourceChanged(QUrl)", src                                        ); }
/* QDockWidget */
void HBQSlots::allowedAreasChanged( Qt::DockWidgetAreas allowedAreas )                                      { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "allowedAreasChanged(int)", allowedAreas                          ); }
void HBQSlots::dockLocationChanged( Qt::DockWidgetArea area )                                               { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "dockLocationChanged(int)", area                                  ); }
void HBQSlots::featuresChanged( QDockWidget::DockWidgetFeatures features )                                  { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "featuresChanged(int)", features                                  ); }
void HBQSlots::topLevelChanged( bool topLevel )                                                             { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "topLevelChanged(bool)", topLevel                                 ); }
void HBQSlots::visibilityChanged( bool visible )                                                            { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "visibilityChanged(bool)", visible                                ); }
/* QCompleter */
void HBQSlots::activated( const QModelIndex & index )                                                       { hbqt_SlotsExecModel(          this, qobject_cast<QObject *>( sender() ), "activated(QModelIndex)", index                                   ); }
void HBQSlots::highlighted( const QModelIndex & index )                                                     { hbqt_SlotsExecModel(          this, qobject_cast<QObject *>( sender() ), "highlighted(QModelIndex)", index                                 ); }
/* QAbstractButton */
void HBQSlots::toggled( bool checked )                                                                      { hbqt_SlotsExecBool(           this, qobject_cast<QObject *>( sender() ), "toggled(bool)", checked                                          ); }
/* QSystemTrayIcon */
void HBQSlots::activated( QSystemTrayIcon::ActivationReason reason )                                        { hbqt_SlotsExecInt(            this, qobject_cast<QObject *>( sender() ), "activated(QSystemTrayIcon::ActivationReason)", reason            ); }
/* QMdiArea */
void HBQSlots::subWindowActivated( QMdiSubWindow * window )                                                 { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "subWindowActivated(QMdiSubWindow)", window                       ); }
/* QMdiSubWindow */
void HBQSlots::aboutToActivate()                                                                            { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "aboutToActivate()"                                               ); }
void HBQSlots::windowStateChanged( Qt::WindowStates oldState, Qt::WindowStates newState )                   { hbqt_SlotsExecIntInt(         this, qobject_cast<QObject *>( sender() ), "windowStateChanged(Qt::WindowStates,Qt::WindowStates)", oldState, newState ); }
/* QAbstractItemDelegate */
void HBQSlots::closeEditor( QWidget * editor, QAbstractItemDelegate::EndEditHint hint )                     { hbqt_SlotsExecPointerInt(     this, qobject_cast<QObject *>( sender() ), "closeEditor(QWidget,int)", editor, hint                          ); }
void HBQSlots::commitData( QWidget * editor )                                                               { hbqt_SlotsExecPointer(        this, qobject_cast<QObject *>( sender() ), "commitData(QWidget)", editor                                     ); }
void HBQSlots::sizeHintChanged( const QModelIndex & index )                                                 { hbqt_SlotsExecModel(          this, qobject_cast<QObject *>( sender() ), "sizeHintChanged(QModelIndex)", index                             ); }
/* QGraphicsScene */
void HBQSlots::sceneRectChanged( const QRectF & rect )                                                      { hbqt_SlotsExecQRectF(         this, qobject_cast<QObject *>( sender() ), "sceneRectChanged(QRectF)", rect                                  ); }
/* QDateTimeEdit */
void HBQSlots::dateChanged( const QDate & date )                                                            { hbqt_SlotsExecQDate(          this, qobject_cast<QObject *>( sender() ), "dateChanged(QDate)", date                                        ); }
void HBQSlots::dateTimeChanged( const QDateTime & datetime )                                                { hbqt_SlotsExecQDateTime(      this, qobject_cast<QObject *>( sender() ), "dateTimeChanged(QDate)", datetime                                ); }
void HBQSlots::timeChanged( const QTime & time )                                                            { hbqt_SlotsExecQTime(          this, qobject_cast<QObject *>( sender() ), "timeChanged(QTime)", time                                        ); }
/* QApplication */
//void HBQSlots::commitDataRequest( QSessionManager & manager )                                             { hbqt_SlotsExecQSessionManager(this, qobject_cast<QObject *>( sender() ), "commitDataRequest(QSessionManager)", manager                     ); }
void HBQSlots::focusChanged( QWidget * old, QWidget * now )                                                 { hbqt_SlotsExecPointerPointer( this, qobject_cast<QObject *>( sender() ), "focusChanged(QWidget,QWidget)"     , old, now                    ); }
void HBQSlots::fontDatabaseChanged()                                                                        { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "fontDatabaseChanged()"                                           ); }
void HBQSlots::lastWindowClosed()                                                                           { hbqt_SlotsExec(               this, qobject_cast<QObject *>( sender() ), "lastWindowClosed()"                                              ); }
//void HBQSlots::saveStateRequest( QSessionManager & manager )                                              { hbqt_SlotsExecQSessionManager(this, qobject_cast<QObject *>( sender() ), "saveStateRequest(QSessionManager)" , manager                     ); }
/* Latest */

/*----------------------------------------------------------------------*/
/*
 * Harbour function to connect signals with slots
 */
HB_FUNC( QT_SLOTS_CONNECT )
{
   HB_BOOL   bRet    = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
HB_TRACE( HB_TR_DEBUG, ( "QT_SLOTS_CONNECT( %s )", hb_parcx( 3 ) ) );
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 );               /* get sender    */
      if( object )
      {
         int i = object->property( hb_parcx( 3 ) ).toInt();
         if ( i == 0 )
         {
            QString signal = hb_parcx( 3 );                                 /* get signal    */
            if( connect_signal( signal, object, t_slots ) )
            {
               PHB_ITEM pBlock = hb_itemNew( hb_param( 4, HB_IT_BLOCK ) );  /* get codeblock */
               t_slots->listBlock << pBlock;

               object->setProperty( hb_parcx( 3 ), ( int ) t_slots->listBlock.size() );
               bRet = HB_TRUE;
            }
         }
HB_TRACE( HB_TR_DEBUG, ( "QT_SLOTS_CONNECT( %s ) %s", hb_parcx( 3 ), bRet ? "Connected" : "not-connected" ) );
      }
   }
   hb_retl( bRet );
}

/*
 * Harbour function to disconnect signals
 */
HB_FUNC( QT_SLOTS_DISCONNECT )
{
   HB_BOOL   bRet    = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
HB_TRACE( HB_TR_DEBUG, ( "QT_SLOTS_DISCONNECT( %s )", hb_parcx( 3 ) ) );
      QObject * object = ( QObject* ) hbqt_pPtrFromObj( 2 );
      if( object )
      {
         const char * slot = hb_parcx( 3 );

         int i = object->property( slot ).toInt();

         if( i > 0 && i <= t_slots->listBlock.size() )
         {
            object->setProperty( slot, QVariant() );

            bRet = ( disconnect_signal( object, slot ) == true );

            hb_itemRelease( t_slots->listBlock.at( i - 1 ) );
            t_slots->listBlock[ i - 1 ] = NULL;

            HB_TRACE( HB_TR_DEBUG, ( "      QT_SLOTS_DISCONNECT: %s    %s", bRet ? "YES" : "NO", slot ) );
         }
HB_TRACE( HB_TR_DEBUG, ( "QT_SLOTS_DISCONNECT( %s ) %s", hb_parcx( 3 ), bRet ? "disConnected" : "not-disConnected" ) );
      }
   }
   hb_retl( bRet );
}

HB_FUNC( QT_SLOTS_NEW )
{
   void * pObj = NULL;

   pObj = ( HBQSlots * ) new HBQSlots();

   hb_retptrGC( hbqt_gcAllocate_HBQSlots( pObj, true ) );
}

/*----------------------------------------------------------------------*/
#else  //__PRITPAL__
/*----------------------------------------------------------------------*/

#include <QtCore/QMetaObject>
#include <QtCore/QMetaMethod>
#include <QtCore/QPointer>
#include <QtCore/QByteArray>
#include <QtCore/QObject>

static void hbqt_SlotsExecBool( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutL( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecInt( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) ) );
   PHB_ITEM p2 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 2, p1, p2 );
   hb_itemRelease( p1 );
   hb_itemRelease( p2 );
}

static void hbqt_SlotsExecIntIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) ) );
   PHB_ITEM p2 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) ) );
   PHB_ITEM p3 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 3, p1, p2, p3 );
   hb_itemRelease( p1 );
   hb_itemRelease( p2 );
   hb_itemRelease( p3 );
}

static void hbqt_SlotsExecIntIntIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) ) );
   PHB_ITEM p2 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) ) );
   PHB_ITEM p3 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) ) );
   PHB_ITEM p4 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 4 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 4, p1, p2, p3, p4 );
   hb_itemRelease( p1 );
   hb_itemRelease( p2 );
   hb_itemRelease( p3 );
   hb_itemRelease( p4 );
}

static void hbqt_SlotsExecQRectInt( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL,  new QRect( (*reinterpret_cast< QRect(*)>(arguments[1])) ) );
   PHB_ITEM p2 = hb_itemPutNI( NULL, (*reinterpret_cast< int(*)>(arguments[2])) );
   hb_vmEvalBlockV( codeBlock, 2, p1, p2 );
   delete ( ( QRect * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
   hb_itemRelease( p2 );
}

static void hbqt_SlotsExecString( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM pString = hb_itemPutC( NULL, ( *reinterpret_cast< QString(*)>(arguments[1])).toAscii().data() );
   hb_vmEvalBlockV( codeBlock, 1, pString );
   hb_itemRelease( pString );
}

#include <QtCore/QModelIndex>

static void hbqt_SlotsExecModel( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM pState = hb_itemPutPtr( NULL, new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, pState );
   delete ( ( QModelIndex * ) hb_itemGetPtr( pState ) );
   hb_itemRelease( pState );
}

static void hbqt_SlotsExecModelModel( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM pState1 = hb_itemPutPtr( NULL, new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) );
   PHB_ITEM pState2 = hb_itemPutPtr( NULL, new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 2 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 2, pState1, pState2 );
   delete ( ( QModelIndex * ) hb_itemGetPtr( pState1 ) );
   delete ( ( QModelIndex * ) hb_itemGetPtr( pState2 ) );
   hb_itemRelease( pState1 );
   hb_itemRelease( pState2 );
}

#include <QtGui/QItemSelection>

static void hbqt_SlotsExecItemSelItemSel( PHB_ITEM * codeBlock, void ** arguments)
{
   PHB_ITEM pState1 = hb_itemPutPtr( NULL, new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 1 ] ) ) ) );
   PHB_ITEM pState2 = hb_itemPutPtr( NULL, new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 2 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 2, pState1, pState2 );
   delete ( ( QItemSelection * ) hb_itemGetPtr( pState1 ) );
   delete ( ( QItemSelection * ) hb_itemGetPtr( pState2 ) );
   hb_itemRelease( pState1 );
   hb_itemRelease( pState2 );
}

#include <QtGui/QTextCharFormat>

static void hbqt_SlotsExecTextCharFormat( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTextCharFormat( ( *reinterpret_cast<QTextCharFormat( * )>( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QTextCharFormat * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecFont( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QFont( ( *reinterpret_cast< QFont( * )>( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QFont * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecQTextCursor( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTextCursor( ( *reinterpret_cast< QTextCursor( * )>( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QTextCursor * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecStringList( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QStringList( ( *reinterpret_cast<QStringList( * )>( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QStringList * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecPointer( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, ( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecPointerInt( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, ( *reinterpret_cast< void*( * ) >( arguments[ 1 ] ) ) );
   PHB_ITEM p2 = hb_itemPutNI( NULL, ( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 2, p1, p2 );
   hb_itemRelease( p1 );
   hb_itemRelease( p2 );
}

static void hbqt_SlotsExecPointerPointer( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, ( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) ) );
   PHB_ITEM p2 = hb_itemPutPtr( NULL, ( *reinterpret_cast< void*( * )>( arguments[ 2 ] ) ) );
   hb_vmEvalBlockV( codeBlock, 2, p1, p2 );
   hb_itemRelease( p1 );
   hb_itemRelease( p2 );
}

static void hbqt_SlotsExecQPoint( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QPoint( ( * reinterpret_cast< QPoint( * )>( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QPoint * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}


static void hbqt_SlotsExecQRectF( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QRectF( ( *reinterpret_cast< QRectF( * )>( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QRectF * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecQUrl( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QUrl( ( *reinterpret_cast< QUrl( * )>( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QUrl * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecQDate( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QDate( ( *reinterpret_cast< QDate( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QDate * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecQDateTime( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QDateTime( ( *reinterpret_cast< QDateTime( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QDateTime * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

static void hbqt_SlotsExecQTime( PHB_ITEM * codeBlock, void ** arguments )
{
   PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTime( ( *reinterpret_cast< QTime( * ) >( arguments[ 1 ] ) ) ) );
   hb_vmEvalBlockV( codeBlock, 1, p1 );
   delete ( ( QTime * ) hb_itemGetPtr( p1 ) );
   hb_itemRelease( p1 );
}

/*----------------------------------------------------------------------*/

static int connect_signal( QString signal, QObject * object, HBQSlots * t_slots )
{
   HB_TRACE( HB_TR_DEBUG, ( "connect_signal: %s", ( char * ) signal.toAscii().data() ) );

   QByteArray theSignal = signal.toAscii()
                              .replace( "QWidget"      , "QWidget *" )
                              .replace( "QPrinter"     , "QPrinter *" )
                              .replace( "QLWItem"      , "QListWidgetItem *" )
                              .replace( "QTWItem"      , "QTreeWidgetItem *" )
                              .replace( "QTblWItem"    , "QTableWidgetItem *" )
                              .replace( "QMdiSubWindow", "QMdiSubWindow *" );

   theSignal = QMetaObject::normalizedSignature( theSignal );

   HB_TRACE( HB_TR_DEBUG, ( "    normalized: %s", ( char * ) theSignal.data() ) );

   if( ! QMetaObject::checkConnectArgs( theSignal, theSignal ) )
   {
      return -1;
   }

   int signalId = object->metaObject()->indexOfSignal( theSignal );
   if( signalId == -1 )
   {
      return -1;
   }
   //  HB_TRACE( HB_TR_DEBUG, ( "   signalId %d", signalId ) );

   int slotId = object->metaObject()->indexOfMethod( theSignal );
   if( slotId == -1 )
   {
      return -1;
   }
   //  HB_TRACE( HB_TR_DEBUG, ( "   slotId %d", slotId ) );
   if( QMetaObject::connect( object, signalId, t_slots, slotId + QObject::staticMetaObject.methodCount(), Qt::AutoConnection ) )
   {
      HB_TRACE( HB_TR_DEBUG, ( "    normalized: %i %i %s", signalId, slotId, ( char * ) theSignal.data() ) );
      return slotId;
   }
   return -1;
}

/*----------------------------------------------------------------------*/

static bool disconnect_signal( QObject * object, QString signal )
{
   QByteArray theSignal = signal.toAscii()
                              .replace( "QWidget"      , "QWidget *" )
                              .replace( "QPrinter"     , "QPrinter *" )
                              .replace( "QLWItem"      , "QListWidgetItem *" )
                              .replace( "QTWItem"      , "QTreeWidgetItem *" )
                              .replace( "QTblWItem"    , "QTableWidgetItem *" )
                              .replace( "QMdiSubWindow", "QMdiSubWindow *" );

   theSignal = QMetaObject::normalizedSignature( theSignal ) ;

   int signalId = object->metaObject()->indexOfSignal( theSignal );
   if( signalId == -1 )
   {
      return false;
   }
// HB_TRACE( HB_TR_DEBUG, ( "   signalId %d  ", signalId ) );
   return QMetaObject::disconnect( object, signalId, 0, 0 );
}

/*----------------------------------------------------------------------*/

static void hbqt_SlotsProxy( HBQSlots * t_slots, int id, QObject * object, void ** arguments )
{
   // array of function calls
   // static void *( arrayFunc[23])(PHB_ITEM *, void ** ) {
   //
   #if 0
   static void * ( arrayFunc[ 23 ] )
   {
      hbqt_SlotsExecInt,
      hbqt_SlotsExecIntInt,
      hbqt_SlotsExecIntIntInt,
      hbqt_SlotsExecIntIntIntInt,
      hbqt_SlotsExecBool,
      hbqt_SlotsExecQRectInt ,
      hbqt_SlotsExecString ,
      hbqt_SlotsExecModel ,
      hbqt_SlotsExecModelModel ,
      hbqt_SlotsExecItemSelItemSel ,
      hbqt_SlotsExecTextCharFormat ,
      hbqt_SlotsExecFont ,
      hbqt_SlotsExecQTextCursor ,
      hbqt_SlotsExecStringList ,
      hbqt_SlotsExecPointer ,
      hbqt_SlotsExecPointerPointer ,
      hbqt_SlotsExecPointerInt ,
      hbqt_SlotsExecQDate,
      hbqt_SlotsExecQDateTime,
      hbqt_SlotsExecQPoint,
      hbqt_SlotsExecQRectF,
      hbqt_SlotsExecQTime,
      hbqt_SlotsExecQUrl
   }
   #endif

   static QList< QByteArray > argCombinations;

   if( argCombinations.size() == 0 )
   {
      argCombinations << "int" <<
                         "int$int" <<
                         "int$int$int" <<
                         "int$int$int$int" <<
                         "bool" <<
                         "QRect$int" <<
                         "QString" <<
                         "QModel" <<
                         "QModel$QModel" <<
                         "QItemSelection$QItemSelection" <<
                         "QTextCharFormat" <<
                         "QFont" <<
                         "QTextCursor" <<
                         "QStringList" <<
                         "pointer" <<
                         "pointer$pointer" <<
                         "pointer$int"<<
                         "QDate" <<
                         "QDateTime" <<
                         "QPoint" <<
                         "QRectF" <<
                         "QTime" <<
                         "QUrl" ;
   }

   const QMetaMethod meta = object->metaObject()->method( id );

   HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy signature %s", meta.signature() ) );

   QList<QByteArray> arrayOfTypes = meta.parameterTypes();

   // HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy objectName %s ", qPrintable( object->objectName() ) ) );
   // HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy className %s ", (char * ) object->metaObject()->className() ) );
   int parameterCount = arrayOfTypes.size();
   QStringList parList;

   for( int i = 0; i < parameterCount; i++ )
   {
      if( arrayOfTypes.at( i ).contains( "::" ) ) // if includes :: is a enum -> int
      {
         parList += "int";
      }
      else
      {
         if( arrayOfTypes.at( i ).contains( "*" ) )  // if includes * is a pointer -> pointer
         {
            parList += "pointer";
         }
         else
         {
            parList += arrayOfTypes.at( i ); //
         }
      }
   }

   QByteArray paramString = parList.join("$").toAscii();
   HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy parList %s ", (char * ) paramString.data() ) );

   if( object )
   {
      char cSlotName[ 20 ];
      sprintf( cSlotName, "SLOT_%d", id );
      int i = object->property( cSlotName ).toInt();
      // HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy %s=%d", cSlotName, i ) );

      if( i > 0 && i <= t_slots->listBlock.size() && hb_vmRequestReenter() )
      {
         if( parameterCount == 0 )
         {
            hb_vmEvalBlockV( t_slots->listBlock.at( i - 1 ), 0 );
         }
         else
         {
            int paramId = argCombinations.indexOf( paramString );
            HB_TRACE( HB_TR_DEBUG, ( "  params=%s,  paramId=%d", ( char *) paramString.data(), paramId ) );

            switch( paramId )
            {
            case 0:   // if( paramString == "int" )
                hbqt_SlotsExecInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
                break;
            case 1:   // if( paramString == "int$int" )
                hbqt_SlotsExecIntInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
                break;
            case 2:   // if( paramString == "int$int$int" )
                hbqt_SlotsExecIntIntInt( (PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
                break;
            case 3:   // if( paramString == "int$int$int$int" )
               hbqt_SlotsExecIntIntIntInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 4:  //  if( paramString == "bool" )
               hbqt_SlotsExecBool( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 5:   // if( paramString == "QRect$int" )
               hbqt_SlotsExecQRectInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 6:   // if( paramString == "QString" )
               hbqt_SlotsExecString( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 7:   // if( paramString == "QModel" )
               hbqt_SlotsExecModel( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 8:   // if( paramString == "QModel$QModel" )
               hbqt_SlotsExecModelModel( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 9:   // if( paramString == "QItemSelection$QItemSelection" )
               hbqt_SlotsExecItemSelItemSel( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 10:   // if( paramString == "QTextCharFormat" )
               hbqt_SlotsExecTextCharFormat( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 11:   // if( paramString == "QFont" )
               hbqt_SlotsExecFont( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 12:   // if( paramString == "QTextCursor" )
               hbqt_SlotsExecQTextCursor( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 13:   // if( paramString == "QStringList" )
               hbqt_SlotsExecStringList( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 14:   // if( paramString == "pointer" )
               hbqt_SlotsExecPointer( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 15:   // if( paramString == "pointer$pointer" )
               hbqt_SlotsExecPointerPointer( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 16:   // if( paramString == "pointer$int" )
               hbqt_SlotsExecPointerInt( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 17:   // if( paramString == "QDate" )
               hbqt_SlotsExecQDate( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 18:   // if( paramString == "QDateTime" )
               hbqt_SlotsExecQDateTime( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 19:   // if( paramString == "QPoint" )
               hbqt_SlotsExecQPoint( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 20:   // if( paramString == "QRectF" )
               hbqt_SlotsExecQRectF( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 21:   // if( paramString == "QTime" )
               hbqt_SlotsExecQTime( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            case 22:   // if( paramString == "QUrl" )
               hbqt_SlotsExecQUrl( ( PHB_ITEM * ) t_slots->listBlock.at( i - 1 ), arguments ) ;
               break;
            }
         }
         hb_vmRequestRestore();
      }
   }
}

HBQSlots::HBQSlots( QObject* parent ) : QObject( parent )
{
}

HBQSlots::~HBQSlots()
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   /* QUESTION: Should there be all remaining active slots disconnected at this point? */

   /*           Should be disconnected, but this is a responsibility of programmer as object is only known to the application */
   listBlock.clear();
}

bool HBQSlots::hbIsConnected( PHB_ITEM pObj, const char * slot )
{
   HB_SYMBOL_UNUSED( pObj );

   QObject * object = ( QObject * ) hbqt_pPtrFromObj( 1 );
   return isConnected( object, slot );
}

bool HBQSlots::isConnected( QObject * object, const char * slot )
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL && listObj[ i ] == object )
      {
         if( object->property( slot ).toInt() == i + 1 )
         {
            return true;
         }
      }
   }
   return false;
}

bool HBQSlots::hbConnect( PHB_ITEM pObj, const char * slot, PHB_ITEM bBlock )
{
   HB_SYMBOL_UNUSED( pObj   );
   HB_SYMBOL_UNUSED( bBlock );

   QObject * object = ( QObject * ) hbqt_pPtrFromObj( 1 );                   /* get sender    */

   if( object )
   {
      if( !isConnected( object, slot ) )
      {
HB_TRACE( HB_TR_DEBUG, ( "AAA 3 %s  %p", slot, object ) );
         bool bConnected = connect_signal( ( QString ) slot, object, this );
HB_TRACE( HB_TR_DEBUG, ( "AAA 4" ) );
         if( bConnected )
         {
            PHB_ITEM pBlock = hb_itemNew( bBlock );                        /* get codeblock */
HB_TRACE( HB_TR_DEBUG, ( "AAA 5" ) );
            listBlock << pBlock;
            listObj   << object;

            object->setProperty( slot, ( int ) listBlock.size() );

            return true;
         }
      }
   }
   return false;
}

bool HBQSlots::hbDisconnect( PHB_ITEM pObj, const char * signal )
{
   HB_SYMBOL_UNUSED( pObj );

   QObject * object = ( QObject* ) hbqt_pPtrFromObj( 1 );

   if( object )
   {
      int i = object->property( signal ).toInt();

      if( i > 0 && i <= listBlock.size() )
      {
         hb_itemRelease( listBlock.at( i - 1 ) );
         listBlock[ i - 1 ] = NULL;
         listObj[ i - 1 ] = NULL;

         bool bRet = disconnect_signal( object, ( QString ) signal );

         HB_TRACE( HB_TR_DEBUG, ( "      QT_SLOTS_DISCONNECT: %s    %s", bRet ? "YES" : "NO", signal ) );

         return bRet;
      }
   }
   return false;
}

bool HBQSlots::hbClear()
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   listBlock.clear();
   return true;
}

int HBQSlots::qt_metacall( QMetaObject::Call c, int id, void **arguments )
{
    id = QObject::qt_metacall( c, id, arguments );
    if( id < 0 || c != QMetaObject::InvokeMetaMethod )
    {
       return id;
    }
    // Q_ASSERT(id < slotList.size());

    hbqt_SlotsProxy( this, id, sender(), arguments );
    return -1;
}


/*----------------------------------------------------------------------*/
/*
 * Harbour function to connect signals with slots
 */
HB_FUNC( QT_SLOTS_CONNECT )
{
   HB_BOOL   bRet    = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 );               /* get sender    */
      if( object )
      {
         int i = object->property( hb_parcx( 3 ) ).toInt();
         if ( i == 0 )
         {
            QString signal = hb_parcx( 3 );                                 /* get signal    */
            int idSignal = connect_signal( signal, object, t_slots ) ;
            if( idSignal >= 0 )
            {
               PHB_ITEM pBlock = hb_itemNew( hb_param( 4, HB_IT_BLOCK ) );  /* get codeblock */
               t_slots->listBlock << pBlock;

               char cSlotName[ 20 ];
               sprintf( cSlotName, "SLOT_%d", idSignal );

               object->setProperty( cSlotName, ( int ) t_slots->listBlock.size() );
               object->setProperty( hb_parcx(3), ( int ) t_slots->listBlock.size() );
               bRet = HB_TRUE;
            }
         }
      }
   }
   if ( !bRet )
        hb_errRT_BASE( EG_ARG, 1100, NULL, "CONNECT", HB_ERR_ARGS_BASEPARAMS );
   hb_retl( bRet );
}

/*
 * Harbour function to disconnect signals
 */
HB_FUNC( QT_SLOTS_DISCONNECT )
{
   HB_BOOL   bRet    = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject* ) hbqt_pPtrFromObj( 2 );
      if( object )
      {
         const char * slot = hb_parcx( 3 );

         int i = object->property( slot ).toInt();

         if( i > 0 && i <= t_slots->listBlock.size() )
         {
            object->setProperty( slot, QVariant() );

            bRet = ( disconnect_signal( object, ( QString ) slot ) == true );

            hb_itemRelease( t_slots->listBlock.at( i - 1 ) );
            t_slots->listBlock[ i - 1 ] = NULL;
         }
      }
   }
   hb_retl( bRet );
}

HB_FUNC( QT_SLOTS_NEW )
{
   void * pObj = NULL;

   pObj = ( HBQSlots * ) new HBQSlots();

   hb_retptrGC( hbqt_gcAllocate_HBQSlots( pObj, true ) );
}

#endif
#endif
