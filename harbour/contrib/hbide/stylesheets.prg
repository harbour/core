/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               17Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbide.ch"

/*----------------------------------------------------------------------*/

FUNCTION hbide_setAppTheme( aTheme )
   LOCAL oTheme
   STATIC sTheme
   oTheme := sTheme
   IF HB_ISARRAY( aTheme )
      sTheme := aTheme
   ENDIF
   RETURN oTheme

/*----------------------------------------------------------------------*/

FUNCTION GetStyleSheet( cWidget, nMode )
   LOCAL txt_
   LOCAL s

   DEFAULT nMode TO HBIDE_ANIMATION_NONE

   IF nMode == HBIDE_ANIMATION_NONE
      RETURN ""
   ENDIF

   txt_:= {}

   DO CASE
   CASE cWidget == "QMenuPop"
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QMenu {                                                                      ' )
      aadd( txt_,      hbide_ideThemeColorCSS( "MenuPop", 1 ) )
      aadd( txt_, '}                                                                            ' )
      ELSE
      aadd( txt_, 'QMenu {                                                                      ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, '}                                                                            ' )
      ENDIF
      aadd( txt_, 'QMenu::item {                                                                ' )
//      aadd( txt_, '    spacing      : 3px; /* spacing between menu bar items */                 ' )
      aadd( txt_, '    margin       : 1px;                                                      ' )
      aadd( txt_, '    padding      : 1px 24px;                                                 ' )
//      aadd( txt_, '    background   : transparent;                                              ' )
//      aadd( txt_, '    border-radius: 4px;                                                      ' )
      aadd( txt_, '    color        : #000000;                                                  ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QMenu::item:selected { /* when selected using mouse or keyboard */           ' )
      aadd( txt_, '    background: #a8a8a8;                                                     ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QMenu::item:pressed {                                                        ' )
      aadd( txt_, '    background: #888888;                                                     ' )
      aadd( txt_, '}                                                                            ' )

   CASE cWidget == "QMenu"

      aadd( txt_, ' QMenu {                                                                     ' )
      aadd( txt_, '     background-color: white;                                                ' )
      aadd( txt_, '     margin: 2px; /* some spacing around the menu */                         ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::item {                                                               ' )
      aadd( txt_, '     padding: 2px 25px 2px 20px;                                             ' )
      aadd( txt_, '     border: 1px solid transparent; /* reserve space for selection border */ ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::item:selected {                                                      ' )
      aadd( txt_, '     border-color: darkblue;                                                 ' )
      aadd( txt_, '     background: rgba(100, 100, 100, 150);                                   ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::icon:checked { /* appearance of a "checked" icon */                  ' )
      aadd( txt_, '     background: gray;                                                       ' )
      aadd( txt_, '     border: 1px inset gray;                                                 ' )
      aadd( txt_, '     position: absolute;                                                     ' )
      aadd( txt_, '     top: 1px;                                                               ' )
      aadd( txt_, '     right: 1px;                                                             ' )
      aadd( txt_, '     bottom: 1px;                                                            ' )
      aadd( txt_, '     left: 1px;                                                              ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::separator {                                                          ' )
      aadd( txt_, '     height: 2px;                                                            ' )
      aadd( txt_, '     background: lightblue;                                                  ' )
      aadd( txt_, '     margin-left: 10px;                                                      ' )
      aadd( txt_, '     margin-right: 5px;                                                      ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator {                                                          ' )
      aadd( txt_, '     width: 13px;                                                            ' )
      aadd( txt_, '     height: 13px;                                                           ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:non-exclusive:unchecked {                                  ' )
      aadd( txt_, '     image: url(:/images/checkbox_unchecked.png);                            ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:non-exclusive:unchecked:selected {                         ' )
      aadd( txt_, '     image: url(:/images/checkbox_unchecked_hover.png);                      ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:non-exclusive:checked {                                    ' )
      aadd( txt_, '     image: url(:/images/checkbox_checked.png);                              ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:non-exclusive:checked:selected {                           ' )
      aadd( txt_, '     image: url(:/images/checkbox_checked_hover.png);                        ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:exclusive:unchecked {                                      ' )
      aadd( txt_, '     image: url(:/images/radiobutton_unchecked.png);                         ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:exclusive:unchecked:selected {                             ' )
      aadd( txt_, '     image: url(:/images/radiobutton_unchecked_hover.png);                   ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:exclusive:checked {                                        ' )
      aadd( txt_, '     image: url(:/images/radiobutton_checked.png);                           ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QMenu::indicator:exclusive:checked:selected {                               ' )
      aadd( txt_, '     image: url(:/images/radiobutton_checked_hover.png);                     ' )
      aadd( txt_, ' }                                                                           ' )

   CASE cWidget == "QMenuBar"

      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QMenuBar {                                                                   ' )
      aadd( txt_,      hbide_ideThemeColorCSS( "MenubarTop", 1 ) )
      aadd( txt_, '}                                                                            ' )
      ELSE
      aadd( txt_, 'QMenuBar {                                                                   ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, '}                                                                            ' )
      ENDIF
      aadd( txt_, 'QMenuBar::item {                                                             ' )
      aadd( txt_, '    spacing      : 3px; /* spacing between menu bar items */                 ' )
      aadd( txt_, '    padding      : 1px 10px;                                                 ' )
      aadd( txt_, '    background   : transparent;                                              ' )
      aadd( txt_, '    border-radius: 4px;                                                      ' )
      aadd( txt_, '    color        : #000000;                                                  ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QMenuBar::item:selected { /* when selected using mouse or keyboard */        ' )
      aadd( txt_, '    background: #a8a8a8;                                                     ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QMenuBar::item:pressed {                                                     ' )
      aadd( txt_, '    background: #888888;                                                     ' )
      aadd( txt_, '}                                                                            ' )

   CASE cWidget == "QToolBar"
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_,      hbide_ideThemeColorCSS( "ToolbarTop", 1 ) )
      aadd( txt_, '     /*spacing: 3px;  spacing between items in the tool bar */               ' )
      aadd( txt_, ' }                                                                           ' )
      ELSE
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, ' }                                                                           ' )
      ENDIF

   CASE cWidget == "QToolBarLR"
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_, '    background-color: qlineargradient(x1:1, y1:0, x2:0, y2:0,                ' )
      aadd( txt_, '                                      stop:0 lightgray, stop:1 darkgray);    ' )
      aadd( txt_, ' }                                                                           ' )
      ELSE
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, ' }                                                                           ' )
      ENDIF

   CASE cWidget == "QToolBarLR5"
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_,      hbide_ideThemeColorCSS( "ToolbarLR", 1 ) )
    * aadd( txt_, '    spacing: 1px; color: white; margin-top: 2px;                             ' )
      aadd( txt_, ' }                                                                           ' )
      ELSE
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
    * aadd( txt_, '    spacing: 1px; color: white; margin-top: 2px;                             ' )
      aadd( txt_, ' }                                                                           ' )
      ENDIF


   CASE cWidget == "QToolBarRL"
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_, '    background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0,                ' )
      aadd( txt_, '                                      stop:0 lightgray, stop:1 darkgray);    ' )
      aadd( txt_, '    /*spacing: 3px;  spacing between items in the tool bar */                ' )
      aadd( txt_, ' }                                                                           ' )
      ELSE
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, ' }                                                                           ' )
      ENDIF

   CASE cWidget == "QToolBarRL5"
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_, '    background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0,                ' )
      aadd( txt_, '                                      stop:0 lightgray, stop:1 darkgray);    ' )
      aadd( txt_, '    spacing: 1px; color: white; margin-top: 2px;                             ' )
      aadd( txt_, ' }                                                                           ' )
      ELSE
      aadd( txt_, 'QToolBar {                                                                   ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, ' }                                                                           ' )
      ENDIF

   CASE cWidget == "QTreeWidgetHB"

      aadd( txt_, ' QTreeWidget {                                                               ' )
      aadd( txt_, '     alternate-background-color: yellow;                                     ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget {                                                               ' )
      aadd( txt_, '     show-decoration-selected: 1;                                            ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget::item:alternate {                                               ' )
      aadd( txt_, '     background: #EEEEEE;                                                    ' )
      aadd( txt_, ' }                                                                           ' )
    * aadd( txt_, ' QTreeWidget::item:selected {                                                ' )
    * aadd( txt_, '     border: 1px solid #6a6ea9;                                              ' )
    * aadd( txt_, ' }                                                                           ' )

   CASE cWidget == "QTreeWidget"

      aadd( txt_, ' QTreeWidget {                                                               ' )
      aadd( txt_, '     alternate-background-color: yellow;                                     ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget {                                                               ' )
      aadd( txt_, '     show-decoration-selected: 1;                                            ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget::item:alternate {                                               ' )
      aadd( txt_, '     background: #EEEEEE;                                                    ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget::item:selected {                                                ' )
      aadd( txt_, '     border: 1px solid #6a6ea9;                                              ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget::item:selected:!active {                                        ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #ABAFE5, stop: 1 #8588B2);          ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget::item:selected:active {                                         ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #6a6ea9, stop: 1 #888dd9);          ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTreeWidget::item:hover {                                                   ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #FAFBFE, stop: 1 #DCDEF1);          ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, ' QTreeWidget {                                                               ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 0,                 ' )
      aadd( txt_, '       stop: 0 rgba(173, 173, 173, 255), stop:1 rgba(255, 255, 255, 255));   ' )
      aadd( txt_, '}                                                                            ' )

   CASE cWidget == "QListView"

      aadd( txt_, ' QListView {                                                                 ' )
      aadd( txt_, '     alternate-background-color: yellow;                                     ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QListView {                                                                 ' )
      aadd( txt_, '     show-decoration-selected: 1;                                            ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QListView::item:alternate {                                                 ' )
      aadd( txt_, '     background: #EEEEEE;                                                    ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QListView::item:selected {                                                  ' )
      aadd( txt_, '     border: 1px solid #6a6ea9;                                              ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QListView::item:selected:!active {                                          ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #ABAFE5, stop: 1 #8588B2);          ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QListView::item:selected:active {                                           ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #6a6ea9, stop: 1 #888dd9);          ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QListView::item:hover {                                                     ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #FAFBFE, stop: 1 #DCDEF1);          ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, ' QListView {                                                                 ' )
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, '    background: qlineargradient(spread:pad, x1:0.755727, y1:0.864, x2:1, y2:0,' )
      aadd( txt_, '           stop:0 rgba(214, 209, 142, 255), stop:1 rgba(255, 255, 255, 255));' )
      ELSE
      aadd( txt_, '    background: qlineargradient(x1: 1, y1: 0, x2: 0, y2: 0,                  ' )
      aadd( txt_, '       stop: 0 rgba(173, 173, 173, 255), stop:1 rgba(255, 255, 255, 255));   ' )
      ENDIF
      aadd( txt_, '}                                                                            ' )

   CASE cWidget == "QMainWindow"

      aadd( txt_, 'QMainWindow::separator {                                                     ' )
      aadd( txt_, '    background: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 0,                  ' )
      aadd( txt_, '       stop: 0 rgba(123, 123, 123, 255), stop:1 rgba(255, 255, 255, 255));   ' )
      aadd( txt_, '    width: 6px; /* when vertical */                                          ' )
      aadd( txt_, '    height: 6px; /* when horizontal */                                       ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QMainWindow::separator:hover {                                               ' )
      aadd( txt_, '    background: rgb(100,100,100);                                            ' )
      aadd( txt_, '}                                                                            ' )

   CASE cWidget == "QStatusBar"
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_, 'QStatusBar {                                                                 ' )
      aadd( txt_,      hbide_ideThemeColorCSS( "statusbar", 1 ) )
      aadd( txt_, ' }                                                                           ' )
      ELSE
      aadd( txt_, 'QStatusBar {                                                                 ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, ' }                                                                           ' )
      ENDIF

   CASE cWidget == "QTabWidget"
      IF nMode == HBIDE_ANIMATION_GRADIENT

      aadd( txt_, 'QTabWidget::pane { /* The tab widget frame */                                ' )
      aadd( txt_, '    border-top: 2px solid #C2C7CB;                                           ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QTabWidget::tab-bar {                                                        ' )
      aadd( txt_, '    background: qlineargradient(x1:0, y1:1, x2:0, y2:0,                      ' )
      aadd( txt_, '                                stop:0 lightgray, stop:1 darkgray);          ' )
      aadd( txt_,      hbide_ideThemeColorCSS( "TabBar", 1 ) )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QTabBar::tab {                                                               ' )
      aadd( txt_,      hbide_ideThemeColorCSS( "Tab", 1 ) )
      aadd( txt_, '    border: 2px solid #94C4C3;                                               ' )
      aadd( txt_, '    border-bottom-color: #C2C7CB; /* same as the pane color */               ' )
      aadd( txt_, '    border-top-left-radius: 6px;                                             ' )
      aadd( txt_, '    border-top-right-radius: 6px;                                            ' )
      aadd( txt_, '    padding-left: 2px;                                                       ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, ' QTabBar::tab:selected {                                                     ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #fafafa, stop: 0.4 #f4f4f4,         ' )
      aadd( txt_, '                                 stop: 0.5 #e7e7e7, stop: 1.0 #fafafa);      ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTabBar::tab:selected:hover {                                               ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 #fafafa, stop: 0.4 #f4f4f4,         ' )
      aadd( txt_, '                                 stop: 0.5 #e7e7e7, stop: 1.0 #fafafa);      ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTabBar::tab:hover {                                                        ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 yellow, stop: 0.4 #f4f4f4,          ' )
      aadd( txt_, '                                 stop: 0.5 #e7e7e7, stop: 1.0 orange);       ' )
      aadd( txt_, ' }                                                                           ' )

      ELSE

      aadd( txt_, 'QTabWidget::pane { /* The tab widget frame */                                ' )
      aadd( txt_, '    border-top: 2px solid #C2C7CB;                                           ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QTabWidget::tab-bar {                                                        ' )
      aadd( txt_, '    /* left: 5px; move to the right by 5px */                                ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QTabBar {                                                                    ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, 'QTabBar::tab {                                                               ' )
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      aadd( txt_, '    border: 2px solid #94C4C3;                                               ' )
      aadd( txt_, '    border-bottom-color: #C2C7CB; /* same as the pane color */               ' )
      aadd( txt_, '    border-top-left-radius: 6px;                                             ' )
      aadd( txt_, '    border-top-right-radius: 6px;                                            ' )
      aadd( txt_, '    padding-left: 2px;                                                       ' )
      aadd( txt_, '}                                                                            ' )
      aadd( txt_, ' QTabBar::tab:selected {                                                     ' )
      aadd( txt_, '     background: rgb( 230,230,230 );                                         ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTabBar::tab:selected:hover {                                               ' )
      aadd( txt_, '     background: yellow;                                                     ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, ' QTabBar::tab:hover {                                                        ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                 ' )
      aadd( txt_, '                                 stop: 0 yellow, stop: 0.4 #f4f4f4,          ' )
      aadd( txt_, '                                 stop: 0.5 #e7e7e7, stop: 1.0 orange);       ' )
      aadd( txt_, ' }                                                                           ' )

      ENDIF
   CASE cWidget == "QPlainTextEdit"

      aadd( txt_, ' QPlainTextEdit {                                                            ' )
      aadd( txt_, '    background: qlineargradient(x1:0, y1:0, x2:0, y2:1,                      ' )
      aadd( txt_, '                                stop:0 white, stop:1 lightblue);             ' )
      aadd( txt_, ' }                                                                           ' )

   CASE cWidget == "QDockWidget"

      aadd( txt_, 'QDockWidget {                                                                ' )
      aadd( txt_, '    border: 1px solid darkgray;                                              ' )
      aadd( txt_, ' }                                                                           ' )
      aadd( txt_, 'QDockWidget::title {                                                         ' )
      IF nMode == HBIDE_ANIMATION_GRADIENT
      aadd( txt_,      hbide_ideThemeColorCSS( "DockWidget", 1 ) )
      ELSE
      aadd( txt_,      hbide_cssColorString( "bg-std" ) )
      ENDIF
      aadd( txt_, '    padding-left: 10px;                                                      ' )
      aadd( txt_, '    padding-top: 4px;                                                        ' )
      aadd( txt_, ' }                                                                           ' )

   CASE cWidget == "PathIsWrong"
      aadd( txt_, "background-color: rgba( 240,120,120,255 );"                                    )

   ENDCASE

   s := ""

   aeval( txt_, {|e| s += trim( e ) + chr( 13 ) + chr( 10 ) } )

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION hbide_cssColorString( cPart )
   LOCAL cStr := ""

   SWITCH lower( cPart )
   CASE "bg-std"
      RETURN ' background-color: ' + hbide_rgbString( 212,208,200 ) + ';'              // Gray - original wondows
   #if 0
   // RETURN ' background-color: ' + hbide_rgbString( 199,212,231 ) + ';'
      RETURN  '    background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
                 hbide_buildGradientString( hbide_loadIdeTheme( 2 ) ) + "); "
   #endif
   ENDSWITCH

   RETURN cStr

/*----------------------------------------------------------------------*/

FUNCTION hbide_rgbString( nR, nG, nB )
   IF HB_ISARRAY( nR )
      RETURN "rgb(" + hb_ntos( nR[ 1 ] ) + "," + hb_ntos( nR[ 2 ] ) + "," + hb_ntos( nR[ 3 ] ) + ")"
   ELSE
      RETURN "rgb(" + hb_ntos( nR ) + "," + hb_ntos( nG ) + "," + hb_ntos( nB ) + ")"
   ENDIF
   RETURN ""

/*----------------------------------------------------------------------*/

FUNCTION hbide_buildGradientString( aGrands )
   LOCAL a_, s := ""

   FOR EACH a_ IN aGrands
      s += "stop:" + hb_ntos( a_[ 1 ] ) + " " + hbide_rgbString( a_[ 2 ], a_[ 3 ], a_[ 4 ] ) + ", "
   NEXT

   RETURN substr( s, 1, Len( s ) - 2 )

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_loadIdeTheme( nTheme )
   // Theme values can be made outer
   //
   DO CASE
   CASE nTheme == 1
      IF empty( hbide_setAppTheme() )
         RETURN { {0,255,255,255}, {0.25,219,230,244}, {0.5,201,217,237}, {0.75,231,242,255} }
      ELSE
         RETURN hbide_setAppTheme()
      ENDIF
   CASE nTheme == 2
      RETURN { {0,173,185,207}, {1,199,212,231} }
   ENDCASE

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_ideThemeColorCSS( cPart, nTheme )
   LOCAL cCSS := ""

   cPart := lower( cPart )

   DO CASE
   CASE cPart == "menupop"
      cCSS := '    background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "menubartop"
      cCSS := '    background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "toolbartop"
      cCSS := '    background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "toolbarlr"
      cCSS := ' background-color: qlineargradient(x1:1, y1:0, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "dockwidget"
      cCSS := ' background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "tabbar"
      cCSS := ' background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "statusbar"
      cCSS := ' background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "tab"
      cCSS := ' background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "tabselected"
      cCSS := ' background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   CASE cPart == "tabselectedhover"
      cCSS := ' background-color: qlineargradient(x1:0, y1:1, x2:0, y2:0, ' + ;
              hbide_buildGradientString( hbide_loadIdeTheme( nTheme ) ) + "); "
   ENDCASE

   RETURN cCSS

/*----------------------------------------------------------------------*/
