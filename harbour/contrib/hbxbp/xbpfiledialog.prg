/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ Compatible xbpFileDialog Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              02Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define QFileDialog_AcceptOpen                           0
#define QFileDialog_AcceptSave                           1

// enum #define QFileDialog_DialogLabel
//
#define QFileDialog_LookIn                               0
#define QFileDialog_FileName                             1
#define QFileDialog_FileType                             2
#define QFileDialog_Accept                               3
#define QFileDialog_Reject                               4

// enum #define QFileDialog_FileMode
// This enum is used to indicate what the user may select in the file dialog;
// i.e. what the dialog will return if the user clicks OK.
//
#define QFileDialog_AnyFile                              0   // The name of a file, whether it exists or not.
#define QFileDialog_ExistingFile                         1   // The name of a single existing file.
#define QFileDialog_Directory                            2   // The name of a directory. Both files and directories are displayed.
#define QFileDialog_ExistingFiles                        3   // The names of zero or more existing files.

// The Options type is a typedef for QFlags<Option>. It stores an OR combination of Option values.
//
#define QFileDialog_ShowDirsOnly                         0x00000001   // Only show directories in the file dialog. By default both files and directories are shown. (Valid only in the Directory file mode.)
#define QFileDialog_DontResolveSymlinks                  0x00000002   // Don't resolve symlinks in the file dialog. By default symlinks are resolved.
#define QFileDialog_DontConfirmOverwrite                 0x00000004   // Don't ask for confirmation if an existing file is selected. By default confirmation is requested.
#define QFileDialog_DontUseNativeDialog                  0x00000010   // Don't use the native file dialog. By default on Mac OS X and Windows, the native file dialog is used.
#define QFileDialog_ReadOnly                             0x00000020   // Indicates that the model is readonly.
#define QFileDialog_HideNameFilterDetails                0x00000040   // Indicates if the is hidden or not.

//enum QFileDialog::ViewMode
//This enum describes the view mode of the file dialog; i.e. what information about each file will be displayed.
//
#define QFileDialog_Detail                               0   // Displays an icon, a name, and details for each item in the directory.
#define QFileDialog_List                                 1   // Displays only an icon and a name for each item in the directory.

#define QDialog_Accepted                                 1
#define QDialog_Rejected                                 0


/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpFileDialog INHERIT XbpWindow

   DATA     bufferSize                            INIT 1024
   DATA     center                                INIT .f.
   DATA     defExtension                          INIT " "
   DATA     fileFilters                           INIT {}
   DATA     noWriteAccess                         INIT .f.
   DATA     openReadOnly                          INIT NIL  // .f.
   DATA     restoreDir                            INIT .f.
   DATA     title                                 INIT NIL  // ""
   DATA     validatePath                          INIT .f.

   METHOD   new()
   METHOD   create()
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()
   METHOD   exeBlock()

   METHOD   open()
   METHOD   saveAs()
   METHOD   extractFileNames()
   METHOD   setStyle()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:new( oParent, oOwner, aPos )

   ::xbpWindow:INIT( oParent, oOwner, aPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:create( oParent, oOwner, aPos )

   ::xbpWindow:create( oParent, oOwner, aPos )

   ::oWidget := QFileDialog():new( ::pParent )
   //::oWidget:setStyle( AppDesktop():style() )
   ::setStyle()
   //::setColorBG( GraMakeRGBColor( { 255,255,255 } ) )
   //::setColorFG( GraMakeRGBColor( { 0,0,0 } ) )

   ::connect( ::pWidget, "accepted()"                , {|o,p| ::exeBlock( 1, p, o ) } )
   ::connect( ::pWidget, "finished(int)"             , {|o,p| ::exeBlock( 2, p, o ) } )
   ::connect( ::pWidget, "rejected()"                , {|o,p| ::exeBlock( 3, p, o ) } )
   ::connect( ::pWidget, "currentChanged(QString)"   , {|o,p| ::exeBlock( 4, p, o ) } )
   ::connect( ::pWidget, "directoryEntered(QString)" , {|o,p| ::exeBlock( 5, p, o ) } )
   ::connect( ::pWidget, "fileSelected(QString)"     , {|o,p| ::exeBlock( 6, p, o ) } )
   ::connect( ::pWidget, "filesSelected(QStringList)", {|o,p| ::exeBlock( 7, p, o ) } )
   ::connect( ::pWidget, "filterSelected(QString)"   , {|o,p| ::exeBlock( 8, p, o ) } )

   ::oParent:addChild( Self )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:exeBlock( nEvent, p1 )
   LOCAL nRet := XBP_ALLOW

   HB_SYMBOL_UNUSED( p1 )

   DO CASE
   CASE nEvent == 3
      IF hb_isBlock( ::sl_quit )
         nRet := eval( ::sl_quit, 0, 0, Self )
      ENDIF
      IF nRet == XBP_REJECT
         ::oWidget:reject()
      ELSE
         ::oWidget:accept()
      ENDIF
   ENDCASE

   RETURN nRet

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:destroy()

   ::xbpWindow:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION Xbp_ArrayToFileFilter( aFilter )

   RETURN ( aFilter[ 1 ] + " ( "+ aFilter[ 2 ] + " )" )

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:open( cDefaultFile, lCenter, lAllowMultiple, lCreateNewFiles )
   LOCAL cFiles := NIL
   LOCAL i, oList, nResult

   HB_SYMBOL_UNUSED( lCreateNewFiles )

   DEFAULT lAllowMultiple TO .F.

   IF !( hb_isLogical( lCenter ) )
      lCenter := ::center
   ENDIF

   ::oWidget:setAcceptMode( QFileDialog_AcceptOpen )

   IF lAllowMultiple
      ::oWidget:setFileMode( QFileDialog_ExistingFiles )
   ENDIF

   IF !empty( ::defExtension )
      ::oWidget:setDefaultSuffix( ::defExtension )
   ENDIF

   IF hb_isChar( ::title )
      ::oWidget:setWindowTitle( ::title )
   ENDIF

   IF hb_isChar( cDefaultFile )
      ::oWidget:setDirectory( cDefaultFile )
   ENDIF

   IF empty( ::fileFilters )
      ::oWidget:setNameFilter( "All File (*.*)" )
   ELSE
      IF len( ::fileFilters ) == 1
         ::oWidget:setNameFilter( Xbp_ArrayToFileFilter( ::fileFilters[ 1 ] ) )
      ELSE
         oList := QStringList():new()
         FOR i := 1 TO len( ::fileFilters )
            oList:append( Xbp_ArrayToFileFilter( ::fileFilters[ i ] ) )
         NEXT
         ::oWidget:setNameFilters( QT_PTROF( oList ) )
      ENDIF
   ENDIF

   IF hb_isLogical( ::openReadOnly )
      ::oWidget:setOption( QFileDialog_ReadOnly, .T. )
   ENDIF

   IF !( lCenter )
      ::setPos()
   ENDIF

   nResult := ::oWidget:exec()

   RETURN IF( nResult == QDialog_Accepted, ::extractFileNames( lAllowMultiple ), NIL )

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:saveAs( cDefaultFile, lFileList, lCenter )
   LOCAL nResult

   DEFAULT lFileList TO .T.

   IF !( hb_isLogical( lCenter ) )
      lCenter := ::center
   ENDIF

   ::oWidget:setAcceptMode( QFileDialog_AcceptSave )

   IF !empty( ::defExtension )
      ::oWidget:setDefaultSuffix( ::defExtension )
   ENDIF

   IF hb_isChar( ::title )
      ::oWidget:setWindowTitle( ::title )
   ENDIF

   IF hb_isChar( cDefaultFile )
      ::oWidget:setDirectory( cDefaultFile )
   ENDIF

//   oStyle := QApplication():style()
//   ::oWidget:setStyle( oStyle )
   ::setStyle()

   IF !( lCenter )
      ::setPos()
   ENDIF
   nResult := ::oWidget:exec()

   RETURN IF( nResult == QDialog_Accepted, ::extractFileNames(), NIL )

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:extractFileNames( lAllowMultiple )
   LOCAL oFiles, i, f_:= {}

   DEFAULT lAllowMultiple TO .F.

   oFiles := QStringList()
   oFiles:pPtr := ::oWidget:selectedFiles()
   FOR i := 1 TO oFiles:size()
      aadd( f_, oFiles:at( i-1 ) )
   NEXT

   IF !( lAllowMultiple )
      f_:= f_[ 1 ]
   ENDIF

   RETURN f_

/*----------------------------------------------------------------------*/

METHOD XbpFileDialog:setStyle()
   LOCAL s := "", txt_:={}

   aadd( txt_, 'QDialog   { background-color: rgb(198,198,198); }' )
   aadd( txt_, 'QDialog   { color: rgb(0,0,0); }' )
   aadd( txt_, 'QLineEdit { background-color: rgb(255,255,255); }' )
   aadd( txt_, 'QAbstractScrollArea { background-color: rgb(240,240,240); }' )
   aadd( txt_, 'QLabel    { background-color: rgb(198,198,198); }' )

   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   ::oWidget:setStyleSheet( "" )
   //::oWidget:setStyleSheet( s )
   //::setColorBG( GraMakeRGBColor( { 100,100,100 } ) )

   RETURN self

/*----------------------------------------------------------------------*/

