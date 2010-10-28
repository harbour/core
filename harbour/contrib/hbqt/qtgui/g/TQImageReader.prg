/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QImageReader( ... )
   RETURN HB_QImageReader():new( ... )

FUNCTION QImageReaderFromPointer( ... )
   RETURN HB_QImageReader():fromPointer( ... )


CREATE CLASS QImageReader INHERIT HbQtObjectHandler FUNCTION HB_QImageReader

   METHOD  new( ... )

   METHOD  autoDetectImageFormat         // (  )                                               -> lBool
   METHOD  backgroundColor               // (  )                                               -> oQColor
   METHOD  canRead                       // (  )                                               -> lBool
   METHOD  clipRect                      // (  )                                               -> oQRect
   METHOD  currentImageNumber            // (  )                                               -> nInt
   METHOD  currentImageRect              // (  )                                               -> oQRect
   METHOD  device                        // (  )                                               -> oQIODevice
   METHOD  error                         // (  )                                               -> nImageReaderError
   METHOD  errorString                   // (  )                                               -> cQString
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  format                        // (  )                                               -> oQByteArray
   METHOD  imageCount                    // (  )                                               -> nInt
   METHOD  imageFormat                   // (  )                                               -> nQImage_Format
   METHOD  jumpToImage                   // ( nImageNumber )                                   -> lBool
   METHOD  jumpToNextImage               // (  )                                               -> lBool
   METHOD  loopCount                     // (  )                                               -> nInt
   METHOD  nextImageDelay                // (  )                                               -> nInt
   METHOD  quality                       // (  )                                               -> nInt
   METHOD  read                          // (  )                                               -> oQImage
                                         // ( oQImage )                                        -> lBool
   METHOD  scaledClipRect                // (  )                                               -> oQRect
   METHOD  scaledSize                    // (  )                                               -> oQSize
   METHOD  setAutoDetectImageFormat      // ( lEnabled )                                       -> NIL
   METHOD  setBackgroundColor            // ( oQColor )                                        -> NIL
   METHOD  setClipRect                   // ( oQRect )                                         -> NIL
   METHOD  setDevice                     // ( oQIODevice )                                     -> NIL
   METHOD  setFileName                   // ( cFileName )                                      -> NIL
   METHOD  setFormat                     // ( oQByteArray )                                    -> NIL
   METHOD  setQuality                    // ( nQuality )                                       -> NIL
   METHOD  setScaledClipRect             // ( oQRect )                                         -> NIL
   METHOD  setScaledSize                 // ( oQSize )                                         -> NIL
   METHOD  size                          // (  )                                               -> oQSize
   METHOD  supportsAnimation             // (  )                                               -> lBool
   METHOD  supportsOption                // ( nOption )                                        -> lBool
   METHOD  text                          // ( cKey )                                           -> cQString
   METHOD  textKeys                      // (  )                                               -> oQStringList
                                         // ( cFileName )                                      -> oQByteArray
                                         // ( oQIODevice )                                     -> oQByteArray
   METHOD  supportedImageFormats         // (  )                                               -> oQList_QByteArray>

   ENDCLASS


METHOD QImageReader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QImageReader( ... )
   RETURN Self


METHOD QImageReader:autoDetectImageFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_autoDetectImageFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:backgroundColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QImageReader_backgroundColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:canRead( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_canRead( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:clipRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QImageReader_clipRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:currentImageNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_currentImageNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:currentImageRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QImageReader_currentImageRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:device( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIODeviceFromPointer( Qt_QImageReader_device( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_error( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:errorString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_errorString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:format( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QImageReader_format( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:imageCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_imageCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:imageFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QImageReader_imageFormat_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QImageReader_imageFormat_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QImageReader_imageFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:jumpToImage( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_jumpToImage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:jumpToNextImage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_jumpToNextImage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:loopCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_loopCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:nextImageDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_nextImageDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:quality( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_quality( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_read_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN QImageFromPointer( Qt_QImageReader_read( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:scaledClipRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QImageReader_scaledClipRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:scaledSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QImageReader_scaledSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setAutoDetectImageFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setAutoDetectImageFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setBackgroundColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setBackgroundColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setClipRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setClipRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setDevice( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setDevice( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setQuality( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setQuality( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setScaledClipRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setScaledClipRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:setScaledSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_setScaledSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QImageReader_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:supportsAnimation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QImageReader_supportsAnimation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:supportsOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_supportsOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:text( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QImageReader_text( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:textKeys( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QImageReader_textKeys( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QImageReader:supportedImageFormats( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QImageReader_supportedImageFormats( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

