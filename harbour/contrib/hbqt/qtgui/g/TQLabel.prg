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


FUNCTION QLabel( ... )
   RETURN HB_QLabel():new( ... )


CREATE CLASS QLabel INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QLabel

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  buddy                         // (  )                                               -> oQWidget
   METHOD  hasScaledContents             // (  )                                               -> lBool
   METHOD  indent                        // (  )                                               -> nInt
   METHOD  margin                        // (  )                                               -> nInt
   METHOD  movie                         // (  )                                               -> oQMovie
   METHOD  openExternalLinks             // (  )                                               -> lBool
   METHOD  picture                       // (  )                                               -> oQPicture
   METHOD  pixmap                        // (  )                                               -> oQPixmap
   METHOD  setAlignment                  // ( nQt::Alignment )                                 -> NIL
   METHOD  setBuddy                      // ( oQWidget )                                       -> NIL
   METHOD  setIndent                     // ( nInt )                                           -> NIL
   METHOD  setMargin                     // ( nInt )                                           -> NIL
   METHOD  setOpenExternalLinks          // ( lOpen )                                          -> NIL
   METHOD  setScaledContents             // ( lBool )                                          -> NIL
   METHOD  setTextFormat                 // ( nQt::TextFormat )                                -> NIL
   METHOD  setTextInteractionFlags       // ( nFlags )                                         -> NIL
   METHOD  setWordWrap                   // ( lOn )                                            -> NIL
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textFormat                    // (  )                                               -> nQt_TextFormat
   METHOD  textInteractionFlags          // (  )                                               -> nQt_TextInteractionFlags
   METHOD  wordWrap                      // (  )                                               -> lBool
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  setMovie                      // ( oQMovie )                                        -> NIL
   METHOD  setNum                        // ( nNum )                                           -> NIL
                                         // ( nNum )                                           -> NIL
   METHOD  setPicture                    // ( oQPicture )                                      -> NIL
   METHOD  setPixmap                     // ( oQPixmap )                                       -> NIL
   METHOD  setText                       // ( cQString )                                       -> NIL

   ENDCLASS


METHOD QLabel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLabel( ... )
   RETURN Self


METHOD QLabel:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:buddy( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QWidget():from( Qt_QLabel_buddy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:hasScaledContents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_hasScaledContents( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:indent( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_indent( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:margin( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_margin( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:movie( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QMovie():from( Qt_QLabel_movie( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:openExternalLinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_openExternalLinks( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:picture( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QPicture():from( Qt_QLabel_picture( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:pixmap( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QPixmap():from( Qt_QLabel_pixmap( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setBuddy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setBuddy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setIndent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setIndent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setMargin( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setMargin( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setOpenExternalLinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setOpenExternalLinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setScaledContents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setScaledContents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setTextInteractionFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setTextInteractionFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setWordWrap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setWordWrap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_text( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:textFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_textFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:textInteractionFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_textInteractionFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:wordWrap( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_wordWrap( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLabel_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setMovie( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setMovie( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setNum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setNum( ::pPtr, ... )
         // RETURN Qt_QLabel_setNum_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setPicture( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setPicture( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setPixmap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setPixmap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLabel:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLabel_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

