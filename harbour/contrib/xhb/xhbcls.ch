/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for cross-compatibility with xhb class code extensions
 *
 * Warning: using this functionality may break logical inheritance
 *          scheme or even some internal class definitions in both
 *          Harbour and xHarbour compilers. In Harbour it's unsupported
 *          feature added here only for strict XHB compatibility.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef XHB_CLS__CH_
#define XHB_CLS__CH_

#ifdef __HARBOUR__

#ifndef __XHARBOUR__

   #include "hboo.ch"

   #xtranslate __xhb_cls_scope( <scope>, .t. ) => <scope> + HB_OO_CLSTP_PERSIST
   #xtranslate __xhb_cls_scope( <scope>, .f. ) => <scope>
   #xtranslate __xhb_cls_scope( , .t. ) => HB_OO_CLSTP_PERSIST
   #xtranslate __xhb_cls_scope( , .f. ) =>

   #xcommand OVERRIDE METHOD <!Message!> [IN] CLASS <!Class!> ;
                             WITH [METHOD] <!Method!> [SCOPE <Scope>] => ;
            __clsModMsg( <Class>():classH, #<Message>, @<Method>() )


   #xcommand EXTEND CLASS <!Class!> WITH <data: DATA, VAR> <!VarName!> ;
                          [SCOPE <scope>] [<persist: PERSISTENT>] ;
                          [<case: NOUPPER>] => ;
            __clsAddMsg( <Class>():classH, #<VarName>, ;
                         __cls_IncData( <Class>():classH ), ;
                         HB_OO_MSG_PROPERTY, NIL, ;
                         __xhb_cls_scope( <scope>, <.persist.> ) )


   #xcommand EXTEND CLASS <!Class!> WITH METHOD <!Method!> [SCOPE <scope>] ;
                          [<persist: PERSISTENT>] [<case: NOUPPER>] => ;
            __clsAddMsg( <Class>():classH, #<Method>, @<Method>(), ;
                         HB_OO_MSG_METHOD, NIL, ;
                         __xhb_cls_scope( <scope>, <.persist.> ) )


   #xcommand EXTEND CLASS <!Class!> WITH MESSAGE <!Message!> METHOD <!Method!> ;
                          [SCOPE <scope>] [<persist: PERSISTENT>] ;
                          [<case: NOUPPER>] => ;
            __clsAddMsg( <Class>():classH, #<Message>, @<Method>(), ;
                         HB_OO_MSG_METHOD, NIL, ;
                         __xhb_cls_scope( <scope>, <.persist.> ) )


   #xcommand EXTEND CLASS <!Class!> WITH MESSAGE <!Message!> INLINE <code,...> ;
                          [SCOPE <scope>] [<persist: PERSISTENT>] ;
                          [<case: NOUPPER>] => ;
            __clsAddMsg( <Class>():classH, #<Message>, ;
                         {| Self | HB_SYMBOL_UNUSED( Self ), <code> }, ;
                         HB_OO_MSG_INLINE, NIL, ;
                         __xhb_cls_scope( <scope>, <.persist.> ) )


   #xcommand EXTEND CLASS <!Class!> WITH MESSAGE <Message>( <params,...> ) ;
                          INLINE <code,...> ;
                          [SCOPE <scope>] [<persist: PERSISTENT>] ;
                          [<case: NOUPPER>] => ;
            __clsAddMsg( <Class>():classH, #<Message>, ;
                         {| Self, <params> | HB_SYMBOL_UNUSED( Self ), <code> }, ;
                         HB_OO_MSG_INLINE, NIL, ;
                         __xhb_cls_scope( <scope>, <.persist.> ) )


#endif /* __XHARBOUR__ */

#endif /* __HARBOUR__ */

#endif /* XHB_CLS__CH_ */
