/*
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbhpdf.h"

/* HPDF_LinkAnnot_SetHighlightMode( hAnnot, nHilightMode ) -> hStatus
       nHilightMode ==
   HPDF_ANNOT_NO_HIGHTLIGHT       1     No highlighting.
   HPDF_ANNOT_INVERT_BOX          2     Invert the contents of the area of annotation.
   HPDF_ANNOT_INVERT_BORDER       3     Invert the annotation's border.
   HPDF_ANNOT_DOWN_APPEARANCE     4     Dent the annotation.
 */
HB_FUNC( HPDF_LINKANNOT_SETHIGHLIGHTMODE )
{
   hb_retnl( ( long ) HPDF_LinkAnnot_SetHighlightMode( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_AnnotHighlightMode ) hb_parni( 2 ) ) );
}

/* HPDF_LinkAnnot_SetBorderStyle( hAnnot, nWidth, nDashOn, nDashOff ) -> hStatus
 */
HB_FUNC( HPDF_LINKANNOT_SETBORDERSTYLE )
{
   hb_retnl( ( long ) HPDF_LinkAnnot_SetBorderStyle( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ), ( HPDF_UINT16 ) hb_parni( 3 ), ( HPDF_UINT16 ) hb_parni( 4 ) ) );
}

/* HPDF_TextAnnot_SetIcon( hAnnot, nIconID ) -> hStatus
       nIconID
   HPDF_ANNOT_ICON_COMMENT
   HPDF_ANNOT_ICON_KEY
   HPDF_ANNOT_ICON_NOTE
   HPDF_ANNOT_ICON_HELP
   HPDF_ANNOT_ICON_NEW_PARAGRAPH
   HPDF_ANNOT_ICON_PARAGRAPH
   HPDF_ANNOT_ICON_INSERT
 */
HB_FUNC( HPDF_TEXTANNOT_SETICON )
{
   hb_retnl( ( long ) HPDF_TextAnnot_SetIcon( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_AnnotIcon ) hb_parni( 2 ) ) );
}

/* HPDF_TextAnnot_SetOpened( hAnnot, lOpened ) -> hStatus
 */
HB_FUNC( HPDF_TEXTANNOT_SETOPENED )
{
   hb_retnl( ( long ) HPDF_TextAnnot_SetOpened( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parl( 2 ) ? HPDF_TRUE : HPDF_FALSE ) );
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateFreeTextAnnot  (HPDF_Page       page,
                        HPDF_Rect       rect,
                        const char     *text,
                        HPDF_Encoder    encoder);
 */
HB_FUNC( HPDF_PAGE_CREATEFREETEXTANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateFreeTextAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateLineAnnot  (HPDF_Page       page,
                     const char     *text,
                     HPDF_Encoder    encoder);
 */
HB_FUNC( HPDF_PAGE_CREATELINEANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retptr( HPDF_Page_CreateLineAnnot( ( HPDF_Page ) hb_parptr( 1 ), hb_parc( 2 ), ( HPDF_Encoder ) hb_parptr( 3 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_Annotation
   HPDF_Page_CreateTextMarkupAnnot (HPDF_Page     page,
                        HPDF_Rect      rect,
                        const char     *text,
                        HPDF_Encoder   encoder,
                        HPDF_AnnotType subType);
 */
HB_FUNC( HPDF_PAGE_CREATETEXTMARKUPANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateTextMarkupAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ), ( HPDF_AnnotType ) hb_parni( 5 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateHighlightAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC( HPDF_PAGE_CREATEHIGHLIGHTANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateHighlightAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateUnderlineAnnot (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC( HPDF_PAGE_CREATEUNDERLINEANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateUnderlineAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateSquigglyAnnot  (HPDF_Page    page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC( HPDF_PAGE_CREATESQUIGGLYANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateSquigglyAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateStrikeOutAnnot  (HPDF_Page   page,
                        HPDF_Rect    rect,
                        const char   *text,
                        HPDF_Encoder encoder);
 */
HB_FUNC( HPDF_PAGE_CREATESTRIKEOUTANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateStrikeOutAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreatePopupAnnot  ( HPDF_Page    page,
                        HPDF_Rect          rect,
                        HPDF_Annotation      parent);
 */
HB_FUNC( HPDF_PAGE_CREATEPOPUPANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreatePopupAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, ( HPDF_Annotation ) hb_parptr( 3 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateStampAnnot  (   HPDF_Page           page,
                        HPDF_Rect           rect,
                        HPDF_StampAnnotName name,
                        const char*         text,
                        HPDF_Encoder      encoder);
 */
HB_FUNC( HPDF_PAGE_CREATESTAMPANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateStampAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, ( HPDF_StampAnnotName ) hb_parni( 3 ), hb_parc( 4 ), ( HPDF_Encoder ) hb_parptr( 5 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateSquareAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
 */
HB_FUNC( HPDF_PAGE_CREATESQUAREANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateSquareAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Annotation)
   HPDF_Page_CreateCircleAnnot (HPDF_Page          page,
                      HPDF_Rect          rect,
                      const char         *text,
                      HPDF_Encoder       encoder);
 */
HB_FUNC( HPDF_PAGE_CREATECIRCLEANNOT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retptr( HPDF_Page_CreateCircleAnnot( ( HPDF_Page ) hb_parptr( 1 ), rc, hb_parc( 3 ), ( HPDF_Encoder ) hb_parptr( 4 ) ) );
#else
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetRGBColor (HPDF_Annotation annot, HPDF_RGBColor color);
 */
HB_FUNC( HPDF_ANNOT_SETRGBCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_RGBColor rgb;

   rgb.r = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rgb.g = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rgb.b = ( HPDF_REAL ) hb_parvnd( 2, 3 );

   hb_retnl( ( long ) HPDF_Annot_SetRGBColor( ( HPDF_Annotation ) hb_parptr( 1 ), rgb ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetCMYKColor (HPDF_Annotation annot, HPDF_CMYKColor color);
 */
HB_FUNC( HPDF_ANNOT_SETCMYKCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_CMYKColor cmyk;

   cmyk.c = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   cmyk.m = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   cmyk.y = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   cmyk.k = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retnl( ( long ) HPDF_Annot_SetCMYKColor( ( HPDF_Annotation ) hb_parptr( 1 ), cmyk ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetGrayColor (HPDF_Annotation annot, HPDF_REAL color);
 */
HB_FUNC( HPDF_ANNOT_SETGRAYCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annot_SetGrayColor( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_SetNoColor (HPDF_Annotation annot);
 */
HB_FUNC( HPDF_ANNOT_SETNOCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annot_SetNoColor( ( HPDF_Annotation ) hb_parptr( 1 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetTitle (HPDF_Annotation annot, const char* name);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETTITLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetTitle( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parc( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetSubject (HPDF_Annotation annot, const char* name);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETSUBJECT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetSubject( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parc( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetCreationDate (HPDF_Annotation annot, HPDF_Date value);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETCREATIONDATE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Date date;

   memset( &date, 0, sizeof( date ) );

   date.year    = hb_parvni( 2, 1 );
   date.month   = hb_parvni( 2, 2 );
   date.day     = hb_parvni( 2, 3 );
   date.hour    = hb_parvni( 2, 4 );
   date.minutes = hb_parvni( 2, 5 );
   date.seconds = hb_parvni( 2, 6 );
   date.ind     = ' ';

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetCreationDate( ( HPDF_Annotation ) hb_parptr( 1 ), date ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetTransparency (HPDF_Annotation annot, HPDF_REAL value);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETTRANSPARENCY )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetTransparency( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetIntent (HPDF_Annotation  annot, HPDF_AnnotIntent  intent);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETINTENT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetIntent( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_AnnotIntent ) hb_parni( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetPopup (HPDF_Annotation annot, HPDF_Annotation popup);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETPOPUP )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetPopup( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_Annotation ) hb_parptr( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetRectDiff (HPDF_Annotation  annot, HPDF_Rect  rect);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETRECTDIFF )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Rect rc;

   rc.left   = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rc.top    = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rc.right  = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   rc.bottom = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetRectDiff( ( HPDF_Annotation ) hb_parptr( 1 ), rc ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetCloudEffect (HPDF_Annotation  annot, HPDF_INT cloudIntensity);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETCLOUDEFFECT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetCloudEffect( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_INT ) hb_parni( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorRGBColor (HPDF_Annotation  annot, HPDF_RGBColor color);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORRGBCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_RGBColor rgb;

   rgb.r = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   rgb.g = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   rgb.b = ( HPDF_REAL ) hb_parvnd( 2, 3 );

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorRGBColor( ( HPDF_Annotation ) hb_parptr( 1 ), rgb ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorCMYKColor (HPDF_Annotation  annot, HPDF_CMYKColor color);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORCMYKCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_CMYKColor cmyk;

   cmyk.c = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   cmyk.m = ( HPDF_REAL ) hb_parvnd( 2, 2 );
   cmyk.y = ( HPDF_REAL ) hb_parvnd( 2, 3 );
   cmyk.k = ( HPDF_REAL ) hb_parvnd( 2, 4 );

   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorCMYKColor( ( HPDF_Annotation ) hb_parptr( 1 ), cmyk ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorGrayColor (HPDF_Annotation  annot, HPDF_REAL color);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORGRAYCOLOR )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorGrayColor( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_REAL ) hb_parnd( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_MarkupAnnot_SetInteriorTransparent (HPDF_Annotation  annot);
 */
HB_FUNC( HPDF_MARKUPANNOT_SETINTERIORTRANSPARENT )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_MarkupAnnot_SetInteriorTransparent( ( HPDF_Annotation ) hb_parptr( 1 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_TextMarkupAnnot_SetQuadPoints ( HPDF_Annotation annot, HPDF_Point lb, HPDF_Point rb, HPDF_Point rt, HPDF_Point lt);
 */
HB_FUNC( HPDF_TEXTMARKUPANNOT_SETQUADPOINTS )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;
   HPDF_Point p3;
   HPDF_Point p4;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 3, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 3, 2 );

   p3.x = ( HPDF_REAL ) hb_parvnd( 4, 1 );
   p3.y = ( HPDF_REAL ) hb_parvnd( 4, 2 );

   p4.x = ( HPDF_REAL ) hb_parvnd( 5, 1 );
   p4.y = ( HPDF_REAL ) hb_parvnd( 5, 2 );

   hb_retnl( ( long ) HPDF_TextMarkupAnnot_SetQuadPoints( ( HPDF_Annotation ) hb_parptr( 1 ), p1, p2, p3, p4 ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annot_Set3DView  ( HPDF_MMgr mmgr,
                   HPDF_Annotation   annot,
                   HPDF_Annotation   annot3d,
                   HPDF_Dict         view);
 */
HB_FUNC( HPDF_ANNOT_SET3DVIEW )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annot_Set3DView( ( HPDF_MMgr ) hb_parptr( 1 ), ( HPDF_Annotation ) hb_parptr( 2 ), ( HPDF_Annotation ) hb_parptr( 3 ), ( HPDF_Dict ) hb_parptr( 4 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_PopupAnnot_SetOpened  (HPDF_Annotation   annot,
                            HPDF_BOOL         opened);
 */
HB_FUNC( HPDF_POPUPANNOT_SETOPENED )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_PopupAnnot_SetOpened( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_BOOL ) hb_parl( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_SetLineEndingStyle (HPDF_Annotation annot, HPDF_LineAnnotEndingStyle startStyle, HPDF_LineAnnotEndingStyle endStyle);
 */
HB_FUNC( HPDF_FREETEXTANNOT_SETLINEENDINGSTYLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_FreeTextAnnot_SetLineEndingStyle( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_LineAnnotEndingStyle ) hb_parni( 2 ), ( HPDF_LineAnnotEndingStyle ) hb_parni( 3 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_Set3PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point kneePoint, HPDF_Point endPoint);
 */
HB_FUNC( HPDF_FREETEXTANNOT_SET3POINTCALLOUTLINE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;
   HPDF_Point p3;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 3, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 3, 2 );

   p3.x = ( HPDF_REAL ) hb_parvnd( 4, 1 );
   p3.y = ( HPDF_REAL ) hb_parvnd( 4, 2 );

   hb_retnl( ( long ) HPDF_FreeTextAnnot_Set3PointCalloutLine( ( HPDF_Annotation ) hb_parptr( 1 ), p1, p2, p3 ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_Set2PointCalloutLine (HPDF_Annotation annot, HPDF_Point startPoint, HPDF_Point endPoint);
 */
HB_FUNC( HPDF_FREETEXTANNOT_SET2POINTCALLOUTLINE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 3, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 3, 2 );

   hb_retnl( ( long ) HPDF_FreeTextAnnot_Set2PointCalloutLine( ( HPDF_Annotation ) hb_parptr( 1 ), p1, p2 ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_FreeTextAnnot_SetDefaultStyle (HPDF_Annotation  annot, const char* style);
 */
HB_FUNC( HPDF_FREETEXTANNOT_SETDEFAULTSTYLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_FreeTextAnnot_SetDefaultStyle( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parc( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetPosition (HPDF_Annotation annot,
                     HPDF_Point startPoint, HPDF_LineAnnotEndingStyle startStyle,
                     HPDF_Point endPoint, HPDF_LineAnnotEndingStyle endStyle);
 */
HB_FUNC( HPDF_LINEANNOT_SETPOSITION )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_Point p1;
   HPDF_Point p2;

   p1.x = ( HPDF_REAL ) hb_parvnd( 2, 1 );
   p1.y = ( HPDF_REAL ) hb_parvnd( 2, 2 );

   p2.x = ( HPDF_REAL ) hb_parvnd( 4, 1 );
   p2.y = ( HPDF_REAL ) hb_parvnd( 4, 2 );

   hb_retnl( ( long ) HPDF_LineAnnot_SetPosition( ( HPDF_Annotation ) hb_parptr( 1 ), p1, ( HPDF_LineAnnotEndingStyle ) hb_parni( 3 ), p2, ( HPDF_LineAnnotEndingStyle ) hb_parni( 5 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetLeader (HPDF_Annotation annot, HPDF_INT leaderLen, HPDF_INT leaderExtLen, HPDF_INT leaderOffsetLen);
 */
HB_FUNC( HPDF_LINEANNOT_SETLEADER )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_LineAnnot_SetLeader( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_LineAnnot_SetCaption (HPDF_Annotation annot, HPDF_BOOL showCaption, HPDF_LineAnnotCapPosition position, HPDF_INT horzOffset, HPDF_INT vertOffset);
 */
HB_FUNC( HPDF_LINEANNOT_SETCAPTION )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_LineAnnot_SetCaption( ( HPDF_Annotation ) hb_parptr( 1 ), hb_parl( 2 ), ( HPDF_LineAnnotCapPosition ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
#else
   hb_retnl( -1 );
#endif
}
/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_Annotation_SetBorderStyle  (HPDF_Annotation  annot,
                                 HPDF_BSSubtype   subtype,
                                 HPDF_REAL        width,
                                 HPDF_UINT16      dash_on,
                                 HPDF_UINT16      dash_off,
                                 HPDF_UINT16      dash_phase);
 */
HB_FUNC( HPDF_ANNOTATION_SETBORDERSTYLE )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Annotation_SetBorderStyle( ( HPDF_Annotation ) hb_parptr( 1 ), ( HPDF_BSSubtype ) hb_parni( 2 ), ( HPDF_REAL ) hb_parnd( 3 ), ( HPDF_UINT16 ) hb_parni( 4 ), ( HPDF_UINT16 ) hb_parni( 5 ), ( HPDF_UINT16 ) hb_parni( 6 ) ) );
#else
   hb_retnl( -1 );
#endif
}
