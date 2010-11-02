/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QCalendarWidget( ... )
   RETURN HB_QCalendarWidget():new( ... )

FUNCTION QCalendarWidgetFromPointer( ... )
   RETURN HB_QCalendarWidget():fromPointer( ... )


CREATE CLASS QCalendarWidget INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QCalendarWidget

   METHOD  new( ... )

   METHOD  dateEditAcceptDelay           // (  )                                               -> nInt
   METHOD  dateTextFormat                // ( oQDate )                                         -> oQTextCharFormat
   METHOD  firstDayOfWeek                // (  )                                               -> nQt_DayOfWeek
   METHOD  headerTextFormat              // (  )                                               -> oQTextCharFormat
   METHOD  horizontalHeaderFormat        // (  )                                               -> nHorizontalHeaderFormat
   METHOD  isDateEditEnabled             // (  )                                               -> lBool
   METHOD  isGridVisible                 // (  )                                               -> lBool
   METHOD  isNavigationBarVisible        // (  )                                               -> lBool
   METHOD  maximumDate                   // (  )                                               -> oQDate
   METHOD  minimumDate                   // (  )                                               -> oQDate
   METHOD  monthShown                    // (  )                                               -> nInt
   METHOD  selectedDate                  // (  )                                               -> oQDate
   METHOD  selectionMode                 // (  )                                               -> nSelectionMode
   METHOD  setDateEditAcceptDelay        // ( nDelay )                                         -> NIL
   METHOD  setDateEditEnabled            // ( lEnable )                                        -> NIL
   METHOD  setDateTextFormat             // ( oQDate, oQTextCharFormat )                       -> NIL
   METHOD  setFirstDayOfWeek             // ( nDayOfWeek )                                     -> NIL
   METHOD  setHeaderTextFormat           // ( oQTextCharFormat )                               -> NIL
   METHOD  setHorizontalHeaderFormat     // ( nFormat )                                        -> NIL
   METHOD  setMaximumDate                // ( oQDate )                                         -> NIL
   METHOD  setMinimumDate                // ( oQDate )                                         -> NIL
   METHOD  setSelectionMode              // ( nMode )                                          -> NIL
   METHOD  setVerticalHeaderFormat       // ( nFormat )                                        -> NIL
   METHOD  setWeekdayTextFormat          // ( nDayOfWeek, oQTextCharFormat )                   -> NIL
   METHOD  verticalHeaderFormat          // (  )                                               -> nVerticalHeaderFormat
   METHOD  weekdayTextFormat             // ( nDayOfWeek )                                     -> oQTextCharFormat
   METHOD  yearShown                     // (  )                                               -> nInt
   METHOD  setCurrentPage                // ( nYear, nMonth )                                  -> NIL
   METHOD  setDateRange                  // ( oQDate, oQDate )                                 -> NIL
   METHOD  setGridVisible                // ( lShow )                                          -> NIL
   METHOD  setNavigationBarVisible       // ( lVisible )                                       -> NIL
   METHOD  setSelectedDate               // ( oQDate )                                         -> NIL
   METHOD  showNextMonth                 // (  )                                               -> NIL
   METHOD  showNextYear                  // (  )                                               -> NIL
   METHOD  showPreviousMonth             // (  )                                               -> NIL
   METHOD  showPreviousYear              // (  )                                               -> NIL
   METHOD  showSelectedDate              // (  )                                               -> NIL
   METHOD  showToday                     // (  )                                               -> NIL

   ENDCLASS


METHOD QCalendarWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCalendarWidget( ... )
   RETURN Self


METHOD QCalendarWidget:dateEditAcceptDelay( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_dateEditAcceptDelay( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:dateTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCharFormatFromPointer( Qt_QCalendarWidget_dateTextFormat( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:firstDayOfWeek( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_firstDayOfWeek( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:headerTextFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QCalendarWidget_headerTextFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:horizontalHeaderFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_horizontalHeaderFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:isDateEditEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_isDateEditEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:isGridVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_isGridVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:isNavigationBarVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_isNavigationBarVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:maximumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QCalendarWidget_maximumDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:minimumDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QCalendarWidget_minimumDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:monthShown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_monthShown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:selectedDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateFromPointer( Qt_QCalendarWidget_selectedDate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:selectionMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_selectionMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateEditAcceptDelay( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setDateEditAcceptDelay( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateEditEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setDateEditEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateTextFormat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setDateTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setFirstDayOfWeek( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setFirstDayOfWeek( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setHeaderTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setHeaderTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setHorizontalHeaderFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setHorizontalHeaderFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setMaximumDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setMaximumDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setMinimumDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setMinimumDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setSelectionMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setSelectionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setVerticalHeaderFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setVerticalHeaderFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setWeekdayTextFormat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setWeekdayTextFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:verticalHeaderFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_verticalHeaderFormat( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:weekdayTextFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextCharFormatFromPointer( Qt_QCalendarWidget_weekdayTextFormat( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:yearShown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_yearShown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setCurrentPage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setCurrentPage( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setDateRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCalendarWidget_setDateRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setGridVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setGridVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setNavigationBarVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setNavigationBarVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:setSelectedDate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCalendarWidget_setSelectedDate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showNextMonth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showNextMonth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showNextYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showNextYear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showPreviousMonth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showPreviousMonth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showPreviousYear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showPreviousYear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showSelectedDate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showSelectedDate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCalendarWidget:showToday( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCalendarWidget_showToday( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

