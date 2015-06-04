/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
//
//                 Pritpal Bedi <bedipritpal@hotmail.com>
//                                 2010
//                          CacheMRG DashBoard
//
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbgtinfo.ch"

/*----------------------------------------------------------------------*/

FUNCTION ShowDashBoard( srv_ )
   LOCAL oCrt, nKey, lCont, nRefSecs, i, a_, nPC, cClr, nCurConxn
   LOCAL nCols, nRows, nSrvs, cLockInfo, cLockList, aLInfo, nn

   DEFAULT srv_ TO { { '10.0.0.94' , 1972, '_system', 'SYS', 20, 'AR', 0, {} },;
                     { '10.0.0.95' , 1972, '_system', 'SYS', 20, 'AR', 0, {} },;
                     { '10.0.0.98' , 1972, '_system', 'SYS', 20, 'AR', 0, {} },;
                     { '10.0.0.123', 1972, '_system', 'SYS', 20, 'AR', 0, {} } ;
                   }

   nSrvs := Len( srv_ )
   nRows := 1 + 1 + 1 + ( nSrvs * 2 ) + 1 + 1  - 2

   #ifndef __DASHBOARD__
   //oCrt := WvgCrt():New( , , { 2,4 }, { nRows+1,46 }, , .T. )
   oCrt := WvgCrt():New( , , { 2,4 }, { nRows+1,46+43+6 }, , .T. )
   oCrt:icon := 1
   oCrt:resizable := .F.
   oCrt:create()
   #endif
   #ifdef __DASHBOARD__
   SetMode( nRows+1+1, 46+43+6+4 - 1 )
   Wvt_ShowWindow( 1 )
   Wvt_SetIcon( 1 )
   #endif

   SetCursor( 0 )
   SetColor( 'N/W' )
   CLS
   SET SCOREBOARD OFF
   hb_gtInfo( HB_GTI_WINTITLE, 'Connections Statistics' )

   nCols := MaxCol()
   Wvg_BoxRaised( 1,2,nRows,nCols-2, {-5,-5,5,5} )
   Wvg_BoxRecessed( 1,2,nRows,nCols-2 )
   Wvg_BoxGroupRaised( 2,4,nRows-1,nCols-4, {-7,-7,7,7} )

   Wvg_InvalidateRect()

   DispOutAt( 2,4,'  Server IP         Avlbl   Used    %  ' + ;
      padl( "Configured", 10 ) + " " + padl( "Available",10 ) + " " + padl( "Usable", 10 ) + " " + padl( "Used",10 ) + " " + "Locks" + "  ", 'W+/B' )

   // Instruct GT to Repaint the Screen with GUI elements.
   IF ! Empty( oCrt )
      oCrt:refresh()
   ENDIF

   nCurConxn := CacheSetConnection()

   FOR i := 1 to Len( srv_ )
      CacheSetServerParams( srv_[ i,1 ], srv_[ i,2 ], srv_[ i,3 ], srv_[ i,4 ], srv_[ i,5 ] )

      srv_[ i,7 ] := CacheAddConnectionEx( srv_[ i,1 ], srv_[ i,2 ], srv_[ i,3 ], ;
                                           srv_[ i,4 ], srv_[ i,5 ], srv_[ i,6 ] )
   NEXT

   CacheSetConnection( iif( nCurConxn <= 0, srv_[ 4,7 ], nCurConxn ) )

   lCont := .F.
   AEval( srv_, {|e_| iif( e_[ 7 ] > 0, lCont := .T., NIL ) } )
   IF lCont
      AEval( srv_, {|e_,i| DispOutAt( 4+((i-1)*2), 4, '  '+pad( e_[ 1 ],15 )+'  ', 'W+/BG' ) } )
      nRefSecs := -19
      do while .T.
         nKey := inkey( 0.1 )
         IF nKey == 27
            EXIT
         ENDIF
         IF ( seconds() - nRefSecs ) >= 10
            FOR i := 1 to Len( srv_ )
               IF srv_[ i,7 ] > 0
                  a_        := CacheGetLicenseInfo( srv_[ i,7 ] )
                  cLockInfo := CacheGetLockTableInfo( srv_[ i,7 ] )
                  aLInfo    := hb_atokens( cLockInfo, "," )
                  FOR each nn in aLInfo
                     nn := val( nn )
                  NEXT
                  cLockList := CacheGetLockList( srv_[ i,7 ] )
                  nPC       := val( a_[ 11 ] ) / val( a_[ 6 ] ) * 100
                  cClr      := if( nPC > 75, 'W+/R', if( nPC > 60, 'N/RB*', 'N/G*' ) )
                  DispOutAt( 4+((i-1)*2), 4+15+4, ' '+padl(a_[ 6 ],5)+'  '+padl(a_[ 11 ],5)+'  '+str( nPC,2,0 )+'%  ' + ;
                              str( aLInfo[ 1 ], 10, 0 ) + " " + str( aLInfo[ 2 ], 10, 0 ) + " " + str( aLInfo[ 3 ], 10, 0 ) + " " + ;
                              str( aLInfo[ 4 ], 10, 0 ) + " " + str( val( cLockList ), 5, 0 )+ "  ", cClr )
               ENDIF
            NEXT
            nRefSecs := seconds()
         ENDIF
      ENDDO
   ENDIF

   FOR i := 1 to Len( srv_ )
      IF srv_[ i,7 ] > 0
         CacheCloseConnection( srv_[ i,7 ] )
      ENDIF
   NEXT

   IF ! Empty( oCrt )
      oCrt:destroy()
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/
