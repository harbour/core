/*
 * $Id$
 */

/*
 *    Pritpal Bedi <bedipritpal@hotmail.com>
 */
/*----------------------------------------------------------------------*/

#include "inkey.ch"
#include "common.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/
// The function has to be called via hb_threadStart( {|| ExecuteActiveX( nActiveX ) } )
//
FUNCTION ExecuteActiveX( nActiveX, xParam )
   Local oCrt, oTBar, oSBar, oStatic, oCom, oXbp, oTree, oItem1, oItem2
   LOCAL oListBox, oStatic2, oDA
   LOCAL oPanel, oPanel1, oPanel2
// LOCAL cVarA  := "Test A", cVarB := "Test B"
// LOCAL aState := {"not selected", "selected", "undefined"}
   LOCAL aParts := {}
// LOCAL oCheck, oRadio, oMLE, cText

   HB_SYMBOL_UNUSED( xParam )
   HB_SYMBOL_UNUSED( oCom )

   //--------------------------- Dialog -------------------------------\\
   #if 1
   oCrt := WvgDialog():new( , , { 30,30 }, { 800,600 }, , .t. )
   oCrt:closable := .t.
   oCrt:create()
   #else
   oCrt := WvgCrt():new( , , { 5,5 }, { 30,60 }, , .t. )
   oCrt:resizeMode := HB_GTI_RESIZEMODE_ROWS
   oCrt:closable := .t.
   oCrt:create()
   SetCursor( .f. )
   #endif

   oDA := oCrt:drawingArea

   //--------------------------- Menu --------------------------------\\
   ActiveXBuildMenu( oCrt, @oStatic, @oStatic2 )

   //--------------------------- ToolBar -----------------------------\\
   oTBar := BuildWvgToolBar( oDA, nActiveX )

   //--------------------------- StatusBar ---------------------------\\
   oSBar   := WvgStatusBar():new( oDA ):create( , , , , , .t. )
   oSBar:panelClick := {|oPanel| WVG_MessageBox( , oPanel:caption ) }
   oPanel  := oSBar:getItem( 1 )
   oPanel:caption := 'My Root Panel'
   oPanel1 := oSBar:addItem()
   oPanel1:caption := 'Ready'
   oPanel2 := oSBar:addItem()
   oPanel2:caption := 'Click on any part!'

   //--------------------------- Static ------------------------------\\
   oStatic := WvgStatic():new( oDA )
   oStatic:type    := WVGSTATIC_TYPE_TEXT
   oStatic:options := WVGSTATIC_TEXT_CENTER
   oStatic:caption := chr(13)+'Implemented   Xbase++ Parts'
   oStatic:create( , , { 0, oTBar:currentSize()[2]+3 }, { 120, oCrt:currentSize()[2]-;
                               oTBar:currentSize()[2]-oSBar:currentSize()[2]-4 }, , .t. )
   oStatic:setColorBG( RGB( 198,198,198 ) )

#if 0  // panel
   //--------------------------- Static + Radio + Checkbox ----------\\
   oStatic2:= WvgStatic():New( oCrt, , { 150, 150 }, { 500,310 }, , .f. )
   //oStatic2:type    := WVGSTATIC_TYPE_RAISEDBOX //BGNDFRAME
   oStatic2:exStyle += WS_EX_WINDOWEDGE
   //oStatic2:options := WVGSTATIC_FRAMETHICK
   oStatic2:create()
   //oStatic2:setColorBG( RGB( 198,198,198 ) )

   oXbp    := WvgPushButton():new( oStatic2 )
   oXbp:caption     := "Hide"
   oXbp:create( , , { 430,275 }, { 60,25 } )
   oXbp:activate    := {|| oStatic2:hide(), oCrt:sendMessage( WM_SIZE, 0, 0 ) }

   oRadio  := WvgRadioButton():new( oStatic2,, { 10,10 }, { 100,15 } )
   oRadio:caption   := "Com 1"
   oRadio:selection := .T.
   oRadio:selected  := {|m1,m2,obj| m1:=m1, m2:=m2, WVG_MessageBox( , obj:caption + IF( obj:selection, '< S >', '< N >' ) ) }
   oRadio:create()

   oRadio              := WvgRadioButton():new( oStatic2,, { 10,35 }, { 100,15 } )
   oRadio:caption   := "Com 2"
   oRadio:create()

   oCheck              := WvgCheckBox():New( oStatic2, , { 10,70 }, { 100,15 }, , .t. )
   oCheck:caption   := 'Checkbox A'
   oCheck:create()
   oCheck:selected  := {|m1,m2,o| m1:=m1,m2:=m2, WVG_MessageBox( , IF( o:getData(), 'I am selected','I am not selected' ) ) }

   // Create first 3State button, passing the position to :create()
   oXbp                := Wvg3State():new( oStatic2 )
   oXbp:caption := "3 State A"
   oXbp:create( , , { 10,100 }, { 100,15 } )
   // Determine current state using mp1
   oXbp:selected := {| m1,m2,oBtn | m2:=m2, oBtn:=oBtn, oPanel1:caption := "3State A ["+aState[ m1+1 ]+"]" }

   // Create second 3State Button, passing the position to :new()
   oXbp                := Wvg3State():new( oStatic2, , { 10,125 }, { 100,15 } )
   oXbp:caption := "3 State B"
   oXbp:create( oStatic2 )
   // Determine current state using :getData()
   oXbp:selected := {| m1,m2,oBtn | m1:=m1,m2:=m2, WVG_MessageBox( , "3State B", aState[ oBtn:getData()+1 ] ) }

   // Create first SLE, specify position using :create()
   // On :typeOut set the focus to the second SLE
   oXbp                := WvgSLE():new( oStatic2 )
   oXbp:autoTab        := .T.
   oXbp:bufferLength   := 20
   // Data code block containing assignment to LOCAL variable
   oXbp:dataLink       := {|x| IIf( x == NIL, cVarA, cVarA := x ) }
   oXbp:create( , , { 10,170 }, { 150,20 } )
   oXbp:setData()
   // Assign the value of the edit buffer to a LOCAL variable when the input focus is lost
   oXbp:killInputFocus := { |x,y,oSLE| x:=x,y:=y, oSLE:getData(), oPanel:caption := "cVarA =" + cVarA }

   // Create second SLE, specify position using :new()
   oXbp                := WvgSLE():new( oStatic2, , { 10,200 }, { 150,20 } )
   oXbp:tabStop        := .T.
   oXbp:bufferLength   := 15
   oXbp:dataLink       := {|x| IIf( x == NIL, cVarB, cVarB := x ) }
   oXbp:create(  )
   oXbp:setData()
   oXbp:killInputFocus := { |x,y,oSLE| x:=x,y:=y, oSLE:getData(), oPanel:caption := "cVarB =" + cVarB }

   // Read file into LOCAL variable
   cText   := MemoRead( 'hbmk_b32.bat' )
   // Create MLE, specify position using :create() and
   // assign data code block accessing LOCAL variable
   oMLE    := WvgMLE():new( oStatic2 )
   oMLE:wordWrap := .F.
   oMLE:border   := .t.
   oMLE:dataLink := {|x| IIf( x==NIL, cText, cText := x ) }
   oMLE:create( oStatic2, , { 180,10 }, { 310,250 } )
   // Copy text from LOCAL variable into edit buffer via :dataLink
   oMLE:setData()
#endif

   //--------------------------- ListBox -----------------------------\\
   oListBox := WvgListBox():new()
   oListBox:create( oStatic, , { 5, 55 }, { 107, 380 } )

   oListBox:setColorFG( RGB( 218,61,34 ) )

   aadd( aParts, 'XbpDialog'     )
   aadd( aParts, 'XbpMenuBar'    )
   aadd( aParts, 'XbpToolBar'    )
   aadd( aParts, 'XbpStatusBar'  )
   aadd( aParts, 'XbpStatic'     )
   aadd( aParts, 'XbpTreeView'   )
   aadd( aParts, 'XbpActiveX'    )
   aadd( aParts, 'XbpListBox'    )
   aadd( aParts, 'XbpPushButton' )
   aadd( aParts, 'XbpCheckBox'   )
   aadd( aParts, 'XbpRadioButton')
   aadd( aParts, 'Xbp3State'     )
   aadd( aParts, 'XbpSLE'        )
   aadd( aParts, 'XbpMLE'        )
   aadd( aParts, 'DataRef'       )

   aeval( aParts, {|e| oListBox:addItem( e ) } )
   oListBox:itemSelected := {|| WVG_MessageBox( , oListBox:getCurItem() ) }
   oListBox:setData( 3 )

   //--------------------------- PushButton --------------------------\\
   oXbp := WvgPushButton():new( oStatic )
   oXbp:caption := "Hide"
   oXbp:create( , , { 20,440 }, {80,30} )
   oXbp:activate:= {|| oStatic:hide(), oCrt:sendMessage( WM_SIZE, 0, 0 ) }

   //--------------------------- TreeView ---------------------------\\
   oTree := WvgTreeView():new( oDA, , { oCrt:currentSize()[1]-160,oTBar:currentSize()[2]+3 }, ;
                                       { 160, oCrt:currentSize()[2]-;
                               oTBar:currentSize()[2]-oSBar:currentSize()[2]-4 }, , .t. )
   oTree:hasLines   := .T.
   oTree:hasButtons := .T.
   oTree:alwaysShowSelection := .T.
   oTree:create()
   oTree:setColorBG( RGB( 120,15,240 ) )
   oTree:setColorFG( RGB( 15,240,120 ) )
   oTree:itemSelected := {|oItem| IF( oItem <> NIL, WVG_MessageBox( , oItem:caption ), NIL ) }

   oItem1 := oTree:rootItem:addItem( "First level A" )

   oTree:rootItem:addItem( "First level B" )

   oItem2 := oItem1:addItem( "Second level A" )
   oItem1:addItem( "Second level B" )

   oItem2:addItem( "Third level A" )
   oItem2:addItem( "Third level B" )
   oItem2:addItem( "Third level C" )

   #if 0
   oItem1:expand( .t. )
   #else
   oTree:showExpanded( .t., 2 )
   #endif

   oTree:setData( oItem2 )

   //--------------------------- Misc Config ------------------------\\
   oTBar:buttonClick := {|oBtn| IF( oBtn:caption == 'Hide' , oStatic:hide(), nil ),;
                                IF( oBtn:caption == 'Show' , oStatic:show(), nil ),;
                                IF( oBtn:caption == 'Tools', oStatic2:show():toFront(), nil ),;
                                IF( oBtn:caption $ 'Hide,Show', oCrt:sendMessage( WM_SIZE, 0, 0 ), NIL ),;
                                 oPanel2:caption := "Button [ " + oBtn:caption + " ] clicked!" }
   oDA:resize := {|| ResizeDialog( oCrt, oTBar, oSBar, oStatic, oCom, oTree ) }

   #if 1
   //--------------------------- Active-X ---------------------------\\
   oCom := BuildActiveXControl( nActiveX, oDA )
   if hb_isObject( oCom )
      oCrt:sendMessage( WM_SIZE, 0, 0 )
      oCrt:show()
      ExeActiveX( nActiveX, oCom, xParam )
   ENDIF
   #else
   oCrt:show()
   DO WHILE .t.
      IF inkey() == K_ESC
         EXIT
      ENDIF
   ENDDO
   #endif

   oCrt:Destroy()
   Return NIL

//----------------------------------------------------------------------//

STATIC FUNCTION ResizeDialog( oCrt, oTBar, oSBar, oStatic, oCom, oTree )
   LOCAL aCrt, aTBar, aSBar
   LOCAL nH, nT
// LOCAL aStatic, aCom, aTree

   aCrt    := oCrt:currentSize()
   aTBar   := oTBar:currentSize()
   aSBar   := oSBar:currentSize()
// aStatic := oStatic:currentSize()
// aTree   := oTree:currentSize()
// aCom    := oCom:currentSize()

   nT := aTBar[2]
   nH := aCrt[2]-aTBar[2]-aSBar[2]

   IF oStatic:isVisible
      oStatic:setPosAndSize( { 0, nT+3 }, { 120, nH-4 }, .t. )
      oCom:setPosAndSize( { 120, nT }, { aCrt[1]-120-150, nH }, .t. )
      oTree:setPosAndSize( { aCrt[1]-150, nT }, { 150, nH }, .t. )
   ELSE
      oCom:setPosAndSize( { 0, nT }, { aCrt[1]-150, nH }, .t. )
      oTree:setPosAndSize( { aCrt[1]-150, nT }, { 150, nH }, .t. )
   ENDIF

   RETURN 1

//----------------------------------------------------------------------//

Static Function ActiveXBuildMenu( oCrt, oStatic, oStatic2 )
   Local oMenuBar, oSubMenu

   oMenuBar := WvgMenuBar():new( oCrt ):create()

   // Define submenu in procedural style.
   // The numeric index of the selected menu item
   // is passed to the Callback code block -> mp1

   oSubMenu       := WvgMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Procedural"
   oSubMenu:addItem( { "Play Charge ~1", } )
   oSubMenu:addItem( { "Play Nannyboo ~2", } )
   oSubMenu:itemSelected := {|mp1| MyFunction( 100+mp1 ) }
   oMenuBar:addItem( { oSubMenu, NIL } )

   // Define submenu in the functional style:
   // A menu item executes a code block that
   // calls a function
   oSubMenu       := WvgMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Functional"
   oSubMenu:addItem( { "Play Opening ~1", {|| MyFunction( 1 ) } } )
   oSubMenu:addItem( { "Play Closing ~2", {|| MyFunction( 2 ) } } )
   oSubMenu:addItem()
   oSubMenu:addItem( { "~MessageBox"    , {|| MyFunction( 3 ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu       := WvgMenu():new( oMenuBar ):create()
   oSubMenu:title := "F~eatures"
   oSubMenu:addItem( { "~Hide or Show Left Panel" , {|| IF( oStatic:isVisible, ;
                              oStatic:hide(), oStatic:show() ), oCrt:sendMessage( WM_SIZE,0,0 ) } } )
   oSubMenu:addItem( { "~Show My Panel" , {|| oStatic2:show() } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   Return NIL

//----------------------------------------------------------------------//

STATIC FUNCTION BuildActiveXControl( nActiveX, oDA )
   LOCAL oCom

   DEFAULT nActiveX TO 2

   oCom := WvgActiveXControl():New( oDA, , { 0, 0 }, { 100, 100 }, , .t. )

   do case
   case nActiveX == 1
      hb_gtInfo( HB_GTI_WINTITLE, 'Shell.Explorer.2'+'  [  '+'http://harbour.vouch.info'+'  ]' )
      oCom:CLSID := 'Shell.Explorer.2'
      oCom:mapEvent( 269, {|| WAPI_OutputDebugString( ' E X P L O R E R - 2 6 9' ) } )
      oCom:mapEvent( 105, {|| WAPI_OutputDebugString( ' E X P L O R E R - 105'   ) } )

   case nActiveX == 11
      hb_gtInfo( HB_GTI_WINTITLE, 'Shell.Explorer.2'+'  [  '+'MSHTML Demo'+'  ]' )
      oCom:CLSID := "MSHTML:" + "<html><h1>Stream Test</h1><p>This HTML content is being loaded from a stream.</html>"
      oCom:mapEvent( 269, {|| QOut( ' E X P L O R E R - 2 6 9' ) } )

   case nActiveX == 2
      #define evClick     1
      #define evDblClk    2
      #define evBtnDown   3
      #define evMouseMove 4
      #define evBtnUp     5

      hb_gtInfo( HB_GTI_WINTITLE, 'AnalogClockControl.AnalogClock' )
      oCom:CLSID := 'AnalogClockControl.AnalogClock'
      oCom:Id    := 5

      oCom:mapEvent( evDblClk, {|| oCom:Value     := seconds()/86400 ,;
                                   oCom:BackColor := RGB( 0,140,210 ),;
                                   oCom:Refresh()                    ,;
                                   oCom:ShowSecondsHand := .t.       ,;
                                   oCom:Hands3D   := .t.             ,;
                                   oCom:Refresh()                    ,;
                                   oCom:showAboutBox()                ;
                                } )

      oCom:mapEvent( evBtnUp, {|nBtn| if( nBtn == 2, oCom:oParent:sendMessage( WM_CLOSE,0,0 ), NIL ) } )

   case nActiveX == 3
      hb_gtInfo( HB_GTI_WINTITLE, 'file://' + hb_DirBase() + 'myharu.pdf' )
      oCom:CLSID := 'file://' + hb_DirBase() + 'myharu.pdf'
      oCom:mapEvent( 269, {|| QOut( ' E X P L O R E R - 2 6 9' ) } )

   case nActiveX == 4
      hb_gtInfo( HB_GTI_WINTITLE, 'RM Chart [ <F12> Attributes  <F11> Next Charts ]' )
      oCom:CLSID := 'RMChart.RMChartX'

      // RMChart does not have event interface.
      // Trying to set it generates GPF.
      // Please download RMChart.ocx from http://www.rmchart.com/ . It is free in everysense.

   case nActiveX == 5
      hb_gtInfo( HB_GTI_WINTITLE, 'Image Viewer' )
      oCom:CLSID := 'SCRIBBLE.ScribbleCtrl.1'

   endcase

   oCom:create()

   RETURN oCom

//----------------------------------------------------------------------//

STATIC FUNCTION ExeActiveX( nActiveX, oCom, xParam )
   Local nKey, sData

   static nTurn := 0

   // After :CREATE() Messages
   //
   if nActiveX == 1
      hb_gtInfo( HB_GTI_WINTITLE, iif( empty( xParam ), 'http://hbide.vouch.info', xParam ) )
      oCom:AddressBar := .t.
      oCom:Navigate( iif( empty( xParam ), 'http://hbide.vouch.info', xParam ) )

   elseif nActiveX == 4
      ConfigureRMChart( oCom )
      oCom:Draw( .t. )
      oCom:Draw2Clipboard()

   elseif nActiveX == 5
      oCom:loadMultiPage( hb_DirBase() + 'myharu.pdf', 2 )
      oCom:addGradientBorder( 10, RGB( 12,20,233 ), RGB( 100,255,20 ), 0 )
      oCom:drawText( 10,10,'Vouch' )
      //oCom:emboss( 3,0 )
      oCom:copy2ClipBoard()
      oCom:view := 11
      oCom:setBackGroundColor( rgb( 225,225,225 ) )
      //oCom:rotate90()

   endif

   do while .t.
      nKey := inkey()

      IF nActiveX == 2
         oCom:Value := seconds()/86400
      ENDIF

      if nKey == K_F12
         if nActiveX == 1
            oCom:Navigate( 'www.vouch.info' )

         elseif nActiveX == 11
            //oCom:document( 0 ):InnerHTML := "<html><h1>Stream Test</h1><p>This HTML content in a document.</html>"

         elseif nActiveX == 4
            oCom:RMCBackColor     := 23456142
            oCom:RMCStyle         := 2
            oCom:RMCUserWatermark := 'Vouch'

            oCom:Region(1):SetProperties( 5.0,5.0,-5.0,-5.0 )

            oCom:Draw( .t. )
         endif

      elseif nKey == K_F11
         if nActiveX == 4
            nTurn++
            if nTurn > 6
               nTurn := 1
            endif
            sData := NIL
            sData := ''

            do case
            case nTurn == 1
               hb_gtInfo( HB_GTI_WINTITLE,'RMChart [ Next:F11 ] ' + 'Stacked Bars' )
               //SetMode( 30,100 )

               sData += "00003600|00004450|000051|000061|000073|00008-6972|00009412|00011Tahoma|100011|10"
               sData += "0035|1000410|10005-5|10006-5|1000911|100101|100111|100181|100200|1002150000|1002"
               sData += "211|100238|100331|100341|100356|100378|100411|100468|100484|100494|10051-6972|10"
               sData += "052-16777216|10053-1828|100541|100558|10056-16777216|10057-16777216|10060-167772"
               sData += "16|10061-16777216|1006315|10064-32|100652|10066-16776961|10180this is the footer"
               sData += "|10181Example of stacked bars|10182Apples*Pears*Cherries*Strawberries|10183 $|10"
               sData += "184This is an optional axis text, sized 9 points and bold\9b|10187Label Nr. 1*La"
               sData += "bel Nr. 2*Label Nr. 3*Label Nr. 4*Label Nr. 5*Label Nr. 6|10196This is an option"
               sData += "al label axis text|110011|110023|110033|110045|110055|11006-1|1100923|110131|110"
               sData += "14-1|110171|11019-16777077|1102111|110221|110236|1105310000*10000*16000*12000*20"
               sData += "000*10000|120011|120023|120033|120045|120055|12006-1|1200927|120131|12014-1|1201"
               sData += "71|12019-16751616|1202111|120221|120236|120535000*7000*4000*15000*10000*10000|13"
               sData += "0011|130023|130033|130045|130055|13006-1|1300982|130131|13014-1|130171|13019-838"
               sData += "8608|1302111|130221|130236|1305310000*3000*12000*10000*5000*20000|140011|140023|"
               sData += "140033|140045|140055|14006-1|1400925|140131|14014-1|140171|14019-4684277|1402111"
               sData += "|140221|140236|140535000*9000*12000*6000*10000*5000"

            case nTurn == 2
               hb_gtInfo( HB_GTI_WINTITLE,'RMChart [ Next:F11 ] '+'Floating Bars' )
               //SetMode( 20,90 )

               sData += "00003550|00004300|000051|000073|00008-2894893|00009412|00011Tahoma|100011|100035"
               sData += "|100045|10005-5|10006-5|1000911|100101|100111|100131|100181|100201|1002113|10022"
               sData += "13|100238|100331|100341|100356|100378|100411|100468|100482|10052-16777216|10053-"
               sData += "1120086|100544|100555|10056-16777216|10057-16777216|10060-16777216|10061-1677721"
               sData += "6|1006316|10064-5383962|100652|10066-16777011|10181Birth of a Killer App|10182Sc"
               sData += "hedule*Reality|10187Design*Development*Testing*Bug Fixing*Documentation*Marketin"
               sData += "g|1020104/01*04/02*04/03*04/04*04/05*04/06*04/07*04/08*04/09*04/10*04/11*04/12*0"
               sData += "5/01|110011|110026|110044|110101|110131|11019-6751336|1102111|110221|1102312|110"
               sData += "531*3*4*6*6*4*7*4*9*3*10*3|120011|120026|120044|120101|120132|12019-47872|120211"
               sData += "1|120221|1202312|120531*.5*1.5*10.5*12*1*12*1*12.5*.5*2*11"

            case nTurn == 3
               hb_gtInfo( HB_GTI_WINTITLE,'RMChart [ Next:F11 ] '+'Four Regions' )
               //SetMode( 40,120 )

               sData += "00003700|00004500|000054|000061|000071|00008-984833|00009412|00011Tahoma|100011|"
               sData += "100032|100042|10005348|10006248|1000910|100101|100111|100181|100200|10021100|100"
               sData += "2211|100238|100331|100341|100355|100378|100481|100491|10051-984833|10052-1677721"
               sData += "6|10053-657956|100541|100558|10056-16777216|10057-16777216|10060-16777216|10061-"
               sData += "16777216|10187Label 1*Label 2*Label 3*Label 4*Label 5|110011|110021|110031|11004"
               sData += "6|110056|11006-1|110091|110131|11014-1|110171|1102111|110221|110235|1105330*40*7"
               sData += "0*60*20|200011|20003352|200042|20005-2|20006248|2000910|200101|200111|200181|200"
               sData += "200|20021100|2002211|200238|200331|200341|200355|200378|200484|200491|20051-9848"
               sData += "33|20052-16777216|20053-657956|200544|200555|20056-16777216|20057-16777216|20060"
               sData += "-16777216|20061-16777216|20187Label 1*Label 2*Label 3*Label 4*Label 5|210011|210"
               sData += "023|210033|210045|210055|21006-1|210091|210101|210131|21014-1|210171|2102111|210"
               sData += "221|210235|2105320*10*15*25*30|220011|220023|220033|220045|220055|22006-1|220091"
               sData += "|220101|220131|22014-1|220171|2202111|220221|220235|2205325*30*10*20*15|230011|2"
               sData += "30023|230033|230045|230055|23006-1|230091|230101|230131|23014-1|230171|2302111|2"
               sData += "30221|230235|2305310*20*40*20*30|240011|240023|240033|240045|240055|24006-1|2400"
               sData += "91|240101|240131|24014-1|240171|2402111|240221|240235|2405340*30*20*30*20|300011"
               sData += "|300032|30004252|30005348|30006-2|3000910|300101|300181|300481|300491|30051-9848"
               sData += "33|30052-16777216|30053-657956|310011|3100251|310031|3100454|310054|310071|31009"
               sData += "1|310121|310151|310161|310171|310182|310211|310221|310235|3105330*50*20*40*60|40"
               sData += "0011|40003352|40004252|40005-2|40006-2|4000910|400101|400111|400131|400181|40020"
               sData += "100|40021250|4002211|400238|400281|400292|400300|400310|400322|400331|400341|400"
               sData += "3510|400378|400482|400492|40051-984833|40052-16777216|40053-984833|400541|400558"
               sData += "|40056-16776961|40057-16777216|400592|40060-16777216|40061-16777216|40183$ |4018"
               sData += "5 %|410011|410021|410031|410043|410053|41006-1|4100950|410131|41014-1|410171|410"
               sData += "19-10496|4102111|410221|4102310|41053240*230*220*180*170*160*145*130*125*115|420"
               sData += "011|4200221|420035|4200422|420052|420061|420071|4200963|420111|420121|420131|420"
               sData += "171|42019-16744448|4202115|420221|4202310|420261|420538.1*6.2*4.3*2.2*1.2*3.1*5."
               sData += "2*11.4*7.3*4.2"

            case nTurn == 4
               hb_gtInfo( HB_GTI_WINTITLE,'RMChart [ Next:F11 ] '+'10 Biggest Companies' )
               //SetMode( 25,90 )

               sData += "00003670|00004450|000051|000061|000071|00008-10185235|00009412|00011Tahoma|10001"
               sData += "1|100035|1000410|10005-5|10006-5|1000912|100101|100111|100131|100181|10020100000"
               sData += "|10021250000|1002211|100239|100281|100292|100300|100310|100322|100331|100341|100"
               sData += "3510|100378|100482|100492|10051-32944|10052-1296|10053-983041|100541|100558|1005"
               sData += "6-1828|10057-16777216|100592|10060-1828|10061-16777216|10180data source: F.A.Z|1"
               sData += "0181The world's 10 biggest industrial companies 2003|10183$ |10184Total turnover"
               sData += " in Mill. Dollar|10185 %|10186Net operating margin|10187Exxon Mobil*Royal Dutch "
               sData += "/ Shell*BP*General Motors*Daimler Chrysler*Ford Motor*Toyota Motor*General Elect"
               sData += "ric*TOTAL*Chevron Texaco|110011|110021|110031|110043|110053|11006-1|1100950|1101"
               sData += "31|11014-1|110171|11019-10496|1102111|110221|1102310|11053242365*235598*232571*1"
               sData += "85524*170457*164196*149321*132797*130067*119703|120011|1200221|120035|1200422|12"
               sData += "0052|120061|120071|1200970|120111|120121|120131|1201421|120171|12019-16744448|12"
               sData += "02115|120221|1202310|120261|120538.9*4.1*4.4*2.1*.3*.3*5.9*11.3*6.7*6"

            case nTurn == 5
               hb_gtInfo( HB_GTI_WINTITLE,'RMChart [ Next:F11 ] '+'Grouped Bars' )
               //SetMode( 25,80 )

               sData += "00003600|00004450|000051|000061|000075|00008-2|00009412|00010paper.jpg|00011Taho"
               sData += "ma|100011|100035|100045|10005-5|10006-5|1000910|100101|100111|100181|100200|1002"
               sData += "1100|1002211|100238|100331|100341|100355|100378|100468|100484|10053-2|100541|100"
               sData += "558|100631|100651|10182First quarter*Second quarter*Third quarter*Fourth quarter"
               sData += "|101872000*2001*2002*2003*2004|110011|110022|110044|110131|1102111|110221|110235"
               sData += "|1105330*20*40*60*10|120011|120022|120044|120131|1202111|120221|120235|1205330*2"
               sData += "0*50*70*60|130011|130022|130044|130131|1302111|130221|130235|1305340*10*30*20*80"
               sData += "|140011|140022|140044|140131|1402111|140221|140235|1405370*50*80*40*30"

            case nTurn == 6
               hb_gtInfo( HB_GTI_WINTITLE,'RMChart [ Next:F11 ] '+'Flow Chart' )
               //SetMode( 30,50 )

               sData += "00003305|00004400|000051|00008-984833|00009412|00011Tahoma|100011|100035|100045|"
               sData += "10005-5|10006-5|10180\7C|010011|010051|010072|010081|0101050|0101125|01012100|01"
               sData += "01325|01014-5952982|01015-5952982|01016255|010191|0102010|01026Start|01030-256|0"
               sData += "10012|010054|0100721|01014-16776961|010222|01024100*100|0102550*75|01026|010272|"
               sData += "010293|010013|010051|010071|010081|0101050|0101175|01012100|0101325|01014-669788"
               sData += "2|01015-6697882|01016255|010191|0102010|01026i = 1|01030-16777216|010014|010054|"
               sData += "0100721|01014-16776961|010222|01024100*100|01025100*150|01026|010272|010293|0100"
               sData += "15|010051|010073|010081|0101050|01011150|01012100|0101350|01014-65536|01015-6553"
               sData += "6|01016255|010191|0102010|01026i = 39?|01030-256|010016|010054|0100721|01014-167"
               sData += "76961|010222|01024100*100|01025200*225|01026|010272|010293|010017|010051|010071|"
               sData += "010081|0101050|01011225|01012100|0101325|01014-6697882|01015-6697882|01016255|01"
               sData += "0191|0102010|01026i =  i + 1|01030-16777216|010018|010054|0100721|01014-16776961"
               sData += "|010222|01024100*100|01025250*275|01026|010272|010293|010019|010051|010073|01008"
               sData += "1|0101050|01011275|01012100|0101350|01014-65536|01015-65536|01016255|010191|0102"
               sData += "010|01026i <= 100|01030-256|0100110|010054|0100721|01014-16776961|010222|0102410"
               sData += "0*100|01025325*350|01026|010272|010293|0100111|010051|010072|010081|0101050|0101"
               sData += "1350|01012100|0101325|01014-5952982|01015-5952982|01016255|010191|0102010|01026S"
               sData += "top|01030-256|0100113|010054|0100721|01014-16776961|010222|01024150*200|01025175"
               sData += "*175|01026|010272|010293|0100114|010051|010081|01010145|01011157|0101250|01014-1"
               sData += "6776961|01015-5383962|01016255|010191|0102010|01026yes|01030-16777216|0100115|01"
               sData += "0051|010071|010081|01010200|01011162|01012100|0101325|01014-6697882|01015-669788"
               sData += "2|01016255|010191|0102010|01026i = 69|01030-16777216|0100116|010054|0100721|0101"
               sData += "4-16776961|010224|0102450*10*10*100|01025300*300*125*125|01026|010272|010293|010"
               sData += "0117|010054|0100721|01014-16776961|010223|01024250*250*100|01025162*125*125|0102"
               sData += "6|010272|010293|0100118|010051|010081|01010100|01011200|0101250|0101325|01014-16"
               sData += "776961|01016255|010191|0102010|01026no|01030-16777216|0100119|010051|010081|0101"
               sData += "010|01011280|0101240|01014-16776961|01016255|010191|0102010|01026yes|01030-16777"
               sData += "216|0100120|010051|010081|01010100|01011322|0101250|01014-16776961|01016255|0101"
               sData += "91|0102010|01026no|01030-16777216|0100120|010051|010079|01010180|01011280|010121"
               sData += "20|01013100|01015-39322|010191|010209|01026RMChart is not a flowchart tool. This"
               sData += " is just an example for the use of CustomObjects!|01030-256"

            endcase

            oCom:Reset()
            oCom:RMCFile := sData
            oCom:Draw( .t. )
         endif

      endif

      if nKey == K_ESC
         exit
      endif
   enddo

   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION ConfigureRMChart( RMChart )
   LOCAL oRegion
   LOCAL oCaption
   LOCAL oChart

   #define RMC_CTRLSTYLEFLAT          0
   #define RMC_PIE_GRADIENT           52
   #define RMC_FULL                   1
   #define RMC_EXPLODE_NONE           0
   #define RMC_VLABEL_ABSOLUTE        6
   #define RMC_HATCHBRUSH_OFF         0

   /* The code pulled from freewin sources */
   RMChart:Font             := "Tahoma"
   RMChart:RMCStyle         := RMC_CTRLSTYLEFLAT

   RMChart:AddRegion()

   oRegion := RMChart:Region( 1 )
   oRegion:Footer = "http://vouch.info"

   oRegion:AddCaption()

   oCaption := oRegion:Caption()
   oCaption:Titel     := "GTWVG Active-X Demo"
   oCaption:FontSize  := 10
   oCaption:Bold      := .T.

   oRegion:AddGridlessSeries()

   oChart := oRegion:GridLessSeries
   oChart:SeriesStyle      := RMC_PIE_GRADIENT
   oChart:Alignment        := RMC_FULL
   oChart:Explodemode      := RMC_EXPLODE_NONE
   oChart:Lucent           := .F.
   oChart:ValueLabelOn     := RMC_VLABEL_ABSOLUTE
   oChart:HatchMode        := RMC_HATCHBRUSH_OFF
   oChart:StartAngle       := 0
   oChart:DataString       := "10*5*20*25"

   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION MyFunction( nMode )

   #define MUSIC_WAITON          {800, 1600}

   do case
   case nMode == 1
      tone( MUSIC_WAITON[1], 1 )
      tone( MUSIC_WAITON[2], 1 )

   case nMode == 2
      tone( MUSIC_WAITON[2], 1 )
      tone( MUSIC_WAITON[1], 1 )

   case nMode == 3
      WVG_MessageBox( , "Button clicked!" )

   case nMode == 101  // Charge
      Eval( {|| tone(523,2),tone(698,2),tone(880,2),tone(1046,4),tone(880,2),tone(1046,8) } )

   case nMode == 102  // NannyBoo
      AEval( {{196,2},{196,2},{164,2},{220,2},{196,4},{164,4}}, {|a| tone(a[1],a[2]) } )

   case nMode == 103  // BADKEY
      tone( 480,0.25 )
      tone( 240,0.25 )

   endcase

   RETURN NIL

//----------------------------------------------------------------------//
