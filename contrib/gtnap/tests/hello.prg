/*
    Ref:  Clipper Hello World, gut level calls
*/
Function HelloWorld()
    Local lValue := .t.                 //  --  Declare local logical
    Local cScreen := SaveScreen()       //  --  Save Screen
    Local nRow := Row()                 //  --  Save Row Position
    Local nCol := Col()                 //  --  Save Colum Position
    Local cCursor := SetCursor()        //  --  Save Cursor Type
    SetCursor(0)                        //  --  No Blinkies
    Scroll(0,0,MaxRow(),MaxCol())       //  --  Clear the screen
    While lValue                        //  --  While True
        DevPos(MaxRow()/2,MaxCol()/2-5); DevOut("Hello World")
        Inkey()                         //  --  Hold for keypress
        if lastkey() == 27              //  --  Check last keypress
            lValue := .f.               //  --  Change Value if Esc
        end
    End
    RestScreen(0,0,MaxRow(),MaxCol(),cScreen)   //  --  Restore all
    SetCursor(cCursor)                          //  --  the original
    DevPos(nRow,nCol); DevOut("")               //  --  Values and
Return (0)                                      //  --  Return Nil
/*
    End Ref:  Clipper Hello World, gut level calls
*/