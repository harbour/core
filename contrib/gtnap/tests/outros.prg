#require "gtnap"
#include "gtnap.ch"

/*---------------------------------------------------------------------------*/

// Create a horizontal layout with an Icon in left cell and message in right cell
STATIC FUNCTION ICON_MESSAGE_LAYOUT( Icon_Type, Message )

    LOCAL IconFile := ""
    LOCAL V_Layout, V_Image, V_ImageView, V_Label
    LOCAL N_Message_Width := 300
    LOCAL N_Label_Col := 1

    IF Icon_Type == "info"
        IconFile := "informac.bmp"
    ELSEIF Icon_Type == "warn"
        IconFile := "adverten.bmp"
    ELSEIF Icon_Type == "quest"
        IconFile := "pergunta.bmp"
    ELSEIF Icon_Type == "error"
        IconFile := "erro.bmp"
    ENDIF

    // Layout WITH icon: two columns, one row
    IF ! IconFile == ""
        V_Layout := NAP_LAYOUT_CREATE(2, 1)
        V_Image := NAP_IMAGE_FROM_FILE(DIRET_BMPS() + IconFile)
        V_ImageView := NAP_IMAGEVIEW_CREATE()

        // Force the size of image view to 64x64
        NAP_IMAGEVIEW_SIZE(V_ImageView, 64, 64)
        // Force the image into control to be scaled, preserving aspect ratio
        NAP_IMAGEVIEW_SCALE(V_ImageView, ekNAP_SCALE_ASPECTDW)
        // Set the image into the view
        NAP_IMAGEVIEW_IMAGE(V_ImageView, V_Image)
        // Set the imageview into cell (0,0) of the layout
        NAP_LAYOUT_IMAGEVIEW(V_Layout, V_ImageView, 0, 0)
        // Horizontal separation between icon and text
        NAP_LAYOUT_HMARGIN(V_Layout, 0, 30)

    // Layout WITHOUT icon: Just one cell
    ELSE
        V_Layout := NAP_LAYOUT_CREATE(1, 1)
        N_Message_Width := 400
        N_Label_Col := 0
    ENDIF

    // Create multiline label and set the text
    V_Label := NAP_LABEL_MULTILINE()
    NAP_LABEL_TEXT(V_Label, Message)
    NAP_LAYOUT_LABEL(V_Layout, V_Label, N_Label_Col, 0)

    // Force the width for message.
    // Larger messages will be splitted into several lines
    NAP_LAYOUT_HSIZE(V_Layout, N_Label_Col, N_Message_Width)

    // The text will be centered vertically with icon
    NAP_LAYOUT_VALIGN(V_Layout, N_Label_Col, 0, ekNAP_ALIGN_CENTER)

    // Margin around layout Top, Right, Bottom, Left
    NAP_LAYOUT_MARGIN4(V_Layout, 40, 10, 0, 40)

    RETURN V_Layout

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE OPTION_BUTTON_CLICK( hEv )

    LOCAL button := NAP_EVENT_BUTTON(hEv)
    LOCAL id := NAP_BUTTON_GET_ID(button)
    NAP_WINDOW_STOP_MODAL(id)
    RETURN

/*---------------------------------------------------------------------------*/

// Create a horizontal layout with several buttons
STATIC FUNCTION BUTTONS_LAYOUT( Window, ButtonList, DefButton )
    LOCAL V_Layout, V_Button
    LOCAL N_Cont, N_NumOpts := LEN(ButtonList)

    // Layout with 'N_NumOpts' columns and one row
    V_Layout := NAP_LAYOUT_CREATE(N_NumOpts, 1)

    FOR N_Cont := 1 TO N_NumOpts
        // Create the button, set text and assign to cell
        V_Button := NAP_BUTTON_PUSH()
        NAP_BUTTON_TEXT(V_Button, ButtonList[N_Cont])
        NAP_BUTTON_ID(V_Button, N_Cont)
        // Button callback
        NAP_BUTTON_ONCLICK(V_Button, {| hEv | OPTION_BUTTON_CLICK(hEv) })
        // Asign button to cell
        NAP_LAYOUT_BUTTON(V_Layout, V_Button, N_Cont - 1, 0)
        // Buttons will be dimensioned with the text
        // But, at least, 60 pixel-width will be forced
        NAP_LAYOUT_HSIZE(V_Layout, N_Cont - 1, 60)

        IF N_Cont == DefButton
            NAP_WINDOW_DEFBUTTON(Window, V_Button)
        ENDIF

    NEXT

    // Separation of 10 pixels between buttons (inter-cell horizontal margin)
    FOR N_Cont := 1 TO N_NumOpts - 1
        NAP_LAYOUT_HMARGIN(V_Layout, N_Cont - 1, 10)
    NEXT

    // Margin around buttons Top, Right, Bottom, Left
    NAP_LAYOUT_MARGIN4(V_Layout, 30, 20, 20, 0)
    RETURN V_Layout

/*---------------------------------------------------------------------------*/

// The user wants to close the PERGUN window without PRESS any button
// Using the [RETURN] or [ESC] keys or the Window [X] close
STATIC PROCEDURE ON_PERGUN_CLOSE( hEv, N_DEFAULT )

    // If [ESC] Any button will be selected
    IF NAP_EVENT_WINCLOSE_ESC(hEv)
        NAP_WINDOW_STOP_MODAL(0)

    // If [X] Any button will be selected
    ELSEIF NAP_EVENT_WINCLOSE_BUTTON(hEv)
        NAP_WINDOW_STOP_MODAL(0)

    // If [RETURN] Default button will be selected
    ELSEIF NAP_EVENT_WINCLOSE_RETURN(hEv)
        // Nothing, the default button will be pressed

    // Others, PERGUN will not be closed
    ELSE
        NAP_EVENT_RESULT_FALSE(hEv)

    ENDIF

RETURN

/*---------------------------------------------------------------------------*/

FUNCTION PERGUN ( C_SubCabec, V_OPCOES, N_DEFAULT, L_PODE_ZERO, C_Cabec_x, C_IconType )

    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL N_Select_Option

    // Create the window and main panel
    // PERGUN window can be closed with [RETURN] and [ESC] key
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_TITLE + ekNAP_WINDOW_RETURN + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()

    // Main Layout: One columns, two rows
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)

    // Create the layout with the icon and text
    V_Layout2 := ICON_MESSAGE_LAYOUT( C_IconType, C_SubCabec )

    // Create the layout with all buttons
    V_Layout3 := BUTTONS_LAYOUT( V_Janela, V_OPCOES, N_DEFAULT )

    // Set the icon-text layout into top cell of main layout
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 0)

    // Set the buttons layout into buttom cell of main layout
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout3, 0, 1)

    // Buttons cell will be aligned to right size
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 1, ekNAP_ALIGN_RIGHT)

    // Asociate main layout with panel
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)

    // Add panel to window
    NAP_WINDOW_PANEL(V_Janela, V_Panel)

    // Window Title
    NAP_WINDOW_TITLE(V_Janela, C_Cabec_x)

    // Window Title
    NAP_WINDOW_ONCLOSE(V_Janela, {| hEv | ON_PERGUN_CLOSE(hEv, N_DEFAULT) })

    // Launch window as modal
    // Main Window will be blocked until a modal window will be closed/acepted
    N_Select_Option := NAP_WINDOW_MODAL(V_Janela)

    RETURN N_Select_Option

/*---------------------------------------------------------------------------*/

FUNCTION MOSTRAR ( C_CdMens, C_SubCabec, C_Cabec_x )

    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_Label1, V_Label2

    // Flags
    // Window title and close button
    // Window will process the [RETURN] and [ESC] keys (close is the default action)
    LOCAL flags := ekNAP_WINDOW_TITLE + ekNAP_WINDOW_CLOSE + ekNAP_WINDOW_RETURN + ekNAP_WINDOW_ESC

    V_Janela := NAP_WINDOW_CREATE(flags)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Label1 := NAP_LABEL_WITH_TEXT(C_CdMens)
    V_Label2 := NAP_LABEL_WITH_TEXT(C_SubCabec)
    NAP_LAYOUT_LABEL(V_Layout1, V_Label2, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 1)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 1, ekNAP_ALIGN_CENTER)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 50, 20, 50)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, C_Cabec_x)
    NAP_WINDOW_MODAL(V_Janela)

    RETURN 0
