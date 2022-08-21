#require "gtnap"
#include "gtnap.ch"     // For GTNAP defines

/*---------------------------------------------------------------------------*/

// Create a horizontal layout with an Icon in left cell and message in right cell
STATIC FUNC ICON_MESSAGE_LAYOUT( Icon_Type, Message )

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
STATIC FUNC BUTTONS_LAYOUT( Window, ButtonList, DefButton )
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

FUNC PERGUN ( C_SubCabec, V_OPCOES, N_DEFAULT, L_PODE_ZERO, C_Cabec_x, C_IconType )

    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL N_Select_Option

    // Create the window and main panel
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_TITLE)
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

    // Launch window as modal
    // Main Window will be blocked until a modal window will be closed/acepted
    N_Select_Option := NAP_WINDOW_MODAL(V_Janela)


    RETURN N_Select_Option


