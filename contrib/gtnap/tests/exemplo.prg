// Porting of ASPEC exemplo.prg to GTNAP

#require "gtnap"
#include "gtnap.ch"     // For GTNAP defines


// TO BE REMOVED
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"
// TO BE REMOVED


PROC MAIN

IF HB_GTVERSION()=="NAP"
    // TO BE REMOVED
    lBoxMessage("You are running in GTNAP Mode", "Pause")
    NAP_GLOBAL_RUNLOOP({|| GTNAP_MAIN() }, {|| GTNAP_END() })

    RETURN
 ELSE
    lBoxMessage("You are NOT running in GTNAP Mode", "Pause")
    RETURN
 ENDIF

RETURN


STATIC PROCEDURE GTNAP_MAIN()

   // LOCAL ch        // TO BE REMOVED


LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2
LOCAL V_Image, V_ImageView, V_MenuVert
LOCAL V_Label1, V_Label2, V_Label3

lBoxMessage("GTNAP_MAIN()!!!!!!!!!!!!!!", "Pause")

DIRET_BMPS(".\bmps\")
NAP_GLOBAL_FONT(24, 0)

// GTNAP NOT USE Global Screen/Pixel Coordinates
// Texts, Buttons and other widget has not the same size in all platorms
// By using Layouts we ensure that interface compositions will be portable
V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD)
V_Panel := NAP_PANEL_CREATE()
V_Layout1 := NAP_LAYOUT_CREATE(2, 1)
V_Layout2 := NAP_LAYOUT_CREATE(1, 5)
V_Image := NAP_IMAGE_FROM_FILE(DIRET_BMPS() + "logaspec.bmp")
V_ImageView := NAP_IMAGEVIEW_CREATE()
V_MenuVert := NAP_MENUVERT_CREATE()
V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de janela")
V_Label2 := NAP_LABEL_WITH_TEXT("Versão 99.9ç(b999) - S99999")
V_Label3 := NAP_LABEL_WITH_TEXT("www.aspec.com.br    Aspec, 1993-2022. Todos os direitos reservados")

NAP_LABEL_BGCOLOR(V_Label2, NAP_COLOR_CYAN())
NAP_LABEL_BGCOLOR(V_Label3, NAP_COLOR_CYAN())
NAP_IMAGEVIEW_IMAGE(V_ImageView, V_Image)
NAP_MENUVERT_ADD(V_MenuVert, "Menu de opções", {|| EXEMPLO_MENU() })
NAP_MENUVERT_ADD(V_MenuVert, "Browse de DBF", {|| EXEMPLO_BROWSE_DBF() })
NAP_MENUVERT_ADD(V_MenuVert, "Browse de vetor", {|| EXEMPLO_BROWSE_VETOR() })
NAP_MENUVERT_ADD(V_MenuVert, "Exibição/edição de texto em memória", {|| EXEMPLO_TEXTO_MEMORIA() })
NAP_MENUVERT_ADD(V_MenuVert, "Exibição/edição de arquivo texto", {|| EXEMPLO_TEXTO_ARQUIVO() })
NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados", {|| EXEMPLO_ENTRADA_DADOS() })
NAP_MENUVERT_ADD(V_MenuVert, "Janelas auxiliares", {|| EXEMPLO_AUXILIARES() })

NAP_LAYOUT_LABEL(V_Layout2, V_Label1, 0, 0)
NAP_LAYOUT_PANEL(V_Layout2, V_MenuVert, 0, 1)
NAP_LAYOUT_LABEL(V_Layout2, V_Label2, 0, 3)
NAP_LAYOUT_LABEL(V_Layout2, V_Label3, 0, 4)
NAP_LAYOUT_VMARGIN(V_Layout2, 2, 300)
NAP_LAYOUT_IMAGEVIEW(V_Layout1, V_ImageView, 0, 0)
NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 1, 0)
NAP_LAYOUT_VALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_TOP)
NAP_LAYOUT_HALIGN(V_Layout2, 0, 0, ekNAP_ALIGN_CENTER)
NAP_LAYOUT_HALIGN(V_Layout2, 0, 3, ekNAP_ALIGN_RIGHT)
NAP_LAYOUT_HALIGN(V_Layout2, 0, 4, ekNAP_ALIGN_RIGHT)
NAP_LAYOUT_VMARGIN(V_Layout2, 0, 20)
NAP_LAYOUT_HMARGIN(V_Layout1, 0, 150)
NAP_LAYOUT_MARGIN4(V_Layout1, 10, 10, 10, 100)
NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
NAP_WINDOW_PANEL(V_Janela, V_Panel)
NAP_WINDOW_TITLE(V_Janela, "Porting of ASPEC exemplo.prg to GTNAP")
NAP_WINDOW_ONCLOSE(V_Janela, {| hEv | ON_MAIN_WINDOW_CLOSE(hEv) })
NAP_WINDOW_SHOW(V_Janela)

// TO BE REMOVED, JUST A TEMPORAL WAY TO SHOW THE PANEL
//NAP_GLOBALPANEL(V_Janela)


RETURN

// The user wants to exit the application
// We can confirm the exit
STATIC PROCEDURE ON_MAIN_WINDOW_CLOSE( /*hEnv*/ )
    NAP_GLOBAL_EXIT()
RETURN

// Final actions before exit the application
// Only Non-graphical
STATIC PROCEDURE GTNAP_END()
    lBoxMessage("GTNAP_END()", "Pause")
    QUIT
RETURN












//
// DELETE THIS CODE
// JUST FOR TEST
//
//  DO WHILE ( ch := Inkey( 0 ) ) != K_ESC
//     // experiment with different paintrefresh interval:
//     DO CASE
//     CASE ch == hb_keyCode( "<" )
//        wvw_SetPaintRefresh( Int( wvw_SetPaintRefresh() / 2 ) )
//        Alert( wvw_SetPaintRefresh() )
//     CASE ch == hb_keyCode( ">" )
//        wvw_SetPaintRefresh( Int( wvw_SetPaintRefresh() * 2 ) )
//        Alert( wvw_SetPaintRefresh() )
//     CASE ch == hb_keyCode( "0" )
//        wvw_SetPaintRefresh( 0 )
//        Alert( wvw_SetPaintRefresh() )
//     OTHERWISE
//        // do nothing. Inkey() has been handled by nAfterInket()
//     ENDCASE
//  ENDDO

// //    Setup_wvw("Exemplo das rotinas de janelamento",35,110)

// QUIT


// NAP_MENUVERT_ADD(V_MenuVert, "Menu de opções", {|| EXEMPLO_MENU() })
// NAP_MENUVERT_ADD(V_MenuVert, "Browse de DBF", {|| EXEMPLO_BROWSE_DBF() })
// NAP_MENUVERT_ADD(V_MenuVert, "Browse de vetor", {|| EXEMPLO_BROWSE_VETOR() })
// NAP_MENUVERT_ADD(V_MenuVert, "Exibição/edição de texto em memória", {|| EXEMPLO_TEXTO_MEMORIA() })
// NAP_MENUVERT_ADD(V_MenuVert, "Exibição/edição de arquivo texto", {|| EXEMPLO_TEXTO_ARQUIVO() })
// NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados", {|| EXEMPLO_ENTRADA_DADOS() })
// NAP_MENUVERT_ADD(V_MenuVert, "Janelas auxiliares", {|| EXEMPLO_AUXILIARES() })



PROC EXEMPLO_MENU
    // TODO
    RETURN

PROC EXEMPLO_BROWSE_DBF()
    // TODO
    RETURN

PROC EXEMPLO_BROWSE_VETOR()
    // TODO
    RETURN

PROC EXEMPLO_TEXTO_MEMORIA()
    // TODO
    RETURN

PROC EXEMPLO_TEXTO_ARQUIVO()
    // TODO
    RETURN

PROC EXEMPLO_ENTRADA_DADOS()
    // TODO
    RETURN

PROC EXEMPLO_AUXILIARES()
    // TODO
    RETURN


//
// Helper functions
//

FUNC DIRET_BMPS(C_DIRET_NEW)
    STATIC C_DIRET_BMPS := ""
    LOCAL C_DIRET_ANT := C_DIRET_BMPS
    IF C_DIRET_NEW # NIL
        C_DIRET_BMPS := C_DIRET_NEW
    ENDIF
    RETURN C_DIRET_ANT





// TO BE REMOVED
#define MB_ICONASTERISK                      64
#define MB_ICONINFORMATION          MB_ICONASTERISK
#define MB_OK                                 0
#define MB_SYSTEMMODAL                     4096
// TO BE REMOVED

FUNCTION lBoxMessage( cMsg, cTitle )

    hb_default( @cTitle, "Info" )
    win_MessageBox( wvw_GetWindowHandle(), cMsg, cTitle, MB_OK + MB_ICONINFORMATION + MB_SYSTEMMODAL )

    RETURN .T.
