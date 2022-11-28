/* encoding: cp850 */
#INCLUDE "ord.ch"
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

//Desativar na "cuademo", para página de código ser a padrão Windows/Linux
//REQUEST HB_CODEPAGE_PT850
REQUEST HB_CODEPAGE_PTISO

REQUEST HB_LANG_PT_BR

ANNOUNCE HB_GT_SYS

//
// FRAN: This code is not required for a CUALIB/GTNAP application
//
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    REQUEST HB_GT_WVW_DEFAULT
//    REQUEST WVW_NOpenWindow, WVW_NSetCurWindow
//    REQUEST HB_GT_WIN
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // PENDENTE_LINUX
//    NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
//
//
//

*
*********
PROC MAIN
*********
*

//
// FRAN:
// Event-driven applications (especially GTK+3 and macOS-Cocoa) cannot be started directly from main().
// They need to set up an event execution loop and other internal structures. In GTNAP based applications,
// we need to move the "main" to another procedure, which will be called from GTNAP when
// the application is ready to start. {|| RUN_MAIN() }
//
IF HB_GTVERSION()=="NAP"
    #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows) || defined(__PLATFORM__LINUX)
       DIRET_BMPS("..\bmps\")
       Setup_nap("Exemplo das rotinas de janelamento",35,110, {|| RUN_MAIN() })
    #else
       #erro "Código não adaptado para esta plataforma"
    #endif
 ELSE
    SETMODE(35,110)
    RUN_MAIN()
 ENDIF


//
// FRAN:
// This is the "real" main procedure for a GTNAP application.
// From here, all CUALIB based code will be the same as GTWVW implementation
//
PROC RUN_MAIN

LOCAL L_FechouComAutoclose, V_Janela

SET CURSOR OFF
SET SCOR OFF
SET EPOCH TO 1940
SET CENT ON
SET DATE BRIT
SET DELE ON
SETBLINK(.F.)    // "*" passa a indicar background intenso
SET AUTOPEN OFF // tornar a abertura do indice cdx não automatica

#if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
    * Nada a fazer.
#elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
    SET(_SET_FILECASE, 1)                 // ADAPTACAO_LINUX
    SET(_SET_DIRCASE , 1)                 // ADAPTACAO_LINUX
    SET(_SET_DIRSEPARATOR, "\")           // ADAPTACAO_LINUX  // não funcionou...
#else
   #erro "Código não adaptado para esta plataforma"
#endif

//Desativar na "cuademo", para página de código ser a padrão Windows/Linux
//hb_cdpSelect("PT850")
//hb_LangSelect("pt_BR","PT850")
hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

*

REQUEST DBFCDX
RDDSETDEFAULT("DBFCDX")
RDDINFO(RDDI_MEMOTYPE,1)

#DEFINE _COR2_PADRAO   "N/W,N/BG*,N,,N/W*"
SETCOLOR(_COR2_PADRAO)

MSETCURSOR( .T. )
// SET EVENTMASK TO INKEY_KEYBOARD+INKEY_LDOWN+INKEY_RDOWN+INKEY_MWHEEL  // Não funcionou com estes DEFINES!
SET EVENTMASK TO 128+2+8+64

PRIVATE INFO_VERSAO := {"99","9","999","999",;
                        "99","9","999","999"}

CUA20 @ 00,00,MAXROW(),MAXCOL() JANELA V_Janela ;
     TITULO "Escolha o tipo de janela" SUBTITULO "%T";
     AJUDA "T?????"

CUA20 ADDIMAGEM V_Janela ARQUIVO DIRET_BMPS()+"logaspec.bmp"  ;
     COORDENADAS 05,05,10,18 AJUDA "B19127"

ESPECIALIZE V_Janela MENU

ADDOPCAO V_JANELA TEXTO "#menu de opções" ;
    ACAO EXEMPLO_MENU() AJUDA "P06671"
ADDOPCAO V_Janela TEXTO "browse de #DBF" ;
    ACAO EXEMPLO_BROWSE_DBF() AJUDA "P06673"
ADDOPCAO V_Janela TEXTO "browse de #vetor" ;
    ACAO EXEMPLO_BROWSE_VETOR() AJUDA "P06675"
ADDOPCAO V_Janela TEXTO "exibição/edição de #texto em memória" ;
    ACAO EXEMPLO_TEXTO_MEMORIA() AJUDA "P06677"
ADDOPCAO V_Janela TEXTO "exibição de #arquivo texto" ;
    ACAO EXEMPLO_TEXTO_ARQUIVO() AJUDA "P06679"
ADDOPCAO V_Janela TEXTO "#entrada de dados" ;
    ACAO EXEMPLO_ENTRADA_DADOS() AJUDA "P06681"
ADDOPCAO V_Janela TEXTO "#janelas auxiliares" ;
    ACAO EXEMPLO_AUXILIARES() AJUDA "P06683"

*
L_FechouComAutoclose := ATIVE(V_Janela)
*
//!! IF L_FechouComAutoclose
//!!    ADVERTE("M?????","Exemplo encerrado pela AUTOCLOSE;"+;
//!!                     "(clicou-se na imagem)")
//!! ELSE
//!!    ADVERTE("M?????","Exemplo encerrado com ESC ou [X]")
//!! ENDIF
*
SET CURSOR ON

//
// FRAN:
// Just like initialization, finish an event-driven application needs to properly close
// the internal message runloop structures.
//
IF HB_GTVERSION()=="NAP"
    NAP_GLOBAL_EXIT()
ENDIF

QUIT
*

PROC EXEMPLO_MENU
    @ 22, 0 SAY ""
    OutStd("EXEMPLO_MENU() Option selected")
    RETURN

PROC EXEMPLO_BROWSE_DBF
    @ 22, 0 SAY ""
    OutStd("EXEMPLO_BROWSE_DBF() Option selected")
    RETURN

PROC EXEMPLO_BROWSE_VETOR
    @ 22, 0 SAY ""
    OutStd("EXEMPLO_BROWSE_VETOR() Option selected")
    RETURN

PROC EXEMPLO_TEXTO_MEMORIA
    @ 22, 0 SAY ""
    OutStd("EXEMPLO_TEXTO_MEMORIA() Option selected")
    RETURN

PROC EXEMPLO_TEXTO_ARQUIVO
    @ 22, 0 SAY ""
    OutStd("EXEMPLO_TEXTO_ARQUIVO() Option selected")
    RETURN

PROC EXEMPLO_ENTRADA_DADOS
    @ 22, 0 SAY ""
    OutStd("EXEMPLO_ENTRADA_DADOS() Option selected")
    RETURN

PROC EXEMPLO_AUXILIARES
    @ 22, 0 SAY ""
    OutStd("EXEMPLO_AUXILIARES() Option selected")
    RETURN


// *******************
// FUNC CONFIRMA_DADOS
// *******************
// LOCAL N_Resposta, L_Confirmacao
// N_Resposta = PERGUN("Dados estao;corretos ?",{"sim","nao"},1)
// IF N_Resposta == 1
//    L_Confirmacao = .T.
// ELSE
//    L_Confirmacao = .F.
// ENDIF
// RETURN L_Confirmacao
// *
// **************************
// FUNC CONFIRMA_CANCELAMENTO
// **************************
// LOCAL N_Resposta, L_Confirmacao
// N_Resposta = PERGUN("Cancelar digitação dos dados ?",{"sim","nao"},2)
// IF N_Resposta == 1
//    L_Confirmacao = .T.
// ELSE
//    L_Confirmacao = .F.
// ENDIF
// RETURN L_Confirmacao
// *
