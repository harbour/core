/* encoding: cp850 */
#INCLUDE "ord.ch"
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

// Desativar na "cuademo", para página de código ser a padrão Windows/Linux
// REQUEST HB_CODEPAGE_PT850
REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

ANNOUNCE HB_GT_SYS

*
*********
PROC MAIN
*********
*
// Application global SETTERS
SET CURSOR OFF
SET SCOR OFF
SET EPOCH TO 1940
SET CENT ON
SET DATE BRIT
SET DELE ON
SETBLINK(.F.)    // "*" passa a indicar background intenso
SET AUTOPEN OFF  // tornar a abertura do indice cdx não automatica
// Desativar na "cuademo", para página de código ser a padrão Windows/Linux
// hb_cdpSelect("PT850")
// hb_LangSelect("pt_BR","PT850")
hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

REQUEST DBFCDX
RDDSETDEFAULT("DBFCDX")
RDDINFO(RDDI_MEMOTYPE,1)

#DEFINE _COR2_PADRAO   "N/W,N/BG*,N,,N/W*"
SETCOLOR(_COR2_PADRAO)
MSETCURSOR( .T. )

// LibreOffice initialization
HBOFFICE_INIT()

//
// Event-driven applications (especially GTK+3 and macOS-Cocoa) cannot be started directly from main().
// They need to set up an event execution loop and other internal structures. In GTNAP based applications,
// we need to move the "main" to another procedure, which will be called from GTNAP when
// the application is ready to start. {|| RUN_MAIN() }
//
IF HB_GTVERSION()=="NAP"
    DIRET_BMPS("../bmps/")
    Setup_nap("Exemplo das rotinas de janelamento", 35, 110, {|| RUN_MAIN() })

 ELSE
    SETMODE(35,110)
    NAP_CUALIB_INIT_LOG()
    RUN_MAIN()

 ENDIF

//
// This is the "real" main procedure for a GTNAP application.
// From here, all CUALIB based code will be the same as original implementation
//
PROC RUN_MAIN

LOCAL L_FechouComAutoclose, V_Janela

PRIVATE INFO_VERSAO := {"99","9","999","999",;
                        "99","9","999","999"}

hb_DispBox( 0, 1, 2, 2, HB_B_DOUBLE_UNI, 0 )

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
ADDOPCAO V_Janela TEXTO "#LibreOffice spreadsheet" ;
    ACAO EXEMPLO_PLANILHA() AJUDA "P06683"
ADDOPCAO V_Janela TEXTO "#LibreOffice textdocument" ;
    ACAO EXEMPLO_TEXT_DOCUMENT() AJUDA "P06683"

L_FechouComAutoclose := ATIVE(V_Janela)

SET CURSOR ON

//
// Just like initialization, finish an event-driven application needs to properly close
// the internal message runloop structures.
//
IF HB_GTVERSION()=="NAP"
    NAP_EXIT()
ENDIF

HBOFFICE_FINISH()

QUIT

*******************
FUNC CONFIRMA_DADOS
*******************
LOCAL N_Resposta, L_Confirmacao
N_Resposta = PERGUN("Dados estao;corretos ?",{"sim","nao"},1)
IF N_Resposta == 1
   L_Confirmacao = .T.
ELSE
   L_Confirmacao = .F.
ENDIF
RETURN L_Confirmacao
*

**************************
FUNC CONFIRMA_CANCELAMENTO
**************************
LOCAL N_Resposta, L_Confirmacao

N_Resposta = PERGUN("Cancelar digitação dos dados ?",{"sim","nao"},2)
IF N_Resposta == 1
   L_Confirmacao = .T.
ELSE
   L_Confirmacao = .F.
ENDIF
RETURN L_Confirmacao
*
