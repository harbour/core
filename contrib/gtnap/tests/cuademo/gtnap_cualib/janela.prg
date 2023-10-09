/* encoding: cp850 */

#pragma DebugInfo=On

/*

                               Aspec - Informatica
                           Direitos Autorais Reservado

   CUA - Objeto janela

*/
*
MEMVAR INFO_VERSAO
STATIC V_PilhaJanelas := {}
*
#INCLUDE "inkey.ch"
#INCLUDE "setcurs.ch"
#INCLUDE "color.ch"
#INCLUDE "common.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "set.ch"
#INCLUDE "recursos.ch"
#INCLUDE "mousecua.ch"
#INCLUDE "cua.ch"
#INCLUDE "gtnap.ch"
*
********************
FUNCTION CriarJanela ( N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec, ;
                       VC_TxtBotoes_10, C_CdTela, V_Janela_Pai, C_SubCabec,;
                       N_EspacamentoEmPixels, N_DeslocaCabecalho,;
                       L_CUA_10)  // parâmetro novo na CUA 2.0
*
LOCAL N_Lin1Livre, N_Col1Livre, N_Lin2Livre, N_Col2Livre
LOCAL VC_Titulo, C_CorInten
LOCAL L_DesenhaBox, N_MargemSuperior, N_MargemDemais
LOCAL L_Embutida := (V_Janela_Pai # NIL)
LOCAL L_MainCoord_Atu := .F.
LOCAL V_RegiaoBotoes := {}, N_CT
LOCAL B_Metodo
LOCAL V_Janela := NIL
LOCAL B_BotAcao := IIF(SOB_MODO_GRAFICO(),{||.T.},{||NIL})
LOCAL L_BotAutoClose := SOB_MODO_GRAFICO()
*
DEFAULT C_Cabec       TO ""
DEFAULT VC_TxtBotoes_10  TO {}
DEFAULT L_Embutida    TO .F.
DEFAULT C_SubCabec    TO ""
DEFAULT L_CUA_10      TO .T.
*
IF .NOT. LEFT(C_CdTela,1) == "T"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdTela,"T0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdTela)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF

* Logar uso da janela, para ter estatística de uso
LOGAINFO_ID_TELA_RELAT_BOTAO("tela",C_CDTELA,NIL,NIL)   // Log de uso de tela no sistema

IF L_CUA_10
   FOR N_CT := 1 TO LEN(VC_TxtBotoes_10)
        AADD(V_RegiaoBotoes,{;
        NIL,;                     // _BOTAO_LIN_INICIAL
        NIL,;                     // _BOTAO_COL_INICIAL
        NIL,;                     // _BOTAO_LIN_FINAL
        NIL,;                     // _BOTAO_COL_FINAL
        VC_TxtBotoes_10[N_CT],;   // _BOTAO_TEXTO_COMANDO
        NIL,;                     // _BOTAO_TEXTO_TRATADO_1
        NIL,;                     // _BOTAO_TEXTO_TRATADO_2
        NIL,;                     // _BOTAO_COL_DESTAQUE
        NIL,;                     // _BOTAO_TEXTO_DESTAQUE
        B_BotAcao,;               // _BOTAO_BLOCO_ACAO
        L_BotAutoClose,;          // _BOTAO_AUTOCLOSE
        NIL,;                     // _BOTAO_CDBOTAO
        NIL,;                     // _BOTAO_ALIAS_MUDA
        NIL,;                     // _BOTAO_RECNO_MUDA
        NIL,;                     // _BOTAO_FILTER_MUDA
        NIL,;                     // _BOTAO_ORDER_MUDA
        NIL,;                     // _BOTAO_EOFOK
        NIL,;                     // _BOTAO_HANDLE_MUDA
        NIL,;                     // _BOTAO_INKEY_DESTAQUE
        NIL,;                     // _BOTAO_INKEY_DESTAQUE_CASE
        NIL,;                     // _BOTAO_HANDLE_PUSHBUTTON
        NIL})                     // _BOTAO_MUDADADOS
   NEXT
ELSE
   IF LEN(VC_TxtBotoes_10) # 0
      ? MEMVAR->CUA_20_NAO_TEM_CLAUSULA_TECLAS
   ENDIF
ENDIF
*

DEFAULT N_DeslocaCabecalho TO 0

IF ";" $ C_CABEC
   IF SOB_MODO_GRAFICO()
      ALARME("M28746","TITULO com mais de uma linha")
      ? MEMVAR->ERRO_CABECALHO_1
   ELSE
      ALERT("TITULO com mais de uma linha")
      ? MEMVAR->ERRO_CABECALHO_1
   ENDIF
ENDIF
C_SubCabec := STRTRAN(C_SubCabec,"%t",C_Cabec)
C_SubCabec := STRTRAN(C_SubCabec,"%T",C_Cabec)
*
IF EMPTY(C_SubCabec) .AND. .NOT. EMPTY(C_Cabec)
   IF SOB_MODO_GRAFICO()
      ALARME("M28748","TITULO preenchido mas SUBTITULO vazio")
      ? MEMVAR->ERRO_CABECALHO_2
   ELSE
      ALERT("TITULO preenchido mas SUBTITULO vazio")
     ? MEMVAR->ERRO_CABECALHO_2
   ENDIF
ENDIF
*
IF N_LinIni == 0 .AND. N_ColIni == 0 .AND. ;
   N_LinFin == MAXROW() .AND. N_ColFin == MAXCOL()
   L_DesenhaBox     := .F.
   N_MargemSuperior := 0
   N_MargemDemais   := 0
ELSE
   L_DesenhaBox     := .T.
   N_MargemSuperior := 1
   N_MargemDemais   := 1
ENDIF
*
N_MargemDemais++   // aumentar margens para fins puramente estéticos
*
IF SOB_MODO_GRAFICO() .AND. .NOT. L_Embutida
   IF L_DesenhaBox
      * Quem desenha o box é a própria WVW, de forma automática.
      *
      L_DesenhaBox := .F.
      *
      * Deduzir da janela o espaço não utilizado pelo box,
      * para que a aparência fique idêntica entre a GTWIN e a GTWVW.
      *
        N_LinIni++
        N_ColIni++
        N_LinFin--
        N_ColFin--
      *
      N_MargemSuperior--
      N_MargemDemais--
   ENDIF
   *
ENDIF
*
N_Col1Livre := N_ColIni + N_MargemDemais      // coordenadas das colunas livres
N_Col2Livre := N_ColFin - N_MargemDemais
*
* não imprime nada, só monta a matriz de cabeçalho devidamente formatado
*
VC_Titulo := StrToVet_(C_SubCabec)
*
* coordenadas das linhas livres
N_Lin1Livre := N_LinIni + N_MargemSuperior + LEN(VC_Titulo) + 1
N_Lin2Livre := N_LinFin - N_MargemDemais   - 1 // ainda será deduzida as teclas de função na TecFunc()
*
* A cor abaixo é utilizada somente em SAY"s, sendo de uso interno do sistema.
* Por este motivo não precisa ser completa
C_CorInten := _Pega_Cor_(SETCOLOR(),CLR_STANDARD,.T.) + "+" + "/" + ;
              _Pega_Cor_(SETCOLOR(),CLR_STANDARD,.F.)
IF "++" $ C_CorInten
   C_CorInten := STRTRAN(C_CorInten,"++","+")
ENDIF
*
IF L_CUA_10
   B_Metodo := {||NIL}   // não faz nada
ELSE
   * trata eventuais botões, ações e imagens, mesmo que janela não tenha especialização
   B_Metodo := {|VX_Janela2| TrataEventos(VX_Janela2) }
ENDIF

*
#DEFINE C_CorJan     SETCOLOR()   // cor da janela (é a da criação)
#DEFINE C_TelaCoberta  NIL        // preenchida na abertura da janela
#DEFINE N_LinAnt     NIL          // (linha do cursor antes da ativação) - na ativação
#DEFINE N_ColAnt     NIL          // (coluna do cursor antes da ativação) - na ativação
#DEFINE N_LinBotoes  NIL          // (linhas ocupadas pelos botões de opções) - na ativação
#DEFINE N_TP_Jan     NIL          // indica o tipo de especializaçao da janela
#DEFINE VX_SubObj    NIL          // contém o subobjeto na qual foi especializada
#DEFINE N_LinMess    NIL          // linha de mensagens - na especialização
#DEFINE N_WindowNum  NIL          // Posicao da janela na pilha de janelas
#DEFINE aGuiObjects  {}
#DEFINE V_BotoesToolBar {}
#DEFINE L_TemScrollVertical   .F.
#DEFINE L_TemScrollHorizontal .F.
#DEFINE L_RolaCima         .F.
#DEFINE L_RolaBaixo        .F.
#DEFINE L_RolaEsquerda     .F.
#DEFINE L_RolaDireita      .F.
#DEFINE N_LinMarcadorVertical   0
#DEFINE N_ColMarcadorHorizontal 0
#DEFINE L_CriarToolBar     .F.
#DEFINE L_MainCoord_Ant NIL
#DEFINE V_LstImagens    {}
#DEFINE L_JanTipoMsgAguarde .F.
#DEFINE V_LstAcoes  {}
#DEFINE N_IdProgressBar1 NIL // #DEFINE POSICAO_45_A_REUTILIZAR  NIL
#DEFINE N_IdProgressBar2 NIL // #DEFINE POSICAO_48_A_REUTILIZAR  NIL
#DEFINE B_SetInkeyAfterBlock_Old NIL
#DEFINE N_ToolBarCodigoAcao  400   // Convencionou-se que iniciava com 400
#DEFINE N_IdScrollBarVertical   NIL // Identificador da ScrollBar Vertical
#DEFINE N_IdScrollBarHorizontal NIL // Identificador da ScrollBar Horizontal
#DEFINE B_ScrollBarVertical     Nil // Bloco de código que especializa a Scroll Vertical
#DEFINE B_ScrollBarHorizontal   Nil // Bloco de código que especializa a Scroll Horizontal
#DEFINE N_ProgressBar           Nil // Guarda a quantidade de ProgressBar. Observação: Para janela do tipo MsgAguarde
#DEFINE N_ItemId                Nil
#DEFINE L_ComEmbutidas          .F.
#define V_LstTituloLabels       {}  // Guarda IDs de título para MudeSubtitle (GTNAP)
#DEFINE N_PaiWindowNum          NIL // Parent window num

*
V_Janela := { N_LinIni , N_ColIni , N_LinFin , N_ColFin , ;
                C_TelaCoberta , N_LinAnt , N_ColAnt , VC_Titulo , N_LinBotoes , ;
                N_Lin1Livre , N_Col1Livre , N_Lin2Livre , N_Col2Livre , ;
                N_LinMess, C_CorJan , C_CorInten, C_CdTela,;
                N_TP_Jan, VX_SubObj, B_Metodo, V_RegiaoBotoes,;
                N_WindowNum, aGuiObjects, N_MargemSuperior, L_DesenhaBox, L_Embutida,;
                V_BotoesToolBar, L_TemScrollVertical, L_TemScrollHorizontal,C_Cabec,;
                L_RolaCima, L_RolaBaixo, L_RolaEsquerda, L_RolaDireita,;
                N_LinMarcadorVertical, N_ColMarcadorHorizontal,;
                L_CriarToolBar, V_Janela_Pai, N_EspacamentoEmPixels, L_MainCoord_Atu,;
                L_MainCoord_Ant, V_LstImagens, N_DeslocaCabecalho, L_JanTipoMsgAguarde,;
                N_IdProgressBar1, V_LstAcoes, L_CUA_10, N_IdProgressBar2, ;
                B_SetInkeyAfterBlock_Old,N_ToolBarCodigoAcao,;
                N_IdScrollBarVertical,N_IdScrollBarHorizontal,B_ScrollBarVertical,B_ScrollBarHorizontal,;
                N_ProgressBar, N_ItemId, L_ComEmbutidas, V_LstTituloLabels, N_PaiWindowNum }

IF V_Janela_Pai # NIL
    #DEFINE L_PaiComEmbutidas  V_Janela_Pai[57]
    L_PaiComEmbutidas := .T.
    #UNDEF L_PaiComEmbutidas
ENDIF

#UNDEF C_CorJan
#UNDEF C_TelaCoberta
#UNDEF N_LinAnt
#UNDEF N_ColAnt
#UNDEF N_LinBotoes
#UNDEF N_TP_Jan
#UNDEF VX_SubObj
#UNDEF N_LinMess
#UNDEF N_WindowNum
#UNDEF aGuiObjects
#UNDEF V_BotoesToolBar
#UNDEF L_TemScrollVertical
#UNDEF L_TemScrollHorizontal
#UNDEF L_RolaCima
#UNDEF L_RolaBaixo
#UNDEF L_RolaEsquerda
#UNDEF L_RolaDireita
#UNDEF N_LinMarcadorVertical
#UNDEF N_ColMarcadorHorizontal
#UNDEF L_CriarToolBar
#UNDEF L_MainCoord_Ant
#UNDEF V_LstImagens
#UNDEF L_JanTipoMsgAguarde
#UNDEF V_LstAcoes
#UNDEF N_IdProgressBar1 // #UNDEF POSICAO_45_A_REUTILIZAR
#UNDEF N_IdProgressBar2 // #UNDEF POSICAO_48_A_REUTILIZAR
#UNDEF B_SetInkeyAfterBlock_Old
#UNDEF N_ToolBarCodigoAcao
#UNDEF N_IdScrollBarVertical
#UNDEF N_IdScrollBarHorizontal
#UNDEF B_ScrollBarVertical
#UNDEF B_ScrollBarHorizontal
#UNDEF N_ProgressBar
#UNDEF N_ItemId
#UNDEF L_ComEmbutidas
#UNDEF V_LstTituloLabels
#UNDEF N_PaiWindowNum

RETURN V_Janela

*
******************
FUNC CriarJanela20(N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec,;
                   C_SubCabec,C_CdTela,X_PARAMETRO_A_REUTILIZAR,N_DeslocaCabecalho,;
                   N_EspacamentoEmPixels,V_Janela_Pai)
******************
LOCAL VX_Janela
LOCAL L_CUA_10  := .F.
LOCAL VC_TxtBotoes_10 := {}
*
VX_Janela := CriarJanela ( N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec, ;
                           VC_TxtBotoes_10, C_CdTela, V_Janela_Pai, C_SubCabec,;
                           N_EspacamentoEmPixels, N_DeslocaCabecalho,;
                           L_CUA_10)  // parâmetro novo na CUA 2.0
RETURN VX_Janela

*
* DEFINICOES PARA USO GERAL
*
#DEFINE N_LinIni                    VX_Janela[01]
#DEFINE N_ColIni                    VX_Janela[02]
#DEFINE N_LinFin                    VX_Janela[03]
#DEFINE N_ColFin                    VX_Janela[04]
#DEFINE C_TelaCoberta               VX_Janela[05]
#DEFINE N_LinAnt                    VX_Janela[06]
#DEFINE N_ColAnt                    VX_Janela[07]
#DEFINE VC_Titulo                   VX_Janela[08]
#DEFINE N_LinBotoes                 VX_Janela[09]
#DEFINE N_Lin1Livre                 VX_Janela[10]        // usado também no JANELA.CH
#DEFINE N_Col1Livre                 VX_Janela[11]        // idem
#DEFINE N_Lin2Livre                 VX_Janela[12]        // idem
#DEFINE N_Col2Livre                 VX_Janela[13]        // idem
#DEFINE N_LinMess                   VX_Janela[14]        // idem
#DEFINE C_CorJan                    VX_Janela[15]        // usado também no JANELA.CH
#DEFINE C_CorInten                  VX_Janela[16]        // idem
#DEFINE C_CdTela                    VX_Janela[17]        // idem
#DEFINE N_TP_Jan                    VX_Janela[18]        // preenchido nos sub-objetos (usado no JANELA.CH)
#DEFINE VX_SubObj                   VX_Janela[19]        // idem
#DEFINE B_Metodo                    VX_Janela[20]        // tem default, mas pode ser sobreposto nos sub-objetos
#DEFINE V_RegiaoBotoes              VX_Janela[21]        // dados sobre os botões de função
#DEFINE N_WindowNum                 VX_Janela[22]
#DEFINE aGuiObjects                 VX_Janela[23]
#DEFINE N_MargemSuperior            VX_Janela[24]
#DEFINE L_DesenhaBox                VX_Janela[25]
#DEFINE L_Embutida                  VX_Janela[26]
#DEFINE V_BotoesToolBar             VX_Janela[27]
#DEFINE L_TemScrollVertical         VX_Janela[28]
#DEFINE L_TemScrollHorizontal       VX_Janela[29]
#DEFINE C_Cabec                     VX_Janela[30]
#DEFINE L_RolaCima                  VX_Janela[31]
#DEFINE L_RolaBaixo                 VX_Janela[32]
#DEFINE L_RolaEsquerda              VX_Janela[33]
#DEFINE L_RolaDireita               VX_Janela[34]
#DEFINE N_LinMarcadorVertical       VX_Janela[35]
#DEFINE N_ColMarcadorHorizontal     VX_Janela[36]
#DEFINE L_CriarToolBar              VX_Janela[37]
#DEFINE V_Janela_Pai                VX_Janela[38]
#DEFINE N_EspacamentoEmPixels       VX_Janela[39]
#DEFINE L_MainCoord_Atu             VX_Janela[40]
#DEFINE L_MainCoord_Ant             VX_Janela[41]
#DEFINE V_LstImagens                VX_Janela[42]
#DEFINE N_DeslocaCabecalho          VX_Janela[43]
#DEFINE L_JanTipoMsgAguarde         VX_Janela[44]
#DEFINE N_IdProgressBar1            VX_Janela[45]
#DEFINE V_LstAcoes                  VX_Janela[46]
#DEFINE L_CUA_10                    VX_Janela[47]
#DEFINE N_IdProgressBar2            VX_Janela[48]
#DEFINE B_SetInkeyAfterBlock_Old    VX_Janela[49]
#DEFINE N_ToolBarCodigoAcao         VX_Janela[50]
#DEFINE N_IdScrollBarVertical       VX_Janela[51]
#DEFINE N_IdScrollBarHorizontal     VX_Janela[52]
#DEFINE B_ScrollBarVertical         VX_Janela[53]
#DEFINE B_ScrollBarHorizontal       VX_Janela[54]
#DEFINE N_ProgressBar               VX_Janela[55]
#DEFINE N_ItemId                    VX_Janela[56]   // For Menuvert, TextView or TableView ids (GTNAP)
#DEFINE L_ComEmbutidas              VX_Janela[57]   // This Janela has child (embutidas) windows
#DEFINE V_LstTituloLabels           VX_Janela[58]   // IDs Labels de título (GTNAP)
#DEFINE N_PaiWindowNum              VX_Janela[59]

*************
PROC AddBotao (VX_Janela,C_TxtBotao,B_AcaoBotao,L_AutoClose,;
               C_CdBotao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda,L_EofOk, L_HandleMuda,;
               L_MudaDados,L_BotaoAutomatico)
*************
DEFAULT L_AutoClose       TO .F.
DEFAULT L_BotaoAutomatico TO .F.
DEFAULT L_MudaDados       TO .F.
*
*
IF L_CUA_10
   ? MEMVAR->ADDBOTAO_EXCLUSIVO_DA_CUA_20
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDBOTAO_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
AADD(V_RegiaoBotoes,{NIL,;          // _BOTAO_LIN_INICIAL
                     NIL,;          // _BOTAO_COL_INICIAL
                     NIL,;          // _BOTAO_LIN_FINAL
                     NIL,;          // _BOTAO_COL_FINAL
                     C_TxtBotao,;   // _BOTAO_TEXTO_COMANDO
                     NIL,;          // _BOTAO_TEXTO_TRATADO_1
                     NIL,;          // _BOTAO_TEXTO_TRATADO_2
                     NIL,;          // _BOTAO_COL_DESTAQUE
                     NIL,;          // _BOTAO_TEXTO_DESTAQUE
                     B_AcaoBotao,;  // _BOTAO_BLOCO_ACAO
                     L_AutoClose,;  // _BOTAO_AUTOCLOSE
                     C_CdBotao,;    // _BOTAO_CDBOTAO
                     L_AliasMuda,;  // _BOTAO_ALIAS_MUDA
                     L_RecnoMuda,;  // _BOTAO_RECNO_MUDA
                     L_FilterMuda,; // _BOTAO_FILTER_MUDA
                     L_OrderMuda,;  // _BOTAO_ORDER_MUDA
                     L_EofOk,;      // _BOTAO_EOFOK
                     L_HandleMuda,; // _BOTAO_HANDLE_MUDA
                     NIL,;          // _BOTAO_INKEY_DESTAQUE
                     NIL,;          // _BOTAO_INKEY_DESTAQUE_CASE
                     NIL,;          // _BOTAO_HANDLE_PUSHBUTTON
                     L_MudaDados})  // _BOTAO_MUDADADOS

*
***************
PROC AddImagem (VX_Janela,C_ArquivoImagem,;
                N_LinIniImagem,N_ColIniImagem,N_LinFinImagem,N_ColFinImagem,;
                N_Keyboard,C_CdBotao)
***************
*
IF .NOT. L_CUA_10
   ? MEMVAR->ADDIMAGEM_ESPECIFICA_PARA_A_CUA_10
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDIMAGEM_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
AADD(V_LstImagens,{C_ArquivoImagem,;  // _IMAGEM_ARQUIVO
                   N_LinIniImagem,;   // _IMAGEM_LIN_INICIAL
                   N_ColIniImagem,;   // _IMAGEM_COL_INICIAL
                   N_LinFinImagem,;   // _IMAGEM_LIN_FINAL
                   N_ColFinImagem,;   // _IMAGEM_COL_FINAL
                   NIL,;              // _IMAGEM_BLOCO_ACAO
                   .F.,;              // _IMAGEM_AUTOCLOSE
                   C_CdBotao,;        // _IMAGEM_CDBOTAO
                   NIL,;              // _IMAGEM_ALIAS_MUDA
                   NIL,;              // _IMAGEM_RECNO_MUDA
                   NIL,;              // _IMAGEM_FILTER_MUDA
                   NIL,;              // _IMAGEM_ORDER_MUDA
                   NIL,;              // _IMAGEM_EOFOK
                   NIL,;              // _IMAGEM_HANDLE_MUDA
                   N_Keyboard,;       // _IMAGEM_KEYBOARD
                   InKeyCaseAlternativo(N_Keyboard)})  // _IMAGEM_KEYBOARD_CASE
*
****************
PROC AddImagem20 (VX_Janela,C_ArquivoImagem,;
                  N_LinIniImagem,N_ColIniImagem,N_LinFinImagem,N_ColFinImagem,;
                  B_AcaoImagem,L_AutoClose,;
                  C_CdBotao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_EofOk, L_HandleMuda, L_MudaDados)
****************
*
DEFAULT L_MudaDados TO .F.
*
IF L_CUA_10
   ? MEMVAR->ADDIMAGEM_ESPECIFICA_PARA_A_CUA_20
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDIMAGEM_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
IF INABILITA_IMAGEM(L_MudaDados)
   B_AcaoImagem := {||.F.}  // Inabilitar a ação da imagem.
ENDIF
*
AADD(V_LstImagens,{C_ArquivoImagem,;  // _IMAGEM_ARQUIVO
                   N_LinIniImagem,;   // _IMAGEM_LIN_INICIAL
                   N_ColIniImagem,;   // _IMAGEM_COL_INICIAL
                   N_LinFinImagem,;   // _IMAGEM_LIN_FINAL
                   N_ColFinImagem,;   // _IMAGEM_COL_FINAL
                   B_AcaoImagem,;     // _IMAGEM_BLOCO_ACAO
                   L_AutoClose,;      // _IMAGEM_AUTOCLOSE
                   C_CdBotao,;        // _IMAGEM_CDBOTAO
                   L_AliasMuda,;      // _IMAGEM_ALIAS_MUDA
                   L_RecnoMuda,;      // _IMAGEM_RECNO_MUDA
                   L_FilterMuda,;     // _IMAGEM_FILTER_MUDA
                   L_OrderMuda,;      // _IMAGEM_ORDER_MUDA
                   L_EofOk,;          // _IMAGEM_EOFOK
                   L_HandleMuda,;     // _IMAGEM_HANDLE_MUDA
                   NIL,;              // _IMAGEM_KEYBOARD
                   NIL,;              // _IMAGEM_KEYBOARD_CASE
                   L_MudaDados})      // _IMAGEM_MUDADADOS
*
************
PROC AddAcao (VX_Janela,N_Keyboard,B_AcaoBotao,L_AutoClose,;
              C_CdBotao, L_AliasMuda, L_RecnoMuda, L_FilterMuda, L_OrderMuda, L_EofOk, L_HandleMuda, L_MudaDados)
************
*
DEFAULT L_MudaDados TO .F.
*
* A função AddAcao sé chamada pelo COMANDO ADDCAO ou pela função AJUSTA_BOTOES()
*
IF L_CUA_10
   ? MEMVAR->ADDACAO_ESPECIFICA_PARA_A_CUA_20
ENDIF
*
IF .NOT. VX_SubObj==NIL
   ? MEMVAR->ADDACAO_EM_JANELA_JA_ESPECIALIZADA
ENDIF
*
IF .NOT. LEFT(C_CdBotao,1) == "B"
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(TROCA(C_CdBotao,"B0123456789?",""))==0
   ? MEMVAR->ERRO_ASSUME
ENDIF
IF .NOT. LEN(C_CdBotao)==6
   ? MEMVAR->ERRO_ASSUME
ENDIF
*
IF INABILITA_ADDACAO(L_MudaDados)
    B_AcaoBotao := {||.F.}  // Inabilitar a ação do comando ADDACAO.
 ENDIF
 *
AADD(V_LstAcoes,{N_Keyboard,;       // _ACAO_KEYBOARD
                 InKeyCaseAlternativo(N_Keyboard),;  // _ACAO_KEYBOARD_CASE
                 B_AcaoBotao,;      // _ACAO_BLOCO_ACAO
                 L_AutoClose,;      // _ACAO_AUTOCLOSE
                 C_CdBotao,;        // _ACAO_CDBOTAO
                 L_AliasMuda,;      // _ACAO_ALIAS_MUDA
                 L_RecnoMuda,;      // _ACAO_RECNO_MUDA
                 L_FilterMuda,;     // _ACAO_FILTER_MUDA
                 L_OrderMuda,;      // _ACAO_ORDER_MUDA
                 L_EofOk,;          // _ACAO_EOFOK
                 L_HandleMuda,;     // _ACAO_HANDLE_MUDA
                 L_MudaDados})      // _ACAO_MUDADADOS
*
******************************
STAT FUNC InKeyCaseAlternativo (N_Keyboard)
******************************
LOCAL N_Keyboard_Case
LOCAL C_Char_Equiv
IF N_KeyBoard # NIL
   * Tornar o N_Keyboard "case insensitive"
   C_Char_Equiv := CHR(N_Keyboard)
   IF C_Char_Equiv >= "a" .AND. C_Char_Equiv <= "z"
      N_Keyboard_Case := ASC(UPPER(C_Char_Equiv))
   ELSEIF C_Char_Equiv >= "A" .AND. C_Char_Equiv <= "Z"
      N_Keyboard_Case := ASC(LOWER(C_Char_Equiv))
   ELSE
      N_Keyboard_Case := N_Keyboard  // não é letra
   ENDIF
ENDIF
RETURN N_Keyboard_Case


******************************
PROC NAP_HELP(VX_Janela)
******************************
LOCAL N_Sel, L_RunHelp := .F.

IF N_TP_Jan == _JAN_ENTRADA_10
    #DEFINE VX_Edicao VX_SubObj
    #DEFINE V_Lst_CdGET VX_Edicao[20]
    N_Sel := NAP_CUALIB_WINDOW_CURRENT_EDIT()
    IF N_Sel > 0
        XXHELP(C_CdTela, C_Cabec, V_Lst_CdGET[N_Sel][1], V_Lst_CdGET)
        L_RunHelp := .T.
    ENDIF
    #UNDEF V_Lst_CdGET
    #UNDEF VX_Edicao
ENDIF

IF .NOT. L_RunHelp
    XXHELP(C_CdTela, C_Cabec, NIL, NIL)
ENDIF

RETURN

**************
FUNCTION Ative ( VX_Janela )
**************
LOCAL N_Largura, N_LinImp, X_Retorno
LOCAL N_CursorAnt := SET(_SET_CURSOR,SC_NONE)        // salvar modo do cursor
LOCAL C_CorAnt    := SETCOLOR(C_CorJan)              // salvar cor anterior
LOCAL B_Ajuda_Ant  // salvar help anterior, se existir novo
LOCAL N_Cont, N_AddRows, N_IdLabel
LOCAL C_Cabec_Aux
LOCAL L_AcrescentarSeparadorSubtitulo, L_MostraGrade
LOCAL L_AutoClose := .F.

// Window flags
LOCAL L_CLOSE_WITH_RETURN := .F.
LOCAL L_CLOSE_WITH_ESC := .F.
LOCAL L_MINIMIZE_BUTTON := .F.
LOCAL L_BUTTONS_NAVIGATION := .F.

// FRAN: A NAppGUI/GTNAP application owns the event cicle.
// The hotkey should be asigned when Window is created after NAP_WINDOW()
IF .NOT. SOB_MODO_GRAFICO()
    B_Ajuda_Ant := SETKEY(K_F1,{||XXHELP(C_CdTela,C_Cabec,NIL,NIL)})
ENDIF
*
IF N_LinBotoes == NIL
    ? MEMVAR->AJUSTA_BOTOES_DEVE_SER_CHAMADA_UMA_VEZ
ENDIF
*
IF C_TelaCoberta == NIL    // se janela ainda não foi aberta, abrí-la
    *
    N_LinAnt    := ROW()
    N_ColAnt    := COL()
    *

    IF (LEN(V_PilhaJanelas) > 0)
        N_PaiWindowNum := V_PilhaJanelas[LEN(V_PilhaJanelas)][1]
    ELSE
        N_PaiWindowNum := 0
    ENDIF

    IF SOB_MODO_GRAFICO()
        * A restauração do trecho da tela anterior que vai ser coberta
        * pela tela atual é feita automaticamente pelo Windows.

        C_TelaCoberta := ""

        IF .NOT. L_Embutida

            IF LEN(V_PilhaJanelas)==0
                // The first window takes the title from 'Setup_nap' (compatible with GTWVW/Cualib)
                C_Cabec_Aux := NIL
            ELSE

                IF Version()=="Harbour 3.2.0dev (r1703241902)"
                    IF CABEC_TESTE_AUTOMATICO()
                        C_Cabec_Aux := StrTran(C_Cabec," ","_")
                        C_Cabec_Aux := subs(Tiracen(C_Cabec_Aux),1,15)+"_"+C_CdTela+"_"
                    ELSE
                        C_Cabec_Aux := HB_OEMtoANSI(C_Cabec)
                    ENDIF
                ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // PENDENTE_LINUX
                    IF CABEC_TESTE_AUTOMATICO()
                        C_Cabec_Aux := StrTran(C_Cabec," ","_")
                        C_Cabec_Aux := subs(Tiracen(C_Cabec_Aux),1,15)+"_"+C_CdTela+"_"
                    ELSE
                        C_Cabec_Aux := C_Cabec
                    ENDIF
                ELSE
                    // FRAN: Harbour 3.2.0dev (r2104281802)
                    C_Cabec_Aux := C_Cabec
                ENDIF
            ENDIF

            IF L_JanTipoMsgAguarde
                IF N_ProgressBar == 1
                    N_LinFin := N_LinFin + 2
                ELSEIF N_ProgressBar == 2
                    N_LinFin := N_LinFin + 4
                Endif
            ENDIF

            // Windows flags based on specializations
            IF N_TP_Jan == NIL
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_PERGUNTAR
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .T.
                L_BUTTONS_NAVIGATION := .T.

            ELSEIF N_TP_Jan == _JAN_INFORMAR
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .T.
                L_BUTTONS_NAVIGATION := .T.

            ELSEIF N_TP_Jan == _JAN_TEXTO_10
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_ARQTEXTO_10
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_ENTRADA_10
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_MENU_VERT
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_SELE_VETO_20
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ELSEIF N_TP_Jan == _JAN_SELE_ARQ_20
                L_CLOSE_WITH_ESC := .T.
                L_CLOSE_WITH_RETURN := .F.

            ENDIF

            IF LEN(V_PilhaJanelas)==0
                L_MINIMIZE_BUTTON := .T.
            ELSE
                L_MINIMIZE_BUTTON := .F.
            ENDIF

            N_WindowNum := NAP_WINDOW(N_LinIni, N_ColIni, N_LinFin, N_ColFin, C_Cabec_Aux, L_CLOSE_WITH_RETURN, L_CLOSE_WITH_ESC, L_MINIMIZE_BUTTON, L_BUTTONS_NAVIGATION)
            NAP_WINDOW_HOTKEY(N_WindowNum, K_F1,{||NAP_HELP(VX_Janela)}, .F.)

            AADD(V_PilhaJanelas,{N_WindowNum,VX_Janela})

        ELSE // L_Embutida
            N_WindowNum := NAP_WINDOW_EMBEDDED(N_PaiWindowNum, N_LinIni, N_ColIni, N_LinFin, N_ColFin, .T.)

        ENDIF // .NOT. L_Embutida

    ELSE // SOB_MODO_GRAFICO()

        C_TelaCoberta := SAVESCREEN(N_LinIni,N_ColIni,N_LinFin,N_ColFin)
        N_WindowNum := LEN(V_PilhaJanelas)

        IF .NOT. L_Embutida
            AADD(V_PilhaJanelas,{LEN(V_PilhaJanelas),VX_Janela})
        ENDIF

    ENDIF // SOB_MODO_GRAFICO()

    // FRAN: Here the code is compatible GTNAP/Text terminals GTXXX
    SCROLL(N_LinIni,N_ColIni,N_LinFin,N_ColFin)      // limpar área

    *
    * montar cabeçalho
    *
    N_Largura := N_Col2Livre-N_Col1Livre+1
    N_LinImp  := N_LinIni+N_MargemSuperior
    *
    FOR N_Cont := 1 TO LEN(VC_Titulo)
        IF SOB_MODO_GRAFICO()
            N_IdLabel := NAP_LABEL(N_WindowNum, N_LinImp-1+N_Cont, N_Col1Livre+N_DeslocaCabecalho, PADC(VC_Titulo[N_Cont],N_Largura-N_DeslocaCabecalho), .F.)
            AADD(V_LstTituloLabels,N_IdLabel)
        ELSE
            SETPOS(N_LinImp-1+N_Cont,N_Col1Livre+N_DeslocaCabecalho)
            DISPOUT(PADC(VC_Titulo[N_Cont],N_Largura-N_DeslocaCabecalho))
        ENDIF
    NEXT
    *
    * montar área de função
    *
    N_LinImp  := N_LinMess
    *

    * Adding Labels
    IF SOB_MODO_GRAFICO()
        IF N_WindowNum = 0 .AND. EH_PRODUCAO()
            DesenhaDrawLabe(VX_Janela)
        ELSEIF N_WindowNum = 1 .AND. .NOT. EH_PRODUCAO()
            DesenhaDrawLabe(VX_Janela)
        ENDIF
    ENDIF

    *
    * Adding Toolbar (only in main (not embutida) windows)
    *
    IF SOB_MODO_GRAFICO() .AND. L_CriarToolBar .AND. .NOT. L_Embutida

        L_AcrescentarSeparadorSubtitulo := .T.
        IF N_TP_Jan == _JAN_SELE_ARQ_20
            #DEFINE VX_Sele  VX_SubObj
            L_MostraGrade  := VX_Sele:CARGO[09]      // Se mostra o grid
            #UNDEF VX_Sele
            IF L_MostraGrade
                L_AcrescentarSeparadorSubtitulo := .F.
            ENDIF
        ENDIF

        IF N_TP_Jan == _JAN_SELE_VETO_20
           #DEFINE VX_Sele  VX_SubObj
           L_MostraGrade  := VX_Sele:CARGO[09]      // Se mostra o grid
           #UNDEF VX_Sele
           IF L_MostraGrade
              L_AcrescentarSeparadorSubtitulo := .F.
           ENDIF
        ENDIF

        * Não exibir saparador, pois o box do texto já serve como separador visual
        IF N_TP_Jan == _JAN_TEXTO_10 .OR. ;
            N_TP_Jan == _JAN_ARQTEXTO_10
            L_AcrescentarSeparadorSubtitulo := .F.
        ENDIF

        * Primeiramente definir a toolbar
        ADDGUI_TOOLBAR(VX_Janela)

    ENDIF   // SOB_MODO_GRAFICO() .AND. L_CriarToolBar

    *
    * Adding Buttons
    *
    IF SOB_MODO_GRAFICO()
        FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
            ADICIONA_BOTAO_PUSH(VX_Janela,N_Cont)
        NEXT

    ELSE // Buttons in text mode
        * imprimir o texto do botao
        FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
            SETPOS(N_LinImp+V_RegiaoBotoes[N_Cont,_BOTAO_LIN_INICIAL],;
            N_Col1Livre+V_RegiaoBotoes[N_Cont,_BOTAO_COL_INICIAL])
            DISPOUT(V_RegiaoBotoes[N_Cont,_BOTAO_TEXTO_TRATADO_2])
        NEXT
        *
        * dar destaque as teclas de função
        FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
            SETPOS(N_LinImp+V_RegiaoBotoes[N_Cont,_BOTAO_LIN_INICIAL],;
            N_Col1Livre+V_RegiaoBotoes[N_Cont,_BOTAO_COL_DESTAQUE])
            DISPOUT(V_RegiaoBotoes[N_Cont,_BOTAO_TEXTO_DESTAQUE],C_CorInten)
        NEXT

    ENDIF   // SOB_MODO_GRAFICO() Buttons

    *
    * Adding Images
    *
    IF SOB_MODO_GRAFICO()
        FOR N_Cont := 1 TO LEN(V_LstImagens)

            NAP_IMAGE(N_WindowNum, N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_INICIAL],;
                    N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_INICIAL],;
                    N_LinIni+V_LstImagens[N_Cont,_IMAGEM_LIN_FINAL  ],;
                    N_ColIni+V_LstImagens[N_Cont,_IMAGEM_COL_FINAL  ],;
                    V_LstImagens[N_Cont,_IMAGEM_ARQUIVO],;
                    V_LstImagens[N_Cont,_IMAGEM_BLOCO_ACAO],;
                    V_LstImagens[N_Cont,_IMAGEM_AUTOCLOSE],;
                    .F.)
        NEXT
    ENDIF // SOB_MODO_GRAFICO() Images

ENDIF // C_TelaCoberta == NIL

IF N_TP_Jan == NIL
    X_Retorno := EVAL(B_Metodo,VX_Janela)
ELSE
    X_Retorno := EVAL(B_Metodo)
ENDIF
 *
 SETCOLOR(C_CorAnt)                    // restaurar cor anterior
 SET(_SET_CURSOR,N_CursorAnt)          // restaurar modo do cursor

 IF .NOT. SOB_MODO_GRAFICO()
    SETKEY(K_F1,B_Ajuda_Ant)            // restaurar ajuda anterior
 ENDIF
 *
 IF .NOT. L_CUA_10
    DestruaJan(VX_Janela,.T.)           // Na CUA 2.0, a janela sempre fecha após ativação
 ELSE

 ENDIF

RETURN X_Retorno   // Ative ( VX_Janela )
*
*
*

*********************
STAT FUNC EH_PRODUCAO
*********************
LOCAL L_EH_PRODUCAO := .T.
#IFDEF _TESTE
   L_EH_PRODUCAO := .F.
#ENDIF
RETURN L_EH_PRODUCAO
*

******************
PROC AJUSTA_BOTOES(VX_Janela)
******************
LOCAL N_Cont
* não imprime nada, só retorna as teclas devidamente formatadas
*
IF N_LinBotoes # NIL
   ? MEMVAR->AJUSTA_BOTOES_SO_PODE_SER_CHAMADA_UMA_VEZ
ENDIF
*
N_LinBotoes := TecFunc(N_Col2Livre-N_Col1Livre+1,@V_RegiaoBotoes)
N_Lin2Livre := N_Lin2Livre - N_LinBotoes
N_LinMess   := N_Lin2Livre+1 // linha de mensagens
*
FOR N_Cont := 1 TO LEN(V_RegiaoBotoes)
   IF V_RegiaoBotoes[N_Cont,_BOTAO_INKEY_DESTAQUE] # NIL .AND. ;  // deu para descobrir o INKEY
      .NOT. L_CUA_10
      * Permitir disparar o evento também através do teclado
      AddAcao(VX_Janela,V_RegiaoBotoes[N_Cont,_BOTAO_INKEY_DESTAQUE],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_BLOCO_ACAO],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_AUTOCLOSE],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_CDBOTAO],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_ALIAS_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_RECNO_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_FILTER_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_ORDER_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_EOFOK],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_HANDLE_MUDA],;
                        V_RegiaoBotoes[N_Cont,_BOTAO_MUDADADOS])
   ENDIF
NEXT
*

**************************
PROC SETA_PARA_TER_TOOLBAR (VX_Janela)
**************************
IF L_CriarToolBar
   ? MEMVAR->CONFIGURACAO_PARA_TER_TOOLBAR_JA_FEITA
ENDIF

L_CriarToolBar := .T.

IF SOB_MODO_GRAFICO()
    * GTNAP nothing to do here
ELSE
   * O modo texto não tem ToolBar, mas o cabeçalho será acrescentado
   * uma linha em branco(no topo), de forma que o programador,
   * ao usar o modo texto durante a fase de desenvolvimento,
   * já não veja, APROXIMADAMENTE, o que a ToolBar esconderá da
   * da tela anterior.
   ASIZE(VC_Titulo,LEN(VC_Titulo)+1)
   AINS(VC_Titulo,1)
   VC_Titulo[1] := ""
ENDIF

N_Lin1Livre++


************************
STAT PROC ADDGUI_TOOLBAR(VX_Janela)
************************
LOCAL L_PermiteEdicao
LOCAL L_TOOLBAR_AINDA_SEM_BOTOES := .T.
LOCAL N_TelaHeight := TelaPrincipalHeight()
LOCAL N_PixelsBotao
LOCAL L_MudaDados
*
* Setar o tamanho do botão de forma proporcional,
* de forma que a altura da ToolBar não ultrapasse
* 2 linhas do sistema.
* Isto torna o tamanho da ToolBar mais proporcional,
* quando usado principalmente em resoluções pequenas
* (ex: 800 x 600 de projetores)
*
IF N_TelaHeight >= 960
    * A partir deste ponto, a ToolBar já é menor que 2 linhas
    * de texto, mesmo usando o tamanho real do BITMAP.
    N_PixelsBotao := 32 // Tamanho real das imagens é 32 x 32
ELSEIF N_TelaHeight >=  864
    N_PixelsBotao := 30
ELSEIF N_TelaHeight >=  768
    N_PixelsBotao := 28
ELSEIF N_TelaHeight >=  720
    N_PixelsBotao := 25
ELSEIF N_TelaHeight >= 600
    N_PixelsBotao := 20
ELSE
    N_PixelsBotao := 18
ENDIF

NAP_TOOLBAR(N_WindowNum, N_PixelsBotao)

IF TEM_BOTAO(VX_Janela,{"Incluir","Alterar","Excluir","Consultar"})
    *
    IF TEM_BOTAO(VX_Janela,{"Incluir"},.T.,@L_MudaDados)
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_INCLUI,_BITMAP_INCLUI_DESAB},"Incluir",{||__Keyboard("I")},0,L_MudaDados)
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Alterar"},.T.,@L_MudaDados)
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_ALTERA,_BITMAP_ALTERA_DESAB},"Alterar",{||__Keyboard("A")},1,L_MudaDados)
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Excluir"},.T.,@L_MudaDados)
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_EXCLUI,_BITMAP_EXCLUI_DESAB},"Excluir",{||__Keyboard("E")},2,L_MudaDados)
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Consultar"})
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_CONSULTA},"Consultar",{||__Keyboard("C")})
    ENDIF
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
    *
ENDIF
*
IF TEM_BOTAO(VX_JANELA,{"Listar","Procurar"})
    *
    IF TEM_BOTAO(VX_Janela,{"Listar"})
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_PRINTER} ,"Listar",{||__Keyboard("L")})
    ENDIF
    *
    IF TEM_BOTAO(VX_Janela,{"Procurar"})
        ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_PESQUISE},"Procurar",{||__Keyboard("P")})
    ENDIF
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
    *
IF TEM_BOTAO(VX_Janela,{"F2=ok"})
    IF TEM_BOTAO(VX_Janela,{"F2=salvar"})
        ? MEMVAR->BOTAO_OK_E_SALVAR_AO_MESMO_TEMPO
    ENDIF
    *
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_OK},"Ok",{||HB_KeyPut(K_F2)})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
*
IF TEM_BOTAO(VX_Janela,{"F2=salvar"},.T.,@L_MudaDados)
    IF TEM_BOTAO(VX_Janela,{"F2=ok"})
        ? MEMVAR->BOTAO_OK_E_SALVAR_AO_MESMO_TEMPO
    ENDIF
    *
    ADICIONA_BOTAO_TOOLBAR(VX_Janela,{_BITMAP_SALVAR,_BITMAP_SALVAR_DESAB},"Salvar",{||HB_KeyPut(K_F2)},,L_MudaDados)
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
*
IF N_TP_Jan == _JAN_TEXTO_10
    #DEFINE B_Edita   VX_SubObj[10]
    L_PermiteEdicao := EVAL(B_Edita)
    #UNDEF B_Edita
ENDIF
IF N_TP_Jan == _JAN_ENTRADA_10
    #DEFINE VX_Edicao      VX_SubObj
    #DEFINE B_EditaGlobal  VX_Edicao[15]
    L_PermiteEdicao := EVAL(B_EditaGlobal)
    #UNDEF B_EditaGlobal
    #UNDEF VX_Edicao
ENDIF
IF N_TP_Jan == _JAN_ARQTEXTO_10
    L_PermiteEdicao := .F.
ENDIF
*
IF N_TP_Jan == _JAN_TEXTO_10 .OR. ;
    N_TP_Jan == _JAN_ENTRADA_10 .OR. ;
    N_TP_Jan == _JAN_ARQTEXTO_10
    *
    IF L_PermiteEdicao
        * No teclado será colocado, de fato, a K_CTRL_F11, mas a AjustaTecla() voltará o conteúdo para K_CTRL_X.
        ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_RECORTAR}, "Recortar", BLOCO_TOOLBAR({||HB_KeyPut(K_CTRL_X_TROCADO_POR_K_CTRL_F11)}, {||NAP_WINDOW_CUT(N_WindowNum)}))
    ENDIF
    *
    * No teclado será colocado, de fato, a K_CTRL_F9, mas a AjustaTecla() voltará o conteúdo para K_CTRL_C.
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_COPIA}, "Copiar", BLOCO_TOOLBAR({||HB_KeyPut(K_CTRL_C_TROCADO_POR_K_CTRL_F9)}, {||NAP_WINDOW_COPY(N_WindowNum)}))
    *
    IF L_PermiteEdicao
        * No teclado será colocado, de fato, a K_CTRL_F10, mas a AjustaTecla() voltará o conteúdo para K_CTRL_V.
        ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_COLAR}, "Colar", BLOCO_TOOLBAR({||HB_KeyPut(K_CTRL_V_TROCADO_POR_K_CTRL_F10)}, {||NAP_WINDOW_PASTE(N_WindowNum)}))
    ENDIF
    *
    IF L_PermiteEdicao
        * No teclado será colocado, de fato, a K_CTRL_F12, mas a AjustaTecla() voltará o conteúdo para K_CTRL_Z.
        ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_DESFAZER}, "Desfazer", BLOCO_TOOLBAR({||HB_KeyPut(K_CTRL_Z_TROCADO_POR_K_CTRL_F12)}, {||NAP_WINDOW_UNDO(N_WindowNum)}))
    ENDIF
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
    *
ENDIF
*
IF C_CdTela == "T03221"  // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","aPfO9X_r63k","Cotacoes_de_Precos")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T01306" .AND. LEN(VC_Titulo) > 1    // DESCONSIDERAR_CHECA_ID
    IF VC_Titulo[2] == "PREGÃO"
        ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","aYae8t-oviM","Corona_Interno")})
        ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
        L_TOOLBAR_AINDA_SEM_BOTOES := .F.

    ELSEIF VC_Titulo[2] == "TOMADA DE PREÇOS"
        ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","05Bukz8ymck","Definicao_de_Lotes_de_Licitacao")})
        ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
        L_TOOLBAR_AINDA_SEM_BOTOES := .F.
    ENDIF

ELSEIF C_CdTela == "T00380"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","8J_vOoVMeOs","Empenho")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T00452"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","6xlfNLDbMP4","Pagamento")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T00757"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","txoHEuXjxvk","Receita")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T03240"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","RzLdTT7eYPg","Equivalencia_Dotacao")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T06101"    // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","p85Wk3xjlfI","Equivalencia_Dotacao_em_registro_preco")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T18125" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","XvdCnede-pQ","Cadastro_das_comissoes_de_avaliacao_e_inventario")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T17261" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V", "tx7C_-sRJ84", "Cadastro_das_localizacoes")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T17257" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","tx7C_-sRJ84","Cadastro_das_localizacoes")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T17535" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","tx7C_-sRJ84","Cadastro_das_localizacoes")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T03253" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","wgDezwHfHFE","Dotacoes_e_controle_de_saldos")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ELSEIF C_CdTela == "T06290" // DESCONSIDERAR_CHECA_ID
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_VIDEO}, "Vídeo Aula", {||EXIBEVIDEO("V","wgDezwHfHFE","Dotacoes_e_controle_de_saldos")})
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.

ENDIF
*
IF N_TP_Jan # NIL   // indica que janela foi especializada
    * Colocar em todas as telas ?
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_CALCULA}, "Calculadora", BLOCO_TOOLBAR({||HB_KeyPut(K_F5)}, {||CALCULADORA()}))
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_AJUDA}, "Ajuda", BLOCO_TOOLBAR({||HB_KeyPut(K_F1)}, {||NAP_HELP(VX_Janela)}))
    *
    ADICIONA_SEPARADOR_AO_TOOLBAR(VX_Janela)
    *
    ADICIONA_BOTAO_TOOLBAR(VX_Janela, {_BITMAP_SAIDA} ,"Saida", BLOCO_TOOLBAR({||HB_KeyPut(K_ESC)}, {||NAP_WINDOW_STOP_MODAL(N_WindowNum, NAP_MODAL_TOOLBAR)}))
    L_TOOLBAR_AINDA_SEM_BOTOES := .F.
ENDIF
*
* A rotina que fecha a janela sempre destroi a ToolBar.
* Portanto, está se supondo que não exista ToolBar sem botões.
IF L_TOOLBAR_AINDA_SEM_BOTOES
    ? MEMVAR->TOOLBAR_SEM_BOTOES
ENDIF


*
********************************
STAT FUNC BLOCO_TOOLBAR(B_TextGT, B_GraphicGT)
LOCAL B_Ret
IF SOB_MODO_GRAFICO()
    B_Ret := B_GraphicGT
ELSE
    B_Ret := B_TextGT
ENDIF
RETURN B_Ret

*
********************************
STAT PROC ADICIONA_BOTAO_TOOLBAR (VX_Janela,V_TOOLBAR_COD_BITMAP,C_TOOLBAR_TOOLTIP,B_TOOLBAR_BLOCO_ACAO, N_SEQUENCIA, L_MudaDados)
********************************
LOCAL N_TOOLBAR_COD_BITMAP
LOCAL C_BASE_PATH := "../imgtbar/"
LOCAL C_ICON_PATHNAME := ""
*
* Este número vai de 400 em diante, e é necessário ao Windows
N_ToolBarCodigoAcao++
*
IF INABILITA_BOTAO_TOOLBAR(L_MudaDados)
    N_TOOLBAR_COD_BITMAP := V_TOOLBAR_COD_BITMAP[2] // Índice 2, imagem indicando botão desabilitado
    B_TOOLBAR_BLOCO_ACAO := {||.F.}   // Inabilitar a ação do botão.
ELSE
    N_TOOLBAR_COD_BITMAP := V_TOOLBAR_COD_BITMAP[1] // Índice 1, imagem indicando botão habilitado
ENDIF
*
AADD(V_BotoesToolBar,{N_ToolBarCodigoAcao,N_TOOLBAR_COD_BITMAP,C_TOOLBAR_TOOLTIP,B_TOOLBAR_BLOCO_ACAO})

IF N_TOOLBAR_COD_BITMAP == _BITMAP_INCLUI
    C_ICON_PATHNAME := C_BASE_PATH + "inclui.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_ALTERA
    C_ICON_PATHNAME := C_BASE_PATH + "altera.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_EXCLUI
    C_ICON_PATHNAME := C_BASE_PATH + "exclui.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_CONSULTA
    C_ICON_PATHNAME := C_BASE_PATH + "consulta.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_OK
    C_ICON_PATHNAME := C_BASE_PATH + "ok.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_PESQUISE
    C_ICON_PATHNAME := C_BASE_PATH + "pesquise.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_PRINTER
    C_ICON_PATHNAME := C_BASE_PATH + "printer.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_SALVAR
    C_ICON_PATHNAME := C_BASE_PATH + "salvar.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_RECORTAR
    C_ICON_PATHNAME := C_BASE_PATH + "recortar.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_COPIA
    C_ICON_PATHNAME := C_BASE_PATH + "copia.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_COLAR
    C_ICON_PATHNAME := C_BASE_PATH + "colar.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_DESFAZER
    C_ICON_PATHNAME := C_BASE_PATH + "desfazer.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_CALCULA
    C_ICON_PATHNAME := C_BASE_PATH + "calcula.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_AJUDA
    C_ICON_PATHNAME := C_BASE_PATH + "ajuda.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_SAIDA
    C_ICON_PATHNAME := C_BASE_PATH + "saida.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_VIDEO
    C_ICON_PATHNAME := C_BASE_PATH + "video.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_ESPACOVAZIO
    C_ICON_PATHNAME := C_BASE_PATH + "espacovazio.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_INCLUI_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "inclui_i.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_ALTERA_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "altera_i.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_EXCLUI_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "exclui_i.bmp"
ELSEIF N_TOOLBAR_COD_BITMAP == _BITMAP_SALVAR_DESAB
    C_ICON_PATHNAME := C_BASE_PATH + "salvar_i.bmp"
ENDIF

NAP_TOOLBAR_BUTTON(N_WindowNum, C_ICON_PATHNAME, C_TOOLBAR_TOOLTIP, B_TOOLBAR_BLOCO_ACAO)

*
*
***************************************
STAT PROC ADICIONA_SEPARADOR_AO_TOOLBAR (VX_Janela)
***************************************
* Este número vai de 400 em diante, e é necessário ao Windows
N_ToolBarCodigoAcao++
NAP_TOOLBAR_SEPARATOR(N_WindowNum)
*
*******************
STAT FUNC TEM_BOTAO(VX_Janela,VC_TeclasBusca, L_ChecaMudaDados, L_MudaDados)
*******************
LOCAL N_Cont
LOCAL L_TemBotao := .F.
LOCAL N_PosBotao
*
DEFAULT L_ChecaMudaDados TO .F.
DEFAULT L_MudaDados      TO .F.
*
FOR N_Cont := 1 TO LEN(VC_TeclasBusca)

    N_PosBotao :=  ASCAN(V_RegiaoBotoes,;
                        {|V_SUBV|XUPPER(VC_TeclasBusca[N_Cont]) == ;
                                    TROCA(XUPPER(ALLTRIM(V_SUBV[_BOTAO_TEXTO_TRATADO_2])),"&","")})

    IF N_PosBotao # 0
        L_TemBotao := .T.
        IF L_ChecaMudaDados
            IF V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS] # NIL
            L_MudaDados := V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS]
            ENDIF
            IF UPPER(VC_TeclasBusca[1]) == "F2=SALVAR"
            L_MudaDados := .T.
            ENDIF
        ENDIF
    ENDIF
NEXT
*
RETURN L_TemBotao


*******************
FUNCTION DestruaJan ( VX_Janela, L_Permitir_CUA_20 )
*******************
LOCAL N_CT
DEFAULT L_Permitir_CUA_20 TO .F.

* se janela foi ativada ao menos uma vez, fechá-la
*
IF .NOT. L_Permitir_CUA_20 .AND. .NOT. L_CUA_10
   ? MEMVAR->DESTRUA_JAN_NAO_EXISTE_NA_CUA_20
ENDIF
*
IF L_JanTipoMsgAguarde
   ? MEMVAR->ERRO_FECHAMENTO_COM_DESTRUAJAN
ENDIF
*
IF C_TelaCoberta # NIL

    IF SOB_MODO_GRAFICO()

        // FRAN: In GTNAP embedded (embutida) window also have to be destroyed
        // GTNAP manages the destruction differences
        NAP_WINDOW_DESTROY(N_WindowNum)

        // Embedded (embutida) window NEVER in V_PilhaJanelas
        IF .NOT. L_Embutida
            IF N_WindowNum # V_PilhaJanelas[LEN(V_PilhaJanelas)][1]  // destruir sempre a última ativada
                ALARME("M28754","Alguma janela aberta não foi fechada - passo 2...")
            ENDIF
            ASIZE(V_PilhaJanelas,LEN(V_PilhaJanelas)-1)

        ENDIF

    ELSE // NOT SOB_MODO_GRAFICO()

        RESTSCREEN(N_LinIni,N_ColIni,N_LinFin,N_ColFin,C_TelaCoberta)  // restaurar tela
        IF .NOT. L_Embutida
            ASIZE(V_PilhaJanelas,LEN(V_PilhaJanelas)-1)
        ENDIF

   ENDIF // SOB_MODO_GRAFICO()
   *
   SETPOS(N_LinAnt,N_ColAnt)            // posiciona o cursor nas coordenadas
   *
   C_TelaCoberta := NIL
   N_LinAnt := N_ColAnt := NIL    // não tem conteúdo em janelas fechadas
   *
ENDIF // C_TelaCoberta # NIL
*
* destruir as matrizes chamadas diretamente pelo objeto janela,
* de modo a facilitar o "garbage collection" automático do clipper.
*
IF VX_SubObj # NIL
   ASIZE(VX_SubObj,0)
ENDIF
ASIZE(VC_Titulo,0)
*
ASIZE(VX_Janela,0)
*
RETURN NIL

*
*******************
FUNCTION Rolamento_ ( VX_Janela , L_Esq , L_Cima , L_Baixo , L_Dir )
*
LOCAL N_CursorAnt , N_LinhaAnt , N_ColunaAnt, N_Cont
LOCAL L_ScrollVerticalMudou := (L_RolaCima  # L_Cima ) .OR. ;
                               (L_RolaBaixo # L_Baixo)
LOCAL L_ScrollHorizontalMudou := (L_RolaEsquerda # L_Esq ) .OR.;
                                 (L_RolaDireita  # L_Dir )
*
* atualizar atributos do objeto janela
L_RolaCima      := L_Cima
L_RolaBaixo     := L_Baixo
L_RolaEsquerda  := L_Esq
L_RolaDireita   := L_Dir
*

IF SOB_MODO_GRAFICO()
    ALARME("M28756","Rolamento_() can't be called in SOB_MODO_GRAFICO")
ENDIF

IF (L_Cima .OR. L_Baixo) .AND. .NOT. L_TemScrollVertical
    ALERT("Erro no rolamento vertical")
    ? MEMVAR->ERRO_VERTICAL
ENDIF
*
IF (L_Esq .OR. L_Dir) .AND. .NOT. L_TemScrollHorizontal
    ALERT("Erro no rolamento horizontal")
    ? MEMVAR->ERRO_HORIZONTAL
ENDIF
*
* imprimir indicativos de rolamento
*
IF L_ScrollVerticalMudou .OR. ;
   L_ScrollHorizontalMudou
   N_CursorAnt := SET(_SET_CURSOR,SC_NONE)          // salvar modo do cursor
   N_LinhaAnt  := ROW()
   N_ColunaAnt := COL()
ENDIF
*
IF L_TemScrollVertical .AND. L_ScrollVerticalMudou
    DispBegin()
    IF .NOT. L_Cima .AND. .NOT. L_Baixo
        * limpar ï¿½rea da barra de rolagem
        SCROLL(N_Lin1Livre,N_Col2Livre+2,N_Lin2Livre,N_Col2Livre+2)
        N_LinMarcadorVertical := 0
    ELSE
        * montar seta para cima
        SETPOS(N_Lin1Livre,N_Col2Livre+2)
        DISPOUT(CHR(30),C_CorInten)      //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
        *
        IF .NOT. L_Cima      // no topo
        N_LinMarcadorVertical := N_Lin1Livre+1
        ELSEIF .NOT. L_Baixo // embaixo
        N_LinMarcadorVertical := N_Lin2Livre-1
        ELSE                 // no meio
        N_LinMarcadorVertical := ROUND((N_Lin1Livre+N_Lin2Livre)/2,0)
        ENDIF
        *
        * montar barra de rolagem
        FOR N_Cont := N_Lin1Livre+1 TO N_Lin2Livre-1
        SETPOS(N_Cont,N_Col2Livre+2)
        IF N_Cont == N_LinMarcadorVertical
            DISPOUT(CHR(219),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
        ELSE
            DISPOUT(CHR(176),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
        ENDIF
        NEXT
        *
        * montar seta para baixo
        SETPOS(N_Lin2Livre,N_Col2Livre+2)
        DISPOUT( CHR(31),C_CorInten)     //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
        *
    ENDIF
    DispEnd()
ENDIF
*
IF L_TemScrollHorizontal .AND. L_ScrollHorizontalMudou
    DispBegin()
    IF .NOT. L_Esq .AND. .NOT. L_Dir
        * limpar ï¿½rea da barra de rolagem
        SCROLL(N_Lin2Livre+2,N_Col1Livre,N_Lin2Livre+2,N_Col2Livre)
        N_ColMarcadorHorizontal := 0
    ELSE
        * montar seta para esquerda
        SETPOS(N_Lin2Livre+2,N_Col1Livre+1)
        DISPOUT(CHR(17),C_CorInten)      //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
        *
        IF .NOT. L_Esq       // na margem esquerda
        N_ColMarcadorHorizontal := N_Col1Livre+2
        ELSEIF .NOT. L_Dir   // na margem direita
        N_ColMarcadorHorizontal := N_Col2Livre-2-1
        ELSE                 // no meio
        N_ColMarcadorHorizontal := ROUND((N_Col1Livre+N_Col2Livre)/2,0)
        ENDIF
        *
        * montar barra de rolagem
        FOR N_Cont := N_Col1Livre+2 TO N_Col2Livre-2
        SETPOS(N_Lin2Livre+2,N_Cont)
        IF N_Cont == N_ColMarcadorHorizontal .OR. ;
            N_Cont == N_ColMarcadorHorizontal+1
            DISPOUT(CHR(219),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
        ELSE
            DISPOUT(CHR(177),C_CorInten)  //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
        ENDIF
        NEXT
        *
        * montar seta para direita
        SETPOS(N_Lin2Livre+2,N_Col2Livre-1)
        DISPOUT( CHR(16),C_CorInten)
        *                                //!! NAO EXIBE EM MODO TEXTO COM LUCIDA CONSOLE
    ENDIF
    DispEnd()
ENDIF
*
IF L_ScrollVerticalMudou .OR. ;
   L_ScrollHorizontalMudou
   SET(_SET_CURSOR,N_CursorAnt)          // restaurar modo do cursor
   SETPOS(N_LinhaAnt,N_ColunaAnt)
ENDIF
*
RETURN NIL
*

*********************
FUNC SOB_MODO_GRAFICO
*********************
LOCAL L_ModoGrafico := .F.
IF HB_GTVERSION()=="NAP"
    #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows) || defined(__PLATFORM__LINUX) || defined(__PLATFORM__DARWIN)
        L_ModoGrafico := .T.
    #else
       #erro "Código não adaptado para esta plataforma"
    #endif
   L_ModoGrafico := .T.
ENDIF
*
RETURN L_ModoGrafico



*
**********************
FUNCTION RegiaoJanela_ ( VX_Janela, N_MRow, N_MCol, ;
                         N_RowTop_Util,  N_ColLeft_Util,;
                         N_RowDown_Util, N_ColRight_Util,;
                         N_Keyboard, V_Botao, V_Imagem )   // passados por referência
*
LOCAL N_RegiaoMouse := BUSCANDO_REGIAO
LOCAL N_PosBotao, N_PosImagem
*
N_Keyboard := NIL
V_Botao    := NIL
V_Imagem   := NIL
*
IF (N_MCol==N_Col2Livre+1 .OR. ;
    N_MCol==N_Col2Livre+2 .OR. ;
    N_MCol==N_Col2Livre+3) .AND. ;
    N_MRow >= N_Lin1Livre .AND. ;
    N_MRow <= N_Lin2Livre
   *
   * clicou na barra de rolagem vertical
   *
   IF N_MRow==N_Lin1Livre
      N_RegiaoMouse := ACIMA_UMA_LINHA
      N_Keyboard := K_UP
   ELSEIF N_MRow==N_Lin2Livre
      N_RegiaoMouse := ABAIXO_UMA_LINHA
      N_Keyboard := K_DOWN
   ELSEIF N_MRow==N_Lin1Livre+1
      N_RegiaoMouse := ACIMA_TOTAL
      IF N_TP_Jan == _JAN_TEXTO_10
         N_Keyboard := K_CTRL_PGUP
      ELSE
         N_Keyboard := K_CTRL_HOME
      ENDIF
   ELSEIF N_MRow==N_Lin2Livre-1
      N_RegiaoMouse := ABAIXO_TOTAL
      IF N_TP_Jan == _JAN_TEXTO_10
         N_Keyboard := K_CTRL_PGDN
      ELSE
         N_Keyboard := K_CTRL_END
      ENDIF
   ELSEIF N_MRow < N_LinMarcadorVertical
      N_RegiaoMouse := ACIMA_UMA_PAGINA
      N_Keyboard := K_PGUP
   ELSEIF N_MRow > N_LinMarcadorVertical
      N_RegiaoMouse := ABAIXO_UMA_PAGINA
      N_Keyboard := K_PGDN
   ELSE
      * clicou sobre o marcador.
   ENDIF
   *
ENDIF
*
IF (N_MRow==N_Lin2Livre+1 .OR. ;
    N_MRow==N_Lin2Livre+2 ).AND. ;
    N_MCol >= N_Col1Livre .AND. ;
    N_MCol <= N_Col2Livre
   *
   * clicou na barra de rolagem horizontal
   *
   IF N_MCol==N_Col1Livre .OR. N_MCol==N_Col1Livre+1
      N_RegiaoMouse := ESQUERDA_UMA_COLUNA
      N_Keyboard := K_LEFT
   ELSEIF N_MCol==N_Col2Livre .OR. N_MCol==N_Col2Livre-1
      N_RegiaoMouse := DIREITA_UMA_COLUNA
      N_Keyboard := K_RIGHT
   ELSEIF N_MCol==N_Col1Livre+2
      N_RegiaoMouse := ESQUERDA_TOTAL
      N_Keyboard := K_HOME
   ELSEIF N_MCol==N_Col2Livre-2
      N_RegiaoMouse := DIREITA_TOTAL
      N_Keyboard := K_END
   ELSEIF N_MCol < N_ColMarcadorHorizontal
      N_RegiaoMouse := ESQUERDA_UMA_PAGINA
      IF N_TP_Jan == _JAN_ARQTEXTO_10
         N_Keyboard := K_CTRL_LEFT
      ELSE
         N_Keyboard := K_LEFT
      ENDIF
   ELSEIF N_MCol > N_ColMarcadorHorizontal+1   // marcador ocupa 2 bytes
      N_RegiaoMouse := DIREITA_UMA_PAGINA
      IF N_TP_Jan == _JAN_ARQTEXTO_10
         N_Keyboard := K_CTRL_RIGHT
      ELSE
         N_Keyboard := K_RIGHT
      ENDIF
   ELSE
      * clicou sobre o marcador.
   ENDIF
   *
ENDIF
*
IF .NOT. SOB_MODO_GRAFICO() .AND. ;
   N_MRow >= N_LinMess+1 .AND. N_MRow <= N_LinMess + N_LinBotoes
   * Clicou nas linhas da área de botoes
   *
   N_PosBotao := ASCAN(V_RegiaoBotoes,{|V_Botao| ;
                       N_MRow >= N_LinMess+V_Botao[_BOTAO_LIN_INICIAL] .AND. ;
                       N_MRow <= N_LinMess+V_Botao[_BOTAO_LIN_FINAL]   .AND. ;
                       N_MCol >= N_Col1Livre+V_Botao[_BOTAO_COL_INICIAL] .AND. ;
                       N_MCol <= N_Col1Livre+V_Botao[_BOTAO_COL_FINAL] })
   *
   IF N_PosBotao # 0   // clicou em cima de um botão
      *
      N_RegiaoMouse := BOTAO_IDENTIFICADO
      V_Botao := V_RegiaoBotoes[N_PosBotao]
      *
      N_Keyboard := V_Botao[_BOTAO_INKEY_DESTAQUE]  // podia ser o _BOTAO_INKEY_DESTAQUE_CASE
      IF N_Keyboard == NIL
         N_RegiaoMouse := BOTAO_NAO_IDENTIFICADO
      ENDIF
      *
   ENDIF
   *
ENDIF
*
IF N_RegiaoMouse == BUSCANDO_REGIAO
   IF N_MRow < N_RowTop_Util
      * clicou acima da area útil
   ELSEIF N_MCol < N_ColLeft_Util
      * clicou à esquerda da area util
   ELSEIF N_MRow > N_RowDown_Util
      * clicou abaixo da area util
   ELSEIF N_MCol > N_ColRight_Util
      * clicou à direita da area útil
   ELSE
      N_RegiaoMouse := AREA_UTIL
   ENDIF
ENDIF
*
IF N_RegiaoMouse == BUSCANDO_REGIAO
   IF N_MRow < N_LinIni
      * clicou acima da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSEIF N_MCol < N_ColIni
      * clicou à esquerda da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSEIF N_MRow > N_LinFin
      * clicou abaixo da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSEIF N_MCol > N_ColFin
      * clicou à direita da janela
      N_RegiaoMouse := FORA_DA_JANELA
   ELSE
      N_RegiaoMouse := REGIAO_SEM_EVENTO
   ENDIF
ENDIF
*
IF SOB_MODO_GRAFICO()
   * Imagens adicionadas pelo programador de forma manual (comando IMAGEM)
   * tem precedência sobre qualquer outro tipo de região da tela.
   * Por exemplo, se uma imagem estiver sobre um browse, o click do mouse não
   * será repassado à tbrowse().
   N_PosImagem := ASCAN(V_LstImagens,{|V_Imagem| ;
                       N_MRow >= N_LinIni+V_Imagem[_IMAGEM_LIN_INICIAL] .AND. ;
                       N_MRow <= N_LinIni+V_Imagem[_IMAGEM_LIN_FINAL]   .AND. ;
                       N_MCol >= N_ColIni+V_Imagem[_IMAGEM_COL_INICIAL] .AND. ;
                       N_MCol <= N_ColIni+V_Imagem[_IMAGEM_COL_FINAL] })
   *
   IF N_PosImagem # 0   // clicou em cima de uma imagem
      * se existir algum evento vinculado ï¿½ imagem
      IF V_LstImagens[N_PosImagem,_IMAGEM_BLOCO_ACAO] # NIL .OR. ;
         V_LstImagens[N_PosImagem,_IMAGEM_KEYBOARD] # NIL  // podia ser o _IMAGEM_KEYBOARD_CASE
         V_Imagem := V_LstImagens[N_PosImagem]
         N_Keyboard := V_Imagem[_IMAGEM_KEYBOARD]  // podia ser o _IMAGEM_KEYBOARD_CASE
         N_RegiaoMouse := SOBRE_IMAGEM
      ENDIF
   ENDIF
ENDIF
*
RETURN N_RegiaoMouse
*


***************
FUNC _Pega_Cor_ ( C_CorCompleta, N_Indice, L_Frente )
*
LOCAL C_CorPar, N_Pos, C_Cor
*
C_CorCompleta := ","+C_CorCompleta+","     // facilita o algoritmo
*
C_CorCompleta := STRTRAN(C_CorCompleta,",","#",,N_Indice+1)
N_Pos    := AT(",",C_CorCompleta)
C_CorPar := LEFT(C_CorCompleta,N_Pos-1)
N_Pos    := RAT("#",C_CorPar)
C_CorPar := SUBSTR(C_CorPar,N_Pos+1)
*
IF .NOT. "/" $ C_CorPar
   C_CorPar := C_CorPar + "/"         // facilita o algoritmo
ENDIF
*
IF L_Frente ==  NIL
   C_Cor := C_CorPar
ELSEIF L_Frente
   C_Cor := LEFT(C_CorPar,AT("/",C_CorPar)-1)
ELSE
   C_Cor := SUBSTR(C_CorPar,AT("/",C_CorPar)+1)
ENDIF
*
RETURN C_Cor
*
***********************
STATIC FUNCTION TecFunc ( N_Largura, V_2RegiaoBotao )
*
LOCAL N_Quant, N_Cont, N_Brancos := 2
LOCAL C_LinhaTxt, C_LinhaWin, N_2LinBotoes
LOCAL C_TxtBotao, L_BotaoNoPadrao
*
* calcular acumulado do tamanho das mensagens
*
FOR N_Cont := 1 TO LEN(V_2RegiaoBotao)
    C_TxtBotao := V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_COMANDO]
    *
    L_BotaoNoPadrao := .T.
    IF LEFT(UPPER(C_TxtBotao),2) $ "I=/A=/E=/C=/X=/L=/P=/"
       IF .NOT. C_TxtBotao == "I=incluir" .AND. ;
          .NOT. C_TxtBotao == "A=alterar" .AND. ;
          .NOT. C_TxtBotao == "E=excluir" .AND. ;
          .NOT. C_TxtBotao == "C=consultar" .AND. ;
          .NOT. C_TxtBotao == "X=anexar" .AND. ;
          .NOT. C_TxtBotao == "L=listar" .AND. ;
          .NOT. C_TxtBotao == "P=procurar"
          L_BotaoNoPadrao := .F.
       ENDIF
    ENDIF
    *
    IF LEFT(XUPPER(C_TxtBotao),7) $ "ESPAÇO=/"
        IF LEN(C_TxtBotao) == 13
            IF .NOT. C_TxtBotao == "Espaço=marcar"
                L_BotaoNoPadrao := .F.
          ENDIF
       ELSEIF LEN(C_TxtBotao) == 23
        IF .NOT. C_TxtBotao == "Espaço=marcar/desmarcar"
            L_BotaoNoPadrao := .F.
          ENDIF
       ELSE
          L_BotaoNoPadrao := .F.
       ENDIF
    ENDIF
    *
    IF LEFT(XUPPER(C_TxtBotao),16) $ "BARRA DE ESPAÇO=/"
        IF LEN(C_TxtBotao) == 22
           IF .NOT. C_TxtBotao == "Barra de espaço=marcar"
              L_BotaoNoPadrao := .F.
           ENDIF
        ELSEIF LEN(C_TxtBotao) == 32
           IF .NOT. C_TxtBotao == "Barra de espaço=marcar/desmarcar"
              L_BotaoNoPadrao := .F.
           ENDIF
        ELSE
           L_BotaoNoPadrao := .F.
        ENDIF
     ENDIF
     *
    IF .NOT. L_BotaoNoPadrao
       *
       // Esta função GS_SERIE(), foi criada temporariamente com a finalidade de testar
       // a serie no janela.prg (testa TEXTO do botão)
       IF GS_SERIE() # NIL
        IF GS_SERIE() $ SERIE_TESTE()+;
                        SERIE_DESENVOLVIMENTO()+;
                        SERIE_SAN()+;
                        SERIE_UNIDADE_CONVERSAO()+;
                        SERIE_PRODUCAO()
           ///? MEMVAR->ERRO_BOTAO_FORA_DO_PADRAO
        ELSE
           LOGAFONT_GENERICO(4,"JAN",NIL,NIL,"Erro 7: Botão fora do padrão: "+C_TxtBotao)
        ENDIF
     ELSE
        LOGAFONT_GENERICO(4,"JAN",NIL,NIL,"Erro 7: Botão fora do padrão: "+C_TxtBotao)
     ENDIF
     *
    ENDIF
    *
    V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1] := " "+C_TxtBotao+" "
NEXT
*
N_2LinBotoes := 0
N_Quant := LEN(V_2RegiaoBotao)
N_Cont := 1
DO WHILE N_Cont <= N_Quant
   N_2LinBotoes++
   *
   * Acumulado de texto dos botões para a versão TEXTO. A quebra de linha
   * é baseada nesta variável, para que os botões fiquem sempre na mesma linha,
   * seja qual for a versão usada.
   C_LinhaTxt := ""
   * Acumulado de texto dos botões para a versão GRÁFICA. Serve para evitar
   * espaçamento diferente de botões na versão GRÁFICA.
   C_LinhaWin := ""
   CalculaRegiaoBotao(N_2LinBotoes,N_Cont,@V_2RegiaoBotao,@C_LinhaTxt,@C_LinhaWin)
   *
   N_Cont++
   DO WHILE IIF(N_Cont <= N_Quant,  ;
      N_Brancos+LEN(C_LinhaTxt+V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1])+2 <= N_Largura , .F.)
      *
      C_LinhaTxt += SPACE(N_Brancos)
      C_LinhaWin += SPACE(N_Brancos)
      CalculaRegiaoBotao(N_2LinBotoes,N_Cont,@V_2RegiaoBotao,@C_LinhaTxt,@C_LinhaWin)
      *
      N_Cont++
   ENDDO
ENDDO
*
RETURN N_2LinBotoes
*
****************************
STAT PROC CalculaRegiaoBotao(N_Nulin,N_Cont,;
                            V_2RegiaoBotao,C_LinhaTxt,C_LinhaWin)   // por referência
****************************
LOCAL N_Pos
LOCAL C_Destaque, C_TeclaAtalho, N_Pos_Destaque := 1
LOCAL C_TxtTratado_2 := V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1]
LOCAL N_Keyboard
*
IF (N_Pos := AT("=",C_TxtTratado_2)) == 0
   IF SOB_MODO_GRAFICO()
        ALARME("M28760","Barra de ações inválida")
        ? MEMVAR->ERRO_FALTA_SINAL_DE_IGUAL
   ELSE
        ALERT("Barra de ações inválida")
        ? MEMVAR->ERRO_FALTA_SINAL_DE_IGUAL
   ENDIF
ENDIF
*
C_Destaque := LEFT(C_TxtTratado_2,N_Pos-1)
C_Destaque := SUBSTR(C_Destaque,2)  // tirar o espaço inicial
*
C_TeclaAtalho := XUPPER(C_Destaque)
IF LEN(C_TeclaAtalho)==1 .AND. (ISALPHA(C_TeclaAtalho) .OR. ISDIGIT(C_TeclaAtalho))
   * Parte esquerda do comando é uma só letra ou só um número
   *
   N_Keyboard := ASC(C_TeclaAtalho)
   *
   * O SUBSTR(,4) é para tirar da busca o " I="   (espaço + letra + igual)
   N_Pos := AT(C_TeclaAtalho,XUPPER(SUBSTR(C_TxtTratado_2,4)))
   IF N_Pos # 0
    IF SOB_MODO_GRAFICO() // reposicionar o sublinhado dentro do botão
        * Remover a letra e o sinal de igual.
        * Colocar a primeira letra em maiúsculo.
        C_TxtTratado_2 := " "+XUPPER(SUBSTR(C_TxtTratado_2,4,1))+SUBSTR(C_TxtTratado_2,5)
        N_Pos := N_Pos+1   // acrescentar o " " também na posição
        *
         * Pegar novamente a tecla de destaque, pois pode estar em minúsculo...
         C_Destaque := SUBSTR(C_TxtTratado_2,N_Pos,1)
         *
         N_Pos_Destaque := N_Pos-1
      ENDIF
   ELSE
      #IFDEF _TESTE   //!! DEPOIS PASSAR A DAR ERRO TAMBï¿½M NA VERSï¿½O DE PRODUCAO
          ? MEMVAR->HOTKEY_NAO_CONSEGUIU_SER_DEDUZIDA_01
      #ENDIF
   ENDIF
   *
ELSEIF C_TeclaAtalho=="ESC"
    N_Keyboard := K_ESC
 ELSEIF C_TeclaAtalho=="ENTER"
    N_Keyboard := K_ENTER
 ELSEIF C_TeclaAtalho=="ESPAÇO"   .OR. ;
        C_TeclaAtalho=="BARRA DE ESPAÇO"
   N_Keyboard := ASC(" ")
ELSEIF C_TeclaAtalho=="F1"
   N_Keyboard := K_F1
ELSEIF C_TeclaAtalho=="F2"
   N_Keyboard := K_F2
ELSEIF C_TeclaAtalho=="F3"
   N_Keyboard := K_F3
ELSEIF C_TeclaAtalho=="F4"
   N_Keyboard := K_F4
ELSEIF C_TeclaAtalho=="F5"
   N_Keyboard := K_F5
ELSEIF C_TeclaAtalho=="F6"
   N_Keyboard := K_F6
ELSEIF C_TeclaAtalho=="F7"
   N_Keyboard := K_F7
ELSEIF C_TeclaAtalho=="F8"
   N_Keyboard := K_F8
ELSEIF C_TeclaAtalho=="F9"
   N_Keyboard := K_F9
ELSEIF C_TeclaAtalho=="F10"
   N_Keyboard := K_F10
ELSEIF C_TeclaAtalho=="F11"
   N_Keyboard := K_F11
ELSEIF C_TeclaAtalho=="F12"
   N_Keyboard := K_F12
ELSE
   #IFDEF _TESTE   //!! DEPOIS PASSAR A DAR ERRO TAMBï¿½M NA VERSï¿½O DE PRODUCAO
       ? MEMVAR->HOTKEY_NAO_CONSEGUIU_SER_DEDUZIDA_02
   #ENDIF
ENDIF
*
C_LinhaTxt += V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_1]
C_LinhaWin += C_TxtTratado_2
*
V_2RegiaoBotao[N_Cont,_BOTAO_LIN_INICIAL    ] := N_Nulin
IF SOB_MODO_GRAFICO()
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_INICIAL    ] := LEN(C_LinhaWin)-LEN(C_TxtTratado_2)
ELSE
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_INICIAL    ] := LEN(C_LinhaTxt)-LEN(C_TxtTratado_2)
ENDIF
V_2RegiaoBotao[N_Cont,_BOTAO_LIN_FINAL      ] := N_Nulin
IF SOB_MODO_GRAFICO()
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_FINAL      ] := LEN(C_LinhaWin)-1
ELSE
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_FINAL      ] := LEN(C_LinhaTxt)-1
ENDIF
V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_TRATADO_2] := C_TxtTratado_2
IF SOB_MODO_GRAFICO()
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_DESTAQUE   ] := LEN(C_LinhaWin)-LEN(C_TxtTratado_2)+N_Pos_Destaque
ELSE
   V_2RegiaoBotao[N_Cont,_BOTAO_COL_DESTAQUE   ] := LEN(C_LinhaTxt)-LEN(C_TxtTratado_2)+N_Pos_Destaque
ENDIF
V_2RegiaoBotao[N_Cont,_BOTAO_TEXTO_DESTAQUE ] := C_Destaque
V_2RegiaoBotao[N_Cont,_BOTAO_INKEY_DESTAQUE ] := N_Keyboard
V_2RegiaoBotao[N_Cont,_BOTAO_INKEY_DESTAQUE_CASE ] := InKeyCaseAlternativo(N_Keyboard)
*
******************
FUNCTION StrToVet_ ( C_Titulo )
*
LOCAL VC_Resultado , N_Tamanho , N_InicioLin
*
* tranforma um string com separador de linhas ";" em um vetor de caracteres
*
N_InicioLin  := 1
VC_Resultado := {}
DO WHILE LEN(SUBSTR(C_Titulo,N_InicioLin)) # 0
   IF (N_Tamanho := AT(";",SUBSTR(C_Titulo,N_InicioLin)) ) == 0
       N_Tamanho := LEN(SUBSTR(C_Titulo,N_InicioLin))+1
   ENDIF
   AADD(VC_Resultado,SUBSTR(C_Titulo,N_InicioLin,N_Tamanho-1))
   N_InicioLin := N_InicioLin + N_Tamanho
ENDDO
*
RETURN VC_Resultado
*
***************************
FUNC CABEC_TESTE_AUTOMATICO  (L_EXIBE_AJUDA_NEW)
***************************
//!! CODIGO QUEBRA GALHO
LOCAL C_SERIE
//!!

STATIC L_EXIBE_AJUDA := .F.
IF L_EXIBE_AJUDA_NEW # NIL
    L_EXIBE_AJUDA := L_EXIBE_AJUDA_NEW
ENDIF

//!! CODIGO QUEBRA GALHO
C_SERIE := "XXXXX"
IF SELECT("XXCONG") # 0
    C_SERIE := XXCONG->SERIE
ENDIF
IF SERIE_EQUIPE_DE_TESTE() .OR. ;   // Equipe de teste
    "/"+TRIM(GS_CDUSUA())+"/" $ "/RICARDO.REGIS/"
    L_EXIBE_AJUDA := .T.
ENDIF
//!! CODIGO QUEBRA GALHO
*
RETURN L_EXIBE_AJUDA
*
*
******************
PROC MudeSubtitulo ( VX_Janela, C_SubCabec_Novo )
******************
LOCAL N_Cont, N_Largura, N_LinImp
LOCAL N_TAM_CAB_ANT := LEN(VC_Titulo)
LOCAL C_Cor_Aux
LOCAL N_POS1, N_POS2
LOCAL L_EH_PROGRESSBAR := GetProgressBar( VX_Janela ) != 0
*
IF C_TelaCoberta == NIL
   ? MEMVAR->JANELA_AINDA_NAO_ATIVADA_NENHUMA_VEZ
ENDIF
*
#IFDEF _TESTE
IF L_EH_PROGRESSBAR                          /// Janela do Tipo ProgressBar, só pode ser
   ? MEMVAR->JANELA_E_DO_TIPO_PROGRESSBAR    /// Manipulada pela função, ANDAMENTO_PROGRESSBAR!
ENDIF
#ENDIF
*
* não imprime nada, só monta a matriz de cabeçalho devidamente formatado
*
C_SubCabec_Novo := STRTRAN(C_SubCabec_Novo,"%t",C_Cabec)
C_SubCabec_Novo := STRTRAN(C_SubCabec_Novo,"%T",C_Cabec)
VC_Titulo := StrToVet_(C_SubCabec_Novo)
*
IF N_TAM_CAB_ANT # LEN(VC_Titulo)
   ? MEMVAR->CABECALHO_COM_NUMERO_DE_LINHAS_DIFERENTE
ENDIF

*
* montar cabeçalho
*
N_Largura := N_Col2Livre-N_Col1Livre+1
N_LinImp  := N_LinIni+N_MargemSuperior
*
C_Cor_Aux := SETCOLOR(C_CorJan)  // mudar para a cor da janela
FOR N_Cont := 1 TO LEN(VC_Titulo)
    IF SOB_MODO_GRAFICO()
        NAP_LABEL_UPDATE(N_WindowNum, V_LstTituloLabels[N_Cont], N_LinImp-1+N_Cont, N_Col1Livre+N_DeslocaCabecalho, PADC(VC_Titulo[N_Cont],N_Largura-N_DeslocaCabecalho))
    ELSE
        SETPOS(N_LinImp-1+N_Cont,N_Col1Livre+N_DeslocaCabecalho)
        DISPOUT(PADC(VC_Titulo[N_Cont],N_Largura-N_DeslocaCabecalho))
    ENDIF
NEXT
SETCOLOR(C_Cor_Aux)
*

******************
FUNC GetCdTelaTopo
******************
LOCAL C_CdTelaTopo := SPACE(06)   // Tnnnnn
LOCAL VX_Janela
*
* Se ALARME() tiver uma tela anterior a ela
IF LEN(V_PilhaJanelas) > 0
    VX_Janela := V_PilhaJanelas[LEN(V_PilhaJanelas),2]  // Janela do topo
    C_CdTelaTopo := C_CdTela
ENDIF
*
RETURN C_CdTelaTopo
*
**************************
FUNC GetCdGET_ou_Menu_Topo
**************************
LOCAL C_CdGET_ou_Menu_Atual := SPACE(06)  // formato Tnnnnn ou Mnnnnn ou Cnnnnn
LOCAL VX_Janela
*
* Se ALARME() tiver uma tela anterior a ela
IF LEN(V_PilhaJanelas) > 0
   VX_Janela := V_PilhaJanelas[LEN(V_PilhaJanelas),2]  // Janela do topo
   IF N_TP_Jan == _JAN_ENTRADA_10
      C_CdGET_ou_Menu_Atual := GetCdGET_Atual_Entrada(VX_Janela)
   ELSEIF N_TP_Jan == _JAN_MENU_VERT
      C_CdGET_ou_Menu_Atual := GetCdOpcao_Atual_Menuvert(VX_Janela)
   ENDIF
ENDIF
RETURN C_CdGET_ou_Menu_Atual
*
***********************

*
***********************
STATIC FUNCTION TrataEventos ( VX_Janela )

LOCAL L_Mais, N_Tecla, N_Pos
LOCAL N_mRow, N_mCol, N_RegiaoMouse, N_Keyboard
LOCAL X_Retorno_Eval, L_FechouComAutoClose := .F.
LOCAL V_Botao, V_Imagem, N_Pos_Acao

IF SOB_MODO_GRAFICO()

    X_Retorno_Eval := NAP_WINDOW_MODAL(N_WindowNum, N_PaiWindowNum, 0)

    IF X_Retorno_Eval == NAP_MODAL_ESC .OR. X_Retorno_Eval == NAP_MODAL_ENTER .OR. X_Retorno_Eval == NAP_MODAL_X_BUTTON .OR. X_Retorno_Eval == NAP_MODAL_TOOLBAR
        L_FechouComAutoClose = .F.
    ELSEIF X_Retorno_Eval > NAP_MODAL_BUTTON_AUTOCLOSE .AND. X_Retorno_Eval <= NAP_MODAL_BUTTON_AUTOCLOSE + NAP_MAX_BUTTONS
        L_FechouComAutoClose = .T.
    ELSEIF X_Retorno_Eval > NAP_MODAL_HOTKEY_AUTOCLOSE .AND. X_Retorno_Eval <= NAP_MODAL_HOTKEY_AUTOCLOSE + NAP_MAX_VKEY
        L_FechouComAutoClose = .T.
    ELSEIF X_Retorno_Eval > NAP_MODAL_IMAGE_AUTOCLOSE .AND. X_Retorno_Eval <= NAP_MODAL_IMAGE_AUTOCLOSE + NAP_MAX_IMAGES
        L_FechouComAutoClose = .T.
    ELSE
        Alert( "Invalid X_Retorno_Eval (" + hb_ntos(X_Retorno_Eval) + ") in TrataEventos")
    ENDIF

ELSE // .NOT. SOB_MODO_GRAFICO()

//
// FRAN: Manual event management ONLY in TEXT terminal versions
//
L_Mais := .T.                                    // simula um DO UNTIL
*
DO WHILE L_Mais
   *
   * InKey_(.T.) é igual ao INKEY(0), mas ativa SET KEY"s
   N_Tecla := Inkey_(.T.,4)
   *
   DO CASE
      CASE N_Tecla == K_LBUTTONDOWN .OR. ;
           N_Tecla == K_LDBLCLK     .OR. ;
           N_Tecla == K_RBUTTONDOWN .OR. ;
           N_Tecla == K_RDBLCLK
           *
           N_mRow := mRow()
           N_mCol := mCol()
           N_RegiaoMouse := RegiaoJanela_(VX_Janela,N_mRow,N_mCol,;
                                          N_Lin1Livre,N_Col1Livre,;
                                          N_Lin2Livre,N_Col2Livre,;
                                          @N_Keyboard,@V_Botao,@V_Imagem)
           *
           // #INCLUDE "mousecua.ch"
           IF (N_RegiaoMouse == BOTAO_IDENTIFICADO .OR. ;  // N_Keyboard preenchido
               N_RegiaoMouse == BOTAO_NAO_IDENTIFICADO)    // N_Keyboard não preenchido

              X_Retorno_Eval := EVAL(V_Botao[_BOTAO_BLOCO_ACAO])
              *
              * Logar uso de botões, para ter estatística de uso
              IF V_Botao[_BOTAO_CDBOTAO] # NIL  // Se for CUA 2.0
                 LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Botao[_BOTAO_CDBOTAO],;
                                              C_CdTela,"Botão "+V_Botao[_BOTAO_TEXTO_COMANDO])   // Log de uso de botão no sistema
              ENDIF

              IF V_Botao[_BOTAO_AUTOCLOSE]
                 DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                 IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                    ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                 ENDIF
                 IF X_Retorno_Eval
                    L_Mais := .F.
                    L_FechouComAutoClose := .T.
                 ENDIF
              ENDIF
           ELSEIF N_RegiaoMouse == SOBRE_IMAGEM
              *

              X_Retorno_Eval := EVAL(V_Imagem[_IMAGEM_BLOCO_ACAO])

              * Logar uso de imagens, para ter estatística de uso
              IF V_Imagem[_IMAGEM_CDBOTAO] # NIL  // Se for CUA 2.0
                 LOGAINFO_ID_TELA_RELAT_BOTAO("botão/ação",V_Imagem[_IMAGEM_CDBOTAO],;
                                              C_CdTela,"Imagem "+V_Imagem[_IMAGEM_ARQUIVO])   // Log de uso de imagem no sistema
              ENDIF

              IF V_Imagem[_IMAGEM_AUTOCLOSE]
                 DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                 IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                    ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                 ENDIF
                 IF X_Retorno_Eval
                    L_Mais := .F.
                    L_FechouComAutoClose := .T.
                 ENDIF
              ENDIF
           ELSEIF N_Keyboard # NIL
              HB_KeyPut(N_Keyboard)
           ENDIF
           *
      OTHER
           IF N_Tecla == K_ESC
              L_Mais := .F.
           ELSE
              N_Pos_Acao := ASCAN(V_LstAcoes,{|V_Acao| ;
                                  V_Acao[_ACAO_KEYBOARD]==N_Tecla .OR. ;
                                  V_Acao[_ACAO_KEYBOARD_CASE]==N_Tecla})
              IF N_Pos_Acao # 0

                 X_Retorno_Eval := EVAL(V_LstAcoes[N_Pos_Acao,_ACAO_BLOCO_ACAO])
                 *
                 IF V_LstAcoes[N_Pos_Acao,_ACAO_AUTOCLOSE]
                    DEFAULT X_Retorno_Eval TO .F. // não fechar janela de menu
                    IF .NOT. VALTYPE(X_Retorno_Eval)=="L" // tem de ser lógico
                       ? MEMVAR->COM_AUTOCLOSE_RETORNO_TEM_DE_SER_LOGICO_OU_NIL
                    ENDIF
                    IF X_Retorno_Eval
                       L_Mais := .F.
                       L_FechouComAutoClose := .T.
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
   ENDCASE
   *
ENDDO
*

ENDIF  //  SOB_MODO_GRAFICO()

RETURN L_FechouComAutoClose

***************************
STATIC PROC DesenhaDrawLabe(VX_Janela)
***************************
    LOCAL C_Text1 := space(26-(LEN(INFO_VERSAO[8])+3)) + "Versão " + INFO_VERSAO[5] + "." + INFO_VERSAO[6] + CHR(VAL(INFO_VERSAO[7])) + "(b"+INFO_VERSAO[8] + ") - S" + TRIM(SERIE_ASPEC_INI())
    LOCAL C_Text2 := "www.aspec.com.br   Aspec " + chr(184) + "1993-" + STR(YEAR(DATE()),4,0) + ". Todos os direitos reservados"
    LOCAL N_Id1 := NAP_LABEL(N_WindowNum, 32, 57, C_Text1, .F.)
    LOCAL N_Id2 := NAP_LABEL(N_WindowNum, 33, 39, C_Text2, .F.)
    NAP_LABEL_FGCOLOR(N_WindowNum, N_Id1, NAP_COLOR_RED())
    NAP_LABEL_BGCOLOR(N_WindowNum, N_Id1, NAP_COLOR_CYAN())
    NAP_LABEL_BGCOLOR(N_WindowNum, N_Id2, NAP_COLOR_CYAN())

*******************************
STATIC FUNC ADICIONA_BOTAO_PUSH(VX_Janela,N_PosBotao)
*******************************
LOCAL V_Botao := V_RegiaoBotoes[N_PosBotao]
LOCAL N_BotId
LOCAL N_Keyboard := V_Botao[_BOTAO_INKEY_DESTAQUE]
LOCAL C_TextoDestaque := V_Botao[_BOTAO_TEXTO_DESTAQUE]
LOCAL C_TextoBotaoAux := V_Botao[_BOTAO_TEXTO_TRATADO_2]
LOCAL N_Pos
LOCAL C_TextoBotaoAux_CodigoPagina
LOCAL B_Bloco := V_Botao[_BOTAO_BLOCO_ACAO]

IF N_Keyboard == NIL

ELSE
    IF LEN(C_TextoDestaque)==1 .AND. ; // Se aceleradora tiver 1 byte de tamanho
        (ISALPHA(C_TextoDestaque) .OR. ISDIGIT(C_TextoDestaque))  // Se for uma letra (sem acentos) ou um número
       *
        N_Pos := AT(XUPPER(C_TextoDestaque),XUPPER(C_TextoBotaoAux))

        IF N_Pos # 0   // Colocar o "&" que torna a caractere um acelerador para o Windows
        C_TextoBotaoAux := LEFT(C_TextoBotaoAux,N_Pos-1)+"&"+;
                            SUBSTR(C_TextoBotaoAux,N_Pos)
        ENDIF
    ENDIF
ENDIF  // N_Keyboard == NIL
*
IF Version()=="Harbour 3.2.0dev (r1703241902)"
    C_TextoBotaoAux_CodigoPagina := HB_OEMtoANSI(C_TextoBotaoAux)
ELSEIF Version()=="Harbour 3.2.0dev (r2011030937)" .OR. Version()=="Harbour 3.2.0dev (r1704061005)" // ADAPTACAO_LINUX
    C_TextoBotaoAux_CodigoPagina := C_TextoBotaoAux
ELSE
    C_TextoBotaoAux_CodigoPagina := C_TextoBotaoAux
ENDIF

N_BotId := NAP_BUTTON(;
            N_WindowNum,;
            N_LinMess + V_Botao[_BOTAO_LIN_INICIAL],;
            N_Col1Livre + V_Botao[_BOTAO_COL_INICIAL],;
            N_LinMess + V_Botao[_BOTAO_LIN_FINAL],;
            N_Col1Livre + V_Botao[_BOTAO_COL_FINAL],;
            {|| C_TextoBotaoAux_CodigoPagina},;
            B_Bloco,;
            V_Botao[_BOTAO_AUTOCLOSE],;
            .F.)

 V_Botao[_BOTAO_HANDLE_PUSHBUTTON] := N_BotId

 // FRAN: These kind of windows don't have V_LstAcoes
IF N_TP_Jan == _JAN_MENU_VERT .OR. N_TP_Jan == _JAN_PERGUNTAR .OR. N_TP_Jan == NIL
    IF N_KeyBoard # NIL
        NAP_WINDOW_HOTKEY(N_WindowNum, N_KeyBoard, V_Botao[_BOTAO_BLOCO_ACAO], V_Botao[_BOTAO_AUTOCLOSE])
    ENDIF
ENDIF

INABILITA_BOTAO_PUSH(VX_Janela, N_PosBotao)
*
RETURN NIL
*

*************************
STAT PROC EXIBEVIDEO(C_TIPO,C_ARQUIVO,C_TITULO)
*************************
LOCAL N_HANDLE
DEFAULT C_TIPO TO "V"

IF SOB_MODO_GRAFICO()
    EXISTENCIA_EXE("EXIBIRTF",GS_EXTENSAO_EXE(),,.F.)
    N_HANDLE := HB_processOpen(DIREXE() + "exibirtf." + GS_EXTENSAO_EXE() + " " + C_TIPO + " " + C_ARQUIVO + " " + C_TITULO)
    IF N_HANDLE < 0
        ALARME("M28320","Não foi possível exibir a Video Aula!")
    ENDIF
ENDIF
INKEYX(2)

****************************
STATIC FUNC INABILITA_IMAGEM(L_MUDADADOS)
****************************
LOCAL L_RET := .F.
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
   IF SELECT("XXPREG") # 0
      IF L_MUDADADOS .AND. (.NOT. EHPRINCIPAL(.F.))
         L_RET := .T.
      ENDIF
   ENDIF
ENDIF
*
RETURN L_RET

********************************
STATIC PROC INABILITA_BOTAO_PUSH(VX_Janela, N_PosBotao)
********************************
LOCAL L_MUDADADOS
LOCAL N_TECLA := V_RegiaoBotoes[N_PosBotao,_BOTAO_INKEY_DESTAQUE]
LOCAL C_TEXTO := UPPER(ALLTRIM(V_RegiaoBotoes[N_PosBotao,_BOTAO_TEXTO_TRATADO_2]))
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
    IF V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS] # NIL
        L_MUDADADOS := V_RegiaoBotoes[N_PosBotao,_BOTAO_MUDADADOS]
    ELSE
        L_MUDADADOS := .F.
    ENDIF
    *
    * Qualquer botao para poder ser inabiltado, precisa usar o parâmetro L_MudaDados.
    * Perguntar ao Marcos se as teclas F2 e F9 continuam sendo inabilitadas automáticamente. //!!
    IF (N_TECLA == K_F2 .AND. C_TEXTO == "F2=SALVAR") .OR. N_TECLA == K_F9 .OR. L_MUDADADOS
        IF SELECT("XXPREG") # 0
            IF .NOT. EHPRINCIPAL(.F.)
                //
                // FRAN: Implement Enable in GTNAP
                //
                //WVW_PBENABLE(N_WindowNum, V_RegiaoBotoes[N_PosBotao,_BOTAO_HANDLE_PUSHBUTTON], .F.)
                V_RegiaoBotoes[N_PosBotao,_BOTAO_BLOCO_ACAO] := {||.F.}
            ENDIF
        ENDIF
    ENDIF
ENDIF
*
***********************************
STATIC FUNC INABILITA_BOTAO_TOOLBAR(L_MudaDados)
***********************************
LOCAL L_RET := .F.
*
DEFAULT L_MudaDados TO .F.
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
    IF L_MudaDados
        IF SELECT("XXPREG") # 0
            IF .NOT. EHPRINCIPAL(.F.)
                L_RET := .T.
            ENDIF
        ENDIF
    ENDIF
ENDIF
*
RETURN L_RET

*****************************
STATIC FUNC INABILITA_ADDACAO(L_MUDADADOS)
*****************************
LOCAL L_RET := .F.
*
IF CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
   IF SELECT("XXPREG") # 0
      IF L_MUDADADOS .AND. (.NOT. EHPRINCIPAL(.F.))
         L_RET := .T.
      ENDIF
   ENDIF
ENDIF
*
RETURN L_RET
*
************************************************
FUNC CHECAR_MUDADADOS_COM_ESTE_SISTEMA_INABILITA()
************************************************
* Esta função tem por objetivo fazer com o teste do parâmetro MUDADADOS,
* seja desconsiderado quando se tratar dos sistemas ORÇAMENTO e PPA
LOCAL L_RET := .T.
*
IF G_SGSIST() # NIL
   IF G_SGSIST() $ "OR/PP" // Orçamento ou PPA
      L_RET := .F.
   ENDIF
ENDIF
*
RETURN L_RET

// ******************************** FIM DO JANELA ********************************
