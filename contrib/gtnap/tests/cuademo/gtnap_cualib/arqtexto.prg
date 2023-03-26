/* encoding: cp850 */

#pragma DebugInfo=On

/*
* Rotina para exibição de arquivos textos
* Autor: Eliseu Castelo Branco\Marcos Augusto   Data: 10/11/94
*/

/* TRANSFORMAR FUNCOES AUXILIARES EM PSEUDO-FUNCOES  */

#INCLUDE "inkey.ch"
#INCLUDE "fileio.ch"
#INCLUDE "setcurs.ch"
#INCLUDE "common.ch"
#INCLUDE "define_cua.ch"
#INCLUDE "janela.ch"


********************
FUNCTION EspArqTexto ( VX_Janela, N_Handle, N_TamLinha, VN_TeclaFim, ;
                       C_PalDestaque, L_NaoRolaVertical, L_NaoRolaHorizontal,;
                       L_SemToolBar  )
*
DEFAULT L_NaoRolaVertical   TO .F.
DEFAULT L_NaoRolaHorizontal TO .F.
DEFAULT L_SemToolBar        TO .F.
*
IF .NOT. L_SemToolBar
   SETA_PARA_TER_TOOLBAR(VX_Janela) // ajusta N_LinIni e N_LinLivre
ENDIF
*
AJUSTA_BOTOES(VX_Janela)  // ajusta Lin2Livre à quantidade de botões de função
*
//IF C_PalDestaque # NIL  //!! previsto no código, mas nunca usado na prática
//   ? MEMVAR->POR_ENQUANTO_EH_SEMPRE_NIL
//ENDIF

IF .NOT. L_NaoRolaVertical
   * prever espaço para scroll bar vertical
   Col2Livre(VX_Janela)--
   Col2Livre(VX_Janela)--
   L_ScrollVertical := .T.
ELSE
   ? MEMVAR->NAO_IMPLEMENTADO    // nao implementado ainda - falar com Marcos
ENDIF
*
IF .NOT. L_NaoRolaHorizontal
   * prever espaço para scroll bar horizontal
   Lin2Livre(VX_Janela)--
   Lin2Livre(VX_Janela)--
   L_ScrollHorizontal := .T.
ELSE
   ? MEMVAR->NAO_IMPLEMENTADO    // nao implementado ainda - falar com Marcos
ENDIF
*
DEFAULT N_TamLinha TO Col2Livre(VX_Janela) - ;
                      Col1Livre(VX_Janela)
DEFAULT VN_TeclaFim TO {}
IF C_PalDestaque # NIL
   C_PalDestaque := XLOWER(C_PalDestaque)
   IF EMPTY(C_PalDestaque)
      C_PalDestaque := NIL
   ENDIF
ENDIF
*
#DEFINE V_Linhas      NIL
#DEFINE L_InicioTela  .F.
#DEFINE N_ColCobertas  0
#DEFINE L_PrimAtivacao .T.    // indica que é primeira ativação da janela
*
N_TP_Jan  := _JAN_ARQTEXTO_10
VX_SubObj := { N_Handle, N_TamLinha, VN_TeclaFim, V_Linhas,;
               L_InicioTela, N_ColCobertas, C_PalDestaque,;
               L_PrimAtivacao, L_NaoRolaVertical, L_NaoRolaHorizontal }
B_Metodo  := {||ArqTexto(VX_Janela)}
*
#UNDEF V_Linhas
#UNDEF L_InicioTela
#UNDEF N_ColCobertas
#UNDEF L_PrimAtivacao
*
RETURN NIL
*
************************
STATIC FUNCTION ArqTexto ( VX_Janela )
*
LOCAL L_ScorAnt, N_CursorAnt, L_FimOK, L_Continua := .T., N_Tecla
LOCAL N_LinhasALer,  L_Remontar , N_Cont, V_Buffer
LOCAL N_AlturaJan  := Lin2Livre(VX_Janela)-Lin1Livre(VX_Janela)+1
LOCAL N_LarguraJan := Col2Livre(VX_Janela)-Col1Livre(VX_Janela)+1
LOCAL L_RolaCima, L_RolaBaixo     // variáveis para evitar o teste
LOCAL X_Retorno
*                                 // quando for movimentação horizontal

LOCAL N_Fsize := 0
LOCAL C_Lin, C_CorAnt
LOCAL N_Keyboard, N_mRow, N_mCol, N_RegiaoMouse, V_TextView
//LOCAL N_PaintRefresh_Old
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       N_PaintRefresh_Old := WVW_SetPaintRefresh(_REPAINT_DEFAULT)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
*
#DEFINE N_Handle       VX_SubObj[01]
#DEFINE N_TamLinha     VX_SubObj[02]
#DEFINE VN_TeclaFim    VX_SubObj[03]
#DEFINE V_Linhas       VX_SubObj[04]  // vetor contendo as linhas do textos
                                      // que estão sendo exibidas na tela
#DEFINE L_InicioTela   VX_SubObj[05]  // posição do ponteiro do arquivo
                                      // em relação às linhas da tela
#DEFINE N_ColCobertas  VX_SubObj[06]
#DEFINE C_PalDestaque  VX_SubObj[07]
#DEFINE L_PrimAtivacao VX_SubObj[08]
#DEFINE L_NaoRolaVertical VX_SubObj[09]
#DEFINE L_NaoRolaHorizontal VX_SubObj[10]

*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF L_PrimAtivacao .AND. SOB_MODO_GRAFICO()
//       AddGuiObject(VX_Janela,DesenhaBoxExterno(VX_Janela),;
//                    CoordenadasBoxExterno(VX_Janela))
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
*
IF L_PrimAtivacao
   V_Linhas := ARRAY(Lin2Livre(VX_Janela) - Lin1Livre(VX_Janela) + 1 )
ENDIF
*
L_ScorAnt   := SET(_SET_SCOREBOARD,.F.)
N_CursorAnt := SET(_SET_CURSOR,SC_NONE)
*
N_LinhasALer := N_AlturaJan
L_Remontar   := .T.


IF SOB_MODO_GRAFICO()

    IF L_PrimAtivacao

        NAP_LOG("STATIC FUNCTION ArqTexto ( VX_Janela )")

        V_TextView := NAP_TEXTVIEW_CREATE()

        LOG_PRINT("TEXTVIEW Coords:" + hb_ntos(Lin1Livre(VX_Janela)) + ", " + hb_ntos(Col1Livre(VX_Janela)) + ", " + hb_ntos(Lin2Livre(VX_Janela)) + ", " + hb_ntos(Col2Livre(VX_Janela)))

        NAP_TEXTVIEW_EDITABLE(V_TextView, .F.)

        IF L_NaoRolaHorizontal == .F.
            LOG_PRINT("TEXTVIEW With HORIZONTAL ScrollBar!!")
        ELSE
            LOG_PRINT("TEXTVIEW  WITHOUT HORIZONTAL ScrollBar!!")
        ENDIF

        IF L_NaoRolaVertical == .F.
            LOG_PRINT("TEXTIVEW With VERTICAL ScrollBar!!")
        ELSE
            LOG_PRINT("TEXTIVEW  WITHOUT VERTICAL ScrollBar!!")
        ENDIF

        NAP_TEXTVIEW_SCROLL(V_TextView, IIF(L_NaoRolaHorizontal==.F.,.T.,.F.), IIF(L_NaoRolaVertical==.F.,.T.,.F.))

        FOR N_Cont := 1 TO LEN(VN_TeclaFim) STEP +1
            NAP_CUALIB_HOTKEY(VN_TeclaFim[N_Cont], {||.T.}, .T.)
        NEXT

        NAP_CUALIB_TEXTVIEW(V_TextView, Lin1Livre(VX_Janela), Col1Livre(VX_Janela), Lin2Livre(VX_Janela), Col2Livre(VX_Janela))

        FileTop2( N_Handle )
        N_Fsize := FileSize2( N_Handle )

        V_Buffer := FReadStr( N_Handle, N_Fsize )

        NAP_CUALIB_TEXTVIEW_WRITE(V_TextView, V_Buffer)
        NAP_CUALIB_TEXTVIEW_CARET(V_TextView, 0)
        L_PrimAtivacao := .F.
    ENDIF



    X_Retorno := NAP_CUALIB_LAUNCH_MODAL({||.T.},{||.T.})
    NAP_LOG("RETORNO ARQUIVO TEXT: " + hb_ntos(X_Retorno))
    // X_Retorno == 1 --> window has been closed by [ESC]
    // X_Retorno == 2 --> window has been closed by [INTRO]
    // X_Retorno == 3 --> window has been closed by [X]
    // X_Retorno >= 1000 --> window has been closed by PushButton
    IF X_Retorno >= 1000
        L_FimOK := .T.
    ELSE
        L_FimOK := .F.
    ENDIF

ELSE

*
DO WHILE L_Continua
   IF N_LinhasAler # 0
      LerInfo(VX_Janela,N_LinhasALer,L_Remontar)
      N_LinhasALer := 0
      L_RolaCima := L_RolaBaixo := NIL     // desatualizados
   ENDIF
   *
   C_CorAnt := SETCOLOR()
   IF SOB_MODO_GRAFICO()
      SETCOLOR("N/W*")   // letra preta com fundo branco
   ENDIF
   *
   DispBegin()
   *
   * se não houver teclas pendentes de processamento, atualizar
   * a tela e o indicativo de rolamento
   N_Cont := 1
   DO WHILE N_Cont <= LEN(V_Linhas) .AND. NEXTKEY()==0
       SETPOS(Lin1Livre(VX_Janela)+N_Cont-1,Col1Livre(VX_Janela))
       IF V_Linhas[N_Cont] == NIL
          DISPOUT(SPACE(N_LarguraJan))    // linha vazia
       ELSE
          C_Lin := PADR( ;
                   SUBSTR(V_Linhas[N_Cont],1+N_ColCobertas,N_LarguraJan),;
                   N_LarguraJan)
          DISPOUT(C_Lin)
          IF C_PalDestaque # NIL
             Destacar(Lin1Livre(VX_Janela)+N_Cont-1,Col1Livre(VX_Janela),;
                      C_Lin,C_PalDestaque,VX_Janela)
          ENDIF
       ENDIF
       N_Cont++
   ENDDO
   *
   DispEnd()
   *
   SETCOLOR(C_CorAnt)
   *
   IF NEXTKEY()==0
      IF L_RolaCima == NIL       // desatualizado
         L_RolaCima  := TemMais(VX_Janela,-1)
         L_RolaBaixo := TemMais(VX_Janela,+1)
      ENDIF
      *
      Rolamento_(VX_Janela,N_ColCobertas#0,L_RolaCima,L_RolaBaixo,;
                 N_TamLinha-N_ColCobertas>N_LarguraJan)
   ENDIF
   *
   N_Tecla := Inkey_(.T.)
   * Teclas combinadas precisam de tratamento especial de "desempate" (ex: CTRL-C).
   N_Tecla := AjustaTecla(N_Tecla)
   *
   DO CASE
      CASE N_Tecla == K_UP   .OR. N_Tecla == K_MWFORWARD
           N_LinhasAler :=  -1
           L_Remontar   := .F.
      CASE N_Tecla == K_DOWN .OR. N_Tecla == K_MWBACKWARD
           N_LinhasAler :=  +1
           L_Remontar   := .F.
      CASE N_Tecla == K_PGUP
           N_LinhasAler :=  -N_AlturaJan
           L_Remontar   := .F.
      CASE N_Tecla == K_PGDN
           N_LinhasAler :=  +N_AlturaJan
           L_Remontar   := .F.
      CASE N_Tecla == K_CTRL_HOME
           FileTop2(N_Handle)
           N_LinhasAler :=  +N_AlturaJan
           L_Remontar   := .T.
           L_InicioTela := .F.    // evita o reposionamento do ponteiro
      CASE N_Tecla == K_CTRL_W_ARBITRADO_TECLADO  // Não dá certo usar a K_CTRL_END diretamente
           * No Harbour, não dá para diferenciar entre o uso da K_CTRL_END e da K_CTRL_W,
           * pois ambas exigem que se pressione a tecla "Ctrl".
           FileBottom2(N_Handle)
           N_LinhasAler :=  -N_AlturaJan
           L_Remontar   := .T.
           L_InicioTela := .T.    // evita o reposionamento do ponteiro
      CASE N_Tecla == K_LEFT
           IF N_ColCobertas > 0
              N_ColCobertas--
           ENDIF
      CASE N_Tecla == K_CTRL_LEFT
           N_ColCobertas := N_ColCobertas - N_LarguraJan
           IF N_ColCobertas < 0
              N_ColCobertas := 0
           ENDIF
      CASE N_Tecla == K_RIGHT
           IF N_ColCobertas < N_TamLinha-N_LarguraJan
              N_ColCobertas++
           ENDIF
      CASE N_Tecla == K_CTRL_RIGHT
           N_ColCobertas := N_ColCobertas + N_LarguraJan
           IF N_ColCobertas > N_TamLinha-N_LarguraJan
              N_ColCobertas := N_TamLinha-N_LarguraJan
           ENDIF
      CASE N_Tecla == K_HOME
           N_ColCobertas := 0
      CASE N_Tecla == K_END
           N_ColCobertas := N_TamLinha-N_LarguraJan
      CASE ASCAN(VN_TeclaFim,N_Tecla) # 0
           L_Continua := .F.
           L_FimOK := .T.
      CASE N_Tecla == K_ESC
           L_Continua := .F.
           L_FimOK := .F.
      case N_Tecla == K_LBUTTONDOWN .OR. N_Tecla == K_LDBLCLK
           *
           N_mRow := mRow()
           N_mCol := mCol()
           N_RegiaoMouse := RegiaoJanela_(VX_Janela,N_mRow,N_mCol,;
                                          Lin1Livre(VX_Janela),;
                                          Col1Livre(VX_Janela),;
                                          Lin2Livre(VX_Janela),;
                                          Col2Livre(VX_Janela),;
                                          @N_Keyboard)
           #INCLUDE "mousecua.ch"
           *
           IF N_RegiaoMouse == AREA_UTIL
              * clicou dentro do texto (não fazer nada)
           ELSEIF N_Keyboard # NIL
              HB_KeyPut(N_Keyboard)
           ENDIF
           *
      CASE N_Tecla == K_CTRL_C_ARBITRADO_TECLADO .OR. ; // Não dá certo usar a K_CTRL_C diretamente
           N_Tecla == K_CTRL_C_ARBITRADO_TOOLBAR
           COPIA_PARA_CLIPBOARD(N_Handle)
   ENDCASE
ENDDO

ENDIF

*
* Ao encerrar uma ativação, deixar posicionado no início da 1a. linha
AjustaPosicao(VX_Janela,-1)
L_InicioTela  := .F.
*
SET(_SET_SCOREBOARD,L_ScorAnt)
SET(_SET_CURSOR,N_CursorAnt)
*
IF L_PrimAtivacao
   LOGA_AJTELAT(C_CdTela,C_Cabec,NIL)  // LOGAR conteúdo de telas
ENDIF
*
L_PrimAtivacao := .F.
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    IF SOB_MODO_GRAFICO()
//       WVW_SetPaintRefresh(N_PaintRefresh_Old)
//    ENDIF
// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
*
RETURN L_FimOK
*
*****************
STAT FUNC TemMais (VX_Janela,N_Sentido)
*****************
LOCAL N_PosArquivo := FilePos2(N_Handle), L_TemMais
LOCAL L_IniTelaAnt := L_InicioTela
*
AjustaPosicao(VX_Janela,N_Sentido)
L_TemMais := ( LEN(FReadLn2(N_Handle,N_Sentido,N_TamLinha))==1 )
FSEEK(N_Handle,N_PosArquivo,FS_SET)         // restaura posição
L_InicioTela := L_IniTelaAnt                // restaura variável
*
RETURN L_TemMais
*
******************************
STAT PROC COPIA_PARA_CLIPBOARD(N_Handle2)
******************************
LOCAL N_PosArquivo := FilePos2(N_Handle2)
LOCAL N_TamBuffer  := FileSize2(N_Handle2)
LOCAL C_Buffer := SPACE(N_TamBuffer)
*
FileTop2(N_Handle2)
*
FRead(N_Handle2,@C_Buffer,N_TamBuffer)
*
SETCLIPBOARD_ASPEC(C_Buffer)
*
FSEEK(N_Handle2,N_PosArquivo,FS_SET)         // restaura posição
*
***********************
STAT PROC AjustaPosicao (VX_Janela,N_LinhasALer)
***********************
* Se sentido da leitura no arquivo foi alterada, ajustar posição do
* ponteiro do arquivo. As linhas lidas serão descartadas, pois
* já se encontram exibidas na tela.
IF N_LinhasALer > 0 .AND. L_InicioTela
   * avançar para final da tela
   FReadLn2(N_Handle,+LinhasComDados(V_Linhas),N_TamLinha)
   L_InicioTela := .F.
ELSEIF N_LinhasALer < 0 .AND. .NOT. L_InicioTela
   * retroceder para início da tela
   FReadLn2(N_Handle,-LinhasComDados(V_Linhas),N_TamLinha)
   L_InicioTela := .T.
ENDIF
*
*******************
STATIC FUNC LerInfo ( VX_Janela, N_LinhasALer, L_Remontar )
*******************
LOCAL V_Buffer, N_Cont, N_ContBuffer, N_LinhasLidas
*
AjustaPosicao(VX_Janela,N_LinhasALer)
*
V_Buffer := FReadLn2(N_Handle,N_LinhasALer,N_TamLinha)   // linhas lidas do arq.
N_LinhasLidas := LEN(V_Buffer)*IIF(N_LinhasAler>=0,+1,-1)
*
IF L_Remontar
   * adicionar linhas a V_Buffer, de modo a preencher toda a tela
   * (indica a necessidade de ajustes na posição das linhas, no caso
   *  da quantidade de linhas do arquivo não preencher toda a tela )
   DO WHILE LEN(V_Buffer) < ABS(N_LinhasAler)
      IF N_LinhasALer >= 0
         AADD(V_Buffer,NIL)
      ELSE
         ASIZE(V_Buffer,LEN(V_Buffer)+1)
         AINS(V_Buffer,1)    // valor "default" já é NIL
      ENDIF
   ENDDO
   N_LinhasLidas := N_LinhasAler
ENDIF
*
* deslocar linhas no vetor de exibir, de modo a conter o V_Buffer
IF N_LinhasLidas > 0         // definir forma de percorrer o vetor
   N_ContBuffer := 1
   FOR N_Cont := 1 TO LEN(V_Linhas) STEP +1
       IF N_Cont+N_LinhasLidas <= LEN(V_Linhas)
          * Deslocar linhas dentro do vetor de exibição
          V_Linhas[N_Cont] := V_Linhas[N_Cont+N_LinhasLidas]
       ELSE
          * Transpor linhas recém-lidas para vetor de exibição
          V_Linhas[N_Cont] := V_Buffer[N_ContBuffer]
          N_ContBuffer++
       ENDIF
   NEXT
ELSEIF N_LinhasLidas < 0
   N_ContBuffer := 1
   FOR N_Cont := LEN(V_Linhas) TO 1 STEP -1
       IF N_Cont+N_LinhasLidas >= 1
          * Deslocar linhas dentro do vetor de exibição
          V_Linhas[N_Cont] := V_Linhas[N_Cont+N_LinhasLidas]
       ELSE
          * Transpor linhas recém-lidas para vetor de exibição
          V_Linhas[N_Cont] := V_Buffer[N_ContBuffer]
          N_ContBuffer++
       ENDIF
   NEXT
ENDIF
*
RETURN NIL
*
************************
STAT FUNC LinhasComDados ( V_Linhas2 )
************************
LOCAL N_Max := 0 , N_Cont
FOR N_Cont := 1 TO LEN(V_Linhas2)
    IF V_Linhas2[N_Cont] # NIL
       N_Max := N_Cont
    ENDIF
NEXT
RETURN N_Max
*
******************
STAT PROC Destacar(N_Lin,N_Col,C_Lin,C_PalDest,VX_Janela)
******************
LOCAL N_Pos
DO WHILE (N_Pos := AT(XLOWER(C_PalDest),C_Lin)) # 0
   SETPOS(N_Lin,N_Col+N_Pos-1)
   DISPOUT(C_PalDest,CorJanInten(VX_Janela))
   C_Lin := SPACE(N_Pos+LEN(C_PalDest))+SUBSTR(C_Lin,N_Pos+LEN(C_PalDest))
ENDDO
*
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    ***************************
//    STAT FUNC DesenhaBoxExterno (VX_Janela)
//    ***************************
//    #define RGB(nR,nG,nB)  ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )
//    RETURN {|| WVW_SetPen(0,0,rgb(210,1210,210)),;
//               WVW_DrawBoxRecessed( N_WindowNum,;
//                                    Lin1Livre(VX_Janela),;
//                                    Col1Livre(VX_Janela),;
//                                    Lin2Livre(VX_Janela),;
//                                    Col2Livre(VX_Janela) ) }
//    *
//    *******************************
//    STAT FUNC CoordenadasBoxExterno(VX_Janela)
//    *******************************
//    RETURN { Lin1Livre(VX_Janela),Col1Livre(VX_Janela),;
//             Lin2Livre(VX_Janela),Col2Livre(VX_Janela)   }

// #elif defined(__PLATFORM__LINUX)
//    // NAO_ADAPTADO_PARA_LINUX_INTERFACE_SEMI_GRAFICA
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
*
*
*********************************

