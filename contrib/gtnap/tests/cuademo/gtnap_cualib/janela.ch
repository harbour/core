/* encoding: cp850 */
/*
* janela.ch
* Pr‚-processa as pseudo-rotinas do objeto janela.
* Autor: Marcos Augusto
*
* Nota: Nao utilize as pseudo-funcoes na sintaxe j  pr‚-processada !
*/

#ifndef JANELA_CH
#define JANELA_CH

#xtranslate Lin1Livre(<janela>)   => <janela>\[10]
#xtranslate Col1Livre(<janela>)   => <janela>\[11]
#xtranslate Lin2Livre(<janela>)   => <janela>\[12]
#xtranslate Col2Livre(<janela>)   => <janela>\[13]
#xtranslate LinMess(<janela>)     => <janela>\[14]

#xtranslate CorJanela(<janela>)   => <janela>\[15]
#xtranslate CorJanInten(<janela>) => <janela>\[16]

#xtranslate GetJanTipoMsgAguarde(<janela>) => <janela>\[44]
#xtranslate SetJanTipoMsgAguarde(<janela>,<lvalue>) => <janela>\[44] := <lvalue>

#define C_TelaCoberta           VX_Janela[05]
#define N_TP_Jan                VX_Janela[18]
#define VX_SubObj               VX_Janela[19]
#define B_Metodo                VX_Janela[20]
#define N_WindowNum             VX_Janela[22]
#DEFINE L_ScrollVertical        VX_Janela[28]
#DEFINE L_ScrollHorizontal      VX_Janela[29]
#DEFINE L_CriarToolBar          VX_Janela[37]
#DEFINE L_CUA_10                VX_Janela[47]
#DEFINE C_CdTela                VX_Janela[17]
#DEFINE V_LstAcoes              VX_Janela[46]
#DEFINE V_LstImagens            VX_Janela[42]
#DEFINE V_RegiaoBotoes          VX_Janela[21]
#DEFINE L_Embutida              VX_Janela[26]
#DEFINE C_Cabec                 VX_Janela[30]
#DEFINE N_IdScrollBarVertical   VX_Janela[51]
#DEFINE N_IdScrollBarHorizontal VX_Janela[52]
#DEFINE B_ScrollBarVertical     VX_Janela[53]
#DEFINE B_ScrollBarHorizontal   VX_Janela[54]
#DEFINE N_IdProgressBar1        VX_Janela[45]
#DEFINE N_IdProgressBar2        VX_Janela[48]
#DEFINE N_ItemId                VX_Janela[56]
#DEFINE L_ComEmbutidas          VX_Janela[57]

#endif