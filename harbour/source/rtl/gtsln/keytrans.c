/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on Slang screen library.
 *
 * Copyright 2000 Marek Paliwoda <paliwoda@inetia.pl>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* a table of function keys translation */
static int transKeyFunTab[] =
{
   SL_KEY_DOWN,      K_DOWN,
   SL_KEY_UP,        K_UP,
   SL_KEY_LEFT,      K_LEFT,
   SL_KEY_RIGHT,     K_RIGHT,
   SL_KEY_HOME,      K_HOME,
   SL_KEY_END,       K_END,
   SL_KEY_BACKSPACE, K_BS,
   SL_KEY_IC,        K_INS,
   SL_KEY_DELETE,    K_DEL,
   SL_KEY_NPAGE,     K_PGDN,
   SL_KEY_PPAGE,     K_PGUP,
   SL_KEY_ESC,       K_ESC,
   SL_KEY_F(1),      K_F1,
   SL_KEY_F(2),      K_F2,
   SL_KEY_F(3),      K_F3,
   SL_KEY_F(4),      K_F4,
   SL_KEY_F(5),      K_F5,
   SL_KEY_F(6),      K_F6,
   SL_KEY_F(7),      K_F7,
   SL_KEY_F(8),      K_F8,
   SL_KEY_F(9),      K_F9,
   SL_KEY_F(10),     K_F10,
   SL_KEY_F(11),     K_SH_F1,
   SL_KEY_F(12),     K_SH_F2,
   SL_KEY_F(13),     K_SH_F3,
   SL_KEY_F(14),     K_SH_F4,
   SL_KEY_F(15),     K_SH_F5,
   SL_KEY_F(16),     K_SH_F6,
   SL_KEY_F(17),     K_SH_F7,
   SL_KEY_F(18),     K_SH_F8,
   SL_KEY_F(19),     K_SH_F9,
   SL_KEY_F(20),     K_SH_F10,
   SL_KEY_F(21),     K_CTRL_F1,
   SL_KEY_F(22),     K_CTRL_F2,
   SL_KEY_F(23),     K_CTRL_F3,
   SL_KEY_F(24),     K_CTRL_F4,
   SL_KEY_F(25),     K_CTRL_F5,
   SL_KEY_F(26),     K_CTRL_F6,
   SL_KEY_F(27),     K_CTRL_F7,
   SL_KEY_F(28),     K_CTRL_F8,
   SL_KEY_F(29),     K_CTRL_F9,
   SL_KEY_F(30),     K_CTRL_F10,
   SL_KEY_F(31),     K_ALT_F1,
   SL_KEY_F(32),     K_ALT_F2,
   SL_KEY_F(33),     K_ALT_F3,
   SL_KEY_F(34),     K_ALT_F4,
   SL_KEY_F(35),     K_ALT_F5,
   SL_KEY_F(36),     K_ALT_F6,
   SL_KEY_F(37),     K_ALT_F7,
   SL_KEY_F(38),     K_ALT_F8,
   SL_KEY_F(39),     K_ALT_F9,
   SL_KEY_F(40),     K_ALT_F10
};

/* a table of alt+letter translation */
static int transAltKeyLetterTab[] =
{
   'A' ,  K_ALT_A ,    'B' ,  K_ALT_B ,    'C' ,  K_ALT_C ,
   'D' ,  K_ALT_D ,    'E' ,  K_ALT_E ,    'F' ,  K_ALT_F ,
   'G' ,  K_ALT_G ,    'H' ,  K_ALT_H ,    'I' ,  K_ALT_I ,
   'J' ,  K_ALT_J ,    'K' ,  K_ALT_K ,    'L' ,  K_ALT_L ,
   'M' ,  K_ALT_M ,    'N' ,  K_ALT_N ,    'O' ,  K_ALT_O ,
   'P' ,  K_ALT_P ,    'Q' ,  K_ALT_Q ,    'R' ,  K_ALT_R ,
   'S' ,  K_ALT_S ,    'T' ,  K_ALT_T ,    'U' ,  K_ALT_U ,
   'V' ,  K_ALT_V ,    'W' ,  K_ALT_W ,    'X' ,  K_ALT_X ,
   'Y' ,  K_ALT_Y ,    'Z' ,  K_ALT_Z
};

/* a table of alt+digit translation */
static int transAltKeyDigitTab[] =
{
   '0' ,  K_ALT_0 ,    '1' ,  K_ALT_1 ,
   '2' ,  K_ALT_2 ,    '3' ,  K_ALT_3 ,
   '4' ,  K_ALT_4 ,    '5' ,  K_ALT_5 ,
   '6' ,  K_ALT_6 ,    '7' ,  K_ALT_7 ,
   '8' ,  K_ALT_8 ,    '9' ,  K_ALT_9
};

#ifdef IBMPC_SYSTEM
/* a table of Dos ScanCode translation - hard coded in Slang */
static unsigned char transDosScanCodeTab[] =
{
   'A' , 0x1E , 'B' , 0x30 , 'C' , 0x2E , 'D' , 0x20 , 'E' , 0x12 ,
   'F' , 0x21 , 'G' , 0x22 , 'H' , 0x23 , 'I' , 0x17 , 'J' , 0x24 ,
   'K' , 0x25 , 'L' , 0x26 , 'M' , 0x32 , 'N' , 0x31 , 'O' , 0x18 ,
   'P' , 0x19 , 'Q' , 0x10 , 'R' , 0x13 , 'S' , 0x1F , 'T' , 0x14 ,
   'U' , 0x16 , 'V' , 0x2F , 'W' , 0x11 , 'X' , 0x2D , 'Y' , 0x15 ,
   'Z' , 0x2C , '1' , 0x78 , '2' , 0x79 , '3' , 0x7A , '4' , 0x7B ,
   '5' , 0x7C , '6' , 0x7D , '7' , 0x7E , '8' , 0x7F , '9' , 0x80 ,
   '0' , 0x81
};
#endif

