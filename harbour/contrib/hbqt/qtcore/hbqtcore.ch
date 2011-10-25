/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef _HBQTCORE_CH
#define _HBQTCORE_CH

// This enum describes the errors that may be returned by the error() function.
#define QFile_NoError                             0        // No error occurred.
#define QFile_ReadError                           1        // An error occurred when reading from the file.
#define QFile_WriteError                          2        // An error occurred when writing to the file.
#define QFile_FatalError                          3        // A fatal error occurred.
#define QFile_ResourceError                       4        //
#define QFile_OpenError                           5        // The file could not be opened.
#define QFile_AbortError                          6        // The operation was aborted.
#define QFile_TimeOutError                        7        // A timeout occurred.
#define QFile_UnspecifiedError                    8        // An unspecified error occurred.
#define QFile_RemoveError                         9        // The file could not be removed.
#define QFile_RenameError                         10       // The file could not be renamed.
#define QFile_PositionError                       11       // The position in the file could not be changed.
#define QFile_ResizeError                         12       // The file could not be resized.
#define QFile_PermissionsError                    13       // The file could not be accessed.
#define QFile_CopyError                           14       // The file could not be copied.

#define QFile_NoOptions                           0        // No options.

// This enum is used by the permission() function to report the permissions and ownership of a file. The values may be OR-ed together to test multiple permissions and ownership values.
#define QFile_ReadOwner                           0x4000   // The file is readable by the owner of the file.
#define QFile_WriteOwner                          0x2000   // The file is writable by the owner of the file.
#define QFile_ExeOwner                            0x1000   // The file is executable by the owner of the file.
#define QFile_ReadUser                            0x0400   // The file is readable by the user.
#define QFile_WriteUser                           0x0200   // The file is writable by the user.
#define QFile_ExeUser                             0x0100   // The file is executable by the user.
#define QFile_ReadGroup                           0x0040   // The file is readable by the group.
#define QFile_WriteGroup                          0x0020   // The file is writable by the group.
#define QFile_ExeGroup                            0x0010   // The file is executable by the group.
#define QFile_ReadOther                           0x0004   // The file is readable by anyone.
#define QFile_WriteOther                          0x0002   // The file is writable by anyone.
#define QFile_ExeOther                            0x0001   // The file is executable by anyone.


#define QLibraryInfo_PrefixPath                   0  // The default prefix for all paths.
#define QLibraryInfo_DocumentationPath            1  // The location for documentation upon install.
#define QLibraryInfo_HeadersPath                  2  // The location for all headers.
#define QLibraryInfo_LibrariesPath                3  // The location of installed librarires.
#define QLibraryInfo_BinariesPath                 4  // The location of installed Qt binaries (tools and applications).
#define QLibraryInfo_PluginsPath                  5  // The location of installed Qt plugins.
#define QLibraryInfo_DataPath                     6  // The location of general Qt data.
#define QLibraryInfo_TranslationsPath             7  // The location of translation information for Qt strings.
#define QLibraryInfo_SettingsPath                 8  // The location for Qt settings.
#define QLibraryInfo_ExamplesPath                 10 // The location for examples upon install.
#define QLibraryInfo_DemosPath                    9  // The location for demos upon install.


// country definitions - extracted by Luigi Ferraris

#define QLocale_AnyCountry                               0
#define QLocale_Afghanistan                              1
#define QLocale_Albania                                  2
#define QLocale_Algeria                                  3
#define QLocale_AmericanSamoa                            4
#define QLocale_Andorra                                  5
#define QLocale_Angola                                   6
#define QLocale_Anguilla                                 7
#define QLocale_Antarctica                               8
#define QLocale_AntiguaAndBarbuda                        9
#define QLocale_Argentina                                10
#define QLocale_Armenia                                  11
#define QLocale_Aruba                                    12
#define QLocale_Australia                                13
#define QLocale_Austria                                  14
#define QLocale_Azerbaijan                               15
#define QLocale_Bahamas                                  16
#define QLocale_Bahrain                                  17
#define QLocale_Bangladesh                               18
#define QLocale_Barbados                                 19
#define QLocale_Belarus                                  20
#define QLocale_Belgium                                  21
#define QLocale_Belize                                   22
#define QLocale_Benin                                    23
#define QLocale_Bermuda                                  24
#define QLocale_Bhutan                                   25
#define QLocale_Bolivia                                  26
#define QLocale_BosniaAndHerzegowina                     27
#define QLocale_Botswana                                 28
#define QLocale_BouvetIsland                             29
#define QLocale_Brazil                                   30
#define QLocale_BritishIndianOceanTerritory              31
#define QLocale_BruneiDarussalam                         32
#define QLocale_Bulgaria                                 33
#define QLocale_BurkinaFaso                              34
#define QLocale_Burundi                                  35
#define QLocale_Cambodia                                 36
#define QLocale_Cameroon                                 37
#define QLocale_Canada                                   38
#define QLocale_CapeVerde                                39
#define QLocale_CaymanIslands                            40
#define QLocale_CentralAfricanRepublic                   41
#define QLocale_Chad                                     42
#define QLocale_Chile                                    43
#define QLocale_China                                    44
#define QLocale_ChristmasIsland                          45
#define QLocale_CocosIslands                             46
#define QLocale_Colombia                                 47
#define QLocale_Comoros                                  48
#define QLocale_DemocraticRepublicOfCongo                49
#define QLocale_PeoplesRepublicOfCongo                   50
#define QLocale_CookIslands                              51
#define QLocale_CostaRica                                52
#define QLocale_IvoryCoast                               53
#define QLocale_Croatia                                  54
#define QLocale_Cuba                                     55
#define QLocale_Cyprus                                   56
#define QLocale_CzechRepublic                            57
#define QLocale_Denmark                                  58
#define QLocale_Djibouti                                 59
#define QLocale_Dominica                                 60
#define QLocale_DominicanRepublic                        61
#define QLocale_EastTimor                                62
#define QLocale_Ecuador                                  63
#define QLocale_Egypt                                    64
#define QLocale_ElSalvador                               65
#define QLocale_EquatorialGuinea                         66
#define QLocale_Eritrea                                  67
#define QLocale_Estonia                                  68
#define QLocale_Ethiopia                                 69
#define QLocale_FalklandIslands                          70
#define QLocale_FaroeIslands                             71
#define QLocale_FijiCountry                              72
#define QLocale_Finland                                  73
#define QLocale_France                                   74
#define QLocale_MetropolitanFrance                       75
#define QLocale_FrenchGuiana                             76
#define QLocale_FrenchPolynesia                          77
#define QLocale_FrenchSouthernTerritories                78
#define QLocale_Gabon                                    79
#define QLocale_Gambia                                   80
#define QLocale_Georgia                                  81
#define QLocale_Germany                                  82
#define QLocale_Ghana                                    83
#define QLocale_Gibraltar                                84
#define QLocale_Greece                                   85
#define QLocale_Greenland                                86
#define QLocale_Grenada                                  87
#define QLocale_Guadeloupe                               88
#define QLocale_Guam                                     89
#define QLocale_Guatemala                                90
#define QLocale_Guinea                                   91
#define QLocale_GuineaBissau                             92
#define QLocale_Guyana                                   93
#define QLocale_Haiti                                    94
#define QLocale_HeardAndMcDonaldIslands                  95
#define QLocale_Honduras                                 96
#define QLocale_HongKong                                 97
#define QLocale_Hungary                                  98
#define QLocale_Iceland                                  99
#define QLocale_India                                    100
#define QLocale_Indonesia                                101
#define QLocale_Iran                                     102
#define QLocale_Iraq                                     103
#define QLocale_Ireland                                  104
#define QLocale_Israel                                   105
#define QLocale_Italy                                    106
#define QLocale_Jamaica                                  107
#define QLocale_Japan                                    108
#define QLocale_Jordan                                   109
#define QLocale_Kazakhstan                               110
#define QLocale_Kenya                                    111
#define QLocale_Kiribati                                 112
#define QLocale_DemocraticRepublicOfKorea                113
#define QLocale_RepublicOfKorea                          114
#define QLocale_Kuwait                                   115
#define QLocale_Kyrgyzstan                               116
#define QLocale_Lao                                      117
#define QLocale_Latvia                                   118
#define QLocale_Lebanon                                  119
#define QLocale_Lesotho                                  120
#define QLocale_Liberia                                  121
#define QLocale_LibyanArabJamahiriya                     122
#define QLocale_Liechtenstein                            123
#define QLocale_Lithuania                                124
#define QLocale_Luxembourg                               125
#define QLocale_Macau                                    126
#define QLocale_Macedonia                                127
#define QLocale_Madagascar                               128
#define QLocale_Malawi                                   129
#define QLocale_Malaysia                                 130
#define QLocale_Maldives                                 131
#define QLocale_Mali                                     132
#define QLocale_Malta                                    133
#define QLocale_MarshallIslands                          134
#define QLocale_Martinique                               135
#define QLocale_Mauritania                               136
#define QLocale_Mauritius                                137
#define QLocale_Mayotte                                  138
#define QLocale_Mexico                                   139
#define QLocale_Micronesia                               140
#define QLocale_Moldova                                  141
#define QLocale_Monaco                                   142
#define QLocale_Mongolia                                 143
#define QLocale_Montserrat                               144
#define QLocale_Morocco                                  145
#define QLocale_Mozambique                               146
#define QLocale_Myanmar                                  147
#define QLocale_Namibia                                  148
#define QLocale_NauruCountry                             149
#define QLocale_Nepal                                    150
#define QLocale_Netherlands                              151
#define QLocale_NetherlandsAntilles                      152
#define QLocale_NewCaledonia                             153
#define QLocale_NewZealand                               154
#define QLocale_Nicaragua                                155
#define QLocale_Niger                                    156
#define QLocale_Nigeria                                  157
#define QLocale_Niue                                     158
#define QLocale_NorfolkIsland                            159
#define QLocale_NorthernMarianaIslands                   160
#define QLocale_Norway                                   161
#define QLocale_Oman                                     162
#define QLocale_Pakistan                                 163
#define QLocale_Palau                                    164
#define QLocale_PalestinianTerritory                     165
#define QLocale_Panama                                   166
#define QLocale_PapuaNewGuinea                           167
#define QLocale_Paraguay                                 168
#define QLocale_Peru                                     169
#define QLocale_Philippines                              170
#define QLocale_Pitcairn                                 171
#define QLocale_Poland                                   172
#define QLocale_Portugal                                 173
#define QLocale_PuertoRico                               174
#define QLocale_Qatar                                    175
#define QLocale_Reunion                                  176
#define QLocale_Romania                                  177
#define QLocale_RussianFederation                        178
#define QLocale_Rwanda                                   179
#define QLocale_SaintKittsAndNevis                       180
#define QLocale_StLucia                                  181
#define QLocale_StVincentAndTheGrenadines                182
#define QLocale_Samoa                                    183
#define QLocale_SanMarino                                184
#define QLocale_SaoTomeAndPrincipe                       185
#define QLocale_SaudiArabia                              186
#define QLocale_Senegal                                  187
#define QLocale_Seychelles                               188
#define QLocale_SierraLeone                              189
#define QLocale_Singapore                                190
#define QLocale_Slovakia                                 191
#define QLocale_Slovenia                                 192
#define QLocale_SolomonIslands                           193
#define QLocale_Somalia                                  194
#define QLocale_SouthAfrica                              195
#define QLocale_SouthGeorgiaAndTheSouthSandwichIslands   196
#define QLocale_Spain                                    197
#define QLocale_SriLanka                                 198
#define QLocale_StHelena                                 199
#define QLocale_StPierreAndMiquelon                      200
#define QLocale_Sudan                                    201
#define QLocale_Suriname                                 202
#define QLocale_SvalbardAndJanMayenIslands               203
#define QLocale_Swaziland                                204
#define QLocale_Sweden                                   205
#define QLocale_Switzerland                              206
#define QLocale_SyrianArabRepublic                       207
#define QLocale_Taiwan                                   208
#define QLocale_Tajikistan                               209
#define QLocale_Tanzania                                 210
#define QLocale_Thailand                                 211
#define QLocale_Togo                                     212
#define QLocale_Tokelau                                  213
#define QLocale_TongaCountry                             214
#define QLocale_TrinidadAndTobago                        215
#define QLocale_Tunisia                                  216
#define QLocale_Turkey                                   217
#define QLocale_Turkmenistan                             218
#define QLocale_TurksAndCaicosIslands                    219
#define QLocale_Tuvalu                                   220
#define QLocale_Uganda                                   221
#define QLocale_Ukraine                                  222
#define QLocale_UnitedArabEmirates                       223
#define QLocale_UnitedKingdom                            224
#define QLocale_UnitedStates                             225
#define QLocale_UnitedStatesMinorOutlyingIslands         226
#define QLocale_Uruguay                                  227
#define QLocale_Uzbekistan                               228
#define QLocale_Vanuatu                                  229
#define QLocale_VaticanCityState                         230
#define QLocale_Venezuela                                231
#define QLocale_VietNam                                  232
#define QLocale_BritishVirginIslands                     233
#define QLocale_USVirginIslands                          234
#define QLocale_WallisAndFutunaIslands                   235
#define QLocale_WesternSahara                            236
#define QLocale_Yemen                                    237
#define QLocale_Yugoslavia                               238
#define QLocale_Zambia                                   239
#define QLocale_Zimbabwe                                 240
#define QLocale_SerbiaAndMontenegro                      241
#define QLocale_Montenegro                               242
#define QLocale_Serbia                                   243
#define QLocale_SaintBarthelemy                          244
#define QLocale_SaintMartin                              245
#define QLocale_LatinAmericaAndTheCaribbean              246

#endif
