#ifndef RMN_DICT_H__
#define RMN_DICT_H__
/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Manipulation des fichiers xml dictionnaire des variables
 * Fichier      : Dict.h
 * Creation     : Mai 2014 - J.P. Gauthier
 *
 * Description  : Basé fortement sur le code d'Yves Chartier afin de convertir
 *                Le binaire un fonctions de librairies.
 *
 * Remarques    :
 *
 * License      :
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation,
 *    version 2.1 of the License.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the
 *    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *    Boston, MA 02111-1307, USA.
 *
 *=========================================================
 */
#include <App.h>
#include "rmn/List.h"

#define DICT_SHORT  0
#define DICT_LONG   1
#define DICT_XML    2
#define DICT_NOTSET 1e+300

#define DICT_ALL        0x00

#define DICT_INCOMPLETE 0x01
#define DICT_FUTURE     0x02
#define DICT_OBSOLETE   0x04
#define DICT_DEPRECATED 0x08
#define DICT_CURRENT    0x10
#define DICT_STATE      0xFF

#define DICT_INTEGER    0x100
#define DICT_REAL       0x200
#define DICT_LOGICAL    0x400
#define DICT_CODE       0x800
#define DICT_TYPE       0xF00

#define DICT_EXACT    0
#define DICT_GLOB     1

#define DICT_MAXLEN   1024

#define DICT_LEN_MEANING 128

typedef enum {
    DICT_ASCII = 0,
    DICT_UTF8 = 1,
    DICT_ISO8859_1 = 2
} TDict_Encoding;

typedef struct {
    time_t Date;                                      //! Date of creation
    double Min;                                       //! Range of values and applied factor
    double Max;
    double Magnitude;
    double Factor;
    double Delta;
    double Precision;  
    char   Origin[32];                                //! Origin of the variable
    char   Name[8];                                   //! NOMVAR
    char   Short[2][256];                             //! Short description in both language (128 def + 128 modifier)
    char   Long[2][DICT_MAXLEN];                      //! Long description in both language
    char   Units[32];                                 //! Units
    int    Nature;                                    //! Mask for state and nature of variable
    int    Pack;                                      //! Optimum packing number of bits
    int    Codes[64];                                 //! List of codes for coded variable
    char   Meanings[64][2][DICT_LEN_MEANING];         //! List of associated meanings
    int    NCodes;                                    //! Number of codes
    int    IP1,IP2,IP3;                               //! Specific IP values
    float  Level;                                     //! Level (decoded IP1)
    int    Kind;                                      //! Kind (decoded IP1)_
    char   ETIKET[13];                                //! Specific etiket value
} TDictVar;

typedef struct {
    time_t Date;                 //! Date of creation
    int  Nature;                 //! Mask for state and nature of type
    char Origin[32];             //! Origin ot the type
    char Name[3];                //! TYPVAR
    char Short[2][128];          //! Short description in both language
    char Long[2][DICT_MAXLEN];   //! Long description in both language
} TDictType;

int        Dict_Load(TDict_Encoding Encoding);
char*      Dict_Loaded(void);
char*      Dict_Version(void);
int        Dict_Parse(char *Filename, TDict_Encoding Encoding);
void       Dict_SetSearch(int SearchMode, int SearchState, char *SearchOrigin, int SearchIP1, int SearchIP2, int SearchIP3, char *SearchETIKET);
void       Dict_SetModifier(char *Modifier);
void       Dict_AddVar(TDictVar *Var);
void       Dict_AddType(TDictType *Type);
TDictVar  *Dict_GetVar(const char * const varName);
TDictType *Dict_GetType(char *Type);
TDictVar  *Dict_IterateVar(TList **Iterator, char *Var);
TDictType *Dict_IterateType(TList **Iterator, char *Type);
void       Dict_PrintVar(TDictVar *DVar, int Format, TApp_Lang Lang);
void       Dict_PrintVars(char *Var, int Format, TApp_Lang Lang);
void       Dict_PrintType(TDictType *DType, int Format, TApp_Lang Lang);
void       Dict_PrintTypes(char *Type, int Format, TApp_Lang Lang);
int        Dict_SortVar(const void * const var0, const void * const var1);
int        Dict_SortType(const void * const type0, const void * const type1);
int        Dict_CheckVar(const void * const var0, const void * const str);
int        Dict_CheckType(const void * const type0, const void * const str);
TDictVar*  Dict_ApplyModifier(TDictVar *Var, char *Modifier);

#endif
