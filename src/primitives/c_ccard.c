/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>

#include <App.h>
#include <rmn/c_ccard.h>

#define MAJUS 0
#define MINUS 1
#define PAREIL 2


typedef struct
{
    char *name;
    char *def;
    char *val;
    char *fin;
    int type;
} keyDef;


//! Get key index in the array of keys
static int findKey(
    //! [in] Name of the key to search
    const char * const name,
    //! [in] Key definition array
    const keyDef keys[],
    //! [in] Number of keys in array
    const int nbKeys
) {
    for (int i = 0; i < nbKeys; i++) {
        if (strcasecmp(name, keys[i].name) == 0) {
            return i;
        }
    }
    return -1;
}


//! Extract element from a list
static void extractValues(
    keyDef keys[],
    const char **argv,
    const int keyIdx,
    int *currentKeyIdx
) {
    // pour l'element contenu dans argv, on fait les operations
    // suivantes:
    //            on elimine le signe = si present,
    //            on elimine les delimiteurs ':' si presents
    //            et on attribue les valeurs ainsi obtenues
    //            a la clef a traiter.
    //
    //        exemple:   *argv =  =-12:34          donnera
    //                             -12 34     qui seront associes
    //                    a deux keys contigues ayant le meme nom dans la liste des keys.

    while (1) {
        if ((*currentKeyIdx > -1) && (strcmp(keys[*currentKeyIdx].name, keys[keyIdx].name)) == 0) {
            int skipEqual = 0;
            if (**argv == '=') {
                skipEqual = 1;
            }

            const char * const colon = strchr(*argv, ':');
            if (colon != (char *) NULL) {
                strncpy(keys[*currentKeyIdx].fin, (*argv) + skipEqual, colon - *argv);
                keys[*currentKeyIdx].fin[colon - *argv + 1] = '\0';
                *argv = colon + 1;
                (*currentKeyIdx)++;
            } else {
                strcpy(keys[*currentKeyIdx].fin, (*argv) + skipEqual);
                (*currentKeyIdx)++;
                break;
            }
        } else {
            Lib_Log(APP_ERROR,APP_LIBRMN,"%s: Debordement de liste ou mode positionnel\n",__func__);
        }
    }
}


//! Convert string to upper case
void str2up(
    const char * const strIn,
    char * const strOut
) {
    // Clean output string
    char * cOut = strOut;
    while(*cOut) {
        *cOut = '\0';
        cOut++;
    }

    const char * cIn = strIn;
    cOut = strOut;
    while(*cIn)
    {
        *cOut = toupper(*cIn);
        cOut++;
        cIn++;
    }
}


//! Figure out the key type (uppercase, lowercase or as-is) and trim 1 char if as-is or lowercase
static int keyCase(
    char * const arg
) {
    int lng = strlen(arg) - 1;

    if (*(arg + lng) == '.') {
        *(arg + lng) = '\0';
        return PAREIL;
    }
    if (*(arg + lng) == '_') {
        *(arg + lng) = '\0';
        return MINUS;
    }
    return MAJUS;
}


//! Convert to lower or upper case if required by the key type
static void convKeyCase(
    char *key,
    int type
) {
    if (type == MAJUS) {
        while(*key) {
            *key = toupper(*key);
            key++;
        }
    } else if (type == MINUS) {
        while(*key) {
            *key = tolower(*key);
            key++;
        }
    }
}


//! Print the call sequence on stderr
static void printKeys(
    //! [in] Key definition array
    const keyDef defo[],
    //! [in] Name of the program
    const char * const name,
    //! [in] Number of key names
    int nbKeys
) {
    fprintf(stderr, "\n *** SEQUENCE D'APPEL ***\n\n");

    fprintf(stderr, "%s \n", name);

    for (int i = 0; i < nbKeys; i++) {
        fprintf(stderr, "          -%s [%s:%s]\n", defo[i].name, defo[i].def, defo[i].val);
    }
    fprintf(stderr, "\n");
}


//! Get positional arguments
//!
//! cette fonction recupere les parametres positionels (c.a.d. ceux
//! qui sont associes a une clef ayant pour nom '-'.
//!
//! Lorsque debut est 1, on recupere les parametres jusqu'a la rencontre
//! d'un nom de clef et on retourne la position du prochain argument contenu dans argv.
//!
//! Lorsque debut est 0, on recupere les parametres positionels qui
//! suivent la sequence '--'. On recupere alors jusqu'a epuisement de argv.
//!
//! La fonction retourne dans npos le total cumulatif des arguments positionels traites
//! et retourne dans nbErrors le cumul des erreurs rencontrees
static const char **posArgs(
    const char **arg,
    keyDef keys[],
    const int nbKeys,
    const int dashKeyIdx,
    int *nbPosArgs,
    int * const posKeyIdx,
    int * const nbErrors
) {
    while (*arg) {
        if ((**arg == '-') && (*posKeyIdx == dashKeyIdx)) {
            return arg;
        }
        if (*posKeyIdx >= nbKeys) {
            arg++;
            (*nbErrors)++;
            return arg;
        }
        if (strcmp(keys[*posKeyIdx].name, keys[dashKeyIdx].name) == 0) {
            strcpy(keys[*posKeyIdx].fin, *arg);
        } else {
            (*nbErrors)++;
        }
        (*posKeyIdx)++;
        (*nbPosArgs)++;
        arg++;
    }
    return arg;
}


//! Parse command line arguments
void c_ccard(
    //! [in] Command line
    const char ** const argv,
    //! [in] Number of arguments
    const int argc,
    //! [in] Array of key names to extract if present
    const char ** const keyNames,
    //! [out] Array of key values
    char val[][CCARD_NCARMAX],
    //! [in] Array of default values for each key name
    const char ** const def,
    //! [in] Number of key names
    const int nbKeys,
    //! [inout] Number of positional parameters on output
    //! However,  this value is  only returned when the input value is
    //! greater than 0.  If the value given on input is smaller or equal
    //! to 0, the value will not be changed.
    //! Furthermore,  npos  =  -111  on  input,  informs  ccard  to
    //! immediately stop program execution upon encountering an error.
    int * const npos
) {
    keyDef keys[CCARD_NKLEMAX];
    for (int i = 0; i < nbKeys; i++) {
        keys[i].name = calloc(strlen(keyNames[i]) + 1, sizeof(char));
        keys[i].def = calloc(strlen(def[i]) + 1, sizeof(char));
        keys[i].val = calloc(CCARD_NCARMAX + 1, sizeof(char));
        keys[i].fin = calloc(CCARD_NCARMAX + 1, sizeof(char));
    }

    int exitOnError = 0;
    if (*npos == -111) {
        exitOnError = 1;
    }

    int returnNpos =  0;
    if (*npos > 0) {
        *npos = 0;
        returnNpos = 1;
    }

    for (int i = 0; i < nbKeys; i++) {
        str2up(keyNames[i], keys[i].name);
        keys[i].type = keyCase(keys[i].name);
        strcpy(keys[i].def, def[i]);
        strcpy(keys[i].val, val[i]);
        strcpy(keys[i].fin, val[i]);
    }

    // obtenir le nom du programme
    const char * const progName = *argv;
    const char ** arg = argv + 1;

    if (argc == 2) {
        if ((strcmp(*arg, "-h") == 0) || (strcmp(*arg, "-H")==0)) {
            printKeys(keys, progName, nbKeys);
            exit(0);
        }
    }

    int nbErrors = 0;
    int npos_interne;

    // verifier si on peut recuperer en mode positionel on cherche la premiere clef '-' dans la liste
    int dashKeyIdx = findKey("-", keys, nbKeys);
    int posKeyIdx = dashKeyIdx;

    if (dashKeyIdx > -1) {
        // recuperer les premiers arguments positionels
        arg = posArgs(arg, keys, nbKeys, dashKeyIdx, &npos_interne, &posKeyIdx, &nbErrors);
        if (nbErrors) {
            Lib_Log(APP_ERROR,APP_LIBRMN,"%s: Trop d'arguments positionels\n",__func__);
            printKeys(keys, progName, nbKeys);
            if (exitOnError) {
                exit(-1);
            }
        }
    }

    int keyIdx = -1;
    int currentKeyIdx = -1;
    while (*arg) {
        if (**arg == '-') {
            // un nom de clef
            if (*((*arg) + 1) == '-') {
                // fin des clefs
                // Recuperer le reste des arguments positionels
                arg++;
                arg = posArgs(arg, keys, nbKeys, dashKeyIdx, &npos_interne, &posKeyIdx, &nbErrors);
                if (nbErrors) {
                    Lib_Log(APP_ERROR,APP_LIBRMN,"%s: Trop d'arguments positionelsl\n",__func__);
                    printKeys(keys, progName, nbKeys);
                    if (exitOnError) {
                        exit(-1);
                    }
                }
                break;
            }

            char * equalChr = (strchr(*arg, '='));
            char * const keyName = calloc(equalChr - *arg + 1, sizeof(char));
            strncpy(keyName, *arg, (equalChr - *arg));
            int keyIdx = findKey(keyName, keys, nbKeys);
            if (keyIdx > -1) {
                currentKeyIdx = keyIdx;
                if (equalChr != (char *) NULL) {
                    // There is an '=' in the argument
                    *arg = equalChr + 1;
                    extractValues(keys, arg, keyIdx, &currentKeyIdx);
                } else {
                    // '=' Not found in arg -> use default value
                    strcpy(keys[keyIdx].fin , keys[keyIdx].def);
                }
            } else {
                Lib_Log(APP_ERROR,APP_LIBRMN,"%s: Clef=%s invalide\n",__func__,keyName);
                printKeys(keys, progName, nbKeys);
                if (exitOnError) {
                    exit(-1);
                }
            }
            free(keyName);
        } else {
            if ((currentKeyIdx != -1) && (strcmp(keyNames[currentKeyIdx], keyNames[keyIdx]) == 0)) {
                extractValues(keys, arg, keyIdx, &currentKeyIdx);
            } else {
                Lib_Log(APP_ERROR,APP_LIBRMN,"%s: Débordement de liste\n",__func__);
                printKeys(keys, progName, nbKeys);
                if (exitOnError) {
                    exit(-1);
                }
            }
        }

        arg++;
    }

    // recopier les valeurs finales des clefs dans val et liberer la memoire
    for (int i = 0; i < nbKeys; i++) {
        convKeyCase(keys[i].fin, keys[i].type);
        strcpy(val[i], keys[i].fin);
        free(keys[i].name);
        free(keys[i].def);
        free(keys[i].val);
        free(keys[i].fin);
    }

    // Retourner le nombre d'arguments positionels si a l'entree, npos > 0
    if (returnNpos) {
        (*npos) = npos_interne;
    }
}
