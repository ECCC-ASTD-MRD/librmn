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

//! \file


#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <rmn/c_ccard.h>

#define MAJUS 0
#define MINUS 1
#define PAREIL 2


//! \private
typedef struct
{
    char *name;
    char *def;
    char *val;
    char *final;
    int type;
} key;


//! \private
//! Get the position of a key in the list
static int getKeyIdx(
    const char * const name,
    key keys[],
    const int nbKeys
) {
    int i = 0;
    char tmpstr[CCARD_NCARMAX];

    strcpy(tmpstr, name);
    while (*(tmpstr + i))
    {
        *(tmpstr + i) = toupper(*(tmpstr + i));
        i++;
    }

    for (int i = 0; i < nbKeys; i++) {
        if (strcmp(tmpstr, keys[i].name) == 0) {
            return i;
        }
    }
    return -1;
}


//! \private
static void extractValues(
    key keys[],
    char **argv,
    int pos,
    int *posc
) {
    // pour l'element contenu dans argv, on fait les operations suivantes:
    //  Éliminer '=' si present,
    //  Éliminer ':' si presents
    //  Attribuer les valeurs ainsi obtenues à la clef a traiter.
    // exemple:   *argv =  =-12:34          donnera
    // -12 34     qui seront associes a deux keys contigues ayant le meme nom dans la liste des keys.

    char *deux_pt;

    while (1) {
        if ((*posc > -1) && (strcmp(keys[*posc].name, keys[pos].name)) == 0) {
            deux_pt = strchr(*argv, ':');
            if (deux_pt != (char *) NULL) {
                *deux_pt = '\0';
                if (**argv == '=') {
                    strcpy(keys[*posc].final, (*argv) + 1);
                } else {
                    strcpy(keys[*posc].final, *argv);
                }
                *argv = deux_pt + 1;
                (*posc)++;
            } else {
                if (**argv == '=') {
                    strcpy(keys[*posc].final, (*argv) + 1);
                } else {
                    strcpy(keys[*posc].final, *argv);
                }
                (*posc)++;
                break;
            }
        } else {
            fprintf(stderr, "\n***ERREUR DEBORDEMENT DE LISTE OU MODE POSITIONNEL\n");
        }
    }
}


//! \private
//! Convert to uppercase
static void convert2Upper(
    //! [in] Input String
    const char * const inStr,
    //! [out] Output string (Uppercase)
    char * const outStr
) {
    // Nullify output string
    char * outC = outStr;
    while(*outC) {
        *outC = '\0';
        outC++;
    }

    // Reset output char pointer to the begining of the string
    outC = outStr;

    const char * inC = inStr;
    while(*inC) {
        *outC = toupper(*inC);
        outC++;
        inC++;
    }
}


//! \private
//! Get key case
static int getKeyCase(
    //! Key definition string
    char *keyDefStr
) {
    const int lng = strlen(keyDefStr) - 1;

    if (*(keyDefStr + lng) == '.') {
        *(keyDefStr + lng) = '\0';
        return PAREIL;
    }
    if (*(keyDefStr + lng) == '_') {
        *(keyDefStr + lng) = '\0';
        return MINUS;
    }
    return MAJUS;
}


//! \private
//! Convert key case
static void convertKeyCase(
    char * const key,
    //! Key case (MAJUS|MINUS)
    const int keyCase
) {
    char * cchar = key;
    if (keyCase == MAJUS) {
        while(*cchar) {
            *cchar = toupper(*cchar);
            cchar++;
        }
    } else if (keyCase == MINUS) {
        while(*cchar) {
            *cchar = tolower(*cchar);
            cchar++;
        }
    }
}


//! \private
// Print command help on stderr
static void printCmdlineRef(
    //! Key definitions
    const key keys[],
    const char * const progName,
    const int nbKeys
) {
    fprintf(stderr, "\n *** SEQUENCE D'APPEL ***\n\n");
    fprintf(stderr, "%s \n", progName);

    for (int i = 0; i < nbKeys; i++) {
        fprintf(stderr, "          -%s [%s:%s]\n", keys[i].name, keys[i].def, keys[i].val);
    }
    fprintf(stderr, "\n");
}


//! \private
//! Get positional parameters
static char **extractPositional(
    char **argv,
    key keys[],
    const int nbKeys,
    int pos,
    int *npos,
    int debut,
    int *erreur
) {
    // cette fonction recupere les parametres positionels (c.a.d. ceux qui sont associes a une clef ayant pour nom '-'.

    // Lorsque debut est 1, on recupere les parametres jusqu'a la rencontre d'un nom de clef et on retourne
    // la position du prochain argument contenu dans argv.

    // Lorsque debut est 0, on recupere les parametres positionels qui suivent la sequence '--'.
    // On recupere alors jusqu'a epuisement de argv.

    // La fonction retourne dans npos le total cumulatif des arguments positionels traites
    // et retourne dans erreur le cumul des erreurs rencontrees

    static int posc;

    if (debut) {
        posc = pos;
    }

    while (*argv) {
        if ((**argv == '-') && (debut)) {
            return argv;
        }
        if (posc >= nbKeys) {
            argv++;
            (*erreur)++;
            return argv;
        }
        if (strcmp(keys[posc].name, keys[pos].name) == 0) {
            strcpy(keys[posc].final, *argv);
        } else {
            (*erreur)++;
        }
        posc++;
        (*npos)++;
        argv++;
    }
    return argv;
}


//! Retrieve command line parameters
void c_ccard(
    //! [in] Array of command line arguments
    char **argv,
    //! [in] Number of command line arguments
    const int argc,
    //! [in] key names
    char **names,
    //! [in, out] Key values
    char vals[][CCARD_NCARMAX],
    //! [in] Default values
    char ** const defs,
    //! [in] Number of keys
    const int nbKeys,
    //! [in, out]
    //! If greater than 0, it will contain the number of positional arguments after to function call.
    //! If it's -111, the program will be terminated when an error is encountered
    int * const npos
) {
    // pour chacune des clefs, on reserve une structure dans laquelle
    // on conserve son nom et son type (i.e., convertir a MAJUScule, MINUScule ou pas-touche),
    // les valeurs de defauts vals et defs ainsi que la valeur finale attribuee a la clef.
    // C'est le contenu de final qui est retourne.

    if (nbKeys > CCARD_NKLEMAX) {
        fprintf(stderr, "\n ***ERREUR - MAXIMUM DE CLES DEPASSE! \n");
        exit(-1);
    }

    key keys[CCARD_NKLEMAX];
    for (int i = 0; i < nbKeys; i++) {
        keys[i].name = calloc(strlen(names[i]) + 1, sizeof(char));
        keys[i].def = calloc(strlen(defs[i]) + 1, sizeof(char));
        keys[i].val = calloc(CCARD_NCARMAX + 1, sizeof(char));
        keys[i].final = calloc(CCARD_NCARMAX + 1, sizeof(char));
    }

    int exitOnError = 0;
    if (*npos == -111) {
        exitOnError = 1;
    }

    int returnNpos = 0;
    if (*npos > 0) {
        *npos = 0;
        returnNpos = 1;
    }

    for (int i = 0; i < nbKeys; i++) {
        convert2Upper(names[i], keys[i].name);
        keys[i].type = getKeyCase(keys[i].name);
        strcpy(keys[i].def, defs[i]);
        strcpy(keys[i].val, vals[i]);
        strcpy(keys[i].final, vals[i]);
#ifndef NDEBUG
        printf("names[%d] = '%s', keys[%d].name = '%s'\n", i, names[i], i, keys[i].name);
#endif
    }

    // obtenir le nom du programme
    const char * const progName = *argv;
    argv++;

    if (argc == 2) {
        if ((strcmp(*argv, "-h") == 0) || (strcmp(*argv, "-H") == 0)) {
            printCmdlineRef(keys, progName, nbKeys);
            exit(0);
        }
    }

    // verifier si on peut recuperer en mode positionel
    // chercher la premiere clef '-' dans la liste
    int posmoin = getKeyIdx("-", keys, nbKeys);

    int npos_interne = -1;
    int erreur = 0;

    // recuperer les premiers arguments positionels
    if (posmoin > -1) {
        argv = extractPositional(argv, keys, nbKeys, posmoin, &npos_interne, 1, &erreur);
        if (erreur) {
            fprintf(stderr, "\n ***ERREUR - TROP D'ARGUMENTS POSITIONELS \n");
            printCmdlineRef(keys, progName, nbKeys);
            if (exitOnError) exit(-1);
        }
    }

    int pos = -1;
    int posc = -1;
    while (*argv) {
        if (**argv == '-') {
            // un nom de clef
            if (*((*argv) + 1) == '-') {
                // fin des clefs
                // on recupere le reste des arguments positionels
                argv++;
                argv = extractPositional(argv, keys, nbKeys, posmoin, &npos_interne, 0, &erreur);
                if (erreur) {
                    fprintf(stderr, "\n ***ERREUR - TROP D'ARGUMENTS POSITIONELS \n");
                    printCmdlineRef(keys, progName, nbKeys);
                    if (exitOnError) exit(-1);
                }
                break;
            }

            char * const pointeur = (strchr(*argv, '='));
            if (pointeur != (char *) NULL) {
                *pointeur = '\0';
            }
            char * keyname = (*argv) + 1;
            pos = getKeyIdx(keyname, keys, nbKeys);
            if (pos > -1) {
                posc = pos;
                if (pointeur != (char *) NULL) {
                    *argv = pointeur + 1;
                    extractValues(keys, argv, pos, &posc);
                } else {
                    strcpy(keys[posc].final , keys[posc].def);
                }
            } else {
                fprintf(stderr, " ***ERREUR CLEF=%s INVALIDE***\n", keyname);
                printCmdlineRef(keys, progName, nbKeys);
                if (exitOnError) exit(-1);
            }
        } else {
            if ((posc != -1) && (strcmp(*(names + posc), *(names + pos)) == 0)) {
                extractValues(keys, argv, pos, &posc);
            } else {
                fprintf(stderr, "\n ***DEBORDEMENT DE LISTE \n");
                printCmdlineRef(keys, progName, nbKeys);
                if (exitOnError) exit(-1);
            }
        }

        argv++;
    } // while (*argv)

    // recopier les valeurs finales des clefs dans vals et liberer la memoire
    for (int i = 0; i < nbKeys; i++) {
        convertKeyCase(keys[i].final, keys[i].type);
        strcpy(vals[i], keys[i].final);
        free(keys[i].name);
        free(keys[i].def);
        free(keys[i].val);
        free(keys[i].final);
    }

    if (returnNpos) {
        (*npos) = npos_interne;
    }
}
