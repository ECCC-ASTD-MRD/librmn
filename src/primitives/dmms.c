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

/************************************************************************
 *    D y n a m i c _ M e m o r y _ M a n a g e m e n t _ S y s t e m   *
 ************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <rpnmacros.h>

#define ERROR 0
#define STACK 0
#define HEAP  1

struct blocmem {
    struct blocmem *fwd;
    struct blocmem *bwd;
    int *data[4];
};

static struct blocmem stack_first, stack_last, heap_first, heap_last;
static struct blocmem *badptr;

static int init = 0, initmem = 0;

static int32_t con;

static int ptrsize, *pointer, debug_mode=0, dejala=0, dmms_noabort=0;

#define single() {\
  if (dejala) {\
    fprintf(stderr," * * * ERROR * * *: more than one task in dmms\n");\
    f77name(tracebck)();\
    exit(50);\
    }\
  else dejala=1;\
}

#define sortie() {\
  dejala=0;\
}


/************************************************************************
 *                      b l o c _ a l l o c                             *
 ************************************************************************/

/**
 *
 *auteur   M. Lepine - fev 92
 *
 *objet(bloc_alloc)
 *
 *     allocation d'un bloc de memoire (heap ou stack ) avec
 *     pointeurs avant et arriere et double pointeurs en boucle comme
 *     mecanisme de validation
 *
 *         ___________________   __________________>
 *        |  ________         | |  ________
 *        | |        |        | | |        |
 *     ----------------      ----------------
 *     |*|*|*| data |*|      |*|*|*| data |*|
 *     ----------------      ----------------
 *  <___|   |________|        |   |________|
 *                            |
 *      |_____________________|
 *
 **/

struct blocmem *bloc_alloc(int nbytes, int mode) {
    struct blocmem *ptbloc;
    unsigned int errptr;
    int lng, nitem, n;
    char *value, *getenv();

    single();
    ptrsize = sizeof(pointer);

    nitem = (nbytes + ptrsize - 1) / ptrsize;
    lng = sizeof(struct blocmem) + (nitem * ptrsize);

    ptbloc = (struct blocmem *) malloc(lng);
    if (ptbloc == NULL) {
        if (dmms_noabort) {
            return (struct blocmem *) NULL;
        } else {
            perror("bloc_alloc error can't allocate");
            fprintf(stderr,"bloc_alloc trying to allocate lng=%d bytes\n",lng);
            f77name(tracebck)();
            exit(7);
        }
    }

    if (! init) {
        heap_first.bwd = (struct blocmem *) NULL;
        heap_first.fwd = &heap_last;
        heap_last.bwd = &heap_first;
        heap_last.fwd = (struct blocmem *) NULL;

        stack_first.bwd = (struct blocmem *) NULL;
        stack_first.fwd = &stack_last;
        stack_last.bwd = &stack_first;
        stack_last.fwd = (struct blocmem *) NULL;

        heap_first.data[0] = (int *) &(heap_first.data[1]);
        heap_first.data[1] = (int *) &(heap_first.data[0]);
        heap_last.data[0] = (int *) &(heap_last.data[1]);
        heap_last.data[1] = (int *) &(heap_last.data[0]);

        stack_first.data[0] = (int *) &(stack_first.data[1]);
        stack_first.data[1] = (int *) &(stack_first.data[0]);
        stack_last.data[0] = (int *) &(stack_last.data[1]);
        stack_last.data[1] = (int *) &(stack_last.data[0]);

        value = getenv("BAD_POINTER");
        if (value != NULL) {
            n = sscanf(value,"%x", &errptr);
            badptr = (struct blocmem *) errptr;
            fprintf(stderr,"Debug bad_pointer to look for is %#p\n", badptr);
        } else {
            badptr = (struct blocmem *) 0;
        }

        initmem = 0;
        value = getenv("INITMEM");
        if (value != NULL) {
            initmem = 1;
            if (strcmp(value,"ON") == 0) {
                con = 0xFFFA5A5A;
            } else {
                n = sscanf(value,"%x",&con);
            }
        }

        value = getenv("DEBUG_MODE");
        debug_mode = ((value != NULL) && (strcmp(value,"OFF") != 0) && (strcmp(value,"0") != 0));
        init = 1;
        if (debug_mode) {
            fprintf(stdout,"DEBUG_MODE %s\n",value);
            fprintf(stdout,"Debug &heap_first =%#p\n", &heap_first);
            fprintf(stdout,"Debug &heap_last =%#p\n", &heap_last);
            fprintf(stdout,"Debug &stack_first =%#p\n", &stack_first);
            fprintf(stdout,"Debug &stack_last =%#p\n", &stack_last);
        }
    }

    if (badptr != (struct blocmem *) 0) {
        if (badptr ==  ptbloc) {
            fprintf(stderr,"bloc_alloc bad_pointer %#x\n",ptbloc);
            f77name(tracebck)();
            exit(10);
        }
    }

    if (mode == HEAP) {
        ptbloc->bwd = heap_last.bwd;
        ptbloc->fwd = &heap_last;
        heap_last.bwd = ptbloc;
        (ptbloc->bwd)->fwd = ptbloc;
    } else {
        ptbloc->bwd = stack_last.bwd;
        ptbloc->fwd = &stack_last;
        stack_last.bwd = ptbloc;
        (ptbloc->bwd)->fwd = ptbloc;
    }

    ptbloc->data[0] = (int *) &(ptbloc->data[nitem+1]);
    ptbloc->data[nitem+1] = (int *) &(ptbloc->data[0]);
    if (debug_mode) {
        fprintf(stdout, "\n");
        fprintf(stdout, "Debug alloc_bloc nitem = %d\n", nitem);
        fprintf(stdout, "Debug alloc_bloc lng = %d\n", lng);
        fprintf(stdout, "Debug alloc_bloc ptbloc =%#p\n", ptbloc);
        fprintf(stdout, "Debug alloc_bloc ptbloc->bwd =%#p\n", ptbloc->bwd);
        fprintf(stdout, "Debug alloc_bloc ptbloc->fwd =%#p\n", ptbloc->fwd);
        fprintf(stdout, "Debug alloc_bloc ptbloc->data[0] =%#p\n", ptbloc->data[0]);
        fprintf(stdout, "Debug alloc_bloc ptbloc->data[nitem+1] =%#p\n", ptbloc->data[nitem+1]);
    }

    if (initmem) {
        lng = (nitem-2) * ptrsize / sizeof(int32_t);
        f77name(afix)(&(ptbloc->data[2]),&con,&lng);
    }

    sortie();
    return ptbloc;
}


//! Check memory block pointers
int bloc_check(
    struct blocmem * ptbloc,
    int msg_level
) {
    if ((debug_mode) || (msg_level > 1)) {
        fprintf(stdout, "\n");
        fprintf(stdout, "Debug check ptbloc =%#p\n", ptbloc);
        fprintf(stdout, "Debug check ptbloc->bwd =%#p\n", ptbloc->bwd);
        fprintf(stdout, "Debug check ptbloc->fwd =%#p\n", ptbloc->fwd);
    }

    if (ptbloc->bwd == NULL) {
        fprintf(stderr, "block_check error: NULL backward pointer ptbloc=%#p\n", ptbloc);
        return -1;
    }

    if (ptbloc->fwd == NULL) {
        fprintf(stderr, "block_check error: NULL forward pointer ptbloc=%#p\n", ptbloc);
        return -2;
    }

    int ** pt = (int **) ptbloc->data[0];

    if ((debug_mode) || (msg_level > 1)) {
        fprintf(stdout, "Debug check ptbloc->data[0] =%#p\n", ptbloc->data[0]);
        fprintf(stdout, "Debug check ptbloc->data[nitem+1] =%#p\n", *pt);
    }

    if (*pt != (int *) &(ptbloc->data[0])) {
        fprintf(stderr, "block_check error: internal pointers destroyed ptbloc=%#p\n", ptbloc);
        return -3;
    }

    if (msg_level > ERROR) fprintf(stderr,"block_check OK \n");

    return 0;
}


//! Deallocate memory block and check pointers
int bloc_dealloc(
    struct blocmem * ptbloc,
    int mode
){
    single();
    if (debug_mode) {
        fprintf(stdout, "\n");
        fprintf(stdout, "Debug bloc_dealloc ptbloc =%#p\n", ptbloc);
    }
    if (mode == HEAP) {
        int err = bloc_check(ptbloc, 0);
        if (err < 0) {
            f77name(tracebck)();
            exit(12);
        }
        (ptbloc->bwd)->fwd = ptbloc->fwd;
        (ptbloc->fwd)->bwd = ptbloc->bwd;
        free(ptbloc);
        sortie();
        return 0;
    } else {
        struct blocmem * pt = ptbloc;
        stack_last.bwd = ptbloc->bwd;
        (ptbloc->bwd)->fwd = &stack_last;
        while (pt != &stack_last) {
            int err = bloc_check(pt, 0);
            if (err < 0) {
                f77name(tracebck)();
                exit(14);
            }
            pt = ptbloc->fwd;
            free(ptbloc);
            ptbloc = pt;
        }
    }
    sortie();
    return 0;
}


//! Validate pointers in memory bloc list (heap and stack)
int mem_check(int mode, int msg_level) {
    struct blocmem *ptbloc;
    if (mode == HEAP) {
        ptbloc = heap_first.fwd;
    } else {
        ptbloc = stack_first.fwd;
    }
    while (ptbloc->fwd != (struct blocmem *) NULL) {
        int err = bloc_check(ptbloc,msg_level);
        if (err < 0) {
            return err;
        }
        ptbloc = ptbloc->fwd;
    }

    return 0;
}


//! Check integrity of blocs in memory bloc list (heap and stack)
int f77name(memoirc)(
    int32_t * msg_level
) {
    if (! init) return 0;

    if (stack_first.fwd != &stack_last) {
        fprintf(stderr,"memoirc warning: stack not empty \n");
    }
    if (*msg_level > 1) {
        fprintf(stdout,"Debug &heap_first =%#p\n",&heap_first);
        fprintf(stdout,"Debug &heap_last =%#p\n",&heap_last);
        fprintf(stdout,"Debug &stack_first =%#p\n",&stack_first);
        fprintf(stdout,"Debug &stack_last =%#p\n",&stack_last);
    }
    int errh = mem_check(HEAP,*msg_level);
    int errs = mem_check(STACK,*msg_level);
    return (errh !=0) ? errh : (errs != 0) ? errs : 0;
}


//! Enable or disable debug messages
void f77name(dmmsdbg)(int32_t * dbgr) {
    debug_mode = (*dbgr == 1) ? 1 : 0;
}


//! Enable or disable abort mode when out of memory
void f77name(dmmsnabt)(int32_t * abort) {
    dmms_noabort = (*abort == 1) ? 1 : 0;
}


void f77name(hpalloc)(int32_t ** addr, int32_t * length, int32_t * errcode, int32_t * abort) {
    if (*length == 0) {
        fprintf(stderr,"HPALLOC error: 0 length\n");
        f77name(tracebck)();
        exit(13);
    }
    struct blocmem * ptbloc = bloc_alloc(8 + *length * sizeof(int32_t) * ((*abort==8) ? 2 : 1),HEAP);
    *addr =  (void *) &(ptbloc->data[2]);
    *errcode = (ptbloc == (struct blocmem *) NULL) ? 1 : 0;
}


void f77name(hpdeallc)(int32_t ** addr, int32_t * errcode, int32_t * abort) {
    int offset=4*sizeof(addr);
    *errcode = bloc_dealloc((*addr)-offset,HEAP);
}


//! Cache aligned allocation
void f77name(ca_alloc)(void **addr, int32_t *length, int32_t *errcode, int32_t *abort, int32_t *fpw2) {
#if defined (AIX)
    int alignment[3] = {128, 128, 512};
#else
    int alignment[3] = {32, 32, 32};
#endif

    if (*length == 0) {
        fprintf(stderr,"CA_ALLOC error: 0 length\n");
        f77name(tracebck)();
        exit(13);
    }

    int nbytes;
    int pw2 = *fpw2;
    if (pw2 < 0) {
        pw2 = -pw2;
        if ((pw2 < 1) || (pw2 > 3)) {
            fprintf(stderr,"ca_alloc wrong value for alignment:%d\n",-pw2);
            exit(33);
        }
        nbytes = alignment[pw2-1];
    } else {
        nbytes = 1 << pw2;
    }
    struct blocmem * ptbloc = bloc_alloc(nbytes + 8 + *length * sizeof(int32_t) * ((*abort==8) ? 2 : 1),HEAP);
    int ** pt_data1 = &(ptbloc->data[1]);
    int ** pt_aligned = &(ptbloc->data[2]) + (nbytes / sizeof(pt_aligned));
    pt_aligned = (void *) (( (intptr_t)pt_aligned) >> pw2);
    pt_aligned = (void *) (( (intptr_t)pt_aligned) << pw2);
    int ajout =  pt_aligned -  pt_data1;
    ptbloc->data[1] = (int *) ptbloc;
    for (int i=0; i <= ajout; i++) {
        ptbloc->data[1+i] = (int *) ptbloc;
    }
    *addr =  (void *) pt_aligned;
    *errcode = (ptbloc == (struct blocmem *) NULL) ? 1 : 0;
}


void f77name(ca_deallc)(int32_t ** addr, int32_t * errcode, int32_t * abort) {
   int **ptr;
   ptr = *addr - sizeof(addr);
   *errcode = bloc_dealloc(*ptr,HEAP);
}


#if defined (OLD_32BIT_CODE)
void f77name(memoirh)(int32_t buf[], int32_t * ind, int32_t * nw){
    int errcode, **ptr;
    struct blocmem *ptbloc;
    int32_t *adr1;

#if !defined (OLD_32BIT_CODE)
    fprintf(stderr, "****************************************************\n");
    fprintf(stderr, "* ERROR: MEMOIRH                                   *\n");
    fprintf(stderr, "* This code is obsolete, will not work on a 64     *\n");
    fprintf(stderr, "* bit architecture and should not be used.         *\n");
    fprintf(stderr, "* allocate shoud be used instead                   *\n");
    fprintf(stderr, "* EXITING                                          *\n");
    fprintf(stderr, "****************************************************\n");
    exit(33);
#else

    if (*nw > 0) {
        ptbloc = bloc_alloc(8 + *nw * sizeof(int32_t),HEAP);
        adr1 = (int32_t *) &(ptbloc->data[2]);
        *ind = (adr1 - buf) + 1;
    } else {
        ptr = (int **) &buf[*ind - 1];
        errcode = bloc_dealloc(ptr-4,HEAP);
    }
#endif
}


void f77name(memoir)(int32_t buf[], int32_t * ind, int32_t * nw) {
    int  errcode, **ptr;
    struct blocmem *ptbloc;
    int32_t *adr1;
#if !defined (OLD_32BIT_CODE)
    fprintf(stderr, "****************************************************\n");
    fprintf(stderr, "* ERROR: MEMOIR                                    *\n");
    fprintf(stderr, "* This code is obsolete, will not work on a 64     *\n");
    fprintf(stderr, "* bit architecture and should not be used.         *\n");
    fprintf(stderr, "* allocate shoud be used instead                   *\n");
    fprintf(stderr, "* EXITING                                          *\n");
    fprintf(stderr, "****************************************************\n");
    exit(44);
#else

    if (*nw > 0) {
        ptbloc = bloc_alloc(8 + *nw * sizeof(int32_t),STACK);
        adr1 = (int32_t *) &(ptbloc->data[2]);
        *ind = (adr1 - buf) + 1;
    } else {
        ptr = (int **) &buf[*ind - 1];
        errcode = bloc_dealloc(ptr-4,STACK);
    }
#endif
}
#endif


void f77name(bkcheck)(int32_t ** addr, int32_t * errcode) {
    *errcode = bloc_check((*addr)-4, 1);
}


void f77name(hpcheck)(int32_t * errcode) {
    if (*errcode == 0) {
        *errcode = mem_check(HEAP, 0);
    } else {
        *errcode = mem_check(HEAP, 1);
    }
}


void f77name(mcheck)(int * errcode){
    if (*errcode == 0) {
        *errcode = mem_check(STACK, 0);
    } else {
        *errcode = mem_check(STACK, 1);
    }
}
