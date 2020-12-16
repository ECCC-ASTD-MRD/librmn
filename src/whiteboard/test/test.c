#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <WhiteBoard.h>
#include "WhiteBoard_proto.h"

#ifndef NULL
#define NULL ((char *)0)
#endif

typedef struct {
    char name[WB_MAXNAMELENGTH + 1];
    char type;
    int size;
    //! [in] Number of elements (0 means a scalar) (1 or more means an array)
    int nbelem;
    //! [in] Numeric representation of active flags
    int options;
} TestParams;


int main (int argc, char **argv) {
    TestParams testParams[] = {
        {"model/Hgrid/is_yinyang", 4, 1, 0, 0x108},
        {"model/Hgrid/yysubgrid", 3, 12, 0, 0x108},
        {"sfc/icelac", 4, 1, 0, 0x108},
        {"sfc/nclass", 2, 4, 0, 0x108},
        {"sfc/nicelvl", 2, 4, 0, 0x108},
        {"sfc/nsurf", 2, 4, 0, 0x108},
        {"sfc/schmsol", 3, 16, 0, 0x108},
        {"sfc/schmurb", 3, 16, 0, 0x108},
        {"sfc/snoalb_anl", 4, 1, 0, 0x108},
        {"model/series/P_serg_srsus_L", 4, 1, 0, 0x108},
        {"model/Vgrid/size-hybm", 2, 4, 0, 0x108},
        {"model/Vgrid/size-hybt", 2, 4, 0, 0x108},
        {"model/Vgrid/hybm", 1, 4, 81, 0x8},
        {"model/Vgrid/hybt", 1, 4, 81, 0x8},
        {"model/Vgrid/am", 1, 8, 81, 0x108},
        {"model/Vgrid/bm", 1, 8, 81, 0x108},
        {"model/Vgrid/at", 1, 8, 81, 0x108},
        {"model/Vgrid/bt", 1, 8, 81, 0x108},
        {"model/Vgrid/rcoef", 1, 4, 4, 0x108},
        {"model/Vgrid/ptop", 1, 8, 0, 0x108},
        {"model/Vgrid/pref", 1, 8, 0, 0x108},
        {"model/Vgrid/vcode", 2, 4, 0, 0x108},
        {"model/outout/pe_master", 2, 4, 0, 0x108},
        {"model/l_minx", 2, 4, 0, 0x108},
        {"model/l_maxx", 2, 4, 0, 0x108},
        {"model/l_miny", 2, 4, 0, 0x108},
        {"model/l_maxy", 2, 4, 0, 0x108},
        {"HGWB/model/Hgrid/glbphy", 2, 4, 10, 0x400},
        {"HGWB/model/Hgrid/glbphycore", 2, 4, 10, 0x400},
        {"HGWB/model/Hgrid/global", 2, 4, 10, 0x400},
        {"HGWB/model/Hgrid/local", 2, 4, 10, 0x400},
        {"HGWB/model/Hgrid/lclphy", 2, 4, 10, 0x400},
        {"model/Hgrid/hgcrot", 2, 4, 4, 0x108},
        {"model/Output/etik", 3, 12, 0, 0x108},
        {"ens/PTP", 4, 1, 0, 0x400},
        {"itf_phy/PHYOUT", 3, 32, 46, 0x800},
        {"itf_phy/TLIFT", 2, 4, 0, 0x800},
        {"itf_phy/DYNOUT", 4, 1, 0, 0x800},
        {"itf_phy/slt_winds", 4, 1, 0, 0x800},
        {"ptopo/ndomains", 2, 4, 0, 0x800},
        {"ptopo/idomain", 2, 4, 0, 0x800},
        {"ptopo/ngrids", 2, 4, 0, 0x800},
        {"ptopo/igrid", 2, 4, 0, 0x800}
    };
    int isGood = 0;


    for (int i = 0; i < sizeof(testParams)/sizeof(testParams[0]); i++) {
        int bufferSize = testParams[i].size * testParams[i].nbelem > 0 ? testParams[i].nbelem : 1;
        unsigned char buffer[bufferSize];
        for (int j = 0; j < bufferSize; j++) {
            buffer[j] = '=';
        }

        // printf("Name = '%s', len = %d\n", testParams[i].name, strlen(testParams[i].name));
        int status = c_wb_put(
            NULL,
            testParams[i].name,
            testParams[i].type,
            testParams[i].size,
            buffer,
            testParams[i].nbelem,
            testParams[i].options,
            strlen(testParams[i].name)
        );
        if (!WB_IS_OK(status)) {
            printf("Status = %d\n", status);
        }
    }

    for (int i = 0; i < sizeof(testParams)/sizeof(testParams[0]); i++) {
        int bufferSize = testParams[i].size * testParams[i].nbelem > 0 ? testParams[i].nbelem : 1;
        unsigned char buffer[bufferSize];
        for (int j = 0; j < bufferSize; j++) {
            buffer[j] = 0;
        }

        int status = c_wb_get(
            NULL,
            testParams[i].name,
            testParams[i].type,
            testParams[i].size,
            buffer,
            testParams[i].nbelem,
            strlen(testParams[i].name)
        );
        if (!WB_IS_OK(status)) {
            printf("Status = %d\n", status);
        }

        for (int j = 0; j < bufferSize; j++) {
            if (buffer[j] != 255) {
                isGood = 1;
                printf("Did not retrive expected value for %s! Got %d at %d!\n", testParams[i].name, buffer[j], j);
            }
        }
    }

    return isGood;
}