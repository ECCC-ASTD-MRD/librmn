#include <App.h>
#include <rmn/rpnmacros.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char **allocate_string_array(int ns)
{
    char **string_array = malloc((ns + 1) * sizeof(char *));
    for (int i = 0; i <= ns; i++) {
        string_array[i] = (char *) NULL;
    }

    return string_array;
}


void free_string_array(char **string_array)
{
    int i = 0;
    while (string_array[i]) {
        free(string_array[i]);
        i++;
    }
    free(string_array);
}


void free_cstring(char *cstring)
{
    free(cstring);
}


void cstring_to_fstring(const char *cstring, char *fstring, int nc)
{
    int i = 0;
    while ((*cstring) && (i < nc)) {
        *fstring = *cstring;
        fstring++;
        cstring++;
        i++;
    }

    for (int j = i; j < nc; j++) {
        *fstring = ' ';
        fstring++;
    }
}


char *fstring_to_cstring(char *fstring, const int nc, int rmblanks)
{
    int i;
    char *cstring, *ctemp;

    cstring = malloc(nc+1);
    ctemp = cstring;
    for (i = 0; i < nc; i++) {
        *ctemp = *fstring;
        ctemp++;
        fstring++;
    }

    *ctemp = '\0';
    ctemp--;
    if (rmblanks) {
        i = nc;
        while ((i > 0) && (*ctemp == ' ')) {
            *ctemp = '\0';
            ctemp--;
        }
    }

    return cstring;
}


char **fill_string_array(char **string_array, char *farray, int nc, int ns, int rmblanks)
{
    for (int i = 0; i<ns; i++) {
        string_array[i] = fstring_to_cstring(farray, nc, rmblanks);
        farray += nc;
    }

    return string_array;
}


void f77name(fs_to_cs)(char *fstring, int *rmblanks, int *ns, F2Cl fnc)
{
    if (*ns == 1) {
        char * cmpstring = malloc(13);
        char * cstring = fstring_to_cstring(fstring, fnc, *rmblanks);
        Lib_Log(APP_LIBFST,APP_DEBUG,"%f: cstring-->%s<--\n",__func__,cstring);
        strcpy(cmpstring, "Label01");
        Lib_Log(APP_LIBFST,APP_DEBUG,"%f: cmpstring-->%s<--\n",__func__,cmpstring);
        Lib_Log(APP_LIBFST,APP_DEBUG,"%f: strncmp sans blancs=%d\n",__func__,strncmp(cstring, cmpstring, 13));
        strcpy(cmpstring, "Label01     ");
        Lib_Log(APP_LIBFST,APP_DEBUG,"%f: cmpstring-->%s<--\n",__func__,cmpstring);
        Lib_Log(APP_LIBFST,APP_DEBUG,"%f: strncmp sans blancs=%d\n",__func__,strncmp(cstring, cmpstring, 13));
        free(cmpstring);
    } else {
        fill_string_array(allocate_string_array(*ns), fstring, fnc, *ns, *rmblanks);
    }
}

void f77name(cs_to_fs)(char *fstring, int *nc)
{
    char * cstring = malloc(12);
    strcpy(cstring, "Test001");

    cstring_to_fstring(cstring, fstring, *nc);

    free(cstring);
}
