#ifndef FC_STRING_H
#define FC_STRING_H

char **allocate_string_array(int ns);
void free_string_array(char **string_array);
void free_cstring(char *cstring);
void cstring_to_fstring(const char *cstring, char *fstring, int nc);
char *fstring_to_cstring(char *fstring, const int nc, int rmblanks);
char **fill_string_array(char **string_array, char *farray, int nc, int ns, int rmblanks);
void f77name(fs_to_cs)(char *fstring, int *rmblanks, int *ns, F2Cl fnc);
void f77name(cs_to_fs)(char *fstring, int *nc);

#endif
