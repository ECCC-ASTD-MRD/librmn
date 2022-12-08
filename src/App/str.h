#define strend(S) (S+strlen(S))

char* strpath(char *Path,char *File);
char* strcatalloc(char *StrTo,char *StrFrom);
void  strtrim(char* Str,char Tok);
void  strrep(char *Str,char Tok,char Rep);
void  strblank2end(char *Str,int Length);
int   strtok_count(char *Str,char Sep);
int   strrindex(char *Str);
int   strmatch(const char *Str,char *Pattern);

