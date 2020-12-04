#define PASTE(a,b) a##b
#define CAT(a,b) PASTE(a,b)

char CAT(PROJECT_NAME,_version)[] = VERSION;
