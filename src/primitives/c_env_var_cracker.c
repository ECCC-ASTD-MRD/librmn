#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <ctype.h>

#include <rmn/rpnmacros.h>

#define MAXSTRINGS 32
#define MAXLINELENGTH 255

#define NONMATCHING -1

#define QUOTES     50
#define NOQUOTES  -2

#define _cptofcd(a,b) (a)


static void call_user_function(
    char *name,
    int index,
    char *value,
    const char * const lang,
    void (*user_function)()
) {
    if (strcmp(lang, "C") == 0)  {
        /* option: user function in C */
#ifdef DEBUG
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: Call user C function\n",__func__);
#endif
        user_function(name, index, value);
    } else if (strcmp(lang, "F") == 0)  {
#ifdef DEBUG
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: Call user Fortran function\n",__func__);
#endif
        /* option: user function in Fortran */
        char * fp1 = _cptofcd(name, strlen(name));
        char * fp2 = _cptofcd(value, strlen(value));
        user_function(fp1, &index, fp2, strlen(fp1), strlen(fp2));
    }
}


// Convert string to upper case
static void convert_toupper(char *string)
{
    for(char * key = string; *key != '\0'; key++) {
        *key = toupper(*key);
    }
}


// Strip all leading whitesapce from a string
static void trimleft(char *string)
{
    char *string_dest;

    if (string != NULL) {
        string_dest = string;
        while (*string != '\0' && isspace(*string)) {
            string++;
        }
        while (*string != '\0') {
            *string_dest++ = *string++;
        }
        *string_dest = '\0';
    }
}


// Strip all trailing whitesapce from a string
static void trimright(char *string) {
    char *string_dest;

    if (string != NULL && string[0] != '\0') {
        string_dest = string + strlen(string) - 1;
        while (isspace(*string_dest)) {
            *string_dest-- = '\0';
        }
    }
}


static void remove_quotes(char * string) {
    for (int counter = 0; counter < strlen(string) - 2; counter++) {
        string[counter] = string[counter + 1 ];
    }

    string[strlen(string) - 2] = '\0';
}


// Check values for matching/non matching quotes parenthesis
int check_start_end_char(char *string, int length)
{
    int space_count = 0;

    // count leading whitespaces in string
    while (isspace(string[space_count])) {
        space_count++;
    }

    if (string[space_count] == '[') {
        if (string[length - 1] == ']') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[length - 1] == ']') {
        if (string[space_count] == '[') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[length - 1] == '(') {
        if (string[space_count] == ')') {
            return QUOTES;
        } else{
            return NONMATCHING;
        }
    }

    if (string[space_count] == '(' ) {
        if(string[length - 1] == ')') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[length - 1] == '}') {
        if (string[space_count] == '{') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[space_count] == '{') {
        if (string[length - 1] == '}') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[length - 1] == '\"') {
        if (string[space_count] == '\"') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[space_count] == '\"') {
        if (string[length - 1] == '\"') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[length - 1] == '\'') {
        if (string[space_count] == '\'') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (string[space_count] == '\'') {
        if (string[length - 1] == '\'') {
            return QUOTES;
        } else {
            return NONMATCHING;
        }
    }

    if (space_count >= 0) {
        return space_count;
    }

    return NOQUOTES;
}


void c_env_var_cracker(
    const char * const fstoption,
    void (*user_function)(),
    const char * const lang
) {
    char * delimiter1 = ";";
    char * delimiter2 = "=";
    char * delimiter3 = ", ";

    char strings[MAXSTRINGS][MAXLINELENGTH];

    char * option = getenv(fstoption);
    if (option == NULL) {
        return;
    }

    if (strlen(option = getenv(fstoption)) == 0) {
        return;
    }

#ifdef DEBUG
    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: option: %s\n",__func__,option);
#endif

    /* parse options (extract variable name and values) using ";" separator */
    char * token = strtok(option, delimiter1);

    int length = 0;
    strncpy(strings[length], token, strlen(token));
    strings[length][strlen(token)] = '\0';
    length++;


    char * name = (char *)malloc(MAXLINELENGTH);
    char * values = (char *)malloc(MAXLINELENGTH);
    char * value = (char *)malloc(MAXLINELENGTH);

    while ((token = strtok(0, delimiter1)) != 0) {
        strncpy(strings[length], token, strlen(token));
        strings[length][strlen(token)] = '\0';
        length++;
    }

    int index = 0;
    for (int i = 0; i < length; i++) {
        /* parse variable name /value using "=" separator */
        token = strtok(strings[i], delimiter2);
        strncpy(name, token, strlen(token));

        name[strlen(token)] = '\0';

        /* convert key to upper case */
        convert_toupper(name);
        /* remove leading whitespace */
        trimleft(name);
        /* remove trailing whitespace */
        trimright(name);

#ifdef DEBUG
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: name=%s\n",__func__,name);
#endif

        /* parse variable name and values using "=" separator */
        while((token = strtok(0, delimiter2)) != NULL) {
            trimleft(token);
            if (strlen(token) == 0) {
                continue;
            }

            strncpy(values, token, strlen(token));
            values[strlen(token)] = '\0';

#ifdef DEBUG
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: Values=%s\n",__func__,values);
#endif

            /* remove leading whitespace */
            trimleft(values);
            /* remove trailing whitespace */
            trimright(values);

            /* check opening and closing parenthesis/quotes */
            int result = check_start_end_char(values, strlen(values));

            if (result == NONMATCHING) {
                return;
            }

            if (result >= QUOTES) {
                /* remove quotes */
                remove_quotes(values);
            }

#ifdef DEBUG
            Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: Value(s) without white space and parentheses: %s, result = %d \n",__func__,values,result);
#endif
            index++;

            if (result >= QUOTES && strchr(values, ',') == NULL) {
                call_user_function(name, index, values, lang, user_function);
            } else  {
                /* case: multiple values per variable */
                index = 1;

                /* parse values using "," separator */
                token = strtok(values, delimiter3);
                strncpy(value, token, strlen(token));
                value[strlen(token)] = '\0';

#ifdef DEBUG
                Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: Value, without white space and parentheses: %s, token = %s\n",__func__,value,token);
#endif

                /* remove leading whitespace */
                trimleft(value);
                /* remove trailing whitespace */
                trimright(value);

                call_user_function(name, index, value, lang, user_function);

                while ((token = strtok(0, delimiter3)) != 0) {
                    index++;
                    strncpy(value, token, strlen(token));
                    value[strlen(token)] = '\0';

                    /* remove leading whitespace */
                    trimleft(value);
                    /* remove trailing whitespace */
                    trimright(value);

                    call_user_function(name, index, value, lang, user_function);
                }
            }
        }
    }

    if (name) {
        free(name);
    }
    if (values) {
        free(values);
    }
    if (value) {
        free(value);
    }
}
