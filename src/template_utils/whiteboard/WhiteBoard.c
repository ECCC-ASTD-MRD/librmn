/* RMNLIB - Library of useful routines for C and FORTRAN programming 
 * Copyright (C) 1975-2005  Environnement Canada
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
#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <rpnmacros.h>

#define C_SOURCE_CODE
#include <WhiteBoard.h>

//! Find trimmed (non blank) length of a Fortran string
//
//! @param[in]     string String of which we want to find the trimmed length
//! @param[in,out] length Must be set to the maximum possible length on call and will be updated to the trimmed length
#define TRIM(string,length) { while( length > 0 && string[length - 1] == ' ' ) length-- ; }


//! Verbosity level
static int message_level = WB_MSG_WARN;

//! Set verbosity level (C callable)
//
//! @param[in] level New verbosity level
//
//! @return The previously let level
int c_wb_verbosity(int level)
{
    int old_level = message_level;
    message_level = level;
    return(old_level);
}


//! Set verbosity level (FORTRAN callable)
wordint f77_name(f_wb_verbosity)(wordint *level)
{
   wordint old_level = message_level;
   message_level = *level;
   return(old_level);
}

//! Line buffer used to process directive files
static char *linebuffer = NULL;
//! Pointer to current character in directive file
static char *current_char = NULL;

//! pointer to defionition table
static DEFTBL *definition_table = NULL;
//! Number of valid entries in definition table
static int definition_table_entries = 0;
static int max_definition_table_entries = 0;


//! Manage definition_table (used by whiteboard read routine)
//
//! @param[in] line Line to process
//! @param[in] define_mode Processing mode for the line: 0 for a simple define, 1 key = value in strict dictionary mode, 2 key = value in normal mode
//
//! @return Position in the defintion table
static int wb_define_check(LINE *line, int define_mode)
{
    int i;

    for (i = 0; i < definition_table_entries; i++) {
        if (definition_table[i].line == line) {
            // A match has been found

            if (define_mode == 1) {
                // Found when it should not (we are processing a define)
                return(WB_ERR_REDEFINE);
            }

            if ( definition_table[i].assigned ) {
                // Already assigned to, should not be assigned again
                if (message_level <= WB_MSG_ERROR) {
                    fprintf(stderr, "ERROR: key value already assigned in this directive file \n");
                }
                return(WB_ERR_REDEFINE);
            }
            definition_table[i].assigned = 1 ;    /* flag assignation */

            return(i);
         }
    }
    if (define_mode == 0) {
        // Not in define mode, strict dictionary mode, not found, fail
        return (WB_ERR_NOTFOUND);
    }

    if (definition_table_entries >= max_definition_table_entries) {
        // Definition table is full
        if (message_level <= WB_MSG_ERROR) {
            fprintf(stderr, "ERROR: too many keys appear in this directive file \n");
        }
        return (WB_ERROR);
    }

    // Create and initialize entry
    definition_table[definition_table_entries].line = line;
    // define(...)
    definition_table[definition_table_entries].defined = define_mode == 1 ? 1 : 0;
    // key=...
    definition_table[definition_table_entries].assigned = define_mode != 1 ? 1 : 0;
    // Return position
    return(definition_table_entries++);
}


//! Default options when creating variables (used by Whiteboard read)
static int default_dict_option = 0;

//! Table of variable creation options (used by Whiteboard read)
static SYMTAB dict_options[] = {
    {WB_IS_LOCAL, "WB_IS_LOCAL"},
    {WB_REWRITE_AT_RESTART, "WB_REWRITE_AT_RESTART"},
    {WB_REWRITE_NONE, "WB_REWRITE_NONE"},
    {WB_REWRITE_UNTIL, "WB_REWRITE_UNTIL"},
    {WB_REWRITE_MANY, "WB_REWRITE_MANY"},
    {WB_READ_ONLY_ON_RESTART, "WB_READ_ONLY_ON_RESTART"},
    {0, NULL}
} ;

//! Table for verbosity options (used by Whiteboard read)
static SYMTAB verb_options[] = {
    {WB_MSG_DEBUG, "WB_MSG_DEBUG"},
    {WB_MSG_INFO, "WB_MSG_INFO"},
    {WB_MSG_WARN, "WB_MSG_WARN"},
    {WB_MSG_ERROR, "WB_MSG_ERROR"},
    {WB_MSG_SEVERE, "WB_MSG_SEVERE"},
    {WB_MSG_FATAL, "WB_MSG_FATAL"},
    {0, NULL}
} ;

typedef PAGE *PAGEPTR;

//! Type for whiteboard instances
typedef struct{
   PAGE *PageChain;
   int validpages;
} WhiteBoard;
typedef WhiteBoard *WhiteBoardPtr;

//! dummy whiteboard instance, returned when freeing a whiteboard
static WhiteBoard DummyWhiteboard;
static WhiteBoardPtr DummyWhiteboardPtr = &DummyWhiteboard;
//! Permanent whiteboard instance, the only one that can be checkpointed
static WhiteBoard BaseWhiteboard;
static WhiteBoardPtr BaseWhiteboardPtr = &BaseWhiteboard;

static int new_page(WhiteBoard *WB, int nlines);

//! Create a new whiteboard instance
//
//! @return Pointer to the new WhiteBoard
WhiteBoard *c_wb_new() {
    int status;
    WhiteBoard *New_WhiteBoard = (WhiteBoard *)malloc(sizeof(WhiteBoard));

    if (New_WhiteBoard == NULL) {
        // Memory allocation failed
        return(NULL);
    }

    New_WhiteBoard->PageChain = NULL;
    New_WhiteBoard->validpages = 0;
    status = new_page(New_WhiteBoard, WB_MAXLINESPERPAGE);
    if (status < 0) return(NULL);
    return New_WhiteBoard;
}

wordint f77_name(f_wb_new)(WhiteBoard **WB){
   WhiteBoard *New_WhiteBoard = c_wb_new();
   *WB = New_WhiteBoard;
   return New_WhiteBoard == NULL ? WB_ERROR : WB_OK;
}


//! Delte a whiteboard instance
int c_wb_free(WhiteBoard *WB) {
    PAGE *temp, *temp2;

    if (WB == NULL) {
        // Global whiteboard page chain is never freed
        return(WB_OK);
    }

    if (WB-> validpages <= 0) {
        // Invalid whiteboard, negative or zero number of valid pages
        return(WB_ERROR);
    }
    WB->validpages = 0;
    temp = WB->PageChain;
    // Free all pages for this whiteboard
    while (temp != NULL) {
        // Keep copy of pointer to current page
        temp2 = temp;
        // Pointer to next page in page chain
        temp = temp->next;
        // Free current page
        free(temp2);
    }
    free(WB);
    return(WB_OK);
}

wordint f77_name(f_wb_free)(WhiteBoard **WB) {
    wordint status = c_wb_free(*WB);
    // Set whiteboard address to address of dummy whiteboard, so that we can trap it
    *WB = DummyWhiteboardPtr;
    return status;
}


//! Error message table
static SYMTAB errors[] = {
    {WB_ERR_NAMETOOLONG , "key name is too long"},
    {WB_ERR_NOTFOUND, "requested item not found"},
    {WB_ERR_READONLY, "key value cannot be redefined"},
    {WB_ERR_WRONGTYPE, "key object and value have different types/length combinations"},
    {WB_ERR_WRONGDIMENSION, "key object has dimensions smaller than value assigned"},
    {WB_ERR_WRONGSTRING, "target string element is too short"},
    {WB_ERR_ALLOC, "could not allocate memory with malloc"},
    {WB_ERR_NOTYPE, "requested type/length combination is not valid"},
    {WB_ERR_NOMEM, "not enough space available to allocate value"},
    {WB_ERR_NOVAL, "key has not been initialized yet"},
    {WB_ERR_BADVAL, "key value is dubious due to a failed put"},
    {WB_ERR_CKPT, "problem opening/reading/writing checkpoint file"},
    {WB_ERR_REDEFINE, "attempt to redefine an entry"},
    {WB_ERR_BIG, "token too large in directive"},
    {WB_ERR_SYNTAX, "syntax error in directive"},
    {WB_ERR_OPTION, "invalid option or combination of options"},
    {WB_ERR_READ, "open/read error in file"},
    {WB_ERROR, ""},
    {0, NULL}
} ;

// THIS MUST REMAIN CONSISTENT WITH Whiteboard.h
static char *datatypes[] = {
    "    ",
    "Real",
    "Int ",
    "Char",
    "Bool"
};


//! Get integer value associated with a string(symbol) from a symbol table
//
//! @param[in] symbol Symbol to search in the table
//! @param[in] table  Table pointer
//
//! @return Value of the symbol if found, 0 otherwise
static int wb_value(char *symbol, SYMTAB *table)
{
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "looking for value of '%s', ", symbol);
    }
    while (table->text != NULL) {
        if (strcmp(table->text, symbol) == 0) {
            if (message_level <= WB_MSG_DEBUG) {
                fprintf(stderr, "found %d\n", table->code);
            }
            return(table->code);
        }
        table++;
    }
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "found NONE\n");
    }
    // No value found in table
    return(0);
}

//! Default user error handler, do nothing
static void null_error_handler()
{
    return ;
}

//! Pointer to the user's FORTRAN handler routine, defaults to internal null_error_handler
//! The error handler receives two pointers to wordint variables
static void (*ErrorHandler)() = &null_error_handler;

//! Set the error handler
//
//! @param[in] UserErrorHandler Pointer to the error handler function
void f77_name(f_wb_error_handler)( void (*UserErrorHandler)() )
{
    ErrorHandler = UserErrorHandler;
}

static char text_of_last_error[WB_MAX_ETRA_ERROR_LEN];
static char *extra_error_message = NULL;
static char extra_error_buffer[WB_MAX_ETRA_ERROR_LEN];

//! Copy a possibly non null terminated string to the extra error buffer
//
//! @param[in] name String of the extra error message
//! @param[in] length Length of the extra error message
static void set_extra_error_message(char *name, int length) {
    if (length > WB_MAX_ETRA_ERROR_LEN - 1) {
        length = WB_MAX_ETRA_ERROR_LEN - 1;
    }
    extra_error_buffer[length] = '\0';
    while(length > 0) {
        extra_error_buffer[length - 1] = name[length - 1];
        length--;
    }
    extra_error_message = &(extra_error_buffer[0]);
}


//! Error handler
//
//! A message will be printed on standard error if severity is >= message level
//! Call the user's error handler, if defined
//
//! @param[in] severity Error level
//! @param[in] code     Error code
//
//! @return The error code specified when calling the function
static int wb_error(int severity, int code) {
    SYMTAB *message = &errors[0];
    wordint Severity = severity;
    wordint Code = code;

    // Do nothing if there is no error or it's below the severity treshold
    if (code == WB_OK || severity < message_level) {
        return(code);
    }
    while (message->text != NULL) {
        if (message->code == code) {
            snprintf(text_of_last_error, sizeof(text_of_last_error) - 1, "%s - %s",
                message->text, (extra_error_message != NULL) ? extra_error_message : "");
            fprintf(stderr, "ERROR: %s \n", text_of_last_error);
            extra_error_message = NULL;
            break;
        }
        message++;
    }
    // Call user's FORTRAN error handler
    (*ErrorHandler)(&Severity ,&Code);
    return(code);
}

//! Error exit macro
//! FIXME: Why is this here instead of in a header?
#define WB_ERR_EXIT(level,code)  return ( wb_error(level, code) )

//! Get the symbolic type code (WB_FORTRAN_...) and validate the type/length combination
//
//! Valid types and size combinations:
//! WB_FORTRAN_REAL must be 4 or 8 bytes long
//! WB_FORTRAN_INT  must be 4 or 8 bytes long
//! WB_FORTRAN_BOOL must be 4 bytes long
//! WB_FORTRAN_CHAR must be 1 to WB_MAXSTRINGLENGTH bytes long
//
//! @param[in] type_code Fortran type code
//! @param[in] size      Size of the type
//! @param[in] length    String length.  Allows excessive length on get operations
//
//! @return The symbolic type code if OK  (WB_FORTRAN_...)
static int get_typecode(unsigned char type_code, int size, int length){
    if ( type_code == WB_FORTRAN_REAL && (size == 4 || size == 8) ) {
        return WB_FORTRAN_REAL;
    }
    if ( type_code == WB_FORTRAN_INT  && (size == 4 || size == 8) ) {
        return WB_FORTRAN_INT;
    }
    if ( type_code == WB_FORTRAN_BOOL && size == 4 ) {
        return WB_FORTRAN_BOOL;
    }
    if ( type_code == WB_FORTRAN_CHAR && (size > 0 && length <= WB_MAXSTRINGLENGTH) ) {
        return WB_FORTRAN_CHAR;
    }
    // Invalid type / length combination
    WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTYPE);
}


//! Copy a string with C or Fortran style
//
//! The src string can be either null byte terminated (C style) or up to src_len bytes long (Fortran style).
//! Trailing blanks are ignored in both cases.
//! If the destination pointer is NULL, no copy takes place, only the trimmed source length is returned.
//
//! @param[in]  src     Source string
//! @param[out] dst     Destination string
//! @param[in]  src_len Source length including possible padding spaces
//! @param[in]  dst_len Destination length
//! @param[in]  pad     Padding character.  If this is null, no padding will be added.
//
//! @return Trimmed source length
static int c_fortran_string_copy(unsigned char *src, unsigned char *dst, int src_len, int dst_len, unsigned char pad)
{
    int i = 0;

    // If there is a null before src_len characters in the source string, act as if it was terminated at null
    while (src[i] != 0 && i < src_len) {
        i++;
    }
    src_len = i;

    // Ignore trailing blanks in source string
    while (src_len > 0 && src[src_len - 1] == ' ') {
        src_len--;
    }

    if (dst == NULL) {
        // No destination, just return trimmed source length
        return src_len;
    }

    if (src_len > dst_len) {
        // Source longer than destination!
        return -1;
    }

    if (pad == 0 && src_len == dst_len) {
        // Not enough space for C string termination!
        return -1;
    }

    // Copy src to dst
    memcpy(dst, src, src_len);

    if (pad) {
        // Pad dstination
        i = src_len;
        while (i < dst_len) {
            dst[i++] = pad;
        }
    } else {
        // Add the C string termination
        dst[src_len] = pad;
    }
    // Return number of significant characters copied
    return src_len;
}

#ifdef NOT_USED
/* FORTRAN callable version of above routine */
wordint f77_name(fortran_string_copy)(unsigned char *src, unsigned char *dst, F2Cl src_len, F2Cl dst_len)
{
   int Ldst = dst_len;
   int Lsrc = src_len;
   return(c_fortran_string_copy(src, dst, Lsrc, Ldst, ' '));
}
#endif

//! Initialize a line with a name coming from a WB_FORTRAN string
//
//! @param[out] line   Line instance pointer
//! @param[in]  name   Name
//! @param[in]  length Length of name
static void fill_line(LINE *line, unsigned char *name, int length)
{
    int pos;
    unsigned char c;

    // Fill to WB_MAXNAMELENGTH with blanks (FORTRAN style)
    for (pos = 0; pos < WB_MAXNAMELENGTH; pos++) {
        c = (pos < length) ? name[pos] : ' ';
        // Force uppercase
        line->m.name.c[pos] = toupper(c);
    }
    // Force NULL terminator
    line->m.name.c[WB_MAXNAMELENGTH] = 0;
}


//! Case insensitive comparison of 2 strings
//
//! Comparison stops when NULL is encountered in either string
//
//! @param[in] str1   First string to compare
//! @param[in] str2   Other string to compare
//! @param[in] length Maximum length to compare
//
//! @return 1 if both strings are the same while ignoring the case, 0 otherwise
static int wb_match_name(unsigned char *str1, unsigned char *str2, int length) {
    int j;
    if (length == 0) {
        // Everything matches 0 characters
        return 1;
    }
    for (j = 0; j < length && *str2 != 0 && *str1 != 0; j++) {
        if (toupper(*str1) != toupper(*str2)) {
            break;
        }
        str1++;
        str2++;
    }
    if ( j == length  || *str1 == 0 || *str2 == 0 ) {
        return 1;
    }

    return 0;
}

//! Find a line in a page using the key name
//
//! @param[in] line          Line to search
//! @param[in] page          Page in which to search
//! @param[in] err_not_found If this is not null, throw an error if the line is not found
//
//! @return Line number if found, error otherwise
static int lookup_line(LINE *line, PAGE *page, int err_not_found) 
{

    if (page != NULL) {
        LINE *pageline;
        unsigned char *str1 = &(line->m.name.c[0]);
        unsigned char *str2
        int i = 0;

        int linelimit = page->NextFreeEntry - 1;

        while (i <= linelimit) {
            pageline = &(page->line[i]);
            str1 = &(pageline->m.name.c[0]);
            if (wb_match_name(str1, str2, WB_MAXNAMELENGTH)) {
                // Found, return it's position in page
                return i;
            }

            if (pageline->m.flags.lines == 0) {
                // Only one entry in this page in this case
                break;
            }
            i += pageline->m.flags.lines;
        }
    }
    if (err_not_found) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTFOUND);
    }
    return WB_ERR_NOTFOUND;
}


//! Allocate new whiteboard page
//
//! @param[in,out] wb     WhiteBoard into which to add the line.  If null add the page to BaseWhiteboardPtr
//! @param[in]     nlines Number of lines to add
//
//! @return 0 on success
static int new_page(WhiteBoard *wb, int nlines)
{
    PAGE *page;
    int pagesize;

    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }
    if (message_level <= wb_MSG_DEBUG) {
        fprintf(stderr, "allocating new page for %d lines \n", nlines);
    }
    // note: sizeof(PAGE) gives the size of a page containing WB_MAXLINESPERPAGE lines
    // adjustment for actual number of lines in page has to be done to allocate correct size
    pagesize = sizeof(PAGE) + sizeof(LINE) * (nlines - WB_MAXLINESPERPAGE);
    page = (PAGE *)malloc(pagesize);
    if (page == NULL) {
        // malloc failed!
        WB_ERR_EXIT(WB_MSG_FATAL, WB_ERR_ALLOC);
    }
    // Zero page contents
    memset(page, 0, pagesize);

    // Next free entry is first entry in page
    page->NextFreeEntry = 0;

    // Set the number of data/metadata lines that the page can contain
    page->LinesInPage = nlines;
    // Update the page chain
    page->next = wb->PageChain;
    // Insert the new page at the beginning of the chain
    wb->PageChain = page;
    wb->validpages++;
    return 0;
}

static char *WhiteBoardCheckpointFile = "Whiteboard.ckpt";

int c_wb_reload();


//! Initialize BaseWhiteboardPtr
//
//! The content will be read from WhiteBoardCheckpointFile if it is found
static int wb_init()
{
    int status;

    if (BaseWhiteboardPtr->PageChain == NULL) {
        int fd = open(WhiteBoardCheckpointFile, O_RDONLY);
        if (fd >= 0) {
            close(fd);
            if (message_level <= WB_MSG_INFO) {
                fprintf(stderr, "whiteboard checkpoint file found, loading it\n");
            }
            return( c_wb_reload() );
        }
        // first time through, allocate first page
        status = new_page(BaseWhiteboardPtr, WB_MAXLINESPERPAGE );
        if (status < 0) {
            // Allocation failed!
            return status;
        }
    }
    return 0;
}


//! Set checkpoint file name
//
//! @param[in] filename File name
//
//! @return 0 on success or error code otherwise
int c_wb_checkpoint_name(char *filename)
{
    extra_error_message = "Setting chekpoint file name";
    WhiteBoardCheckpointFile = (char *)malloc(strlen(filename));
    if (WhiteBoardCheckpointFile == NULL) {
        WB_ERR_EXIT(WB_MSG_FATAL, WB_ERR_ALLOC);
    }
    strncpy(WhiteBoardCheckpointFile, filename, strlen(filename));
    return(wb_init());
}


//! Set checkpoint file name
//
//! @param[in] filename  File name
//! @param[in] lfilename Length of the file name
//
//! @return 0 on success or error code otherwise
wordint f77_name(f_wb_checkpoint_name)(char *filename, F2Cl lfilename)
{
    int Lfilename = lfilename;
    extra_error_message = "Setting chekpoint file name";
    WhiteBoardCheckpointFile = (char *)malloc(Lfilename + 1);
    if (WhiteBoardCheckpointFile == NULL) {
        WB_ERR_EXIT(WB_MSG_FATAL, WB_ERR_ALLOC);
    }
    c_fortran_string_copy(filename, WhiteBoardCheckpointFile, Lfilename, Lfilename, '\0');
    return(wb_init());
}


//! Get checkpoint file name
//
//! @param[out] filename  File name
//! @param[in]  lfilename Maximum length for the file name
//
//! @return 0 on success or error code otherwise
int c_wb_checkpoint_get_name(char *filename, int Lfilename)
{
    extra_error_message = "NO checkpoint file found while getting chekpoint file name";
    if (WhiteBoardCheckpointFile == NULL) {
        WB_ERR_EXIT(WB_MSG_FATAL, WB_ERR_ALLOC);
    }
    strncpy(filename, WhiteBoardCheckpointFile, Lfilename);
    return(wb_init());
}


//! Get checkpoint file name
//
//! @param[out] filename  File name
//! @param[in]  lfilename Maximum length for the file name
//
//! @return 0 on success or error code otherwise
wordint f77_name(f_wb_checkpoint_get_name)(char *filename, F2Cl lfilename)
{
    int Lfilename = lfilename;
    extra_error_message = "NO checkpoint file found while getting chekpoint file name";
    if (WhiteBoardCheckpointFile == NULL) {
        WB_ERR_EXIT(WB_MSG_FATAL,WB_ERR_ALLOC);
    }
    c_fortran_string_copy(WhiteBoardCheckpointFile, filename, Lfilename, Lfilename, ' ');
    return(wb_init());
}


//! Add lines to a WhiteBoard
//
//! A new page may be added to the WhiteBoard if it's necessary to store the new lines
//
//! @param[in,out] wb     Whiteboard to use.  If this is null, the WhiteBoard pointed by BaseWhiteboardPtr wil be used
//! @param[out]    page   Pointer to the page pointer to set the page containing the new lines
//! @param[in]     nlines Number of lines to add
static LINE *new_line(WhiteBoard *wb, PAGE **page, int nlines)
{
    PAGE *lookuppage;
    LINE *result;
    int status;

    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }
    if (BaseWhiteboardPtr->PageChain == NULL) {
        // First time through
        status = wb_init();
        if (status < 0) {
            // Init failed!
            return(NULL);
        }
    }
    // Start at beginning of pagechain
    lookuppage = wb->PageChain;

    // While page is not full
    while (lookuppage->NextFreeEntry + nlines > lookuppage->LinesInPage) {
        if (lookuppage->next == NULL) {
            // Already last page, allocate a new page
            // Make sure allocated page is big enough
            status = new_page(wb, ( nlines > WB_MAXLINESPERPAGE ) ? nlines : WB_MAXLINESPERPAGE );
            if (status < 0) {
                // Allocation of new page failed
                return(NULL);
            }
            // Potential target is new allocated page
            lookuppage = wb->PageChain;
            break;
        } else {
            // Look into next page
            lookuppage = lookuppage->next;
        }
    }
    result = &(lookuppage->line[lookuppage->NextFreeEntry]);
    *page = lookuppage;
    return result;
}


//! Set line flags from the provided numeric value
//
//! @param[out] line    Line in which to set the flags
//! @param[in]  options Numeric value representing the active flags
//
//! @return WB_OK on success, error code otherwise
static int options_to_flags(LINE *line, int options)
{
    line->m.flags.array = (options & WB_IS_ARRAY) ? 1 : 0;
    // Not created by a restart
    line->m.flags.fromrestart = (options & WB_CREATED_BY_RESTART) ? 1 : 0;
    line->m.flags.readonly = (options & WB_REWRITE_NONE) ? 1 : 0;
    // Mark as bad value in case it fails unless it is a create only call
    line->m.flags.badval = (options & WB_BADVAL) ? 1 : 0;
    // Variable has the local attribute, this will be used when checkpointing
    line->m.flags.islocal = (options & WB_IS_LOCAL) ? 1 : 0;
    line->m.flags.resetuntil = (options & WB_REWRITE_UNTIL) ? 1 : 0;
    line->m.flags.resetmany = (options & WB_REWRITE_MANY) ? 1 : 0;
    line->m.flags.noresetafterrestart = (options & WB_READ_ONLY_ON_RESTART) ? 1 : 0;
    line->m.flags.initialized = (options & WB_INITIALIZED) ? 1 : 0;
    if (line->m.flags.readonly + line->m.flags.resetmany + line->m.flags.resetuntil > 1) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_OPTION);
    }
    return WB_OK;
}


//! Get the numeric representation of the active flags of the provided line
//
//! @param[in] line Line from which to get the flags
//
//! @return Numeric representation of the active flags
static int flags_to_options(LINE *line)
{
    int options = 0;
    if (line->m.flags.array) options += WB_IS_ARRAY;
    if (line->m.flags.fromrestart) options += WB_CREATED_BY_RESTART;
    if (line->m.flags.readonly) options += WB_REWRITE_NONE;
    if (line->m.flags.badval) options += WB_BADVAL;
    if (line->m.flags.islocal) options += WB_IS_LOCAL;
    if (line->m.flags.resetuntil) options += WB_REWRITE_UNTIL;
    if (line->m.flags.resetmany) options += WB_REWRITE_MANY;
    if (line->m.flags.noresetafterrestart) options += WB_READ_ONLY_ON_RESTART;
    if (line->m.flags.initialized) {
        options += WB_INITIALIZED;
    } else {
        options += WB_NOTINITIALIZED;
    }
    return options;
}


//! Find line in WhiteBoard with it's name
//
//! @param[in]  wb            WhiteBoard in which to search.  If this is null, default to BaseWhiteboardPtr
//! @param[in]  name          Name to search for
//! @param[out] elementtype   Type of the element(s)
//! @param[out] elementsize   Size of each element in bytes
//! @param[out] elements      Number of elements in array, 0 if scalar
//! @param[out] result_line   Line that bears the given name
//! @param[out] result_page   Page in which the name was found
//! @param[in]  err_not_found If not 0, throw an error if the line is not found
//! @param[in]  name_length   Length of the name
static int c_wb_lookup(WhiteBoard *wb, unsigned char *name, int *elementtype, int *elementsize, int *elements,
                        LINE **result_line, PAGE **result_page, int err_not_found, int name_length)
{
    LINE line;
    int target_page = 0;
    int target_line = -1;
    PAGE *lookuppage = NULL;

    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }

    TRIM(name, name_length)
    if (name_length > WB_MAXNAMELENGTH) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NAMETOOLONG);
    }

    lookuppage = wb->PageChain;
    // Put key name into local line
    fill_line(&line, name, name_length);
#ifdef DEBUG
    printf("target name = :%s:\n", &(line.m.name.c[0]));
#endif
    while (lookuppage != NULL) {
        // No screaming if not found, full length match
        target_line = lookup_line(&line, lookuppage, 0);
        if (target_line >= 0) {
            // A matching key has been found in this page
            *elementtype = lookuppage->line[target_line].m.flags.type;
            *elementsize = lookuppage->line[target_line].m.flags.elementsize;
            if (lookuppage->line[target_line].m.flags.array == 1) {
                *elements = lookuppage->line[target_line].m.data.a.MaxElements;
            } else {
                *elements = 0;
            }
            *result_line = &(lookuppage->line[target_line]);
            *result_page = lookuppage;
            return WB_MAXLINESPERPAGE * target_page + target_line;
         }
        target_page++;
        lookuppage = lookuppage->next;
    }
    if (err_not_found) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTFOUND);
    }
    return WB_ERR_NOTFOUND;
}

//! Get the metadata associated with a WhiteBoard name for Fortran
//
//! @param[in]  wb            WhiteBoard in which to search
//! @param[in]  name          Name to search for
//! @param[out] elementtype   Type of the element(s)
//! @param[out] elementsize   Size of each element in bytes
//! @param[out] elements      Number of elements in array, 0 if scalar
//! @param[out] options       Numeric representation of active flags
//! @param[in]  name_length   Length of the name
wordint f77_name(f_wb_get_meta)(WhiteBoard **wb, unsigned char *name, wordint *elementtype, wordint *elementsize,
                                       wordint *elements, wordint *options, F2Cl name_length)
{
    LINE *line;
    PAGE *page;
    int length = name_length;
    int status;

    // No screaming if not found
    status = c_wb_lookup(*wb,name, elementtype, elementsize, elements, &line, &page, 0, length);
    if (status < 0) {
        return status;
    }

    *options = flags_to_options(line);
    return status;
}


//! FIXME Replace these with iso_c_binding
void f77_name(f_logical_move)(void *, void *, wordint *);
void f77_name(f_logical2int)(void *, void *, wordint *);
void f77_name(f_int2logical)(void *, void *, wordint *);


//! get the data associated with a whiteboard entry
//
//! @param[in]  wb          WhiteBoard in which to search.
//! @param[in]  name        Name of key (length MUST be supplied in name_length)
//! @param[in]  type        Type represented by one character: R/I/L/C , key type real/inetger/logical/character
//! @param[in]  size        Size in bytes of each element 4/8 for R/I/L, 1->WB_MAXSTRINGLENGTH for character strings
//! @param[out] dest        Pointer to where data is written (everything considered as unsigned bytes)
//! @param[in]  nbelem      Number of elements that can be stored into value
//! @param[in]  name_length Length of name
//
//! @return Dimension of whiteboard array if array, 0 if scalar, < 0 if error
int c_wb_get(WhiteBoard *WB, unsigned char *name, char type, int size, unsigned char *dest, int nbelem, int name_length){
    LINE *line;
    PAGE *page;
    int element_type, element_size, nb_elements;
    int status, typecode, array, result;
    int i;
    unsigned char *csrc;

    extra_error_message = " invalid whiteboard instance";
    if (WB == DummyWhiteboardPtr) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTFOUND);
    }
    if (WB == NULL) {
        WB = BaseWhiteboardPtr;
    }

    TRIM(name, name_length)
    set_extra_error_message(name, name_length);

    if (type == WB_FORTRAN_BOOL && size == 1) {
        // Special case: force 4 byte container for FORTRAN logical type
        size = 4;
    }

    // Check validity of type / ltype combination
    typecode = get_typecode(type, size, WB_MAXSTRINGLENGTH);
    if (typecode < 0) {
        // Bad type / ltype combination
        return(typecode);
    }
    if (nbelem < 0) {
        // A negative number of values is a bad idea
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
    }
    array = (nbelem > 0) ? 1 : 0;
    if (nbelem == 0) {
        // Scalar
        nbelem = 1;
    }

    // No screaming if not found
    status = c_wb_lookup(WB, name, &element_type, &element_size, &nb_elements, &line, &page, 0, name_length);
    if (status < 0) {
        WB_ERR_EXIT(WB_MSG_INFO, WB_ERR_NOTFOUND);
    }
    if (line->m.flags.badval == 1) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BADVAL);
    }
    if (line->m.flags.initialized != 1) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOVAL);
    }

    // Types MUST match
    if (element_type != typecode) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGTYPE);
    }

    // For non character variables, element length must match
    // For character variables, the length verification will be performed when copying values
    if (element_type != WB_FORTRAN_CHAR && element_size != size ) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGTYPE);
    }

    if (array && line->m.flags.array != 1) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
    }

    // we can now proceed to copy the value(s) from whiteboard entry
    if (array) {
        if (nbelem > line->m.data.a.MaxElements) {
            // Will do a short get
            nbelem = line->m.data.a.MaxElements;
        }
        // if successful return max dimension of array
        result = line->m.data.a.MaxElements;
        // Array data is always stored in lines following metadata
        csrc = &((line + 1)->d.data[0]);
    } else {
        // Scalar
        result = 0;
        // Scalar data is stored starting in metadata line
        csrc = &(line->m.data.c[0]);
    }
    if (element_type != WB_FORTRAN_CHAR) {
        // Not a character string
        wordint nbelemf = nbelem;
        if (element_type == WB_FORTRAN_BOOL) {
            // Special case for FORTRAN logicals
            // Fortran helper to move ints into logicals
            f77_name(f_int2logical)(dest , csrc, &nbelemf);
        } else {
            memcpy(dest, csrc, nbelem * size);
        }
    } else {
        // Character strings,  use trimmed to padded copy
        for (i = 0; i < nbelem; i++) {
            int tempstat = c_fortran_string_copy(csrc, dest, element_size, size, ' ');
            if (tempstat < 0) {
                // Not enough space to store string
                WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGSTRING);
            }
            dest += size;
            csrc += element_size;
        }
    }
    return result;
}


//! @copydoc c_wb_get
wordint f77_name(f_wb_get)(WhiteBoard **wb, unsigned char *name, wordint *type, wordint *size, void *dest,
                           wordint *nbelem, F2Cl name_length)
{
    // FIXME Check typing!  The compiler probably throws a buch of warnings for these!
    // FIXME Why copy to local variables prior to the function call for pass-by-value parameters?
    // FIXME Why not replace all of this with a proper iso_c_binding interface?
    int _name_length = name_length;
    int _type = *type;
    int _nbelem = *nbelem;
    char _size = *size;
    return c_wb_get(*wb, name, _type, _size, value, _nbelem, _name_length);
}


//! Address of the line that was the target of the last put/create operation. Used by wb_read to avoid a costly lookup
static LINE *LastPutLine = NULL;


//! Store entry in a WhiteBoard
//
//! @param[in,out] wb          WhiteBoard in which to store.
//! @param[in]     name        Name of key (length MUST be supplied in name_length)
//! @param[in]     type        Type represented by one character: R/I/L/C , key type real/inetger/logical/character
//! @param[in]     size        Size in bytes of each element 4/8 for R/I/L, 1->WB_MAXSTRINGLENGTH for character strings
//! @param[in]     src         Pointer to data asssociated with key (everything considered as unsigned bytes)
//! @param[in]     nbelem      Number of elements (0 means a scalar) (1 or more means an array)
//! @param[in]     options     Numeric representation of active flags
//! @param[in]     name_length Name Length
//
//! @return Smaller than 0 in case of error.  0 for non character scalars, string length if string, max size of array if array
int c_wb_put(WhiteBoard *wb, unsigned char *name, char type, int size, unsigned char *src, int nbelem, int options,
             int name_length)
{
    LINE *line;
    PAGE *page;
    int stored_type, stored_size, stored_nbelem;
    int lookup_result;
    int typecode, array, result;
    int i;
    unsigned char *dest;

    extra_error_message = " invalid whiteboard instance";
    if (wb == DummyWhiteboardPtr) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTFOUND);
    }
    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }
    extra_error_message = "";

    TRIM(name, name_length)
    set_extra_error_message(name, name_length);
    LastPutLine = NULL;

    if (Type == WB_FORTRAN_BOOL && size == 1) {
        // Special case, force 4 byte container for FORTRAN logical type
        size = 4;
    }

    // Check validity of requested Type/Ttype combination
    typecode = get_typecode(Type, size, size);
    if (typecode < 0) {
        // Bad Type/size combination
        return typecode;
    }

    if (nbelem < 0) {
        // A negative number of values is a bad idea
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
    }

    // Array if nbelem > 0 , scalar if nbelem == 0
    array = (nbelem > 0) ? 1 : 0;
    if (nbelem == 0) {
        // Scalar
        nbelem = 1;
    }

    // No screaming if not found
    lookup_result = c_wb_lookup(wb, name, &stored_type, &stored_size, &stored_nbelem, &line, &page, 0, name_length);
    if (lookup_result >= 0) {
        // Entry has been found, will overwrite it, the options argument is ignored,
        // Flags are checked for consistency and permissions
        if (options & WB_CREATE_ONLY) {
            // Redefinition not allowed
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_REDEFINE);
        }
        // Above error has to be ignored if it was coming from a restart and create from restart flag then has to be erased
        if (line->m.flags.readonly && line->m.flags.initialized ) {
            // Entry is READONLY
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_READONLY);
        }
        // Mark as bad value in case it fails
        line->m.flags.badval = 1;
        if (line->m.flags.array == 1 && array != 1) {
            // Array to scalar or scalar to array is a NO NO
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
        }

        // Types MUST match
        if (stored_type != typecode) {
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGTYPE);
        }

        // For non character variables, element length must match
        // For character variables, the length verification will be performed when copying values
        if (stored_type != WB_FORTRAN_CHAR && stored_size != size) {
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGTYPE);
        }

        if (array &&  line->m.data.a.MaxElements < nbelem) {
            // Array in whiteboard is too small
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
        }
    } else {
        // If the lookup didn't find the entry, create it, otherwise, return error code
        if (lookup_result != WB_ERR_NOTFOUND) {
            return lookup_result;
        }

        // Number of storage lines is 1 for scalars using less than 9 bytes
        int lines = 1;
        // Arrays and strings need supplementary storage space
        if (array || typecode == WB_FORTRAN_CHAR) {
            lines += ((nbelem * size) + sizeof(DATALINE) - 1 ) / sizeof(DATALINE);
        }

        // Create new entry with space for lines
        line = new_line(wb, &page, lines);
        if (line == NULL) {
            // No need to use error macro, it will already have been called
            return WB_ERR_ALLOC;
        }

        // Set line name
        fill_line(line, name, name_length);
        stored_size = size;
        if (typecode == WB_FORTRAN_CHAR && array == 0 ) {
            // Scalar string, round size up to DATALINE size
            stored_size = ( (stored_size + sizeof(DATALINE) - 1) / sizeof(DATALINE) ) * sizeof(DATALINE);
        }
        line->m.flags.type = typecode;
        line->m.flags.elementsize = stored_size;
        // Only one entry in page if lines>WB_MAXLINESPERPAGE
        line->m.flags.lines = ( lines > WB_MAXLINESPERPAGE ) ? 0 : lines;
        line->m.flags.readonly = (options & WB_REWRITE_NONE) ? 1 : 0;
        line->m.flags.resetmany = (options & WB_REWRITE_MANY) ? 1 : 0;
        line->m.flags.resetuntil = (options & WB_REWRITE_UNTIL) ? 1 : 0;
        if (line->m.flags.readonly + line->m.flags.resetmany + line->m.flags.resetuntil > 1) {
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_OPTION);
        }
        line->m.flags.initialized = 0;
        // Variable has the local attribute, this will be used when checkpointing
        line->m.flags.islocal = (options & WB_IS_LOCAL) ? 1 : 0;
        // Mark as bad value in case it fails unless it is a create only call
        line->m.flags.badval = (options & WB_CREATE_ONLY) ? 0 : 1;
        // Not created by a restart
        line->m.flags.fromrestart = 0;
        line->m.flags.noresetafterrestart = (options & WB_READ_ONLY_ON_RESTART) ? 1 : 0;
        line->m.flags.array = array;
        // Arrays are not explicitely initialized for the time being
        if (array) {
            line->m.data.a.UsedElements = nbelem;
            line->m.data.a.MaxElements = nbelem;
        } else {
            // Scalar, set data to 0
            line->m.data.i[0] = 0;
            line->m.data.i[1] = 0;
        }
        // Check if there is enough space in page
        int lines_available = page->LinesInPage - page->NextFreeEntry;
        if (lines_available < lines) {
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOMEM);
        }
        // Allocate space for data, adjust page pointer to next available entry
        page->NextFreeEntry += lines;
    }

    LastPutLine = line;
    if (options & WB_CREATE_ONLY) {
        // Create entry only, do not copy value(s)
        if (array) {
            result = line->m.data.a.MaxElements;
        } else {
            result = 0;
        }
    } else {
        // Copy the value(s) into (old or new) whiteboard entry
        if (array) {
            // One dimensional array
            if (nbelem > line->m.data.a.MaxElements) {
                // Not enough space to store values
                WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
            }
            // If successful, return max dimension of array
            result = line->m.data.a.MaxElements;
            // Array data is always stored in lines following metadata
            dest = &((line + 1)->d.data[0]);
        } else {
            // Scalar
            result = 0;
            // Scalar data is stored starting in metadata line
            dest = &(line->m.data.c[0]);
        }
        if (typecode != WB_FORTRAN_CHAR) {
            // Not a character string
            wordint nbelemf = nbelem;
            if (typecode == WB_FORTRAN_BOOL) {
                // Special case for FORTRAN logicals; use a Fortran helper to move logicals into ints
                f77_name(f_logical2int)(dest, src, &nbelemf);
            } else {
                memcpy(dest, src, nbelem * size);
            }
        } else {
            // Character strings,  use trimmed to padded copy
            for (i = 0 ; i < nbelem ; i++) {
                result = c_fortran_string_copy(src, dest, size, stored_size, ' ');
                if (result < 0) {
                    // Not enough space to store string!
                    WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGSTRING);
                }
                src += size;
                dest += stored_size;
            }
        }
        // Mark entry as initialized
        line->m.flags.initialized = 1;
        // Mark entry as good
        line->m.flags.badval = 0;
    }

    return result;
}


//! @copydoc c_wb_put
wordint f77_name(f_wb_put)(WhiteBoard **wb, unsigned char *name, wordint *type, wordint *size, void *src, wordint *nbelem, wordint *options, F2Cl name_length){
   char _type = *type;
   int _name_length = name_length;
   int _size = *size;
   int _nbelem = *nbelem;
   int _options = *options;
   return(c_wb_put(*wb, name, _type, _size, src, _nbelem, _options, _name_length));
}


//! Write checkpoint file of the WhiteBoard referenced by BaseWhiteboardPtr
int c_wb_checkpoint()
{
    PAGE *page;
    int pageno = 0;
    int zero = 0;
    int status;
    int fd = open(WhiteBoardCheckpointFile, O_WRONLY|O_CREAT, 0777);

    if (fd < 0) {
        // Can't open checkpoint file
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
    }
    page = BaseWhiteboardPtr->PageChain;

    // Write signature
    status = write(fd, "WBckp100", 8);
    if (status < 0) {
        // Write error!
        close(fd);
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
    }

    while (page != NULL) {
        // Write number of lines in page
        status = write(fd, &(page->LinesInPage), 4);
        if (status < 0) {
            close(fd);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
        }
        // Write number of next entry
        status = write(fd, &(page->NextFreeEntry), 4);
        if (status < 0) {
            close(fd);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
        }
        // Write page
        status = write(fd, &(page->line[0]), sizeof(LINE) * page->LinesInPage);
        if (status < 0) {
            close(fd);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
        }
        if (message_level <= WB_MSG_DEBUG) {
            fprintf(stderr, "wb_checkpoint: Page %d, length %d lines, Next entry %d \n",
                    pageno, page->LinesInPage, page->NextFreeEntry);
        }
        page = page->next;
        pageno++;
    }

    // 0 length page is the end marker
    status = write(fd, &zero, 4);
    if (status < 0) {
        close(fd);
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
    }

    return 0;
}

//! @copydoc c_wb_checkpoint
wordint f77_name(f_wb_checkpoint)(WhiteBoard **WB){
   return(c_wb_checkpoint());
}

/*
 basic whiteboard check/action routine, the Whiteboard "swiss knife"
  name   : pointer to character string containing name of key (length MUST be supplied in Lname)
  OptionsMask: options mask to be tested, call Action if OptionsMask&options  is non zero
  Lname  : length of key pointed to by name (FORTRAN style)
  printflag: print messages if nonzero
  Action:  routine to be called if name and OptionsMask &options  is non zero
  blinddata: pointer to be passed to Action routine as a second argument (first argument is matching line)
*/

typedef int (*ACTION)(LINE *,void *);
int c_wb_check(WhiteBoard *WB, unsigned char *name, int OptionsMask, int Lname, int printflag, ACTION Action, void *blinddata ){
   PAGE *lookuppage;
   int pageno=0;
   int printcount=0;
   int status;

   extra_error_message = " invalid whiteboard instance";
   if(WB == DummyWhiteboardPtr) WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_NOTFOUND);
   if(WB == NULL) WB=BaseWhiteboardPtr;

   TRIM(name,Lname)
   lookuppage = WB->PageChain;

   while( lookuppage != NULL ){
      int i,linelimit;
      i=0;
      linelimit=lookuppage->NextFreeEntry - 1;
      while( i <= linelimit ){
         int Options;
         LINE *line=&(lookuppage->line[i]);
         unsigned char *c1=&(line->m.name.c[0]);
         Options = flags_to_options(line);
         if( (Options & OptionsMask) && wb_match_name(c1,name,Lname) ) {
            printcount--;
            if(printflag){
               fprintf(stderr,"Page %2d, Line %3d, KEY=%s Datatype=%s[%4d] Size=%4d %s %s %s %s %s %s %s %s\n",
                     pageno,i,c1,
                     datatypes[line->m.flags.type],
                     line->m.flags.elementsize,
                     (line->m.flags.array) ? line->m.data.a.MaxElements : 0,
                     (line->m.flags.resetuntil) ? "LOCKABLE" : "        ",
                     (line->m.flags.initialized) ? "SET  " : "UNSET",
                     (line->m.flags.badval) ? "BAD " : "GOOD",
                     (line->m.flags.resetmany) ? "NOTLOCKABLE" : "           ",
                     (line->m.flags.noresetafterrestart) ? "ROonRst" : "RWonRst",
                     (line->m.flags.readonly) ? "LOCKED  " : "WRITABLE",
                     (line->m.flags.islocal) ? "LOCAL " : "GLOBAL",
                     (line->m.flags.fromrestart) ? "FromRestart" : "FromPut"
                     );
               }
            if(Action != NULL) {
               status = (*Action)(line,blinddata);
               if(status < 0) return(status);
               }
            }
         if(line->m.flags.lines == 0) break;   /* only one entry in this page in this case */
         i += line->m.flags.lines;
         }
      lookuppage=lookuppage->next;
      pageno++;
      }
return(-printcount);
}
wordint f77_name(f_wb_check)(WhiteBoard **WB, unsigned char *name, wordint *optionsmask, F2Cl lname){
   int OptionsMask=*optionsmask;
   int Lname=lname;
   /* print flag on, no action routine is supplied, no blind data pointer is supplied */
   return( c_wb_check(*WB, name, OptionsMask, Lname, 1, NULL, NULL) );
}

/* action routine to make a variable read-only */
static int MakeReadOnly(LINE *line, void *blinddata){
   if(line->m.flags.initialized == 0) WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_NOVAL); /* making read-only a non initialized variable ???!!! */
   if( message_level <= WB_MSG_DEBUG ) 
      fprintf(stderr,"key '%s' is now read-only\n",&(line->m.name.c[0]));
   line->m.flags.readonly = 1; line->m.flags.resetmany = 0;
   return(0);
}

/* action routine to make a variable no longer read-only */
static int MakeNotReadOnly(LINE *line, void *blinddata){
   if( message_level <= WB_MSG_DEBUG ) 
      fprintf(stderr,"key '%s' is now no longer read-only\n",&(line->m.name.c[0]));
   line->m.flags.readonly = 0;
   return(0);
}

/* action routine to make a variable undefined (no longer initialized) */
static int MakeUndefined(LINE *line, void *blinddata){
   if( message_level <= WB_MSG_DEBUG ) 
      fprintf(stderr,"key '%s' is now undefined\n",&(line->m.name.c[0]));
   line->m.flags.initialized = 0;
   line->m.flags.badval = 0;
   return(0);
}

/* action routine to make a variable undefined (no longer initialized) */
static int MakeCreatedByRestart(LINE *line, void *blinddata){
   if( message_level <= WB_MSG_DEBUG ) 
      fprintf(stderr,"key '%s' is now marked as created by a restart\n",&(line->m.name.c[0]));
   line->m.flags.fromrestart = 1;
   return(0);
}

/* framework for broadcasting values across MPI tiles */
typedef struct{
   wordint pe_root;
   wordint pe_me;
   char *domain;
   F2Cl ldomain;
   void (*broadcast_function)();
   void (*allreduce_function)();
   } BROADCAST;
static BROADCAST broadcast_table;

void f77_name(f_wb_bcst_init)(wordint *pe_root, wordint *pe_me, char *domain, void (*broadcast_function)(), void (*allreduce_function)() ){
   broadcast_table.pe_root = *pe_root;
   broadcast_table.pe_me = *pe_me;
   broadcast_table.domain = domain;
   broadcast_table.broadcast_function = broadcast_function;
   broadcast_table.allreduce_function = allreduce_function;
}

static int BroadcastLine(LINE *line, void *blinddata){
fprintf(stderr,"Broadcasting %s :-)\n",line->m.name.c);
   return(0);
}

wordint f77_name(f_wb_bcst)(WhiteBoard **WB, unsigned char *name, wordint *wildcard, F2Cl lname){
   int Lname = lname;
   LINE line;
   int i,status;

   fill_line(&line,name,Lname);
   if(*wildcard != 0) {   /* wildcard match, get rid of trailing blanks */
      for( i=0 ; i<WB_MAXNAMELENGTH-1 ; i++ ) {
         if(line.m.name.c[i]==' ') line.m.name.c[i]=0;
         }
      }
   status = c_wb_check(*WB, name, -1, Lname, message_level<=WB_MSG_INFO, BroadcastLine, &broadcast_table) ;
   return(status);
}

typedef struct{
   unsigned char *UserKeyArray;
   int UserKeyLength;
   int UserMaxLabels;
   } KEYS;

/* action routine used to copy keys into user table when collecting a list of keys */
static int CopyKeyName(LINE *line, void *blinddata){
   unsigned char *key=&(line->m.name.c[0]);
   KEYS *keys=(KEYS *)blinddata;
   if(keys->UserMaxLabels <= 0) return(WB_ERROR);   /* too many keys for user array */
   c_fortran_string_copy(key, keys->UserKeyArray, WB_MAXNAMELENGTH, keys->UserKeyLength, ' ');
   (keys->UserMaxLabels)--;
   keys->UserKeyArray += keys->UserKeyLength;
   return(0);
}

/* get a list of keys matching a name */
wordint f77_name(f_wb_get_keys)(WhiteBoard **WB, unsigned char *labels, wordint *nlabels, unsigned char *name, F2Cl llabels, F2Cl lname){
   int Lname = lname;
   int status;
   KEYS keys;

   keys.UserKeyArray = labels;
   keys.UserMaxLabels = *nlabels;
   keys.UserKeyLength = llabels;
   status = c_wb_check(*WB, name, -1, Lname, message_level<=WB_MSG_INFO, CopyKeyName, &keys) ;
   return (status);
}

/* make readonly all entries that have the WB_REWRITE_UNTIL attribute */
int c_wb_lock(WhiteBoard *WB, unsigned char *name, int Lname){
   unsigned char localname[WB_MAXSTRINGLENGTH];
   int llname;
   llname=c_fortran_string_copy(name,localname,Lname,sizeof(localname),'\0');
   if( message_level <= WB_MSG_DEBUG )
      fprintf(stderr,"locking variables with name beginning with '%s'\n",localname);
   return( c_wb_check(WB, localname, WB_REWRITE_UNTIL, llname, 1, MakeReadOnly, NULL) );
}
wordint f77_name(f_wb_lock)(WhiteBoard **WB, unsigned char *name, F2Cl lname){
   int Lname=lname;
   int status = c_wb_lock(*WB, name,Lname);
   return(status);
}

/* write whiteboard checkpoint file */
int c_wb_reload(){
   int pageno=0;
   int pagelen;
   int status;
   char signature[9];
   int fd=open(WhiteBoardCheckpointFile,O_RDONLY);

   if(fd < 0) WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_CKPT);  /* OOPS, cannot open checkpoint file */
   if(BaseWhiteboardPtr->PageChain != NULL) WB_ERR_EXIT(WB_MSG_ERROR,WB_ERROR);
   status=read(fd,signature,8) ; signature[8]=0;
   if( status!=8 || 0!=strncmp(signature,"WBckp100",8) ) { close(fd); WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_CKPT); }

   status=read(fd,&pagelen,4);     /* page size */
   while(pagelen>0){
      status=new_page(BaseWhiteboardPtr, pagelen);                    /* allocate new page */
      status=read(fd,&(BaseWhiteboardPtr->PageChain->NextFreeEntry),4);  /* next usable entry in page */
      if( message_level <= WB_MSG_DEBUG )
         fprintf(stderr,"wb_reload: Page %d, length=%d lines, next entry=%d\n",
                 pageno,BaseWhiteboardPtr->PageChain->LinesInPage,BaseWhiteboardPtr->PageChain->NextFreeEntry);
      status=read(fd,&(BaseWhiteboardPtr->PageChain->line[0]),sizeof(LINE)*pagelen);  /* read page */
      status=read(fd,&pagelen,4);     /* page size of next page, 0 means no more */
      pageno++;
      }
   close(fd);
/* if variable is WB_REWRITE_AT_RESTART, erase the read-only flag */
   status=c_wb_check(BaseWhiteboardPtr, (unsigned char*)"", WB_REWRITE_AT_RESTART, 0, 0, MakeNotReadOnly, NULL);
/* if variable is WB_READ_ONLY_ON_RESTART, make sure it is now marked as reasd-only */
   status=c_wb_check(BaseWhiteboardPtr, (unsigned char*)"", WB_READ_ONLY_ON_RESTART, 0, 0, MakeReadOnly, NULL);
/* if variable is local (not the same on ALL MPI whiteboards, mark it as not initialized */
   status=c_wb_check(BaseWhiteboardPtr, (unsigned char*)"", WB_IS_LOCAL, 0, 0, MakeUndefined, NULL);
/* mark all variables as having been created by a restart */
   status=c_wb_check(BaseWhiteboardPtr, (unsigned char*)"", -1, 0, 0, MakeCreatedByRestart, NULL);
   if( message_level<=WB_MSG_INFO )
      fprintf(stderr,"whiteboard has been reloaded, variables read only after restart have been locked\n");
   return(WB_OK);
}
wordint f77_name(f_wb_reload)(WhiteBoard **WB){
   int status=c_wb_reload();
   return(status);
}

static int Action1(LINE *line, void *blinddata){   /* DUMMY ACTION ROUTINE */
   fprintf(stderr,"Action1 has been called, key=%s\n",&(line->m.name.c[0]));
   return(0);
}

#define MISC_BUFSZ 1024
/* get a line from file, reset current character pointer */
static int wb_get_line(FILE *infile){
   current_char=fgets(linebuffer, MISC_BUFSZ, infile);
   if( message_level<=WB_MSG_INFO && current_char )
      fprintf(stderr,">>%s",linebuffer);
   if(current_char) return(*current_char);
   else             return(EOF);
}

/* push character back onto input buffer */
static int wb_ungetc(int c){
   if( current_char==NULL || linebuffer==NULL ) return(WB_ERROR);
   if( current_char > linebuffer ) {
      current_char--;
      *current_char = c;
      }
   else {
      return(WB_ERROR);
      }
   return(WB_OK);
}

/* get next character from input stream, take care of \newline sequence */
static int wb_getc(FILE *infile){
   int c;

   if( current_char==NULL ) {                  /* no buffer pointer, fill buffer */
      c=wb_get_line(infile);
      if( current_char==NULL ) return(EOF);    /* End Of File or error */
      }
s: if( *current_char==0 ) {
      c=wb_get_line(infile);           /* try to refill empty buffer */
      if( current_char==NULL ) return(EOF);    /* End Of File or error */
      }
   c=*current_char++ ;                         /* get next character and bump pointer */
   if( c=='\\' && *current_char=='\n' ) {      /* \newline, get rid of it */
      current_char++;                          /* point to character after newline */
      goto s;                                  /* and start all over again */
      }
   return(c);
}

/* flush input until newline or EOF , return what was found */
static int wb_flush_line(FILE *infile){
   int c=wb_getc(infile);
   while( c!='\n' && c!=EOF && c!=';' ) c=wb_getc(infile);
   return (c);
}

/* skip blanks and tabs from input, return next character, newline, or EOF ,
    take care of comments, recognize ; as newline    */
static int wb_get_nonblank(FILE *infile){
   int c=wb_getc(infile);
  if( c==EOF ) return(EOF);
   while( (c==' ' || c=='\t' || c=='#' || c=='!' ) && c!=EOF ) {  /* space, tab, comment character */
      if( c=='#' || c=='!' ) {             /* comment, get rid of the rest of the line */
         c=wb_flush_line(infile);
         if( c==EOF ) return(EOF);
         }
      c=wb_getc(infile);           /* get next character */
      }
   if( c==';' ) c='\n' ;      /* treat a non quoted ; as a newline */
   return (c);
}

/* special error print when reading directives, print directive line up to error, add ^^^^ error marker, print rest of line */
static void wb_read_error(){
   int temp;
   if(current_char==NULL || linebuffer==NULL) return;
   temp=*current_char;
   *current_char=0;
   if( message_level<=WB_MSG_ERROR )
      fprintf(stderr,"ERROR in directives, offending line:\n%s^^^^",linebuffer);
   *current_char=temp;
   if( message_level<=WB_MSG_ERROR )
      fprintf(stderr,"%s\n",current_char);
}

/* get next token (alphanum_, number, delimited string, single character, force non quoted tokens to uppercase */
static int wb_get_token(unsigned char *token,FILE *infile,int maxtoken, int noskip){
   int ntoken=0;
   int c;

   if(noskip)  /* do not skip spaces */
      c=wb_getc(infile);
   else
      c=wb_get_nonblank(infile);
   if( c==EOF ) return(WB_ERROR);
   token[ntoken++] = toupper(c);  /* first character */
   maxtoken--; if(maxtoken<0) goto error_big;
   if(isalpha(c)) {   /* collect alphanum _ token */
      for( c=wb_getc(infile) ; isalnum(c) || c=='_' ; c=wb_getc(infile) ) {
         maxtoken--; if(maxtoken<0) goto error_big;
         token[ntoken++]=toupper(c) ;
         }
      if( c==EOF ) return(WB_ERROR);
      if(wb_ungetc(c)==EOF ) return(WB_ERROR);  /* push back extra input character */
      }
   else if( c=='\'' || c=='"' ) {         /* collect ' or " delimited string */
      int quote=c;
      c=wb_getc(infile);
      while( c!=quote && c!='\n' && c!=EOF) {   /* look for matching quote , error end if newline/EOF  ecountered */
         if(c==EOF || c=='\n') break;
         maxtoken--; if(maxtoken<0) goto error_big;
         token[ntoken++]=c;
         c=wb_getc(infile);
         }
      if( c=='\n' ) {
         if( message_level<=WB_MSG_ERROR ) fprintf(stderr,"ERROR: improperly terminated string\n");
         if(wb_ungetc(c)==EOF ) return(WB_ERROR); /* push back newline that has been erroneously swallowed */
         }
      if(c==EOF || c=='\n') return(WB_ERROR);
      maxtoken--; if(maxtoken<0) goto error_big;
      token[ntoken++]=c;                       /* store end delimiter in token */
      }
   else if(isdigit(c) || c=='.' || c=='+' || c=='-' ) {  /* digit, point, sign, potential number or boolean */
      c=wb_getc(infile) ;
      if( isalpha(c) && token[ntoken-1]=='.' ) {    /* collect .true. , .false. , etc ... */
         while( isalpha(c) || c=='.' ) {
            maxtoken--; if(maxtoken<0) goto error_big;
            token[ntoken++]=toupper(c) ;\
            c=wb_getc(infile);
            }
         }
      else {    /* collect a potential number */
         while( isdigit(c) || c=='.' || c=='-' || c=='+' || c=='E' || c=='e' ) {
            maxtoken--; if(maxtoken<0) goto error_big;
            token[ntoken++]=toupper(c) ;
            c=wb_getc(infile);
            }
         }
      if( c==EOF ) return(WB_ERROR);
      if(wb_ungetc(c)==EOF ) return(WB_ERROR);  /* push back extra input character */
      }
   /* none of the above means single character */
   maxtoken--; if(maxtoken<0) goto error_big;
   token[ntoken]=0;  /* null terminate token */

   if( message_level<=WB_MSG_DEBUG )
      fprintf(stderr,"GetToken, maxtoken=%d, ntoken=%d, nospkip=%d, token='%s'\n",maxtoken,ntoken,noskip,token);
   return(ntoken);
error_big:    /* token is too big to be stored in supplied array */
   wb_read_error() ;
   WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_BIG);
}

/* process options set [option,option,...] or (option,option,...) */
static int wb_options(FILE *infile,char start_delim,char end_delim,SYMTAB *option_table){
   unsigned char Token[WB_MAXNAMELENGTH+1];
   int options=0;
   int ntoken,newoption;

   ntoken=wb_get_token(Token,infile,2,0) ; /* get starting delimiter ( or [  */
   if(Token[0]!=start_delim || ntoken!=1) goto error_syntax ;

   while(Token[0] != end_delim ) {
      ntoken=wb_get_token(Token,infile,sizeof(Token),0) ; /* expect keyname  */
      if( !isalpha(Token[0]) || ntoken<=0 ) goto error_syntax ;
      newoption = wb_value((char *)Token,option_table);
      if( newoption == 0 ) goto error_option;
      options += newoption;
      if( message_level<=WB_MSG_DEBUG )
         fprintf(stderr,"newoption=%d, options=%d\n",newoption,options);
      ntoken=wb_get_token(Token,infile,2,0) ; /* expect end delimiter ( or ]  or comma ,  */
      if( (Token[0]!=end_delim && Token[0]!=',') || ntoken!=1) goto error_syntax ;
      }
   return(options);
error_option:
   wb_read_error() ;
   wb_flush_line(infile);
   WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_OPTION);
error_syntax:
   wb_read_error() ;
   wb_flush_line(infile);
   WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_SYNTAX);
}

/* process define directive  define(key_name[TYPE,array_size], ... , ... )  TYPE=R4/R8/I4/I8/L1/Cn  */
static int wb_define(WhiteBoard *WB, FILE *infile, char *package){
   unsigned char name[WB_MAXNAMELENGTH+1];
   unsigned char type[WB_MAXNAMELENGTH+1];
   unsigned char length[WB_MAXNAMELENGTH+1];
   unsigned char Token[MISC_BUFSZ];
   int ntoken;
   int key_type,len_type,nread,array_length,options=default_dict_option;
   char key_c;
   int opt_cmd=0;
   int desc_cmd=0;
   int status=WB_OK;

   ntoken=wb_get_token(Token,infile,2,0) ; /* get (  */
   if(Token[0]!='(' || ntoken!=1) goto error_syntax ;

   strncpy((char *)name,package,strlen(package));
   ntoken=wb_get_token(name+strlen(package), infile, sizeof(name)-strlen(package), 0) ; /* expect keyname  */
   if( !isalpha(name[strlen(package)]) || ntoken<=0 ) goto error_syntax ;

   ntoken=wb_get_token(Token,infile,2,0) ; /* expect [  */
   if(Token[0]!='[' || ntoken!=1) goto error_syntax ;

   ntoken=wb_get_token(type,infile,sizeof(type),0) ; /* expect entry type and length  */
   /*if( !isalpha(type[0]) || ntoken<=0 ) goto error_syntax ;   validate type and length */
   sscanf((char *)type,"%c%d%n",&key_c,&len_type,&nread);
   switch(key_c) {          /* get type code */
      case 'R' : key_type=WB_FORTRAN_REAL ; break ;
      case 'I' : key_type=WB_FORTRAN_INT ; break ;
      case 'C' : key_type=WB_FORTRAN_CHAR ; break ;
      case 'L' : key_type=WB_FORTRAN_BOOL ; break ;
      default: goto error_syntax ;
      }

   if(nread != ntoken ) goto error_syntax ;
   if( (key_type=get_typecode(key_type,len_type,len_type)) < 0 ) goto error_syntax ;  /* check type and length combination */

   ntoken=wb_get_token(Token,infile,2,0) ; /* expect , */
   if(Token[0]!=',' || ntoken!=1) goto error_syntax ;

   ntoken=wb_get_token(length,infile,sizeof(length),0) ; /* expect array length  */
   if( !isdigit(length[0]) || ntoken<=0 ) goto error_syntax ;
   sscanf((char *)length,"%d%n",&array_length,&nread);
   if(nread != ntoken ) goto error_syntax ;

   ntoken=wb_get_token(Token,infile,2,0) ; /* expect ]  */
   if(Token[0]!=']' || ntoken!=1) goto error_syntax ;

   ntoken=wb_get_token(Token,infile,2,0) ; /* expect ) or ,  */
   if( (Token[0]!=')' && Token[0]!=',') || ntoken!=1) goto error_syntax ;

   if( message_level<=WB_MSG_DEBUG )
      fprintf(stderr,"type=%c,key_type=%d,len=%d,array_length=%d\n",key_c,key_type,len_type,array_length);
   while(Token[0] != ')' ) {
      ntoken=wb_get_token(Token,infile,WB_MAXNAMELENGTH,0) ; /* expect subcommand name OPT or DESC  */
      if( strcmp("OPT",(char *)Token)==0 && opt_cmd==0 ) {   /* OPT= subcommand */
         ntoken=wb_get_token(Token,infile,2,0) ; /* expect =  */
         if(Token[0]!='=' || ntoken!=1) goto error_syntax ;
         options =  wb_options(infile,'[',']',dict_options);
         opt_cmd++;
         }
      else if( strcmp("DESC",(char *)Token)==0 && desc_cmd==0 ) {   /*DESC= subcommand */
         ntoken=wb_get_token(Token,infile,2,0) ; /* expect =  */
         if(Token[0]!='=' || ntoken!=1) goto error_syntax ;
         ntoken=wb_get_token(Token,infile,MISC_BUFSZ,0) ; /* expect quoted string  */
         if(Token[0]!='\'' && Token[0]!='"') goto error_syntax ;
         desc_cmd++;  /* DESC is ignored for now, just counted */
         }
      else {
         if( message_level<=WB_MSG_ERROR )
            fprintf(stderr,"invalid/undefined define subcommand\n");
         goto error_syntax ;
         }
      ntoken=wb_get_token(Token,infile,WB_MAXNAMELENGTH,0) ; /* expect ) or ,  */
      if( (Token[0]!=')' && Token[0]!=',') || ntoken!=1) goto error_syntax ;
      }
   status=c_wb_put(WB,name,key_type,len_type,Token,array_length,options|WB_CREATE_ONLY,strlen((char *)name));
   if(status<0) goto error_syntax;                 /* put failed for some reason */
   status=wb_define_check(LastPutLine,1);  /* must not already be in table */
   if(status<0) goto error_syntax;                 /* already defined in table, OOPS */
   wb_flush_line(infile);
   return(status);
error_syntax:
   wb_read_error() ;
   wb_flush_line(infile);
   WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_SYNTAX);
}

/* process key= directive from file Token contains key name, package contains package prefix string */
static int wb_key(WhiteBoard *WB, FILE *infile,unsigned char *Token, char *package, int Options){
   int status=WB_OK;
   int err_not_found=1;
   int elementtype,elementsize,elements,ntoken,nread,items;
   LINE *line;
   PAGE *page;
   unsigned char name[WB_MAXNAMELENGTH+1];
   unsigned char separator[3];
   unsigned char buffer[WB_MAXSTRINGLENGTH+3];  /* add the two delimiters and the null terminator */
   unsigned char *dataptr;

   strncpy((char *)name,package,strlen(package));   /* concatenate package name with keyname */
   strncpy((char *)name+strlen(package),(char *)Token,sizeof(name)-strlen(package));
   name[WB_MAXNAMELENGTH] = 0;

   if(message_level<=WB_MSG_INFO) fprintf(stderr,"Assigning to '%s'\n",name);
   extra_error_message=name;
   ntoken=wb_get_token(Token,infile,2,0) ; /* expect =  */
   if(Token[0]!='=' || ntoken!=1) {
      if( message_level<=WB_MSG_ERROR )
         fprintf(stderr,"= sign not found where expected \n");
      goto error_syntax ;
      }

   status = c_wb_lookup(WB,name,&elementtype,&elementsize,&elements,&line,&page,err_not_found,strlen((char *)name));
   if(status<0) goto error_syntax;  /* key MUST be defined */

   if( Options==WB_STRICT_DICTIONARY )
      status = wb_define_check(line,0);    /* must be found in table if in strict dictionary mode */
   else
      status = wb_define_check(line,2);    /* must be inserted in table if in normal mode */
   if( status<0 ) goto error_syntax;               /* other source of error : key already assigned a value in this file */

   if(elements == 0) elements = 1;           /* scalar, 1 item */
      if(line->m.flags.array)                /* array data */
         dataptr = &((line+1)->d.data[0]);
      else                                   /* scalar data */
         dataptr = &(line->m.data.c[0]);
   while(elements--){                        /* get values for up to max items for key */
      ntoken=wb_get_token(buffer,infile,sizeof(buffer)-1,0);
      if( ntoken <= 0 ) goto error_syntax ;

      switch(elementtype){
         case(WB_FORTRAN_INT):
            if(elementsize==4){              /* 4 byte integer */
               int *target = (int *)dataptr;
               items = sscanf((char *)buffer,"%d%n",target,&nread);
               if(nread != ntoken || items != 1) goto error_syntax ;
               }
            else if(elementsize==8){         /* 8 byte integer */
               long long *target = (long long *)dataptr;
               items = sscanf((char *)buffer,"%lld%n",target,&nread);
               if(nread != ntoken || items != 1) goto error_type ;
               }
            else
               goto error_syntax;
            break;
         case(WB_FORTRAN_REAL):
            if(elementsize==4){              /* 4 byte real */
               float *target = (float *)dataptr;
               items = sscanf((char *)buffer,"%E%n",target,&nread);
               if(nread != ntoken || items != 1) goto error_syntax ;
               }
            else if(elementsize==8) {        /* 8 byte real */
               double *target = (double *)dataptr;
               items = sscanf((char *)buffer,"%lE%n",target,&nread);
               if(nread != ntoken || items != 1) goto error_syntax ;
               }
            else
               goto error_syntax;
            break;
         case(WB_FORTRAN_BOOL):
            if(elementsize==4){              /* FORTRAN logical */
               if( strncmp(".FALSE.",(char *)buffer,7) == 0 || strncmp(".F.",(char *)buffer,3) == 0 ) *dataptr = 0;
               else if( strncmp(".TRUE.",(char *)buffer,6) == 0 || strncmp(".T.",(char *)buffer,3) == 0 ) *dataptr = 1;
               else goto error_type;
               }
            else
               goto error_syntax;
            break;
         case(WB_FORTRAN_CHAR): /* copy string in buffer into target, mit firs and last character, the quotes */
            nread = c_fortran_string_copy(buffer+1, dataptr, strlen((char *)buffer)-2, elementsize, ' ');
            if(nread < 0) goto error_string;
            break;
         default:
            goto error_syntax;
         }
      dataptr += elementsize;

      ntoken=wb_get_token(separator,infile,sizeof(separator)-1,0);   /* separator must be either comma or newline */
      if( ntoken != 1 ) goto error_syntax;
      if( separator[0] == ',' ) continue ;
      if( separator[0] == '\n' ) break ;
      else                      goto error_syntax;
      }
   if( separator[0] != '\n' ) goto error_toomany;
   line->m.flags.initialized = 1 ;  /* mark entry as initialized */
   line->m.flags.badval = 0 ;
   return(status);
error_toomany:
   if(message_level<=WB_MSG_ERROR) {
      fprintf(stderr,"attempting to assign too many values to %s (type %s)\n",name,datatypes[elementtype]);
      goto error_syntax;
      }
error_string:
   if(message_level<=WB_MSG_ERROR) {
      fprintf(stderr,"string %s is too long to be assigned to %s (length=%d>%d)\n",buffer,name,strlen((char *)buffer)-2,elementsize);
      goto error_syntax;
      }
error_type:
   if(message_level<=WB_MSG_ERROR) {
      fprintf(stderr,"type mismatch error while assigning value %s to %s (type %s)\n",buffer,name,datatypes[elementtype]);
      goto error_syntax;
      }
error_syntax:
   wb_read_error() ;
   wb_flush_line(infile);
   WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_SYNTAX);
}

/* read a dictionary or user directive file */

int c_wb_read(WhiteBoard *WB, char *filename, char *package, char *section, int Options, int Lfilename, int Lpackage, int Lsection){
   DEFTBL mytable[MISC_BUFSZ];
   char localbuffer[MISC_BUFSZ];
   char localfname[MISC_BUFSZ];
   unsigned char Token[MISC_BUFSZ];
   char Package[WB_MAXNAMELENGTH];
   char Section[WB_MAXNAMELENGTH];
   int i,status;
   FILE *infile;
   int ntoken;
   int temp;
   int version=999,items,nread;
   int errors=0;

   extra_error_message = " invalid whiteboard instance";
   if(WB == DummyWhiteboardPtr) WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_NOTFOUND);
   if(WB == NULL) WB=BaseWhiteboardPtr;

   TRIM(filename,Lfilename)
   TRIM(package,Lpackage)
   TRIM(section,Lsection)
   linebuffer=localbuffer;
   definition_table=mytable; definition_table_entries=0; max_definition_table_entries=MISC_BUFSZ;  /* initialize definition table control */
   for( i=0 ; i<MISC_BUFSZ ; i++) { definition_table[i].line = NULL ; definition_table[i].assigned = 0 ; definition_table[i].defined = 0 ; }

   for( i=0 ; i<Lfilename && i<MISC_BUFSZ-1 && filename[i]!=0 ; i++) localfname[i] = filename[i];
   localfname[i] = 0;  /* filename collected */
   if(message_level<=WB_MSG_DEBUG) fprintf(stderr,"localfname='%s'\n",localfname);

   for( i=0 ; i<Lpackage && i<WB_MAXNAMELENGTH-1 && package[i]!=0 ; i++) Package[i] = toupper(package[i]);
   Package[i] = 0;     /* package name collected */
   if(message_level<=WB_MSG_DEBUG) fprintf(stderr,"Package='%s'\n",Package);

   for( i=0 ; i<Lsection && i<WB_MAXNAMELENGTH-1 && section[i]!=0 ; i++) Section[i] = toupper(section[i]);
   Section[i] = 0;     /* section_name  collected */
   if(message_level<=WB_MSG_DEBUG) fprintf(stderr,"Section='%s'\n",Section);

   extra_error_message = localfname;  /* add directive file name to error message */
   current_char = NULL;               /* make sure that no previous input is left in buffers */
   infile=fopen(localfname,"r");      /* try to open file */
   if(infile==NULL) WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_READ);  /* OOPS, cannot open/read file */

   extra_error_message = NULL;

   while(1){   /* loop until desired section "@xxxx" is found or end of directive file */
      temp = wb_get_nonblank(infile);
      while( temp!= '@' && temp!= EOF ) {   /* look for @ as first nonblank character in line (or End of file) */
         temp = wb_get_line(infile);
         temp = wb_get_nonblank(infile);
         }
      if(temp==EOF) {                    /* end of file or error , requested section not found */
         fclose(infile);
         extra_error_message = Section;  /* add section name to error message */
         WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_NOTFOUND);
         }
      ntoken = wb_get_token(Token,infile,MISC_BUFSZ-1,1);
      if( strncmp((char *)Token,Section,strlen(Section))==0 ) break;   /* exit loop if requested section is found */
      }
   if( message_level<=WB_MSG_INFO )fprintf(stderr,"INFO: directive section %s found\n",Section);

   /* we have found our section, let's process it */
   temp = wb_get_line(infile);     /* get rid of rest of @section line */
   default_dict_option = 0;                /* default options = none unless there is a directive */
   ntoken = wb_get_token(Token,infile,MISC_BUFSZ-1,0);
   while( strncmp((char *)Token,"@",1) && ntoken!=WB_ERROR ) {  /* loop until end of section (beginning of next section or EOF) */
      if( strncmp((char *)Token,"OPTIONS",7)==0 ) {   /* default options directive */
         default_dict_option = wb_options(infile,'(',')',dict_options);
         }
      else if( strncmp((char *)Token,"MESSAGES",8)==0 ) {   /* verbosity control directive */
         message_level = wb_options(infile,'(',')',verb_options);
         }
      else if( strncmp((char *)Token,"DEFINE",6)==0 && Options!=WB_FORBID_DEFINE ) {  /* define directive (if allowed) */
         status = wb_define(WB,infile,Package);
         if(status<0) errors++;
         }
      else if( isalpha(Token[0]) ){   /* must be key= */
         status = wb_key(WB,infile,Token,Package,Options);
         if(status<0) errors++;
         }
      else if( Token[0] == '\n' ) {  /* ignore newlines */
         temp = '\n';
         }
      else{
         if( message_level<=WB_MSG_ERROR )
            fprintf(stderr,"Unexpected token found at beginning of directive: '%s'\n",Token);
         errors++;
         wb_flush_line(infile);
         }
      ntoken = wb_get_token(Token,infile,MISC_BUFSZ-1,0);   /* get next token */
      }
   fclose(infile);    /* we are done, close file and return success */
   if( message_level<=WB_MSG_INFO || ( message_level<=WB_MSG_ERROR && errors>0 ) )
      fprintf(stderr,"INFO: %d error(s) detected\n",errors);
   return( errors==0 ? WB_OK : WB_ERROR);
}

/* read a dictionary or user directive file (FORTRAN version) */
wordint f77_name(f_wb_read)(WhiteBoard **WB, char *package, char *filename, char *section, wordint *options, F2Cl lpackage, F2Cl lfilename,  F2Cl lsection){
   int Lfilename=lfilename;
   int Lpackage=lpackage;
   int Lsection=lsection;
   int Options=*options;

/*
   if(*WB != NULL) fprintf(stderr,"INFO: files can only be read into MAIN WhiteBoard, option IGNORED\n");
*/
   return(c_wb_read(*WB, filename, package, section, Options, Lfilename, Lpackage, Lsection));
}

void f77_name(c_wb_test)() {
   int status,myint,myint2;
   long long myll,myll2;
   float myreal,myreal2;
   double mydouble,mydouble2;
   int integer_array[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25};
   int integer_array2[25];
   char *string1="this is string 1";
   char string2[32];
   WhiteBoard *WB=NULL;

   message_level = WB_MSG_INFO;
   status=WB_PUT_C(WB,"string1",string1,strlen(string1),0+WB_REWRITE_UNTIL);
   printf("status=%d\n",status);
   myint = 12; myll=1212;
   myreal=12.12; mydouble=2424.68;
   status=WB_PUT_I4(WB,"valeur1",&myint,WB_CREATE_ONLY);
   printf("status=%d\n",status);
   status=WB_PUT_I8(WB,"ll1",&myll,0+WB_READ_ONLY_ON_RESTART);
   printf("status=%d\n",status);
   status=WB_PUT_R4(WB,"reel1",&myreal,0+WB_READ_ONLY_ON_RESTART);
   printf("status=%d\n",status);
   status=WB_PUT_R8(WB,"dble1",&mydouble,0+WB_REWRITE_UNTIL);
   printf("status=%d\n",status);
   myint = -134;myll=-134431;
   myreal=-13.45; mydouble=-12345.6789;
   status=WB_PUT_I4(WB,"valeur2",&myint,0+WB_REWRITE_UNTIL);
   printf("status=%d\n",status);
   status=WB_PUT_I8(WB,"ll2",&myll,0);
   printf("status=%d\n",status);
   status=WB_PUT_R4(WB,"reel2",&myreal,0);
   printf("status=%d\n",status);
   status=WB_PUT_R8(WB,"dble2",&mydouble,0);
   printf("status=%d\n",status);
printf("========\n");
   status=c_wb_check(WB,(unsigned char *)"", -1, 0, 1, NULL,NULL);
   printf("c_wb_check printed %d entries\n",status);
printf("========\n");
   printf("before c_wb_lock\n");
   c_wb_lock(WB,(unsigned char *)"D",1);

   c_wb_checkpoint();

   BaseWhiteboardPtr->PageChain=NULL;
   c_wb_reload();

   status=WB_GET_C(WB,"string1",string2,32);
   string2[31]=0;
   printf("status=%d, string2='%s'\n",status,string2);
   myint2 = -1; myll2=-1;
   myreal2=-1.0; mydouble2=-1.1111111111;
   status=WB_GET_I4(WB,"valeur1",&myint2);
   printf("status=%d, myint2=%d\n",status,myint2);
   status=WB_GET_I8(WB,"ll1",&myll2);
   printf("status=%d, myll2=%lld\n",status,myll2);
   status=WB_GET_R4(WB,"reel1",&myreal2);
   printf("status=%d, myreal2=%lf\n",status,myreal2);
   status=WB_GET_R8(WB,"dble1",&mydouble2);
   printf("status=%d, mydouble2=%f\n",status,mydouble2);
   myint2 = -1; myll2=-1;
   myreal2=-1.0; mydouble2=-1.1111111111;
   status=WB_GET_I4(WB,"valeur2",&myint2);
   printf("status=%d, myint2=%d\n",status,myint2);
   status=WB_GET_I8(WB,"ll2",&myll2);
   printf("status=%d, myll2=%lld\n",status,myll2);
   status=WB_GET_R4(WB,"reel2",&myreal2);
   printf("status=%d, myreal2=%f\n",status,myreal2);
   status=WB_GET_R8(WB,"dble2",&mydouble2);
   printf("status=%d, mydouble2=%lf\n",status,mydouble2);
printf("========\n");
   status=WB_PUT_I4V(WB,"intarray1",integer_array,25,0);
   printf("status=%d\n",status);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,5);
   printf("status=%d, integer_array2[4]=%d\n",status,integer_array2[4]);

   c_wb_checkpoint();

   BaseWhiteboardPtr->PageChain=NULL;
   c_wb_reload();

printf("========\n");
   status=WB_PUT_I4V(WB,"intarray1",integer_array,31,0);
   printf("status=%d\n",status);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,5);
   printf("status=%d, integer_array2[4]=%d\n",status,integer_array2[4]);
   status=c_wb_check(WB,(unsigned char *)"", -1, 0, 1, NULL,NULL);
   printf("c_wb_check printed %d entries\n",status);
printf("========\n");
   status=WB_PUT_I4V(WB,"intarray1",integer_array,10,0);
   printf("status=%d\n",status);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,5);
   printf("status=%d, integer_array2[4]=%d\n",status,integer_array2[4]);
printf("========\n");
   status=WB_GET_I4V(WB,"intarray1",integer_array2,15);
   printf("status=%d, integer_array2[14]=%d\n",status,integer_array2[14]);
printf("==== Locking Test ====\n");
   status=c_wb_check(WB,(unsigned char *)"V", -1, 1, 1, Action1,NULL);
   printf("c_wb_check printed %d entries\n",status);
   c_wb_lock(WB,"V",1);
   status=c_wb_check(WB,(unsigned char *)"V", -1, 1, 1, Action1,NULL);
   printf("c_wb_check printed %d entries\n",status);
printf("==== END of Locking Test ====\n");
}
