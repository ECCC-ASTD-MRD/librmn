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

// #include <rpnmacros.h>
#include "rpnmacros.h"

#include "WhiteBoard.h"


//------------------------------------------------------------------------------
// Function like macros
//------------------------------------------------------------------------------


//! Find trimmed (non blank) length of a Fortran string
//
//! @param[in]     string String of which we want to find the trimmed length
//! @param[in,out] length Must be set to the maximum possible length on call and will be updated to the trimmed length
#define TRIM(string,length) { while( length > 0 && string[length - 1] == ' ' ) length-- ; }

//! Error exit macro
#define WB_ERR_EXIT(level,code)  return ( wb_error(level, code) )

//------------------------------------------------------------------------------
// Types
//------------------------------------------------------------------------------


//! Pointer to a function that operates on a line and receives extra data with it's second argument
//! This allows the "map" functionnal programming technique
typedef int (*LineWorker)(wb_line *, void *);

typedef WhiteBoard *WhiteBoardPtr;

typedef struct {
    char *name;
    int nameLength;
    int userMaxLabels;
} wb_keys;

//! MPI broadcast information
typedef struct {
    wordint pe_root;
    wordint pe_me;
    char *domain;
    F2Cl ldomain;
    void (*broadcast_function)();
    void (*allreduce_function)();
} wb_mpi_broadcast;


//------------------------------------------------------------------------------
// Data
//------------------------------------------------------------------------------


//! Default options when creating variables (used by Whiteboard read)
static int default_dict_option = 0;

//! Table of variable creation options (used by Whiteboard read)
static wb_symbol dict_options[] = {
    {WB_IS_LOCAL, "WB_IS_LOCAL"},
    {WB_REWRITE_AT_RESTART, "WB_REWRITE_AT_RESTART"},
    {WB_REWRITE_NONE, "WB_REWRITE_NONE"},
    {WB_REWRITE_UNTIL, "WB_REWRITE_UNTIL"},
    {WB_REWRITE_MANY, "WB_REWRITE_MANY"},
    {WB_READ_ONLY_ON_RESTART, "WB_READ_ONLY_ON_RESTART"},
    {0, NULL}
};

//! Table for verbosity options (used by Whiteboard read)
static wb_symbol verb_options[] = {
    {WB_MSG_DEBUG, "WB_MSG_DEBUG"},
    {WB_MSG_INFO, "WB_MSG_INFO"},
    {WB_MSG_WARN, "WB_MSG_WARN"},
    {WB_MSG_ERROR, "WB_MSG_ERROR"},
    {WB_MSG_SEVERE, "WB_MSG_SEVERE"},
    {WB_MSG_FATAL, "WB_MSG_FATAL"},
    {0, NULL}
};

//! Error message table
static wb_symbol errors[] = {
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
};

// THIS MUST REMAIN CONSISTENT WITH Whiteboard.h
static char *datatypes[] = {
    "    ",
    "Real",
    "Int ",
    "Char",
    "Bool"
};

//! dummy whiteboard instance, returned when freeing a whiteboard
static WhiteBoard DummyWhiteboard;
static WhiteBoardPtr DummyWhiteboardPtr = &DummyWhiteboard;
//! Permanent whiteboard instance, the only one that can be checkpointed
static WhiteBoard BaseWhiteboard;
static WhiteBoardPtr BaseWhiteboardPtr = &BaseWhiteboard;

//! Verbosity level
static int message_level = WB_MSG_WARN;

//! Line buffer used to process directive files
//! @todo Figure out if this needs to have file scope!
static char *linebuffer = NULL;
//! Pointer to current character in directive file
static char *current_char = NULL;

//! Pointer to defionition table
static wb_definition *definition_table = NULL;
//! Number of valid entries in definition table
static int definition_table_entries = 0;
static int max_definition_table_entries = 0;

static char text_of_last_error[WB_MAX_ETRA_ERROR_LEN];
static char *extra_error_message = NULL;
static char extra_error_buffer[WB_MAX_ETRA_ERROR_LEN];

static char *WhiteBoardCheckpointFile = "Whiteboard.ckpt";

//! Address of the line that was the target of the last put/create operation. Used by wb_read to avoid a costly lookup
static wb_line *wb_lastputline = NULL;


//------------------------------------------------------------------------------
// Prototypes
//------------------------------------------------------------------------------
static int new_page(WhiteBoard *WB, int nlines);

int c_wb_reload();

//! @todo Replace these with iso_c_binding
void f77_name(f_logical_move)(void *, void *, wordint *);
void f77_name(f_logical2int)(void *, void *, wordint *);
void f77_name(f_int2logical)(void *, void *, wordint *);

static wb_mpi_broadcast wb_broadcast_cfg;


//------------------------------------------------------------------------------
// Functions
//------------------------------------------------------------------------------


//! Set verbosity level (C callable)
//
//! @param[in] level New verbosity level
//
//! @return The previously let level
int c_wb_verbosity(int level)
{
    int old_level = message_level;
    message_level = level;
    return old_level;
}


//! Set verbosity level (FORTRAN callable)
wordint f77_name(f_wb_verbosity)(wordint *level)
{
   wordint old_level = message_level;
   message_level = *level;
   return old_level;
}


//! Manage definition_table (used by whiteboard read routine)
//
//! @param[in] line Line to process
//! @param[in] define_mode Processing mode for the line: 0 for a simple define, 1 key = value in strict dictionary mode, 2 key = value in normal mode
//
//! @return Position in the defintion table
static int wb_define_check(wb_line *line, int define_mode)
{
    int i;

    for (i = 0; i < definition_table_entries; i++) {
        if (definition_table[i].line == line) {
            // A match has been found

            if (define_mode == 1) {
                // Found when it should not (we are processing a define)
                return WB_ERR_REDEFINE;
            }

            if ( definition_table[i].assigned ) {
                // Already assigned to, should not be assigned again
                if (message_level <= WB_MSG_ERROR) {
                    fprintf(stderr, "ERROR: key value already assigned in this directive file \n");
                }
                return WB_ERR_REDEFINE;
            }
            definition_table[i].assigned = 1 ;    /* flag assignation */

            return i;
         }
    }
    if (define_mode == 0) {
        // Not in define mode, strict dictionary mode, not found, fail
        return WB_ERR_NOTFOUND;
    }

    if (definition_table_entries >= max_definition_table_entries) {
        // Definition table is full
        if (message_level <= WB_MSG_ERROR) {
            fprintf(stderr, "ERROR: too many keys appear in this directive file \n");
        }
        return WB_ERROR;
    }

    // Create and initialize entry
    definition_table[definition_table_entries].line = line;
    // define(...)
    definition_table[definition_table_entries].defined = define_mode == 1 ? 1 : 0;
    // key=...
    definition_table[definition_table_entries].assigned = define_mode != 1 ? 1 : 0;
    // Return position
    return definition_table_entries++;
}


//! Create a new whiteboard instance
//
//! @return Pointer to the new WhiteBoard
WhiteBoard *c_wb_new() {
    int status;
    WhiteBoard *newWb = (WhiteBoard *)malloc(sizeof(WhiteBoard));

    if (newWb == NULL) {
        // Memory allocation failed
        return NULL;
    }

    newWb->firstpage = NULL;
    newWb->validpages = 0;
    status = new_page(newWb, WB_MAXLINESPERPAGE);
    if (status < 0) {
        free(newWb);
        return NULL;
    }
    return newWb;
}


//! Create a new WhiteBoard instance
//
//! @param[out] wb New WhiteBoard instance
//
//! @return WB_OK on scuccess or WB_ERROR if the creation failed
wordint f77_name(f_wb_new)(WhiteBoard **wb){
    *wb = c_wb_new();
    return *wb == NULL ? WB_ERROR : WB_OK;
}


//! Delte a whiteboard instance
int c_wb_free(WhiteBoard *wb) {
    wb_page *temp, *temp2;

    if (wb == NULL) {
        // Global whiteboard page chain is never freed
        return WB_OK;
    }

    if (wb-> validpages <= 0) {
        // Invalid whiteboard, negative or zero number of valid pages
        return WB_ERROR;
    }
    wb->validpages = 0;
    temp = wb->firstpage;
    // Free all pages for this whiteboard
    while (temp != NULL) {
        // Keep copy of pointer to current page
        temp2 = temp;
        // Pointer to next page in page chain
        temp = temp->next;
        // Free current page
        free(temp2);
    }
    free(wb);
    return WB_OK;
}


//! @copydoc c_wb_free
wordint f77_name(f_wb_free)(WhiteBoard **wb) {
    wordint status = c_wb_free(*wb);
    // Set whiteboard address to address of dummy whiteboard, so that we can trap it
    *wb = DummyWhiteboardPtr;
    return status;
}


//! Get integer value associated with a string(symbol) from a symbol table
//
//! @param[in] symbol Symbol to search in the table
//! @param[in] table  Table pointer
//
//! @return Value of the symbol if found, 0 otherwise
static int wb_value(char *symbol, wb_symbol *table)
{
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "looking for value of '%s', ", symbol);
    }
    while (table->text != NULL) {
        if (strcmp(table->text, symbol) == 0) {
            if (message_level <= WB_MSG_DEBUG) {
                fprintf(stderr, "found %d\n", table->code);
            }
            return table->code;
        }
        table++;
    }
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "found NONE\n");
    }
    // No value found in table
    return 0;
}

//! Default user error handler, do nothing
static void null_error_handler()
{
    return ;
}

//! Pointer to the user's FORTRAN handler routine, defaults to internal null_error_handler
//! The error handler receives two pointers to wordint variables
static void (*errorHandler)() = &null_error_handler;


//! Set the error handler
//
//! @param[in] userErrorHandler Pointer to the error handler function
void f77_name(f_wb_error_handler)( void (*userErrorHandler)() )
{
    errorHandler = userErrorHandler;
}


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
    wb_symbol *message = &errors[0];
    wordint Severity = severity;
    wordint Code = code;

    // Do nothing if there is no error or it's below the severity treshold
    if (code == WB_OK || severity < message_level) {
        return code;
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
    (*errorHandler)(&Severity ,&Code);
    return code;
}


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
static int c_fortran_string_copy(char *src, char *dst, int src_len, int dst_len, char pad)
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
wordint f77_name(fortran_string_copy)(char *src, char *dst, F2Cl src_len, F2Cl dst_len)
{
   int Ldst = dst_len;
   int Lsrc = src_len;
   return c_fortran_string_copy(src, dst, Lsrc, Ldst, ' ');
}
#endif


//! Initialize a line with a name coming from a WB_FORTRAN string
//
//! @param[out] line   Line instance pointer
//! @param[in]  name   Name
//! @param[in]  length Length of name
static void fill_line(wb_line *line, char *name, int length)
{
    int pos;
    char c;

    // Fill to WB_MAXNAMELENGTH with blanks (FORTRAN style)
    for (pos = 0; pos < WB_MAXNAMELENGTH; pos++) {
        c = (pos < length) ? name[pos] : ' ';
        // Force uppercase
        line->meta.name.carr[pos] = toupper(c);
    }
    // Force NULL terminator
    line->meta.name.carr[WB_MAXNAMELENGTH] = 0;
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
static int wb_match_name(char *str1, char *str2, int length) {
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
//! @param[in] line        Line to search
//! @param[in] page        Page in which to search
//! @param[in] errNotFound If this is not null, throw an error if the line is not found
//
//! @return Line number if found, error otherwise
static int lookup_line(wb_line *line, wb_page *page, int errNotFound)
{
    if (page != NULL) {
        wb_line *pageline;
        int i = 0;
        while (i < page->firstFreeLine) {
            pageline = &(page->line[i]);
            if (wb_match_name(line->meta.name.carr, pageline->meta.name.carr, WB_MAXNAMELENGTH)) {
                // Found, return it's position in page
                return i;
            }

            if (pageline->meta.flags.lines == 0) {
                // Only one entry in this page in this case
                break;
            }
            i += pageline->meta.flags.lines;
        }
    }
    if (errNotFound) {
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
    wb_page *page;
    int pagesize;

    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "allocating new page for %d lines \n", nlines);
    }
    // note: sizeof(wb_page) gives the size of a page containing WB_MAXLINESPERPAGE lines
    // adjustment for actual number of lines in page has to be done to allocate correct size
    pagesize = sizeof(wb_page) + sizeof(wb_line) * (nlines - WB_MAXLINESPERPAGE);
    page = (wb_page *)malloc(pagesize);
    if (page == NULL) {
        // malloc failed!
        WB_ERR_EXIT(WB_MSG_FATAL, WB_ERR_ALLOC);
    }
    // Zero page contents
    memset(page, 0, pagesize);

    // Next free entry is first entry in page
    page->firstFreeLine = 0;

    // Set the number of data/metadata lines that the page can contain
    page->nbLines = nlines;
    // Update the page chain
    page->next = wb->firstpage;
    // Insert the new page at the beginning of the chain
    wb->firstpage = page;
    wb->validpages++;
    return 0;
}


//! Initialize BaseWhiteboardPtr
//
//! The content will be read from WhiteBoardCheckpointFile if it is found
static int wb_init()
{
    int status;

    if (BaseWhiteboardPtr->firstpage == NULL) {
        int fd = open(WhiteBoardCheckpointFile, O_RDONLY);
        if (fd >= 0) {
            close(fd);
            if (message_level <= WB_MSG_INFO) {
                fprintf(stderr, "whiteboard checkpoint file found, loading it\n");
            }
            return c_wb_reload();
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
    return wb_init();
}


//! Set checkpoint file name
//
//! @param[in] filename       File name
//! @param[in] filenameLength Length of the file name
//
//! @return 0 on success or error code otherwise
wordint f77_name(f_wb_checkpoint_name)(char *filename, F2Cl filenameLength)
{
    int Lfilename = filenameLength;
    extra_error_message = "Setting chekpoint file name";
    WhiteBoardCheckpointFile = (char *)malloc(Lfilename + 1);
    if (WhiteBoardCheckpointFile == NULL) {
        WB_ERR_EXIT(WB_MSG_FATAL, WB_ERR_ALLOC);
    }
    c_fortran_string_copy(filename, WhiteBoardCheckpointFile, Lfilename, Lfilename, '\0');
    return wb_init();
}


//! Get checkpoint file name
//
//! @param[out] filename       File name
//! @param[in]  filenameLength Maximum length for the file name
//
//! @return 0 on success or error code otherwise
int c_wb_checkpoint_get_name(char *filename, int Lfilename)
{
    extra_error_message = "NO checkpoint file found while getting chekpoint file name";
    if (WhiteBoardCheckpointFile == NULL) {
        WB_ERR_EXIT(WB_MSG_FATAL, WB_ERR_ALLOC);
    }
    strncpy(filename, WhiteBoardCheckpointFile, Lfilename);
    return wb_init();
}


//! Get checkpoint file name
//
//! @param[out] filename       File name
//! @param[in]  filenameLength Maximum length for the file name
//
//! @return 0 on success or error code otherwise
wordint f77_name(f_wb_checkpoint_get_name)(char *filename, F2Cl filenameLength)
{
    int Lfilename = filenameLength;
    extra_error_message = "NO checkpoint file found while getting chekpoint file name";
    if (WhiteBoardCheckpointFile == NULL) {
        WB_ERR_EXIT(WB_MSG_FATAL,WB_ERR_ALLOC);
    }
    c_fortran_string_copy(WhiteBoardCheckpointFile, filename, Lfilename, Lfilename, ' ');
    return wb_init();
}


//! Add lines to a WhiteBoard
//
//! A new page may be added to the WhiteBoard if it's necessary to store the new lines
//
//! @param[in,out] wb     Whiteboard to use.  If this is null, the WhiteBoard pointed by BaseWhiteboardPtr wil be used
//! @param[out]    page   Pointer to the page pointer to set the page containing the new lines
//! @param[in]     nlines Number of lines to add
static wb_line *new_line(WhiteBoard *wb, wb_page **page, int nlines)
{
    wb_page *lookuppage;
    wb_line *result;
    int status;

    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }
    if (BaseWhiteboardPtr->firstpage == NULL) {
        // First time through
        status = wb_init();
        if (status < 0) {
            // Init failed!
            return NULL;
        }
    }
    // Start at beginning of pagechain
    lookuppage = wb->firstpage;

    // While page is not full
    while (lookuppage->firstFreeLine + nlines > lookuppage->nbLines) {
        if (lookuppage->next == NULL) {
            // Already last page, allocate a new page
            // Make sure allocated page is big enough
            status = new_page(wb, ( nlines > WB_MAXLINESPERPAGE ) ? nlines : WB_MAXLINESPERPAGE );
            if (status < 0) {
                // Allocation of new page failed
                return NULL;
            }
            // Potential target is new allocated page
            lookuppage = wb->firstpage;
            break;
        } else {
            // Look into next page
            lookuppage = lookuppage->next;
        }
    }
    result = &(lookuppage->line[lookuppage->firstFreeLine]);
    *page = lookuppage;
    return result;
}


//! Set line flags from the provided numeric value
//
//! @param[out] line    Line in which to set the flags
//! @param[in]  options Numeric value representing the active flags
//
//! @return WB_OK on success, error code otherwise
static int options_to_flags(wb_line *line, int options)
{
    line->meta.flags.array = (options & WB_IS_ARRAY) ? 1 : 0;
    // Not created by a restart
    line->meta.flags.fromrestart = (options & WB_CREATED_BY_RESTART) ? 1 : 0;
    line->meta.flags.readonly = (options & WB_REWRITE_NONE) ? 1 : 0;
    // Mark as bad value in case it fails unless it is a create only call
    line->meta.flags.badval = (options & WB_BADVAL) ? 1 : 0;
    // Variable has the local attribute, this will be used when checkpointing
    line->meta.flags.islocal = (options & WB_IS_LOCAL) ? 1 : 0;
    line->meta.flags.resetuntil = (options & WB_REWRITE_UNTIL) ? 1 : 0;
    line->meta.flags.resetmany = (options & WB_REWRITE_MANY) ? 1 : 0;
    line->meta.flags.noresetafterrestart = (options & WB_READ_ONLY_ON_RESTART) ? 1 : 0;
    line->meta.flags.initialized = (options & WB_INITIALIZED) ? 1 : 0;
    if (line->meta.flags.readonly + line->meta.flags.resetmany + line->meta.flags.resetuntil > 1) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_OPTION);
    }
    return WB_OK;
}


//! Get the numeric representation of the active flags of the provided line
//
//! @param[in] line Line from which to get the flags
//
//! @return Numeric representation of the active flags
static int flags_to_options(wb_line *line)
{
    int options = 0;
    if (line->meta.flags.array) options += WB_IS_ARRAY;
    if (line->meta.flags.fromrestart) options += WB_CREATED_BY_RESTART;
    if (line->meta.flags.readonly) options += WB_REWRITE_NONE;
    if (line->meta.flags.badval) options += WB_BADVAL;
    if (line->meta.flags.islocal) options += WB_IS_LOCAL;
    if (line->meta.flags.resetuntil) options += WB_REWRITE_UNTIL;
    if (line->meta.flags.resetmany) options += WB_REWRITE_MANY;
    if (line->meta.flags.noresetafterrestart) options += WB_READ_ONLY_ON_RESTART;
    if (line->meta.flags.initialized) {
        options += WB_INITIALIZED;
    } else {
        options += WB_NOTINITIALIZED;
    }
    return options;
}


//! Find line in WhiteBoard with it's name
//
//! @param[in]  wb          WhiteBoard in which to search.  If this is null, default to BaseWhiteboardPtr
//! @param[in]  name        Name to search for
//! @param[out] elementtype Type of the element(s)
//! @param[out] elementsize Size of each element in bytes
//! @param[out] elements    Number of elements in array, 0 if scalar
//! @param[out] result_line Line that bears the given name
//! @param[out] result_page Page in which the name was found
//! @param[in]  errNotFound If not 0, throw an error if the line is not found
//! @param[in]  nameLength   Length of the name
static int c_wb_lookup(WhiteBoard *wb, char *name, int *elementtype, int *elementsize, int *elements,
                        wb_line **result_line, wb_page **result_page, int errNotFound, int nameLength)
{
    wb_line line;
    int target_page = 0;
    int target_line = -1;
    wb_page *lookuppage = NULL;

    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }

    TRIM(name, nameLength)
    if (nameLength > WB_MAXNAMELENGTH) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NAMETOOLONG);
    }

    lookuppage = wb->firstpage;
    // Put key name into local line
    fill_line(&line, name, nameLength);
#ifdef DEBUG
    printf("target name = :%s:\n", &(line.meta.name.carr[0]));
#endif
    while (lookuppage != NULL) {
        // No screaming if not found, full length match
        target_line = lookup_line(&line, lookuppage, 0);
        if (target_line >= 0) {
            // A matching key has been found in this page
            *elementtype = lookuppage->line[target_line].meta.flags.type;
            *elementsize = lookuppage->line[target_line].meta.flags.elementsize;
            if (lookuppage->line[target_line].meta.flags.array == 1) {
                *elements = lookuppage->line[target_line].meta.data.desc.maxElements;
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
    if (errNotFound) {
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
//! @param[in]  nameLength   Length of the name
wordint f77_name(f_wb_get_meta)(WhiteBoard **wb, char *name, wordint *elementtype, wordint *elementsize,
                                       wordint *elements, wordint *options, F2Cl nameLength)
{
    wb_line *line;
    wb_page *page;
    int length = nameLength;
    int status;

    // No screaming if not found
    status = c_wb_lookup(*wb,name, elementtype, elementsize, elements, &line, &page, 0, length);
    if (status < 0) {
        return status;
    }

    *options = flags_to_options(line);
    return status;
}


//! Get the data associated with a whiteboard entry
//
//! @param[in]  wb          WhiteBoard in which to search.
//! @param[in]  name        Name of key (length MUST be supplied in nameLength)
//! @param[in]  type        Type represented by one character: R/I/L/C , key type real/inetger/logical/character
//! @param[in]  size        Size in bytes of each element 4/8 for R/I/L, 1->WB_MAXSTRINGLENGTH for character strings
//! @param[out] dest        Pointer to where data is written (everything considered as unsigned bytes)
//! @param[in]  nbelem      Number of elements that can be stored into value
//! @param[in]  nameLength Length of name
//
//! @return Dimension of whiteboard array if array, 0 if scalar, < 0 if error
int c_wb_get(WhiteBoard *WB, char *name, char type, int size, unsigned char *dest, int nbelem, int nameLength){
    wb_line *line;
    wb_page *page;
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

    TRIM(name, nameLength)
    set_extra_error_message(name, nameLength);

    if (type == WB_FORTRAN_BOOL && size == 1) {
        // Special case: force 4 byte container for FORTRAN logical type
        size = 4;
    }

    // Check validity of type / ltype combination
    typecode = get_typecode(type, size, WB_MAXSTRINGLENGTH);
    if (typecode < 0) {
        // Bad type / ltype combination
        return typecode;
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
    status = c_wb_lookup(WB, name, &element_type, &element_size, &nb_elements, &line, &page, 0, nameLength);
    if (status < 0) {
        WB_ERR_EXIT(WB_MSG_INFO, WB_ERR_NOTFOUND);
    }
    if (line->meta.flags.badval == 1) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BADVAL);
    }
    if (line->meta.flags.initialized != 1) {
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

    if (array && line->meta.flags.array != 1) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
    }

    // we can now proceed to copy the value(s) from whiteboard entry
    if (array) {
        if (nbelem > line->meta.data.desc.maxElements) {
            // Will do a short get
            nbelem = line->meta.data.desc.maxElements;
        }
        // if successful return max dimension of array
        result = line->meta.data.desc.maxElements;
        // Array data is always stored in lines following metadata
        csrc = (line + 1)->data.data;
    } else {
        // Scalar
        result = 0;
        // Scalar data is stored starting in metadata line
        csrc = line->meta.data.carr;
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
            int tempstat = c_fortran_string_copy((char *)csrc, (char *)dest, element_size, size, ' ');
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
wordint f77_name(f_wb_get)(WhiteBoard **wb, char *name, wordint *type, wordint *size, void *dest,
                           wordint *nbelem, F2Cl nameLength)
{
    // FIXME Check typing!  The compiler probably throws a buch of warnings for these!
    // FIXME Why copy to local variables prior to the function call for pass-by-value parameters?
    // FIXME Why not replace all of this with a proper iso_c_binding interface?
    int _nameLength = nameLength;
    int _type = *type;
    int _nbelem = *nbelem;
    char _size = *size;
    return c_wb_get(*wb, name, _type, _size, dest, _nbelem, _nameLength);
}


//! Store entry in a WhiteBoard
//
//! @param[in,out] wb          WhiteBoard in which to store.  If this is null, the WhiteBoard refrenced by DummyWhiteboardPtr is used.
//! @param[in]     name        Name of key (length MUST be supplied in nameLength)
//! @param[in]     type        Type represented by one character: R/I/L/C , key type real/inetger/logical/character
//! @param[in]     size        Size in bytes of each element 4/8 for R/I/L, 1->WB_MAXSTRINGLENGTH for character strings
//! @param[in]     src         Pointer to data asssociated with key (everything considered as unsigned bytes)
//! @param[in]     nbelem      Number of elements (0 means a scalar) (1 or more means an array)
//! @param[in]     options     Numeric representation of active flags
//! @param[in]     nameLength Name Length
//
//! @return Smaller than 0 in case of error.  0 for non character scalars, string length if string, max size of array if array
int c_wb_put(WhiteBoard *wb, char *name, char type, int size, unsigned char *src, int nbelem, int options,
             int nameLength)
{
    wb_line *line;
    wb_page *page;
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

    TRIM(name, nameLength)
    set_extra_error_message(name, nameLength);
    wb_lastputline = NULL;

    if (type == WB_FORTRAN_BOOL && size == 1) {
        // Special case, force 4 byte container for FORTRAN logical type
        size = 4;
    }

    // Check validity of requested Type/Ttype combination
    typecode = get_typecode(type, size, size);
    if (typecode < 0) {
        // Bad type/size combination
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
    lookup_result = c_wb_lookup(wb, name, &stored_type, &stored_size, &stored_nbelem, &line, &page, 0, nameLength);
    if (lookup_result >= 0) {
        // Entry has been found, will overwrite it, the options argument is ignored,
        // Flags are checked for consistency and permissions
        if (options & WB_CREATE_ONLY) {
            // Redefinition not allowed
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_REDEFINE);
        }
        // Above error has to be ignored if it was coming from a restart and create from restart flag then has to be erased
        if (line->meta.flags.readonly && line->meta.flags.initialized ) {
            // Entry is READONLY
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_READONLY);
        }
        // Mark as bad value in case it fails
        line->meta.flags.badval = 1;
        if (line->meta.flags.array == 1 && array != 1) {
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

        if (array &&  line->meta.data.desc.maxElements < nbelem) {
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
            lines += ((nbelem * size) + sizeof(wb_linedata) - 1 ) / sizeof(wb_linedata);
        }

        // Create new entry with space for lines
        line = new_line(wb, &page, lines);
        if (line == NULL) {
            // No need to use error macro, it will already have been called
            return WB_ERR_ALLOC;
        }

        // Set line name
        fill_line(line, name, nameLength);
        stored_size = size;
        if (typecode == WB_FORTRAN_CHAR && array == 0 ) {
            // Scalar string, round size up to wb_linedata size
            stored_size = ( (stored_size + sizeof(wb_linedata) - 1) / sizeof(wb_linedata) ) * sizeof(wb_linedata);
        }
        line->meta.flags.type = typecode;
        line->meta.flags.elementsize = stored_size;
        // Only one entry in page if lines>WB_MAXLINESPERPAGE
        line->meta.flags.lines = ( lines > WB_MAXLINESPERPAGE ) ? 0 : lines;
        line->meta.flags.readonly = (options & WB_REWRITE_NONE) ? 1 : 0;
        line->meta.flags.resetmany = (options & WB_REWRITE_MANY) ? 1 : 0;
        line->meta.flags.resetuntil = (options & WB_REWRITE_UNTIL) ? 1 : 0;
        if (line->meta.flags.readonly + line->meta.flags.resetmany + line->meta.flags.resetuntil > 1) {
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_OPTION);
        }
        line->meta.flags.initialized = 0;
        // Variable has the local attribute, this will be used when checkpointing
        line->meta.flags.islocal = (options & WB_IS_LOCAL) ? 1 : 0;
        // Mark as bad value in case it fails unless it is a create only call
        line->meta.flags.badval = (options & WB_CREATE_ONLY) ? 0 : 1;
        // Not created by a restart
        line->meta.flags.fromrestart = 0;
        line->meta.flags.noresetafterrestart = (options & WB_READ_ONLY_ON_RESTART) ? 1 : 0;
        line->meta.flags.array = array;
        // Arrays are not explicitely initialized for the time being
        if (array) {
            line->meta.data.desc.usedElements = nbelem;
            line->meta.data.desc.maxElements = nbelem;
        } else {
            // Scalar, set data to 0
            line->meta.data.iarr[0] = 0;
            line->meta.data.iarr[1] = 0;
        }
        // Check if there is enough space in page
        int lines_available = page->nbLines - page->firstFreeLine;
        if (lines_available < lines) {
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOMEM);
        }
        // Allocate space for data, adjust page pointer to next available entry
        page->firstFreeLine += lines;
    }

    wb_lastputline = line;
    if (options & WB_CREATE_ONLY) {
        // Create entry only, do not copy value(s)
        if (array) {
            result = line->meta.data.desc.maxElements;
        } else {
            result = 0;
        }
    } else {
        // Copy the value(s) into (old or new) whiteboard entry
        if (array) {
            // One dimensional array
            if (nbelem > line->meta.data.desc.maxElements) {
                // Not enough space to store values
                WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGDIMENSION);
            }
            // If successful, return max dimension of array
            result = line->meta.data.desc.maxElements;
            // Array data is always stored in lines following metadata
            dest = &((line + 1)->data.data[0]);
        } else {
            // Scalar
            result = 0;
            // Scalar data is stored starting in metadata line
            dest = line->meta.data.carr;
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
                result = c_fortran_string_copy((char *)src, (char *)dest, size, stored_size, ' ');
                if (result < 0) {
                    // Not enough space to store string!
                    WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_WRONGSTRING);
                }
                src += size;
                dest += stored_size;
            }
        }
        // Mark entry as initialized
        line->meta.flags.initialized = 1;
        // Mark entry as good
        line->meta.flags.badval = 0;
    }

    return result;
}


//! @copydoc c_wb_put
wordint f77_name(f_wb_put)(WhiteBoard **wb, char *name, wordint *type, wordint *size, void *src, wordint *nbelem, wordint *options, F2Cl nameLength){
   char _type = *type;
   int _nameLength = nameLength;
   int _size = *size;
   int _nbelem = *nbelem;
   int _options = *options;
   return c_wb_put(*wb, name, _type, _size, src, _nbelem, _options, _nameLength);
}


//! Write checkpoint file of the WhiteBoard referenced by BaseWhiteboardPtr
int c_wb_checkpoint()
{
    wb_page *page;
    int pageno = 0;
    int zero = 0;
    int status;
    int fd = open(WhiteBoardCheckpointFile, O_WRONLY|O_CREAT, 0777);

    if (fd < 0) {
        // Can't open checkpoint file
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
    }
    page = BaseWhiteboardPtr->firstpage;

    // Write signature
    status = write(fd, "WBckp100", 8);
    if (status < 0) {
        // Write error!
        close(fd);
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
    }

    while (page != NULL) {
        // Write number of lines in page
        status = write(fd, &(page->nbLines), 4);
        if (status < 0) {
            close(fd);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
        }
        // Write number of next entry
        status = write(fd, &(page->firstFreeLine), 4);
        if (status < 0) {
            close(fd);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
        }
        // Write page
        status = write(fd, &(page->line[0]), sizeof(wb_line) * page->nbLines);
        if (status < 0) {
            close(fd);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
        }
        if (message_level <= WB_MSG_DEBUG) {
            fprintf(stderr, "wb_checkpoint: Page %d, length %d lines, Next entry %d \n",
                    pageno, page->nbLines, page->firstFreeLine);
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
   return c_wb_checkpoint();
}


//! Invoke the provided function on mathing lines
//
//! @param[in] wb         WhiteBoard in which to search.  If this is null, the WhiteBoard refrenced by DummyWhiteboardPtr is used.
//! @param[in] name       Entry name (length MUST be supplied in nameLength)
//! @param[in] optionMask Mask for the options to be tested
//! @param[in] nameLength Length of the entry name
//! @param[in] print      Print messages if nonzero
//! @param[in] fncptr     Function pointer to use when the options specified optionMask are set.  The referenced function must accept 2 arguments: a Line pointer and a pointer to extra data.
//! @param[in] extra_data Pointer to be passed as the second argument of fncptr (first argument is matching line)
//
//! @return Number of matches, but as a negative number.  Why!?
int c_wb_check(WhiteBoard *wb, char *name, int optionMask, int nameLength, int print, LineWorker fncptr, void *extra_data )
{
    wb_page *lookuppage;
    int pageno = 0;
    int match_count = 0;
    int status;

    extra_error_message = " invalid whiteboard instance";
    if (wb == DummyWhiteboardPtr) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTFOUND);
    }
    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }

    TRIM(name, nameLength)
    lookuppage = wb->firstpage;

    while (lookuppage != NULL) {
        int i = 0;
        int linelimit = lookuppage->firstFreeLine - 1;
        while( i <= linelimit ) {
            wb_line *line = &(lookuppage->line[i]);
            char *c1 = line->meta.name.carr;
            int options = flags_to_options(line);
            if ( (options & optionMask) && wb_match_name(c1, name, nameLength) ) {
                match_count--;
                if (print) {
                    fprintf(stderr,
                            "Page %2d, Line %3d, KEY=%s Datatype=%s[%4d] Size=%4d %s %s %s %s %s %s %s %s\n",
                            pageno, i, c1,
                            datatypes[line->meta.flags.type],
                            line->meta.flags.elementsize,
                            (line->meta.flags.array) ? line->meta.data.desc.maxElements : 0,
                            (line->meta.flags.resetuntil) ? "LOCKABLE" : "        ",
                            (line->meta.flags.initialized) ? "SET  " : "UNSET",
                            (line->meta.flags.badval) ? "BAD " : "GOOD",
                            (line->meta.flags.resetmany) ? "NOTLOCKABLE" : "           ",
                            (line->meta.flags.noresetafterrestart) ? "ROonRst" : "RWonRst",
                            (line->meta.flags.readonly) ? "LOCKED  " : "WRITABLE",
                            (line->meta.flags.islocal) ? "LOCAL " : "GLOBAL",
                            (line->meta.flags.fromrestart) ? "FromRestart" : "FromPut"
                    );
                }
                if (fncptr != NULL) {
                    status = (*fncptr)(line, extra_data);
                    if (status < 0) {
                        return status;
                    }
                }
            }
            if (line->meta.flags.lines == 0) {
                // Only one entry in this page in this case
                break;
            }
            i += line->meta.flags.lines;
        }
        lookuppage = lookuppage->next;
        pageno++;
    }
    return -match_count;
}


//! Print flags of lines with the provided name and that match the option mask
//
//! @param[in] wb         WhiteBoard in which to search the lines
//! @param[in] name       Entry name (length MUST be supplied in nameLength)
//! @param[in] optionMask Mask for the options to be tested
//! @param[in] nameLength Length of the entry name
//
//! @return Number of matches, but as a negative number.
wordint f77_name(f_wb_check)(WhiteBoard **wb, char *name, wordint *optionMask, F2Cl nameLength)
{
   int _optionMask = *optionMask;
   int _nameLength = nameLength;
   // print flag on, no action routine is supplied, no blind data pointer is supplied
   return c_wb_check(*wb, name, _optionMask, _nameLength, 1, NULL, NULL);
}


//! c_wb_check callback to set the read-only line attribute
//
//! @param[in] line      Line to set read-only
//! @param[in] blinddata Unused
//
//! @return 0 on scuccess.  Error code otherwise.
static int wb_cb_read_only(wb_line *line, void *blinddata)
{
    if (line->meta.flags.initialized == 0) {
        // making read-only a non initialized variable ???!!!
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOVAL);
    }
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "key '%s' is now read-only\n", &(line->meta.name.carr[0]));
    }
    line->meta.flags.readonly = 1;
    line->meta.flags.resetmany = 0;
    return 0;
}


//! c_wb_check callback to remove the read-only attribute of the provided line
//
//! @param[in] line      Line to set read-only
//! @param[in] blinddata Unused
//
//! @return Always 0
static int wb_cb_read_write(wb_line *line, void *blinddata)
{
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "key '%s' is now no longer read-only\n", &(line->meta.name.carr[0]));
    }
    line->meta.flags.readonly = 0;
    return 0;
}


//! c_wb_check callback to set the undefined (no longer initialized) attribute of the provided line
//
//! @param[in] line      Line to set read-only
//! @param[in] blinddata Unused
//
//! @return Always 0
static int wb_cb_undefined(wb_line *line, void *blinddata)
{
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "key '%s' is now undefined\n", &(line->meta.name.carr[0]));
    }
    line->meta.flags.initialized = 0;
    line->meta.flags.badval = 0;
    return 0;
}


//! c_wb_check callback to set the fromtrestart attribute of the provided line
//
//! @param[in] line      Line to set read-only
//! @param[in] blinddata Unused
//
//! @return Always 0
static int wb_cb_from_restart(wb_line *line, void *blinddata)
{
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "key '%s' is now marked as created by a restart\n", &(line->meta.name.carr[0]));
    }
    line->meta.flags.fromrestart = 1;
    return 0;
}


//! Configure WhiteBoard MPI broadcasts
//
//! @param[in] pe_root
//! @param[in] pe_me
//! @param[in] domain
//! @param[in] broadcast_function
//! @param[in] allreduce_function
void f77_name(f_wb_bcst_init)(wordint *pe_root, wordint *pe_me, char *domain, void (*broadcast_function)(), void (*allreduce_function)() )
{
    wb_broadcast_cfg.pe_root = *pe_root;
    wb_broadcast_cfg.pe_me = *pe_me;
    wb_broadcast_cfg.domain = domain;
    wb_broadcast_cfg.broadcast_function = broadcast_function;
    wb_broadcast_cfg.allreduce_function = allreduce_function;
}


//! Callback function for c_wb_check that prints a message saying it's broadcasting the name of the line
//
//! @return Always 0
static int wb_print_bcast_line(wb_line *line, void *blinddata)
{
    fprintf(stderr, "Broadcasting %s :-)\n", line->meta.name.carr);
    return 0;
}


//! Print "Broadcasting %lineName" for each matching line
//
//! @param[in] wb         WhiteBoard in which to search
//! @param[in] name       Name to match
//! @param[in] wildcard   If this in not null, replace the first blank in the provided name with the string terminaison
//! @param[in] nameLength Length of the name
//
//! @return Number of matches, but as a negative number.
wordint f77_name(f_wb_bcst)(WhiteBoard **wb, char *name, wordint *wildcard, F2Cl nameLength)
{
    int _nameLength = nameLength;
    wb_line line;

    fill_line(&line, name, _nameLength);
    if (*wildcard != 0) {
        // Wildcard matches, get rid of trailing blanks
        for (int i = 0; i < WB_MAXNAMELENGTH - 1; i++) {
            if (line.meta.name.carr[i] == ' ') {
                line.meta.name.carr[i] = 0;
            }
        }
    }

    return c_wb_check(*wb, name, -1, _nameLength, message_level <= WB_MSG_INFO, wb_print_bcast_line, &wb_broadcast_cfg);
}


//! Callback function for c_wb_check that copies keys into user table when collecting a list of keys
//
//! @param[in] line Line from which to copy the name
//! @param[in] blinddata A pointer to an instance of wb_keys.
//
//! @return Always 0
static int wb_copy_key_name(wb_line *line, void *blinddata)
{
    wb_keys *keys = (wb_keys *)blinddata;
    if (keys->userMaxLabels <= 0) {
        // Too many keys for user array
        return WB_ERROR;
    }
    // c_fortran_string_copy does not allocate memory for the destination (keys->name); the user must make sure it's allocated
    c_fortran_string_copy(line->meta.name.carr, keys->name, WB_MAXNAMELENGTH, keys->nameLength, ' ');
    (keys->userMaxLabels)--;
    keys->name += keys->nameLength;

    return 0;
}


//! Get a list of keys matching a name
//
//! @param[in] wb
//! @param[in] labels
//! @param[in] nlabels
//! @param[in] name
//! @param[in] llabels
//! @param[in] nameLength
//
wordint f77_name(f_wb_get_keys)(WhiteBoard **wb, char *labels, wordint *nlabels, char *name, F2Cl llabels, F2Cl nameLength)
{
   int _nameLength = nameLength;
   wb_keys keys;

   keys.name = labels;
   keys.userMaxLabels = *nlabels;
   keys.nameLength = llabels;

   return c_wb_check(*wb, name, -1, _nameLength, message_level <= WB_MSG_INFO, wb_copy_key_name, &keys);
}


//! Convert all the lines with the WB_REWRITE_UNTIL attribute to read-only
//
//! @param[in] wb         Whiteboard instance
//! @param[in] name       Name of the lines to lock
//! @param[in] nameLength Length of name
//
//! @return Number of matches, but as a negative number.
int c_wb_lock(WhiteBoard *wb, char *name, int nameLength){
    char localname[WB_MAXSTRINGLENGTH];
    int llname = c_fortran_string_copy(name, localname, nameLength, sizeof(localname), '\0');
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "locking variables with name beginning with '%s'\n", localname);
    }
    return c_wb_check(wb, localname, WB_REWRITE_UNTIL, llname, 1, wb_cb_read_only, NULL);
}


//! @copydoc c_wb_lock
wordint f77_name(f_wb_lock)(WhiteBoard **wb, char *name, F2Cl nameLength){
   int _nameLength = nameLength;
   return c_wb_lock(*wb, name, _nameLength);
}


//! Write WhiteBoard checkpoint file
int c_wb_reload()
{
    int pageno = 0;
    int pagelen;
    int status;
    char signature[9];
    int fd = open(WhiteBoardCheckpointFile, O_RDONLY);

    if (fd < 0) {
        // Cannot open checkpoint file
        WB_ERR_EXIT(WB_MSG_ERROR,WB_ERR_CKPT);
    }
    if (BaseWhiteboardPtr->firstpage != NULL) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERROR);
    }
    status = read(fd, signature, 8);
    signature[8] = 0;
    if (status != 8 || 0 != strncmp(signature, "WBckp100", 8)) {
        close(fd);
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_CKPT);
    }

    // Page size
    status = read(fd, &pagelen, 4);
    while (pagelen > 0) {
        // Allocate new page
        status = new_page(BaseWhiteboardPtr, pagelen);
        // Next usable entry in page
        status = read(fd, &(BaseWhiteboardPtr->firstpage->firstFreeLine), 4);
        if (message_level <= WB_MSG_DEBUG) {
            fprintf(stderr,"wb_reload: Page %d, length=%d lines, next entry=%d\n",
                    pageno, BaseWhiteboardPtr->firstpage->nbLines, BaseWhiteboardPtr->firstpage->firstFreeLine);
        }
        // Read page
        status = read(fd, &(BaseWhiteboardPtr->firstpage->line[0]), sizeof(wb_line) * pagelen);
        // Page size of next page, 0 means no more
        status = read(fd ,&pagelen, 4);
        pageno++;
    }
    close(fd);

    // If variable is WB_REWRITE_AT_RESTART, erase the read-only flag
    status = c_wb_check(BaseWhiteboardPtr, "", WB_REWRITE_AT_RESTART, 0, 0, wb_cb_read_write, NULL);
    // If variable is WB_READ_ONLY_ON_RESTART, make sure it is now marked as reasd-only
    status = c_wb_check(BaseWhiteboardPtr, "", WB_READ_ONLY_ON_RESTART, 0, 0, wb_cb_read_only, NULL);
    // If variable is local (not the same on ALL MPI whiteboards, mark it as not initialized
    status = c_wb_check(BaseWhiteboardPtr, "", WB_IS_LOCAL, 0, 0, wb_cb_undefined, NULL);
    // Mark all variables as having been created by a restart
    status = c_wb_check(BaseWhiteboardPtr, "", -1, 0, 0, wb_cb_from_restart, NULL);

    if (message_level <= WB_MSG_INFO) {
        fprintf(stderr, "whiteboard has been reloaded, variables read only after restart have been locked\n");
    }

    return WB_OK;
}


//! @copydoc c_wb_reload
wordint f77_name(f_wb_reload)(WhiteBoard **wb){
   return c_wb_reload();
}


//! Get a line from file, reset current character pointer
//
//! @param[in] infile File from which to read the line
//
//! @return The address of linebuffer on scuccess or EOF otherwise
static int wb_get_line(FILE *infile)
{
    //! @bug linebuffer has never been allocated; memory corruption is happening here!
    //! @bug There is a type missmatch between the return of fget (char *) and this function (int) and current_char (char *)!
    //! @bug A int is not big enough to hold a char * on x86_64!
    current_char = fgets(linebuffer, WB_MISC_BUFSZ, infile);
    if (message_level <= WB_MSG_INFO && current_char) {
        fprintf(stderr, ">>%s", linebuffer);
    }
    if (current_char) {
        return *current_char;
    } else {
        return EOF;
    }
}


//! Push character back onto the input buffer
//
//! @param[in] lastchar Character to push back
//
//! @return WB_OK on success, WB_ERROR otherwise
//! @todo Figure out why lastchar is an int instead of an char
static int wb_ungetc(int lastchar)
{
    if (current_char == NULL || linebuffer == NULL) {
        return WB_ERROR;
    }
    if (current_char > linebuffer) {
        current_char--;
        *current_char = lastchar;
    } else {
        return WB_ERROR;
    }
    return WB_OK;
}


//! Get next character from input stream, take care of newline sequence
//
//! @param[in] infile File from whic to read
//
//! @return 
static int wb_getc(FILE *infile)
{
    //! @todo Get rid of the goto
    //! @todo Stop using file static variables if there is not a bloody good reason to do so!
    int cbuff;

    // No buffer pointer, fill buffer
    if (current_char == NULL) {
        cbuff = wb_get_line(infile);
        if (current_char == NULL) {
            // End Of File or error
            return EOF;
        }
    }

    while (*current_char == 0) {
        cbuff = wb_get_line(infile);
        if (current_char == NULL) {
            return EOF;
        }
        // Gget next character and bump pointer
        cbuff = *current_char++;
        // Get rid of newlines
        if (cbuff == '\\' && *current_char == '\n' ) {
            // Point to character after newline
            current_char++;
        } else {
            break;
        }
    }

    return cbuff;
}


//! Flush input until newline or EOF , return what was found
//
//! @param infile File from which to read
//
//! @return The last newline, semicolon or EOF character encountered
static int wb_flush_line(FILE *infile)
{
    int c = wb_getc(infile);
    while (c != '\n' && c != EOF && c != ';') {
        c = wb_getc(infile);
    }
    return c;
}


//! Skip blanks and tabs from input, return next character, newline, or EOF, take care of comments, recognize ; as newline
//
//! @param infile File from which to read
//
//! @return The possibly converted character
static int wb_get_nonblank(FILE *infile)
{
    int c = wb_getc(infile);
    if (c == EOF) {
        return EOF;
    }
    while( (c == ' ' || c == '\t' || c == '#' || c == '!' ) && c != EOF ) {
        // space, tab, comment characters
        if (c == '#' || c == '!') {
            // Comment -> get rid of the rest of the line
            c = wb_flush_line(infile);
            if (c == EOF) {
                return EOF;
            }
        }
        // Get next character
        c = wb_getc(infile);
    }
    if (c == ';') {
        // treat a non quoted ; as a newline
        c = '\n';
    }
    return c;
}


//! Print a special error when reading directives, print directive line up to error, add ^^^^ error marker, print rest of line
static void wb_read_error()
{
    int temp;
    if (current_char == NULL || linebuffer == NULL) {
        return;
    }
    temp = *current_char;
    *current_char = 0;
    if (message_level <= WB_MSG_ERROR) {
        fprintf(stderr, "ERROR in directives, offending line:\n%s^^^^", linebuffer);
    }
    *current_char = temp;
    if (message_level <= WB_MSG_ERROR) {
        fprintf(stderr, "%s\n", current_char);
    }
}


//! Get next token (alphanum_, number, delimited string, single character, force non quoted tokens to uppercase
//
//! @param[out] token          Token read from file.  This must be previously allocated
//! @param[in]  infile         File from which to read
//! @param[in]  maxTokenLength Maximum size of token
//! @param[in]  noskip         If non zero, do not skip spaces
//
//! @return Length of the token read or error code
static int wb_get_token(char *token, FILE *infile, int maxTokenLength, int noskip)
{
    int tokenLength = 0;
    int c;

    if (noskip) {
            // Do not skip spaces
            c = wb_getc(infile);
    } else {
        c = wb_get_nonblank(infile);
    }
    if (c == EOF) return WB_ERROR;

    // First character
    token[tokenLength++] = toupper(c);
    maxTokenLength--;
    if (maxTokenLength < 0) {
        // Token is too big to be stored in supplied array
        wb_read_error() ;
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BIG);
    }
    if (isalpha(c)) {
        // Collect alphanum _ token
        for (c = wb_getc(infile); isalnum(c) || c == '_'; c = wb_getc(infile)) {
            maxTokenLength--;
            if (maxTokenLength < 0) {
                // Token is too big to be stored in supplied array
                wb_read_error();
                WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BIG);
            }
            token[tokenLength++] = toupper(c) ;
        }
        if (c == EOF ) {
            return WB_ERROR;
        }
        // push back extra input character
        if (wb_ungetc(c) == EOF) {
            return WB_ERROR;
        }
    } else if (c == '\'' || c == '"') {
        // Collect ' or " delimited string
        int quote = c;
        c = wb_getc(infile);
        // Look for matching quote, error end if newline/EOF  ecountered
        while (c != quote && c != '\n' && c != EOF) {
            if (c == EOF || c == '\n') break;
            maxTokenLength--;
            if (maxTokenLength < 0) {
                // Token is too big to be stored in supplied array
                wb_read_error() ;
                WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BIG);
            }
            token[tokenLength++] = c;
            c = wb_getc(infile);
        }
        if (c == '\n') {
            if (message_level <= WB_MSG_ERROR ) {
                fprintf(stderr, "ERROR: improperly terminated string\n");
            }
            if (wb_ungetc(c) == EOF) {
                // Push back newline that has been erroneously swallowed
                return WB_ERROR;
            }
        }
        if (c == EOF || c == '\n') {
            return WB_ERROR;
        }
        maxTokenLength--;
        if (maxTokenLength < 0) {
            // Token is too big to be stored in supplied array
            wb_read_error();
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BIG);
        }
        // Store end delimiter in token
        token[tokenLength++] = c;
    } else if (isdigit(c) || c == '.' || c == '+' || c == '-' ) {
        /* digit, point, sign, potential number or boolean */
        c = wb_getc(infile) ;
        if (isalpha(c) && token[tokenLength - 1] == '.') {
            // Collect .true. , .false. , etc ...
            while (isalpha(c) || c == '.') {
                maxTokenLength--;
                if (maxTokenLength < 0) {
                    // Token is too big to be stored in supplied array
                    wb_read_error();
                    WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BIG);
                }
                token[tokenLength++] = toupper(c);\
                c = wb_getc(infile);
            }
        } else {
            // Collect a potential number
            while (isdigit(c) || c == '.' || c == '-' || c == '+' || c == 'E' || c == 'e' ) {
                maxTokenLength--;
                if (maxTokenLength < 0) {
                    // Token is too big to be stored in supplied array
                    wb_read_error();
                    WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BIG);
                }
                token[tokenLength++] = toupper(c);
                c = wb_getc(infile);
            }
        }
        if (c == EOF) {
            return WB_ERROR;
        }
        // Push back extra input character
        if (wb_ungetc(c) == EOF) {
            return WB_ERROR;
        }
    }

    // None of the above means single character
    maxTokenLength--;
    if (maxTokenLength < 0) {
        // Token is too big to be stored in supplied array
        wb_read_error() ;
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_BIG);
    }
    // Null terminate token
    token[tokenLength] = 0;

    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "GetToken, maxTokenLength=%d, tokenLength=%d, nospkip=%d, token='%s'\n", 
                maxTokenLength, tokenLength, noskip, token);
    }
    return tokenLength;
}


//! Process options set [option,option,...] or (option,option,...)
//
//! @param[in] infile       File from which to read
//! @param[in] start_delim  Option start delimiter
//! @param[in] end_delim    Option end delimiter
//! @param[in] option_table Option table
//
//! @return Bit field of the active options
static int wb_options(FILE *infile, char start_delim, char end_delim, wb_symbol *option_table)
{
    char token[WB_MAXNAMELENGTH + 1];
    int options = 0;
    int ntoken, newoption;

    // Get starting delimiter ( or [
    ntoken = wb_get_token(token, infile, 2 ,0);
    if (token[0] != start_delim || ntoken != 1) {
        wb_read_error() ;
        wb_flush_line(infile);
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_SYNTAX);
    }

    while (token[0] != end_delim) {
        // Expect keyname
        ntoken = wb_get_token(token, infile, sizeof(token), 0);
        if (!isalpha(token[0]) || ntoken <= 0) {
            wb_read_error() ;
            wb_flush_line(infile);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_SYNTAX);
        }
        newoption = wb_value(token, option_table);
        if (newoption == 0) {
            wb_read_error() ;
            wb_flush_line(infile);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_OPTION);
        }
        options += newoption;
        if (message_level <= WB_MSG_DEBUG) {
            fprintf(stderr, "newoption=%d, options=%d\n", newoption, options);
        }
        // expect end delimiter ( or ]  or comma ,
        ntoken = wb_get_token(token, infile, 2, 0);
        if ((token[0] != end_delim && token[0] != ',') || ntoken != 1) {
            wb_read_error() ;
            wb_flush_line(infile);
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_SYNTAX);
        }
    }
    return options;
}


//! Process define directive define(key_name[TYPE,array_size], ... , ... )  TYPE=R4/R8/I4/I8/L1/Cn
//
//! @param[in] wb      WhiteBoard in which to store the definition read
//! @param[in] infile  File from which to read
//! @param[in] package 
//
//! @return WB_OK on success, error code otherwise
static int wb_define(WhiteBoard *wb, FILE *infile, char *package){
    char name[WB_MAXNAMELENGTH + 1];
    char type[WB_MAXNAMELENGTH + 1];
    char length[WB_MAXNAMELENGTH + 1];
    char token[WB_MISC_BUFSZ];
    int ntoken;
    int key_type, len_type, nread, array_length;
    int options = default_dict_option;
    char key_c;
    int opt_cmd = 0;
    int desc_cmd = 0;
    int status = WB_OK;

    // Get (
    ntoken = wb_get_token(token, infile, 2, 0);
    if (token[0] != '(' || ntoken != 1) goto error_syntax;

    strncpy((char *)name, package, strlen(package));
    // Expect keyname
    ntoken = wb_get_token(name + strlen(package), infile, sizeof(name) - strlen(package), 0) ;
    if (!isalpha(name[strlen(package)]) || ntoken <= 0) goto error_syntax;

    // expect [
    ntoken = wb_get_token(token, infile, 2, 0);
    if (token[0] != '[' || ntoken != 1) goto error_syntax;

    // Expect entry type and length
    ntoken = wb_get_token(type, infile, sizeof(type), 0);

    sscanf((char *)type, "%c%d%n", &key_c, &len_type, &nread);
    // Get type code
    switch(key_c) {
        case 'R' : key_type = WB_FORTRAN_REAL; break;
        case 'I' : key_type = WB_FORTRAN_INT;  break;
        case 'C' : key_type = WB_FORTRAN_CHAR; break;
        case 'L' : key_type = WB_FORTRAN_BOOL; break;
        default: goto error_syntax;
    }

    if (nread != ntoken) goto error_syntax;
    // Check type and length combination
    if ((key_type = get_typecode(key_type, len_type, len_type)) < 0) goto error_syntax;

    // Expect ,
    ntoken = wb_get_token(token, infile, 2, 0);
    if (token[0] != ',' || ntoken != 1) goto error_syntax;

    // Expect array length
    ntoken = wb_get_token(length, infile, sizeof(length), 0);
    if (!isdigit(length[0]) || ntoken <= 0) goto error_syntax;
    sscanf((char *)length, "%d%n", &array_length, &nread);
    if (nread != ntoken) goto error_syntax;

    // Expect ]
    ntoken = wb_get_token(token, infile, 2, 0) ;
    if (token[0] != ']' || ntoken != 1) goto error_syntax;

    // Expect ) or ,
    ntoken = wb_get_token(token, infile, 2, 0);
    if ((token[0] != ')' && token[0] != ',') || ntoken != 1) goto error_syntax;

    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "type=%c,key_type=%d,len=%d,array_length=%d\n", key_c, key_type, len_type, array_length);
    }
    while (token[0] != ')') {
        // Expect subcommand name OPT or DESC
        ntoken = wb_get_token(token, infile, WB_MAXNAMELENGTH, 0);
        if (strcmp("OPT", (char *)token) == 0 && opt_cmd == 0 ) {
            // OPT= subcommand
            // Expect =
            ntoken = wb_get_token(token, infile, 2, 0);
            if (token[0] != '=' || ntoken != 1) goto error_syntax;
            options = wb_options(infile, '[', ']', dict_options);
            opt_cmd++;
        } else if (strcmp("DESC", (char *)token) == 0 && desc_cmd == 0) {
            // DESC= subcommand
            // Expect =
            ntoken = wb_get_token(token, infile, 2, 0);
            if (token[0] != '=' || ntoken != 1) goto error_syntax;
            // Expect quoted string
            ntoken = wb_get_token(token, infile, WB_MISC_BUFSZ, 0);
            if (token[0] != '\'' && token[0] != '"') goto error_syntax;
            // DESC is ignored for now, just counted
            desc_cmd++;
        } else {
            if (message_level <= WB_MSG_ERROR) {
                fprintf(stderr, "invalid/undefined define subcommand\n");
            }
            goto error_syntax ;
        }
        // expect ) or ,
        ntoken = wb_get_token(token, infile, WB_MAXNAMELENGTH, 0);
        if( (token[0] != ')' && token[0] != ',') || ntoken != 1) goto error_syntax;
    }
    status = c_wb_put(wb, name, key_type, len_type, (unsigned char*)token, array_length, options | WB_CREATE_ONLY, strlen((char *)name));
    // Put failed for some reason
    if (status < 0) goto error_syntax;
    // Must not already be in table
    status = wb_define_check(wb_lastputline, 1);
    if (status < 0) goto error_syntax;
    wb_flush_line(infile);
    return status;
error_syntax:
    wb_read_error() ;
    wb_flush_line(infile);
    WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_SYNTAX);
}


//! Process key= directive from file Token contains key name, package contains package prefix string
static int wb_key(WhiteBoard *wb, FILE *infile, char *token, char *package, int options)
{
    int status = WB_OK;
    int errNotFound = 1;
    int elementtype, elementsize, elements, ntoken, nread, items;
    wb_line *line;
    wb_page *page;
    char name[WB_MAXNAMELENGTH + 1];
    char separator[3];
    // Add the two delimiters and the null terminator
    char buffer[WB_MAXSTRINGLENGTH + 3];
    unsigned char *dataptr;

    // Concatenate package name with keyname
    strncpy((char *)name, package, strlen(package));
    strncpy((char *)name + strlen(package), (char *)token, sizeof(name) - strlen(package));
    name[WB_MAXNAMELENGTH] = 0;

    if (message_level <= WB_MSG_INFO) {
        fprintf(stderr, "Assigning to '%s'\n", name);
    }
    extra_error_message=name;
    // Expect =
    ntoken = wb_get_token(token, infile, 2, 0);
    if (token[0] != '=' || ntoken != 1) {
        if (message_level <= WB_MSG_ERROR) {
            fprintf(stderr, "= sign not found where expected \n");
        }
        goto error_syntax ;
    }

    status = c_wb_lookup(wb, name, &elementtype, &elementsize, &elements, &line, &page, errNotFound, strlen(name));
    // Key MUST be defined
    if (status < 0) goto error_syntax;

    if (options == WB_STRICT_DICTIONARY) {
        // Must be found in table if in strict dictionary mode
        status = wb_define_check(line, 0);
    } else {
        // Must be inserted in table if in normal mode
        status = wb_define_check(line, 2);
    }
    if (status < 0) {
        // Other source of error : key already assigned a value in this file
        goto error_syntax;
    }

    if (elements == 0) {
        // Scalar, 1 item
        elements = 1;
    }
    if (line->meta.flags.array) {
        // array data
        dataptr = &((line+1)->data.data[0]);
    } else {
        // Scalar data
        dataptr = line->meta.data.carr;
    }
    // Get values for up to max items for key
    while(elements--) {
        ntoken = wb_get_token(buffer, infile, sizeof(buffer) - 1, 0);
        if (ntoken <= 0) goto error_syntax;

        switch (elementtype) {
            case WB_FORTRAN_INT:
                if (elementsize == 4) {
                    // 4 byte integer
                    int *target = (int *)dataptr;
                    items = sscanf(buffer, "%d%n", target, &nread);
                    if (nread != ntoken || items != 1) goto error_syntax;
                } else if (elementsize == 8) {
                    // 8 byte integer
                    long long *target = (long long *)dataptr;
                    items = sscanf(buffer, "%lld%n", target, &nread);
                    if (nread != ntoken || items != 1) goto error_type;
                } else {
                    goto error_syntax;
                }
                break;
            case WB_FORTRAN_REAL:
                if (elementsize == 4) {
                    // 4 byte real
                    float *target = (float *)dataptr;
                    items = sscanf(buffer, "%E%n", target, &nread);
                    if (nread != ntoken || items != 1) goto error_syntax;
                } else if (elementsize == 8) {
                    // 8 byte real
                    double *target = (double *)dataptr;
                    items = sscanf(buffer, "%lE%n", target, &nread);
                    if (nread != ntoken || items != 1) goto error_syntax;
                } else {
                    goto error_syntax;
                }
                break;
            case WB_FORTRAN_BOOL:
                if (elementsize == 4) {
                    // FORTRAN logical
                    if (strncmp(".FALSE.", buffer, 7) == 0 || strncmp(".F.", buffer, 3) == 0 ) *dataptr = 0;
                    else if (strncmp(".TRUE.", buffer, 6) == 0 || strncmp(".T.", buffer, 3) == 0 ) *dataptr = 1;
                    else goto error_type;
                } else {
                    goto error_syntax;
                }
                break;
            case WB_FORTRAN_CHAR:
                // Copy string in buffer into target, mit firs and last character, the quotes
                nread = c_fortran_string_copy(buffer + 1, (char *)dataptr, strlen(buffer) - 2, elementsize, ' ');
                if (nread < 0) goto error_string;
                break;
            default:
                goto error_syntax;
        }
        dataptr += elementsize;

        // Separator must be either comma or newline
        ntoken = wb_get_token(separator, infile, sizeof(separator) - 1, 0);
        if (ntoken != 1) goto error_syntax;
        if (separator[0] == ',') continue ;
        if (separator[0] == '\n') {
            break;
        } else {
            goto error_syntax;
        }
    }
    if (separator[0] != '\n' ) goto error_toomany;
    // Mark entry as initialized
    line->meta.flags.initialized = 1;
    line->meta.flags.badval = 0;
    return status;
error_toomany:
    if (message_level <= WB_MSG_ERROR) {
        fprintf(stderr, "attempting to assign too many values to %s (type %s)\n", name, datatypes[elementtype]);
        goto error_syntax;
    }
error_string:
    if (message_level <= WB_MSG_ERROR) {
        fprintf(stderr, "string %s is too long to be assigned to %s (length=%ld>%d)\n", buffer, name, strlen(buffer) - 2, elementsize);
        goto error_syntax;
    }
error_type:
    if (message_level <= WB_MSG_ERROR) {
        fprintf(stderr, "type mismatch error while assigning value %s to %s (type %s)\n", buffer, name, datatypes[elementtype]);
        goto error_syntax;
    }
error_syntax:
    wb_read_error() ;
    wb_flush_line(infile);
    WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_SYNTAX);
}


//! Read a dictionary or user directive file
int c_wb_read(WhiteBoard *wb, char *filename, char *package, char *section, int options, int filenameLength,
              int package_length, int section_length)
{
    wb_definition mytable[WB_MISC_BUFSZ];
    char localbuffer[WB_MISC_BUFSZ];
    char localfname[WB_MISC_BUFSZ];
    char token[WB_MISC_BUFSZ];
    char Package[WB_MAXNAMELENGTH];
    char Section[WB_MAXNAMELENGTH];
    int i, status;
    FILE *infile;
    int ntoken;
    int temp;
    int errors = 0;

    extra_error_message = " invalid whiteboard instance";
    if (wb == DummyWhiteboardPtr) {
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTFOUND);
    }
    if (wb == NULL) {
        wb = BaseWhiteboardPtr;
    }

    TRIM(filename, filenameLength)
    TRIM(package, package_length)
    TRIM(section, section_length)
    // @bug External pointer (linebuffer) updated with local variable (localbuffer)!
    linebuffer = localbuffer;

    // Initialize definition table control
    // @bug External pointer (definition_table) updated with local variable (mytable)!
    definition_table = mytable;
    definition_table_entries = 0;
    max_definition_table_entries = WB_MISC_BUFSZ;
    for (i = 0; i < WB_MISC_BUFSZ; i++) {
        definition_table[i].line = NULL;
        definition_table[i].assigned = 0;
        definition_table[i].defined = 0;
    }

    for (i = 0; i < filenameLength && i < WB_MISC_BUFSZ - 1 && filename[i] != 0; i++) {
        localfname[i] = filename[i];
    }
    // Filename collected
    localfname[i] = 0;
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "localfname='%s'\n", localfname);
    }

    for (i = 0; i < package_length && i < WB_MAXNAMELENGTH - 1 && package[i] != 0; i++) {
        Package[i] = toupper(package[i]);
    }
    // Package name collected
    Package[i] = 0;
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "Package='%s'\n", Package);
    }

    for (i = 0; i < section_length && i < WB_MAXNAMELENGTH - 1 && section[i] != 0; i++) {
        Section[i] = toupper(section[i]);
    }
    // Section_name collected
    Section[i] = 0;
    if (message_level <= WB_MSG_DEBUG) {
        fprintf(stderr, "Section='%s'\n", Section);
    }

    // Add directive file name to error message
    extra_error_message = localfname;
    // Make sure that no previous input is left in buffers
    current_char = NULL;
    // Try to open file
    infile = fopen(localfname, "r");
    if (infile == NULL) {
        // Can't open/read file!
        WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_READ);
    }

    extra_error_message = NULL;

    // Loop until desired section "@xxxx" is found or end of directive file
    int notFound = 1;
    while (notFound) {
        temp = wb_get_nonblank(infile);
        // Look for @ as first nonblank character in line (or End of file)
        while (temp != '@' && temp != EOF) {
            temp = wb_get_line(infile);
            temp = wb_get_nonblank(infile);
        }
        if (temp == EOF) {
            // Reached EOF; Requested section not found
            fclose(infile);
            // Add searched section name to error message
            // @bug External pointer (extra_error_message) updated with local variable (Section)!
            extra_error_message = Section;
            WB_ERR_EXIT(WB_MSG_ERROR, WB_ERR_NOTFOUND);
        }
        ntoken = wb_get_token(token, infile, WB_MISC_BUFSZ - 1, 1);
        notFound = (strncmp((char *)token, Section, strlen(Section)) != 0);
    }
    if (message_level <= WB_MSG_INFO) {
        fprintf(stderr, "INFO: directive section %s found\n", Section);
    }

    // Section found, process it
    // Get rid of rest of @section line
    temp = wb_get_line(infile);
    // Default options = none unless there is a directive
    default_dict_option = 0;
    ntoken = wb_get_token(token, infile, WB_MISC_BUFSZ - 1, 0);
    // Loop until end of section (beginning of next section or EOF)
    while (strncmp((char *)token, "@", 1) && ntoken != WB_ERROR ) {
        if (strncmp((char *)token, "OPTIONS", 7) == 0 ) {
            // Default options directive
            default_dict_option = wb_options(infile, '(', ')', dict_options);
        } else if (strncmp((char *)token, "MESSAGES", 8) == 0 ) {
            // Verbosity control directive
            message_level = wb_options(infile,'(',')',verb_options);
        } else if (strncmp((char *)token, "DEFINE", 6) == 0 && options != WB_FORBID_DEFINE ) {
            // Define directive (if allowed)
            status = wb_define(wb, infile, Package);
            if (status < 0) errors++;
        } else if (isalpha(token[0])) {
            // Must be key=
            status = wb_key(wb, infile, token, Package, options);
            if (status < 0) errors++;
        } else if (token[0] == '\n') {
            // Ignore newlines
            temp = '\n';
        } else {
            if (message_level <= WB_MSG_ERROR) {
                fprintf(stderr, "Unexpected token found at beginning of directive: '%s'\n", token);
            }
            errors++;
            wb_flush_line(infile);
        }
        // Get next token
        ntoken = wb_get_token(token, infile, WB_MISC_BUFSZ - 1, 0);
    }

    // We are done, close file and return success
    fclose(infile);
    if (message_level <= WB_MSG_INFO || (message_level <= WB_MSG_ERROR && errors > 0)) {
        fprintf(stderr, "INFO: %d error(s) detected\n", errors);
    }
    return errors == 0 ? WB_OK : WB_ERROR;
}

/* read a dictionary or user directive file (FORTRAN version) */
wordint f77_name(f_wb_read)(WhiteBoard **wb, char *package, char *filename, char *section, wordint *options, F2Cl packageLength, F2Cl filenameLength,  F2Cl sectionLength){
   int _filenameLength = filenameLength;
   int _packageLength = packageLength;
   int _sectionLength = sectionLength;
   int _options = *options;

   return c_wb_read(*wb, filename, package, section, _options, _filenameLength, _packageLength, _sectionLength);
}