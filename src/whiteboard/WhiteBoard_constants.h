/*! @file WhiteBoard_constants.h Constant defintion for WhiteBoard.
 * Since this file is meant to be included from C or Fortran, all comments must
 * be declared with the old C syntax.
 */

#ifndef WHITEBOARD_VERSION
/*! @todo Code version MUST NOT be a literal in the code; that's versionning system's responsability */
#define WHITEBOARD_VERSION "2.0.0"

#define WB_FORTRAN_REAL 1
#define WB_FORTRAN_INT  2
#define WB_FORTRAN_CHAR 3
#define WB_FORTRAN_BOOL 4
#define WB_OPTION_SET(options,option) (0 .ne. iand(options,option))

#define WB_IS_ARRAY 4096
#define WB_REWRITE_AT_RESTART 2048
#define WB_REWRITE_MANY 1024
#define WB_REWRITE_UNTIL 512
#define WB_REWRITE_NONE 256
#define WB_DEFAULT WB_REWRITE_NONE
#define WB_READ_ONLY_ON_RESTART 128
#define WB_INITIALIZED 64
#define WB_BADVAL 32
#define WB_HAS_RULES 16
#define WB_IS_LOCAL 8
#define WB_CREATED_BY_RESTART 4
#define WB_NOTINITIALIZED 2
#define WB_CREATE_ONLY 1

#define WB_STRICT_DICTIONARY 2
#define WB_ALLOW_DEFINE 1
#define WB_FORBID_DEFINE 0

#define WB_MSG_DEBUG 6
#define WB_MSG_INFO 5
#define WB_MSG_WARN 4
#define WB_MSG_ERROR 3
#define WB_MSG_SEVERE 2
#define WB_MSG_FATAL 1

#define WB_OK 0
#define WB_ERROR -1
#define WB_ERR_NAMETOOLONG -1000
#define WB_ERR_NOTFOUND -1001
#define WB_ERR_READONLY -1002
#define WB_ERR_WRONGTYPE -1003
#define WB_ERR_WRONGDIMENSION -1004
#define WB_ERR_ALLOC -1005
#define WB_ERR_NOTYPE -1006
#define WB_ERR_NOMEM -1007
#define WB_ERR_NOVAL -1008
#define WB_ERR_BADVAL -1009
#define WB_ERR_WRONGSTRING -1010
#define WB_ERR_CKPT -1011
#define WB_ERR_REDEFINE -1012
#define WB_ERR_BIG -1013
#define WB_ERR_SYNTAX -1014
#define WB_ERR_OPTION -1015
#define WB_ERR_READ -1016

#define WB_IS_OK(errcode) (errcode >= WB_OK)
#define WB_IS_ERROR(errcode) (errcode <= WB_ERROR)

#define WB_MAXSTRINGLENGTH 520
#define WB_MAXNAMELENGTH 27
#define WB_MAX_ETRA_ERROR_LEN 256
#define WB_MISC_BUFSZ 1024

#define WB_MAXLINESPERPAGE 32
#define WB_MAXLINESPERPAGESHIFT 8

#endif
