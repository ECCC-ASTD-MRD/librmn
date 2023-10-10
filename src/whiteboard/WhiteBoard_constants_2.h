/*! @file WhiteBoard_constants.h Constant defintion for WhiteBoard.
 * Since this file is meant to be included from C or Fortran, all comments must
 * be declared with the old C syntax.
 */

#ifndef WHITEBOARD_VERSION_2
#define WHITEBOARD_VERSION_2
#define WB_OPTION_SET(options,option) (0 .ne. iand(options,option))

#define WB_IS_OK(errcode) (errcode >= WB_OK)
#define WB_IS_ERROR(errcode) (errcode <= WB_ERROR)

#endif
/*! @todo Code version MUST NOT be a literal in the code; that's versionning system's responsability */

character(len=5)   :: WHITEBOARD_VERSION = "2.0.0"

integer, parameter :: WB_FORTRAN_REAL = 1
integer, parameter :: WB_FORTRAN_INT  = 2
integer, parameter :: WB_FORTRAN_CHAR = 3
integer, parameter :: WB_FORTRAN_BOOL = 4

integer, parameter :: WB_IS_ARRAY             = 4096
integer, parameter :: WB_REWRITE_AT_RESTART   = 2048
integer, parameter :: WB_REWRITE_MANY         = 1024
integer, parameter :: WB_REWRITE_UNTIL        = 512
integer, parameter :: WB_REWRITE_NONE         = 256
integer, parameter :: WB_DEFAULT              = WB_REWRITE_NONE
integer, parameter :: WB_READ_ONLY_ON_RESTART = 128
integer, parameter :: WB_INITIALIZED          = 64
integer, parameter :: WB_BADVAL               = 32
integer, parameter :: WB_HAS_RULES            = 16
integer, parameter :: WB_IS_LOCAL             = 8
integer, parameter :: WB_CREATED_BY_RESTART   = 4
integer, parameter :: WB_NOTINITIALIZED       = 2
integer, parameter :: WB_CREATE_ONLY          = 1

integer, parameter :: WB_STRICT_DICTIONARY = 2
integer, parameter :: WB_ALLOW_DEFINE      = 1
integer, parameter :: WB_FORBID_DEFINE     = 0

integer, parameter :: WB_MSG_DEBUG  = 6
integer, parameter :: WB_MSG_INFO   = 5
integer, parameter :: WB_MSG_WARN   = 4
integer, parameter :: WB_MSG_ERROR  = 3
integer, parameter :: WB_MSG_SEVERE = 2
integer, parameter :: WB_MSG_FATAL  = 1

integer, parameter :: WB_OK                 = 0
integer, parameter :: WB_ERROR              = -1
integer, parameter :: WB_ERR_NAMETOOLONG    = -1000
integer, parameter :: WB_ERR_NOTFOUND       = -1001
integer, parameter :: WB_ERR_READONLY       = -1002
integer, parameter :: WB_ERR_WRONGTYPE      = -1003
integer, parameter :: WB_ERR_WRONGDIMENSION = -1004
integer, parameter :: WB_ERR_ALLOC          = -1005
integer, parameter :: WB_ERR_NOTYPE         = -1006
integer, parameter :: WB_ERR_NOMEM          = -1007
integer, parameter :: WB_ERR_NOVAL          = -1008
integer, parameter :: WB_ERR_BADVAL         = -1009
integer, parameter :: WB_ERR_WRONGSTRING    = -1010
integer, parameter :: WB_ERR_CKPT           = -1011
integer, parameter :: WB_ERR_REDEFINE       = -1012
integer, parameter :: WB_ERR_BIG            = -1013
integer, parameter :: WB_ERR_SYNTAX         = -1014
integer, parameter :: WB_ERR_OPTION         = -1015
integer, parameter :: WB_ERR_READ           = -1016
integer, parameter :: WB_ERR_INITRESTART    = -1017

integer, parameter :: WB_MAXSTRINGLENGTH    = 520
integer, parameter :: WB_MAXNAMELENGTH      = 27
integer, parameter :: WB_MAX_ETRA_ERROR_LEN = 256
integer, parameter :: WB_MISC_BUFSZ         = 1024

integer, parameter :: WB_MAXLINESPERPAGE      = 32
integer, parameter :: WB_MAXLINESPERPAGESHIFT = 8
