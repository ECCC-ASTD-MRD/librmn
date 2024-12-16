#ifndef IO_SERVER_CB_DATA_H_
#define IO_SERVER_CB_DATA_H_

#if 0
// List of defines that are used from both C and Fortran code
#endif

#define CB_NO_COMMIT_val 0
#define CB_COMMIT_val 1
#define CB_PEEK_val 2

#define DCB_SERVER_BOUND_TYPE_val 0
#define DCB_CLIENT_BOUND_TYPE_val 1
#define DCB_CHANNEL_TYPE_val 2

#define CB_SUCCESS_val               0
#define CB_ERROR_val                -1
#define CB_INVALID_POINTER_val      -2
#define CB_INVALID_VERSION_val      -3
#define CB_INVALID_FIRST_val        -4
#define CB_INVALID_FULL_IN_val      -5
#define CB_INVALID_FULL_OUT_val     -6
#define CB_INVALID_PARTIAL_IN_val   -7
#define CB_INVALID_PARTIAL_OUT_val  -8
#define CB_INSUFFICIENT_SPACE_val   -12
#define CB_TIMEOUT_val              -20
#define CB_NOT_ALLOWED_val          -30

#define DCB_INVALID_CAPACITY_val    -60
#define DCB_INVALID_RANK_val        -61
#define DCB_INVALID_INSTANCE_val    -62
#define DCB_WRONG_CALLER_ROLE_val   -63
#define DCB_INVALID_BUFFER_ID_val   -64

#ifndef IN_FORTRAN_CODE
enum
{
    CB_NO_COMMIT = CB_NO_COMMIT_val,
    CB_COMMIT    = CB_COMMIT_val,
    CB_PEEK      = CB_PEEK_val
};

enum
{
    DCB_SERVER_BOUND_TYPE = DCB_SERVER_BOUND_TYPE_val, //!< CB whose data is going from client to server
    DCB_CLIENT_BOUND_TYPE = DCB_CLIENT_BOUND_TYPE_val, //!< CB whose data is going from server to client
    DCB_CHANNEL_TYPE      = DCB_CHANNEL_TYPE_val       //!< Server process that only serves as a communication channel (either server- or client-bound)
};

// Error codes
enum
  {
    CB_SUCCESS                    = CB_SUCCESS_val,
    CB_ERROR                      = CB_ERROR_val,
    CB_ERROR_INVALID_POINTER      = CB_INVALID_POINTER_val,
    CB_ERROR_INVALID_VERSION      = CB_INVALID_VERSION_val,
    CB_ERROR_INVALID_FIRST        = CB_INVALID_FIRST_val,
    CB_ERROR_INVALID_FULL_IN      = CB_INVALID_FULL_IN_val,
    CB_ERROR_INVALID_FULL_OUT     = CB_INVALID_FULL_OUT_val,
    CB_ERROR_INVALID_PARTIAL_IN   = CB_INVALID_PARTIAL_IN_val,
    CB_ERROR_INVALID_PARTIAL_OUT  = CB_INVALID_PARTIAL_OUT_val,
    CB_ERROR_INSUFFICIENT_SPACE   = CB_INSUFFICIENT_SPACE_val,
    CB_ERROR_TIMEOUT              = CB_TIMEOUT_val,
    CB_ERROR_NOT_ALLOWED          = CB_NOT_ALLOWED_val,

    DCB_ERROR_INVALID_CAPACITY    = DCB_INVALID_CAPACITY_val,
    DCB_ERROR_INVALID_RANK        = DCB_INVALID_RANK_val,
    DCB_ERROR_INVALID_INSTANCE    = DCB_INVALID_INSTANCE_val,
    DCB_ERROR_WRONG_CALLER_ROLE   = DCB_WRONG_CALLER_ROLE_val,
    DCB_ERROR_INVALID_BUFFER_ID   = DCB_INVALID_BUFFER_ID_val,
};

#else
        integer, parameter :: CB_NO_COMMIT = CB_NO_COMMIT_val
        integer, parameter :: CB_COMMIT = CB_COMMIT_val
        integer, parameter :: CB_PEEK = CB_PEEK_val

        integer, parameter :: DCB_SERVER_BOUND_TYPE = DCB_SERVER_BOUND_TYPE_val
        integer, parameter :: DCB_CLIENT_BOUND_TYPE = DCB_CLIENT_BOUND_TYPE_val
        integer, parameter :: DCB_CHANNEL_TYPE      = DCB_CHANNEL_TYPE_val

        integer, parameter :: CB_KIND_CHAR      = -1
        integer, parameter :: CB_KIND_INTEGER_4 = -4
        integer, parameter :: CB_KIND_INTEGER_8 = -8
        integer, parameter :: CB_KIND_REAL_4    = -4
        integer, parameter :: CB_KIND_REAL_8    = -8

        integer(C_INT), parameter :: CB_SUCCESS                    = CB_SUCCESS_val
        integer(C_INT), parameter :: CB_ERROR                      = CB_ERROR_val
        integer(C_INT), parameter :: CB_ERROR_INVALID_POINTER      = CB_INVALID_POINTER_val
        integer(C_INT), parameter :: CB_ERROR_INVALID_VERSION      = CB_INVALID_VERSION_val
        integer(C_INT), parameter :: CB_ERROR_INVALID_FIRST        = CB_INVALID_FIRST_val
        integer(C_INT), parameter :: CB_ERROR_INVALID_FULL_IN      = CB_INVALID_FULL_IN_val
        integer(C_INT), parameter :: CB_ERROR_INVALID_FULL_OUT     = CB_INVALID_FULL_OUT_val
        integer(C_INT), parameter :: CB_ERROR_INVALID_PARTIAL_IN   = CB_INVALID_PARTIAL_IN_val
        integer(C_INT), parameter :: CB_ERROR_INVALID_PARTIAL_OUT  = CB_INVALID_PARTIAL_OUT_val
        integer(C_INT), parameter :: CB_ERROR_INSUFFICIENT_SPACE   = CB_INSUFFICIENT_SPACE_val
        integer(C_INT), parameter :: CB_ERROR_TIMEOUT              = CB_TIMEOUT_val
        integer(C_INT), parameter :: CB_ERROR_NOT_ALLOWED          = CB_NOT_ALLOWED_val

        integer(C_INT), parameter :: DCB_ERROR_INVALID_CAPACITY    = DCB_INVALID_CAPACITY_val
        integer(C_INT), parameter :: DCB_ERROR_INVALID_RANK        = DCB_INVALID_RANK_val
        integer(C_INT), parameter :: DCB_ERROR_INVALID_INSTANCE    = DCB_INVALID_INSTANCE_val
        integer(C_INT), parameter :: DCB_ERROR_WRONG_CALLER_ROLE   = DCB_WRONG_CALLER_ROLE_val
        integer(C_INT), parameter :: DCB_ERROR_INVALID_BUFFER_ID   = DCB_INVALID_BUFFER_ID_val
#endif

#ifndef IN_FORTRAN_CODE
#include <stdint.h>
typedef int64_t data_element; //!< Type of individual elements stored in a circular buffer type container
#else
integer, parameter :: CB_DATA_ELEMENT      = C_INT64_T
integer, parameter :: CB_DATA_ELEMENT_KIND = CB_KIND_INTEGER_8
#endif

#undef CB_NO_COMMIT_val
#undef CB_COMMIT_val
#undef CB_PEEK_val

#undef DCB_SERVER_BOUND_TYPE_val
#undef DCB_CLIENT_BOUND_TYPE_val
#undef DCB_CHANNEL_TYPE_val

#undef CB_SUCCESS_val             
#undef CB_ERROR_val         
#undef CB_INVALID_POINTER_val     
#undef CB_INVALID_VERSION_val     
#undef CB_INVALID_FIRST_val       
#undef CB_INVALID_FULL_IN_val     
#undef CB_INVALID_FULL_OUT_val    
#undef CB_INVALID_PARTIAL_IN_val  
#undef CB_INVALID_PARTIAL_OUT_val 
#undef CB_INSUFFICIENT_SPACE_val
#undef CB_TIMEOUT_val             
#undef CB_NOT_ALLOWED_val

#undef DCB_INVALID_CAPACITY_val
#undef DCB_INVALID_RANK_val
#undef DCB_INVALID_INSTANCE_val
#undef DCB_WRONG_CALLER_ROLE_val
#undef DCB_INVALID_BUFFER_ID_val

#endif /* IO_SERVER_CB_DATA_H_ */
