#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>
#include <Python.h>
#include <string.h>
// Protect ourselves against using deprecated API functions from earlier versions
#define NPY_NO_DEPRECATED_API NPY_1_9_API_VERSION
#include <numpy/arrayobject.h>
#include "rmn.h"
#include <App.h>
#include <stddef.h> // for offsetof
#include <structmember.h> // From Python
#include <sys/stat.h>
#include "indexing.h"

/*
 * Documentation links
 * - This uses the concepts of section 2.3:
 *   https://docs.python.org/3/extending/newtypes_tutorial.html#providing-finer-control-over-data-attributes
 * - If we expect people to subclass our types, we should add support for
 *   cyclic garbage collection https://docs.python.org/3/extending/newtypes_tutorial.html#supporting-cyclic-garbage-collection
 * - https://docs.python.org/3/extending/newtypes_tutorial.html#supporting-cyclic-garbage-collection
 * Iterators:
 * - getiterfunc: https://docs.python.org/3/c-api/typeobj.html#c.PyTypeObject.tp_iter
 * - iternextfunc: https://docs.python.org/3/c-api/typeobj.html#c.PyTypeObject.tp_iternext
 *
 * PyTypeObject objects in detail: https://docs.python.org/3/c-api/typeobj.html
 *
 * Important structures: https://docs.python.org/3/c-api/structures.html
 * - PyGetSetDef https://docs.python.org/3/c-api/structures.html#c.PyGetSetDef
 *
 * Implementing functions and methods: https://docs.python.org/3/c-api/structures.html#implementing-functions-and-methods
 * - Converting args to C types and building values to return:
 *   https://docs.python.org/3/c-api/arg.html#strings-and-buffers
 * - Creating Python strings in general: https://docs.python.org/3/c-api/unicode.html#creating-and-accessing-unicode-strings
 *   - PyUnicode_FromString, PyUnicode_FromFormat
 * - Converting between Python and C integers: https://docs.python.org/3/c-api/long.html
 *   - PyLong_FromLong, PyLong_AsLong
 *
 * Exceptions: https://docs.python.org/3/c-api/exceptions.html
 * - Raising exceptions: https://docs.python.org/3/c-api/exceptions.html#raising-exceptions
 * - Exception classes: https://docs.python.org/3/c-api/exceptions.html#standard-exceptions
 *
 * Utilities:
 * - Sys module: https://docs.python.org/3/c-api/sys.html
 *   - PySys_WriteStderr, PySys_FormatStderr
 */

/*
 * TODO: While going through the documentation, I found that functions, when
 * they return things, should Py_INCREF() the thing before they return it.
 * Not all the time: for example, in the tutorial, Custom_new doesn't Py_INCREF()
 * the self object before returning it.  However, in section 2.3 of the tutorial,
 * the getters return `Py_NEW_REF()` of the thing they are returning.  In the
 * case of the py_fst_record_get_nomvar(), we are doing 'return PyUnicode_FromString()'
 * and the document for PyUnicode_FromString() says it returns a "new reference"
 * which I think means the refcount of the object it returns has already been
 * incremented.
 */

static PyObject *rmn_get_test_record(PyObject *self, PyObject * Py_UNUSED(args));
static PyObject *rmn_get_index_columns(PyObject *self, PyObject *args);
static PyMethodDef mymodule_method_defs[] = {
    {
        .ml_name = "get_test_record",
        .ml_doc = "Get a test record",
        .ml_flags = METH_NOARGS,
        .ml_meth = rmn_get_test_record,
    },
    {
        .ml_name = "get_index_columns",
        .ml_doc = "Get index as a tuple of columns to put in a Pandas DataFrame",
        .ml_flags = METH_VARARGS,
        .ml_meth = rmn_get_index_columns,
    },
    {NULL, NULL, 0, NULL},
};

static PyModuleDef mymodulemodule = {
    PyModuleDef_HEAD_INIT,
    .m_name = "_rmn",
    .m_doc = "Example module that creates a Person type",
    .m_size = -1,
    .m_methods = mymodule_method_defs,
};

/*******************************************************************************
 * fst24_file declarations
*******************************************************************************/
struct py_fst24_file {
    PyObject_HEAD
    struct fst24_file_ *ref;
    PyObject *filename; // for debugging
    PyObject *options; // for debugging
};
static PyObject *py_fst24_file_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static void py_fst24_file_dealloc(struct py_fst24_file *self);
static int py_fst24_file_init(struct py_fst24_file *self, PyObject *args, PyObject *kwds);
static PyObject *py_fst24_file_str(struct py_fst24_file *self, PyObject *Py_UNUSED(args));
static PyObject *py_fst24_file_new_query(struct py_fst24_file *self, PyObject *args, PyObject *kwds);
static PyObject *py_fst24_file_write(struct py_fst24_file *self, PyObject *args, PyObject *kwds);
static PyObject *py_fst24_file_get_record_by_index(struct py_fst24_file *self, PyObject *args, PyObject *kwds);

static PyMemberDef py_fst24_file_member_def[] = {
    {
        .name = "filename",
        .type = T_OBJECT_EX,
        .offset = offsetof(struct py_fst24_file, filename),
        .flags = 0,
        .doc = "filename of the file"
    },
    {
        .name = "options",
        .type = T_OBJECT_EX,
        .offset = offsetof(struct py_fst24_file, options),
        .flags = 0,
        .doc = "Options"
    },
    {NULL},
};

static PyObject *py_fst24_file_close(struct py_fst24_file *self, PyObject *Py_UNUSED(args));
static PyObject *py_fst24_file_retself(PyObject *self, PyObject *args);
static PyMethodDef py_fst24_file_method_defs[] = {
    {
        .ml_name = "new_query",
        .ml_flags = METH_VARARGS|METH_KEYWORDS,
        .ml_meth = (PyCFunction) py_fst24_file_new_query,
        .ml_doc  = "Return a query object for file",
    },
    {
        .ml_name = "write",
        .ml_flags = METH_VARARGS|METH_KEYWORDS,
        .ml_meth = (PyCFunction) py_fst24_file_write,
        .ml_doc = "Write a record into a file",
    },
    {
        .ml_name = "__enter__",
        .ml_flags = METH_VARARGS,
        .ml_meth = (PyCFunction) py_fst24_file_retself,
        .ml_doc = NULL,
    },
    {
        .ml_name = "__exit__",
        .ml_flags = METH_VARARGS,
        .ml_meth = (PyCFunction) py_fst24_file_close,
        .ml_doc = NULL,
    },
    {
        .ml_name = "get_record_by_index",
        .ml_flags = METH_VARARGS,
        .ml_meth = (PyCFunction) py_fst24_file_get_record_by_index,
        .ml_doc = "Get a record by its file_index",
    },
    {
        .ml_name = "close",
        .ml_flags = METH_NOARGS,
        .ml_meth = (PyCFunction) py_fst24_file_close,
        .ml_doc = "Explicitly close an fst24_file.  NOTE: It is preferred to either let files get closed automatically or use a `with` block to have explicit control over when a file is closed",
    },
    {NULL, NULL, 0, NULL},
};

static PyObject *py_fst24_file_empty_query(struct py_fst24_file *self);
static PyTypeObject py_fst24_file_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_rmn.fst24_file",
    .tp_doc = "Python fst24_file object",
    .tp_basicsize = sizeof(struct py_fst24_file),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst24_file_new,
    .tp_dealloc = (destructor) py_fst24_file_dealloc,
    .tp_init = (initproc) py_fst24_file_init,
    .tp_str = (reprfunc) py_fst24_file_str,
    .tp_repr = (reprfunc) py_fst24_file_str,
    .tp_members = py_fst24_file_member_def,
    .tp_methods = py_fst24_file_method_defs,
    .tp_iter = (getiterfunc) py_fst24_file_empty_query,
};
/*******************************************************************************
 * fst_query declarations
*******************************************************************************/
struct py_fst_query {
    PyObject_HEAD
    fst_query *ref;
    // Because internally the query's find_next method needs to refer to the
    // underlying file, the python object will need to hold a reference to the
    // python fst24_file object so that said python file object doesn't get
    // closed.
    //
    // For example, a quick oneliner like
    //
    //      my_query = rmn.fst24_file(filename=...).new_query(nomvar='<<')
    //
    // after this line, the refcount of the rmn.fst24_file object goes to zero
    // and the file gets closed (fst24_close()) by py_fst24_file_dealloc().
    // then, doing
    //
    //      for record in my_query:
    //          ...
    //
    // fails because the query's associated file is closed.  This is why this
    // reference is necessary.
    struct py_fst24_file *file_ref;
};

static PyObject *py_fst_query_rewind(struct py_fst_query *self);
static PyMethodDef py_fst_query_method_defs[] = {
    {
        .ml_name = "rewind",
        .ml_meth = (PyCFunction) py_fst_query_rewind,
        .ml_flags = METH_NOARGS,
        .ml_doc = "Reset a query to the beginning of the results",
    },
    {NULL, NULL, 0, NULL},
};

static PyObject *py_fst_query_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static PyObject *py_fst_query_iternext(struct py_fst_query *self);
static PyObject *py_fst_query_get_iter(struct py_fst_query *self);
static void py_fst_query_dealloc(struct py_fst_query *self);
static PyTypeObject py_fst_query_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_rmn.fst_query",
    .tp_doc = "Python fst query object",
    .tp_basicsize = sizeof(struct py_fst_query),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_dealloc = (destructor) py_fst_query_dealloc,
    .tp_new = py_fst_query_new,
    .tp_iternext = (iternextfunc) py_fst_query_iternext,
    .tp_iter = (getiterfunc) PyObject_SelfIter,
    .tp_methods = py_fst_query_method_defs,
};

/*******************************************************************************
 * fst_record declarations
*******************************************************************************/
struct py_fst_record {
    PyObject_HEAD
    fst_record rec;
    PyObject *data_array;
    struct py_fst24_file *origin;
};

// For all the fields that are of basic type we can do it this way, but for the
// string types we have to implement properties
static PyMemberDef py_fst_record_members[] = {
#if __WORDSIZE != 64
# error "Expecting __WORDSIZE to be 64 so that type 'T_LONG' from Python's defines (structmember.h) works with the attributes from the record struct that are int64_t"
#endif

    // Dates
    {
        .name = "dateo",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.dateo),
        .flags = 0,
        .doc = "Raw dateo of record",
    },
    {
        .name = "datev",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.datev),
        .flags = 0,
        .doc = "Raw datev of record",
    },

    // Type information
    {
        .name = "data_type",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.data_type),
        .flags = 0,
        .doc = "Data type of elements (see FST_TYPE* constants of librmn)"
    },
    {
        .name = "data_bits",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.data_bits),
        .flags = 0,
        .doc = "Data bits per elements in memory"
    },
    {
        .name = "pack_bits",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.pack_bits),
        .flags = 0,
        .doc = "Bits per element stored in the file"
    },

    // Dimensions of data
    {
        .name = "ni",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ni),
        .flags = 0,
        .doc = "NI of this fst_record"
    },
    {
        .name = "nj",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.nj),
        .flags = 0,
        .doc = "NJ of this fst_record"
    },
    {
        .name = "nk",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.nk),
        .flags = 0,
        .doc = "NK of this fst_record"
    },

    // DEET,NPAS
    {
        .name = "deet",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.deet),
        .flags = 0,
        .doc = "Length of time steps in seconds"
    },
    {
        .name = "npas",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.npas),
        .flags = 0,
        .doc = "Time step number"
    },

    // IP
    {
        .name = "ip1",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ip1),
        .flags = 0,
        .doc = "IP1 of this fst_record"
    },
    {
        .name = "ip2",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ip2),
        .flags = 0,
        .doc = "IP2 of this fst_record"
    },
    {
        .name = "ip3",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ip3),
        .flags = 0,
        .doc = "IP3 of this fst_record"
    },

    {
        .name = "ig1",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ig1),
        .flags = 0,
        .doc = "IG1 of this fst_record"
    },
    {
        .name = "ig2",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ig2),
        .flags = 0,
        .doc = "IG2 of this fst_record"
    },
    {
        .name = "ig3",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ig3),
        .flags = 0,
        .doc = "IG3 of this fst_record"
    },
    {
        .name = "ig4",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ig4),
        .flags = 0,
        .doc = "IG4 of this fst_record"
    },
    {
        .name = "file_index",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.file_index),
        .doc = "Persistent index of the record in the file.  Can be used to access the record in the same file at a later time provided the file has not been modified.",
    },
    {.name = NULL},
};

static PyObject *py_fst_record_get_typvar(struct py_fst_record *self);
static int py_fst_record_set_typvar(struct py_fst_record *self, PyObject *to_assign, void *closure);
static PyObject *py_fst_record_get_grtyp(struct py_fst_record *self);
static int py_fst_record_set_grtyp(struct py_fst_record *self, PyObject *to_assign, void *closure);
static PyObject *py_fst_record_get_nomvar(struct py_fst_record *self);
static int py_fst_record_set_nomvar(struct py_fst_record *self, PyObject *to_assign, void *closure);
static PyObject *py_fst_record_get_etiket(struct py_fst_record *self);
static PyObject * py_fst_record_get_data(struct py_fst_record *self);
static int py_fst_record_set_data(struct py_fst_record *self, PyObject *to_assign, void *closure);
static int py_fst_record_set_etiket(struct py_fst_record *self, PyObject *to_assign, void *closure);
static PyGetSetDef py_fst_record_properties[] = {
    {
        .name = "typvar",
        .get = (getter) py_fst_record_get_typvar,
        .set = (setter) py_fst_record_set_typvar,
        .doc = "Type of field (forecast, analysis, climatology)",
        .closure = NULL,
    },
    {
        .name = "grtyp",
        .get = (getter) py_fst_record_get_grtyp,
        .set = (setter) py_fst_record_set_grtyp,
        .doc = "Type of geographical projection",
        .closure = NULL,
    },
    {
        .name = "nomvar",
        .get = (getter) py_fst_record_get_nomvar,
        .set = (setter) py_fst_record_set_nomvar,
        .doc = "Name of the variable for this record",
        .closure = NULL,
    },
    {
        .name = "etiket",
        .get = (getter) py_fst_record_get_etiket,
        .set = (setter) py_fst_record_set_etiket,
        .doc = "Label of the variable for this record",
        .closure = NULL,
    },
    {
        .name = "data",
        .get = (getter) py_fst_record_get_data,
        .set = (setter) py_fst_record_set_data,
        .doc = "The data of the record.  Read only as needed",
        .closure = NULL,
    },
    {NULL},
};

static PyObject * py_fst_record_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static int py_fst_record_init(struct py_fst_record *self, PyObject *args, PyObject *kwds);
static void py_fst_record_dealloc(struct py_fst_record *self);
static PyObject * py_fst_record_str(struct py_fst_record *self);
static PyObject *py_fst_record_richcompare(struct py_fst_record *self, PyObject *other, int op);
static PyMethodDef py_fst_record_method_defs[] = {
    {NULL, NULL, 0, NULL},
};

static PyTypeObject py_fst_record_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_rmn.fst_record",
    .tp_doc = "Python fst_record object",
    .tp_basicsize = sizeof(struct py_fst_record),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst_record_new,
    .tp_init = (initproc) py_fst_record_init,
    .tp_str = (reprfunc) py_fst_record_str,
    .tp_members = py_fst_record_members,
    .tp_getset = py_fst_record_properties,
    .tp_methods = py_fst_record_method_defs,
    .tp_dealloc = (destructor) py_fst_record_dealloc,
    .tp_richcompare = (richcmpfunc) py_fst_record_richcompare,
    // .tp_dict = ...
};

/*******************************************************************************
 * Type object for custom exception
*******************************************************************************/

static PyObject * RpnExc_FstFileError;
static PyObject * RpnExc_InvalidFstDataTypeError;
static int py_rmn_create_exceptions()
{
    RpnExc_FstFileError = PyErr_NewExceptionWithDoc("rmn.FstFileError",
            "Invalid fst file", NULL, NULL);
    if(RpnExc_FstFileError == NULL){
        return -1;
    }

    RpnExc_InvalidFstDataTypeError = PyErr_NewExceptionWithDoc("rmn.InvalidFstDataTypeError",
            "Invalid data type for fst record", NULL, NULL);
    if(RpnExc_InvalidFstDataTypeError == NULL){
        return -1;
    }

    return 0;
}

/*******************************************************************************
 * fst_file implementations
*******************************************************************************/

static PyObject *py_fst24_file_new(PyTypeObject *type, PyObject *args, PyObject *kwds){
    struct py_fst24_file *self = (struct py_fst24_file *) type->tp_alloc(type, 0);
    if(self == NULL) {
        return NULL;
    }

    self->filename = NULL;
    self->options = NULL;

    return (PyObject *) self;
}

// 
static int py_fst24_file_init(struct py_fst24_file *self, PyObject *args, PyObject *kwds){
    static char *kwlist[] = {"filename", "options", NULL};
    char *options = NULL;
    char *filename = NULL;
    // NOTE: I do not have to free the string buffers.  Documentation says
    // it creates a Python object and provides a const char * pointing into
    // its buffer and states: "You won’t have to release any memory yourself."
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "|$ss", kwlist, &filename, &options)){
        // Current exception already set by function
        return -1;
    }

    if(filename == NULL){
        PyErr_SetString(PyExc_TypeError, "__init__(): missing required keyword-only argument 'filename'");
        return -1;
    }

    // We might want to remove this to allow the constructor to create a new
    // empty fst file if the file doesn't exist
    // struct stat statbuf;
    // if(stat(filename, &statbuf) != 0){
    //     PyErr_Format(PyExc_FileNotFoundError, "[Errno %d] %s: '%s'", errno, strerror(errno), filename);
    //     return -1;
    // }

    struct fst24_file_ *ref = fst24_open(filename, options);

    if(ref == NULL){
        char * app_error = App_ErrorGet();
        PyErr_Format(RpnExc_FstFileError, "%s: '%s'", app_error, filename);
        return -1;
    }

    PyObject *py_filename = PyUnicode_FromString(filename);
    if(py_filename == NULL){
        // Exception already set?
        return -1;
    }

    PyObject *py_options = PyUnicode_FromString(filename);
    if(py_options == NULL){
        // Exception already set?
        return -1;
    }

    // NOTE: we are in the __init__ function, so we know that there is no
    // self->filename object prior to this assignment, otherwise, we would
    // need to decrease its refcount.
    //
    //      PyObject *tmp = self->filename;
    //      Py_INCREF(py_filename);
    //      self->filename = py_filename;
    //      Py_XDECREF(tmp);
    //
    // and there is probably a good reason why the tutorial says to do the
    // above instead of what I would have done:
    //
    //      Py_INCREF(py_filename);
    //      Py_XDECREF(self->filename);
    //      self->filename = py_filename;
    //
    Py_INCREF(py_filename);
    self->filename = py_filename;


    Py_INCREF(py_options);
    self->options = py_options;

    self->ref = ref;

    return 0;
}

static PyObject *py_fst24_file_retself(PyObject *self, PyObject *args){
    Py_INCREF(self);
    return self;
}

static PyObject *py_fst24_file_close(struct py_fst24_file *self, PyObject *Py_UNUSED(args))
{
    // PySys_FormatStderr("%s(): filename=%U\n", __func__, self->filename);
    if(!fst24_close(self->ref)){
        PyErr_SetString(RpnExc_FstFileError, App_ErrorGet());
    }
    self->ref = NULL;
    Py_RETURN_NONE;
}


int init_fst_record_from_args_and_keywords(fst_record *rec, PyObject *args, PyObject *kwds){

    *rec = default_fst_record;

    static char *kwlist[] = {"dateo", "datev", "data_type", "data_bits", "pack_bits", // iiiii
                             "deet", "npas",                                          // ii
                             "ni", "nj", "nk", // iii
                             "ip1", "ip2", "ip3", // iii
                             "ig1", "ig2", "ig3", "ig4", // iiii
                             "typvar", "grtyp", "nomvar", "etiket", // ssss
                             "record", // O
    };
    // Values must be initialized because if the keyword argumetn is not
    // specified the PyArg_ParseTupleAndKeywords will not change them
    // TODO : Look at default_fst_record to see what values represent "no value".
    char *nomvar = NULL;
    char *grtyp = NULL;
    char *typvar = NULL;
    char *etiket = NULL;
    struct py_fst_record *py_record_arg = NULL;
    // PyArg_ParseTupleAndKeywords will set nomvar to point to the buffer of a
    // of some python object, which means it is tied to the lifetime of that
    // object.  Therefore we can't do what what we do with ip3.  Same for all
    // types where we pass the address of a pointer.
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "|$iiiiiiiiiiiiiiiiissssO", kwlist, 
                &rec->dateo, &rec->datev, &rec->data_type, &rec->data_bits, &rec->pack_bits,
                &rec->deet, &rec->npas,
                &rec->ni, &rec->nj, &rec->nk,
                &rec->ip1, &rec->ip2, &rec->ip3,
                &rec->ig1, &rec->ig2, &rec->ig3, &rec->ig4,
                &typvar, &grtyp, &nomvar, &etiket,
                &py_record_arg
    )){
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): ERROR Parsing arguments", __func__);
        return 1;
    }

    /*
     * Something seems inelegant about this.  If people use this function with
     * file.new_query(record=some_rec, ip1=8), the ip1=8 will have no effec and
     * should either be considered an error or maybe we could modify this to
     * take everything from the record some_rec, then set ip1=8.
     *
     * I think that would be something like 
     *      return_rec = default_fst_record;
     *      specific_rec = default_fst_record;
     *      record_arg = default_fst_record;
     *      PyArg_ParseTupleAndKeywords(...., &specific_rec->dateo, &specific_rec->datev, ..., &typvar, &grtyp, &nomvar, &etiket)
     * then
     *      if(py_record_arg != null){
     *          // Use everything from the record provided as an argument
     *          *return_rec = *py_record_arg->rec;
     *          // For each attribute, if it was provided specifically, use that instead
     *          if(specific_rec->ip1 != default_fst_record->ip1){ return_rec->ip1 = specific_rec->ip1; }
     *          if(specific_rec->ip2 != default_fst_record->ip2){ return_rec->ip2 = specific_rec->ip2; }
     *          ...
     *          if(nomvar != NULL){strncpy(nomvar, return_rec->nomvar, sizeof(return_rec->nomvar));}
     *          ...
     *      } else {
     *          *return_rec = *specific_rec;
     *      }
     *
     *
     */
    if(py_record_arg != NULL){
        *rec = py_record_arg->rec;
        return 0;
    }


    if(typvar != NULL){
        strncpy(rec->typvar, typvar, sizeof(rec->typvar));
    }
    if(grtyp != NULL){
        strncpy(rec->grtyp, grtyp, sizeof(rec->grtyp));
    }
    if(nomvar != NULL){
        strncpy(rec->nomvar, nomvar, sizeof(rec->nomvar));
    }
    if(etiket != NULL){
        strncpy(rec->etiket, etiket, sizeof(rec->etiket));
    }

    return 0;
}

static PyObject *py_fst24_file_new_query(struct py_fst24_file *self, PyObject *args, PyObject *kwds){

    fst_record criteria;
    int err = init_fst_record_from_args_and_keywords(&criteria, args, kwds);
    if(err){
        return NULL;
    }

    // if(criteria.dateo != default_fst_record.dateo){
    //     Lib_Log(APP_LIBRMN, APP_DEBUG, "WARNING: Removing dateo from search criteria\n");
    //     criteria.dateo = default_fst_record.dateo;
    // }

    fst_query *new_query = fst24_new_query(self->ref, &criteria, NULL);
    if(new_query == NULL){
        PyErr_SetString(RpnExc_FstFileError, App_ErrorGet());
        return NULL;
    }

    // Because py_fst_query_new uses type->tp_alloc, it automatically returns
    // an object with a refcount of 1.
    struct py_fst_query *py_new_query = (struct py_fst_query *) py_fst_query_new(&py_fst_query_type, NULL, NULL);
    py_new_query->ref = new_query;

    Py_INCREF(self);
    py_new_query->file_ref = self;

    return (PyObject *)py_new_query;
}

static PyObject *py_fst24_file_empty_query(struct py_fst24_file *self)
{
    // Should be able to call py_fst24_file_new_query with no arguments
    // but for that I need to build an empty tuple and empty dictionary for
    // the args and kwargs
    fst_query *new_query = fst24_new_query(self->ref, &default_fst_record, NULL);
    if(new_query == NULL){
        PyErr_SetString(RpnExc_FstFileError, App_ErrorGet());
        return NULL;
    }

    struct py_fst_query *py_new_query = (struct py_fst_query *) py_fst_query_new(&py_fst_query_type, NULL, NULL);
    py_new_query->ref = new_query;

    Py_INCREF(self);
    py_new_query->file_ref = self;

    return (PyObject *)py_new_query;
}

static PyObject *py_fst24_file_write(struct py_fst24_file *self, PyObject *args, PyObject *kwds)
{
    static char *kwlist[] = {"record", "rewrite", NULL};
    struct py_fst_record *py_rec;
    int rewrite = 0;
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "Op", kwlist, &py_rec, &rewrite)){
        return NULL;
    }

    /*
     * Ensure argument is of type py_fst_record
     */
    PyTypeObject *arg_t = Py_TYPE(py_rec);
    if(arg_t != &py_fst_record_type){
        PyErr_Format(PyExc_TypeError, "Argument must be '%s' not '%s'", py_fst_record_type.tp_name, arg_t->tp_name);
    }

    /*
     * Probably should be handled by fst24_write
     */
    if(py_rec->rec.data == NULL){
        PyErr_Format(PyExc_ValueError, "Attempting to write record with NULL data would cause segfault!!!");
        return NULL;
    }

    /*
     * Make the underlying C call with error checking
     */
    if(fst24_write(self->ref, &py_rec->rec, rewrite) != 1){
        PyErr_Format(RpnExc_FstFileError, "%s", App_ErrorGet());
        return NULL;
    }

    /*
     * Nothing to return, exception would have been raised if error
     * happened.
     */
    Py_RETURN_NONE;
}

static PyObject *py_fst24_file_get_record_by_index(struct py_fst24_file *self, PyObject *args, PyObject *kwds)
{
    int index = -1;
    int ok = PyArg_ParseTuple(args, "i", &index);
    if(!ok){
        return NULL;
    }
    fst_record rec;
    if(fst24_get_record_by_index(self->ref, index, &rec)){
        PyErr_SetString(RpnExc_FstFileError, App_ErrorGet());
        return NULL;
    }

    struct py_fst_record *py_rec = (struct py_fst_record *) py_fst_record_new(&py_fst_record_type, NULL, NULL);
    py_rec->rec = rec;

    return (PyObject *)py_rec;
}

static PyObject *py_fst24_file_str(struct py_fst24_file *self, PyObject *Py_UNUSED(args)){
    return PyUnicode_FromFormat("fst24_file(filename=%S, options=%S)", self->filename, self->options);
}

static void py_fst24_file_dealloc(struct py_fst24_file *self)
{
    if(self->filename == NULL){
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s() file=(null)\n", __func__);
    } else {
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s() file=%s\n", __func__, PyUnicode_AsUTF8(self->filename));
        // PySys_FormatStderr("%s() file=%U\n", __func__, self->filename);
    }

    Py_XDECREF(self->filename);
    Py_XDECREF(self->options);
    if(self->ref != NULL){
        // TODO Error handling for this
        fst24_close(self->ref);
    } else {
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): self->ref is NULL so not calling fst24_close()\n", __func__);
    }
    Py_TYPE(self)->tp_free((PyObject *)self);
}

/*******************************************************************************
 * fst_query implementations
 ******************************************************************************/
static PyObject *py_fst_query_new(PyTypeObject *type, PyObject *args, PyObject *kwds){
    // The python object returned by the type->tp_alloc method has a refcount
    // of 1 already set on it.
    struct py_fst_query *self = (struct py_fst_query *) type->tp_alloc(type, 0);
    if(self == NULL){
        return NULL;
    }

    self->ref = NULL;

    // I wonder if we can prevent users from creating query objects and
    // somehow ensure that they are only created using the `fst24_file.new_query`
    // method.
    self->file_ref = NULL;

    return (PyObject *)self;
}

static void py_fst_query_dealloc(struct py_fst_query *self)
{
    if(self->file_ref == NULL || self->file_ref->filename == NULL){
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s() file=(null)\n", __func__);
    } else {
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s() file=%s\n", __func__, PyUnicode_AsUTF8(self->file_ref->filename));
    }

    if(self->ref != NULL){
        fst24_query_free(self->ref);
    }

    // Py_XDECREF() instead of Py_DECREF() because the file may be null since
    // we are not enforcing that users only create queries using the `.new_query()`
    // method of the `py_fst24_file`.
    struct py_fst24_file *tmp;
    tmp = self->file_ref;
    self->file_ref = NULL;
    Py_XDECREF(tmp);
}

static PyObject *py_fst_query_get_iter(struct py_fst_query *self){
    // This is more for when a container type wants to provide an iterator
    // but I think the query *is* the iterator.  If the thing already is
    // an iterator, this method should return the object instance itself.
    return (PyObject *)self;
}

static PyObject *py_fst_query_iternext(struct py_fst_query *self)
{
    fst_record result = default_fst_record;
    fst_query *query = self->ref;

    if(!fst24_find_next(query, &result)){
        // Iterators signal the end of iteration by raising this
        // exception type, this is not an error
        PyErr_SetString(PyExc_StopIteration, "No more results in query");
        return NULL;
    }

    // TODO Records that come from a file need to hold a reference to that file
    // otherwise reading their data could attempt to read from a file that
    // doesn't exist anymore.
    struct py_fst_record *py_rec = (struct py_fst_record *) py_fst_record_new(&py_fst_record_type, NULL, NULL);
    py_rec->rec = result;
    py_rec->origin = self->file_ref;
    Py_INCREF(py_rec->origin);

    return (PyObject *)py_rec;
}

static PyObject *py_fst_query_rewind(struct py_fst_query *self)
{
    int ok = fst24_rewind_search(self->ref);
    if(!ok){
        PyErr_Format(RpnExc_FstFileError, "Call to fst24_rewind_search(%p) failed", self->ref);
        return NULL;
    }

    Py_RETURN_NONE;
}

static PyObject * py_fst_record_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    struct py_fst_record *self = (struct py_fst_record *) type->tp_alloc(type, 0);
    self->rec = default_fst_record;

    if(self == NULL){
        return NULL;
    }

    return (PyObject *)self;
}

static void py_fst_record_dealloc(struct py_fst_record *self)
{
    // PySys_FormatStderr("%s()\n", __func__);
    // TODO: Need to be careful when freeing the memory:
    // suppose the user has a record:
    // >>> my_record = <a record>  # refcount(my_record)=1
    // >>> data = my_record.data # refcount(my_record)=1, refcount(data)=2 (record has a reference, data is another)
    // >>> my_record = None # refcount(my_record)=0, refcount(data)=1
    // The user still has a reference to the numpy array whose data is rec.data
    // so rec.data should not be freed.

    Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): Freeing self->rec=%p, self->rec.data=%p\n", __func__, &(self->rec), self->rec.data);
    if(self->rec.data != NULL){
        // If rec.data != NULL, that means we created a numpy array holding the
        // data, that data should belong to the numpy array
        self->rec.data = NULL;
    }

    fst24_record_free(&self->rec);

    // Decrease the refcount of the numpy array, this stuff with tmp is because
    // of the possibility of reference cycles.
    //
    //      Py_DECREF(self->data_array);
    //
    // could lead to
    // - destructor of self
    //   - decref self->attrib
    //     - call destructor of self->attrib
    //       - Reference cycle leads to destructor of self with self->data_array
    //         having not yet been set to NULL,
    //
    // so we want to set the attribute to NULL before we decref the object the
    // attribute points to.
    //
    // py_fst_record cannot have a reference to itself but it can be subclassed
    // and we want to do it the way the documentation says we should.
    PyObject *tmp;
    tmp = self->data_array;
    self->data_array = NULL;
    Py_XDECREF(tmp);

    tmp = (PyObject *)self->origin;
    self->origin = NULL;
    Py_XDECREF(tmp);

    Py_TYPE(self)->tp_free((PyObject *)self);
}

static int py_fst_record_init(struct py_fst_record *self, PyObject *args, PyObject *kwds)
{
    int err = init_fst_record_from_args_and_keywords(&self->rec, args, kwds);
    if(err){
        return -1;
    }

    return 0;
}

static PyObject * py_fst_record_str(struct py_fst_record *self)
{
    return PyUnicode_FromFormat("fst_record(nomvar='%s', ETIKET='%s', ni=%d, nj=%d, nk=%d dateo=%d, ip1=%d, ip2=%d, ip3=%d, deet=%d, npas=%d, dty=%d, grtyp=%s, ig1=%d, ig2=%d, ig3=%d, ig4=%d)",
            self->rec.nomvar, self->rec.etiket,
            self->rec.ni, self->rec.nj, self->rec.nk,
            self->rec.dateo,
            self->rec.ip1, self->rec.ip2, self->rec.ip3,
            self->rec.deet, self->rec.npas, self->rec.data_type, self->rec.grtyp,
            self->rec.ig1, self->rec.ig2, self->rec.ig3, self->rec.ig4
    );
}
static PyObject *get_string_attr_trim(PyObject *self, size_t offset, int len)
{
    char buf[len+1];
    strncpy(buf, (void*)self + offset, len+1);
    char *p = buf+len;
    for(; p != buf; p--){
        if(*p != ' ' && *p != '\0'){
            break;
        }
    }
    *(p+1) = '\0';

    return PyUnicode_FromString(buf);
}


static PyObject *py_fst_record_get_nomvar(struct py_fst_record *self)
{
    return get_string_attr_trim((PyObject *)self, offsetof(struct py_fst_record, rec.nomvar), FST_NOMVAR_LEN);
}

static PyObject *py_fst_record_get_typvar(struct py_fst_record *self)
{
    return get_string_attr_trim((PyObject *)self, offsetof(struct py_fst_record, rec.typvar), FST_TYPVAR_LEN);
}

static PyObject *py_fst_record_get_grtyp(struct py_fst_record *self)
{
    return get_string_attr_trim((PyObject *)self, offsetof(struct py_fst_record, rec.grtyp), FST_GTYP_LEN);
}

static PyObject *py_fst_record_get_etiket(struct py_fst_record *self)
{
    return get_string_attr_trim((PyObject *)self, offsetof(struct py_fst_record, rec.etiket), FST_ETIKET_LEN);
}


static int set_attr_string_pad_space(PyObject* self, PyObject *to_assign, size_t offset, size_t len, char *name){
    /*
     * I don't know if some things expect the spaces at the end so just to
     * be sure, when setting, I will add them.
     */
    Py_ssize_t size;
    const char *typvar_buf = PyUnicode_AsUTF8AndSize(to_assign, &size);
    // Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): size = %lu\n", __func__, size);
    if(size > len + 1){
        PyErr_Format(PyExc_ValueError, "Cannot assign string longer than '%d' to %s of record: %lu", len, name, size);
        return -1;
    }
    const char *p = typvar_buf;
    char *q = (void*)self + offset;
    int i = 0;
    for(; i < len ; i++){
        if( *p == '\0' ){
            break;
        }
        q[i] = p[i];
    }
    for(; i < len ; i++){
        q[i] = ' ';
    }
    q[len] = '\0';
    return 0;
}

static int py_fst_record_set_grtyp(struct py_fst_record *self, PyObject *to_assign, void *closure)
{
    return set_attr_string_pad_space((PyObject *)self, to_assign, offsetof(struct py_fst_record, rec.grtyp), FST_GTYP_LEN, "grtyp");
}

static int py_fst_record_set_typvar(struct py_fst_record *self, PyObject *to_assign, void *closure)
{
    return set_attr_string_pad_space((PyObject *)self, to_assign, offsetof(struct py_fst_record, rec.typvar), FST_TYPVAR_LEN, "typvar");
}

static int py_fst_record_set_nomvar(struct py_fst_record *self, PyObject *to_assign, void *closure)
{
    return set_attr_string_pad_space((PyObject *)self, to_assign, offsetof(struct py_fst_record, rec.nomvar), FST_NOMVAR_LEN, "nomvar");
}

static int py_fst_record_set_etiket(struct py_fst_record *self, PyObject *to_assign, void *closure)
{
    return set_attr_string_pad_space((PyObject *)self, to_assign, offsetof(struct py_fst_record, rec.etiket), FST_ETIKET_LEN, "etiket");
}

static void free_capsule_ptr(void *capsule) {
    const char * name = PyCapsule_GetName(capsule);
    void * ptr = PyCapsule_GetPointer(capsule, name);
    Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): Name=%s: freeing pointer %p\n", __func__, name, ptr);
    free(ptr);
};

static int librmn_type_to_numpy_type(int librmn_type){
    switch(librmn_type){
        case 5: return NPY_FLOAT32;
        case 123: /*TODO*/ return NPY_FLOAT64;
        default: return -1;
    }
}

static PyObject * py_fst_record_get_data(struct py_fst_record *self)
{
    if(self->data_array == NULL){

        // If no data has been set on the record and there is no file to read
        // the data from, return None.  This access is not an error.
        if(self->rec.do_not_touch.handle < 0){
            Py_RETURN_NONE;
        }

        if(fst24_read_record(&self->rec) < 0) {
            PyErr_Format(RpnExc_FstFileError, "Accessing record data: %s", App_ErrorGet());
            return NULL;
        }

        // TODO: Implemented a basic checking to ensure we can only access the
        //       data of float and double type records because those are the
        //       only types for which I've implemented the creation of a numpy
        //       array from the data.
        //       - Need to verify that the checking is correct
        //       - Need to code for creation of numpy arrays for the other
        //         possible types.
        switch(self->rec.data_type & (~FST_TYPE_TURBOPACK)){
            case FST_TYPE_REAL:
            case FST_TYPE_REAL_IEEE:
                break;
            default:
                PyErr_Format(PyExc_NotImplementedError, "%s() Cannot get data for record that is of other type than FST_TYPE_REAL or FST_TYPE_REAL_IEEE: %d", __func__, self->rec.data_type);
                return NULL;
        }

        int typenum;
        switch(self->rec.data_bits){
            case 32: typenum = NPY_FLOAT32; break;
            case 64: typenum = NPY_FLOAT64; break;
            default:
                PyErr_Format(PyExc_NotImplementedError, "%s() Only records with elements of size (record.data_bits) equal to 32 (float) and 64 (double) are handled: %d", __func__, self->rec.data_bits);
                return NULL;
        }

        // TODO: Consider if we want to have the shape be (ni, nj) if nk == 1,
        // otherwise we can just always return arrays of shape (ni, nj, nk).
        int ndims = 3;
        npy_intp dims[3] = {self->rec.ni, self->rec.nj, self->rec.nk};

        PyObject * array = PyArray_SimpleNewFromData(ndims, dims, typenum, self->rec.data);
        if(array == NULL){
            return NULL;
        }

        // TODO: Ensure data ordering is OK (C vs Fortran)
        //       Do we need to pass the flag NPY_ARRAY_F_CONTIGUOUS
        PyArray_UpdateFlags((PyArrayObject*)array, NPY_ARRAY_C_CONTIGUOUS);
        // Documentation says that we should not set NPY_ARRAY_OWNDATA manually:
        // https://numpy.org/doc/stable/reference/c-api/array.html#c.NPY_ARRAY_OWNDATA
        // it says to use a certain test as an example, this test can be found
        // in the source code of numpy: numpy/_core/tests/test_mem_policy.py:78-100
        // TODO: Make make sure librmn doesn't deallocate the record data:
        // - The numpy array may outlive the record
        //   def f(fst_file):
        //      r = ... get record from file
        //      return r.data # record gets deleted but numpy array lives on
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): Passing %p to PyCapsule_New\n", __func__, (self->rec).data);
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): BTW, self->rec=%p\n", __func__, &(self->rec));
        PyObject *capsule = PyCapsule_New(self->rec.data, "numpy array data capsule", (PyCapsule_Destructor)&free_capsule_ptr);
        if(capsule == NULL){
            Py_DECREF(array);
            return NULL;
        }

        if(PyArray_SetBaseObject((PyArrayObject *)array, capsule) < 0){
            Py_DECREF(array);
            Py_DECREF(capsule);
            return NULL;
        }

        self->data_array = array;
    }

    Py_INCREF(self->data_array);
    return self->data_array;
}

static int py_fst_record_set_data(struct py_fst_record *self, PyObject *to_assign, void *closure)
{
    if(self->data_array != NULL){
        PyObject *tmp = self->data_array;
        self->data_array = NULL;
        Py_XDECREF(self->data_array);
    }

    if(to_assign == NULL){
        // When the object to assign is NULL, that means the python code wants
        // us to simply delete the attribute.
        return 0;
    }

    // TODO: I did some guessing to implement basic checks on the type of the
    //       numpy array assigned to record.data to handle floats and doubles
    //       and make sure that the type of the array elements is coherent with
    //       the values of record.data_type and record.data_bits.
    //       I did a lot of guessing which should be checked.
    //       - Ensure verification is correct
    //       - Add code to handle other types of records
    switch(self->rec.data_type & (~FST_TYPE_TURBOPACK)){
        case FST_TYPE_REAL:
        case FST_TYPE_REAL_IEEE:
            break;
        default:
            PyErr_Format(PyExc_NotImplementedError, "%s() Cannot set data for record that is of other type than FST_TYPE_REAL or FST_TYPE_REAL_IEEE: %d", __func__, self->rec.data_type);
            return -1;
    }
    int type = PyArray_TYPE((PyArrayObject *)to_assign);
    int expected_data_bits;

    switch(type){
        case NPY_FLOAT32: expected_data_bits = 32; break;
        case NPY_FLOAT64: expected_data_bits = 64; break;
        default:
            PyErr_Format(PyExc_NotImplementedError, "Only numpy arrays of type NPY_FLOAT32 or NPY_FLOAT64 are handled", __func__);
            return -1;
    }

    if(expected_data_bits != self->rec.data_bits){
        PyErr_Format(PyExc_TypeError, "Data array to be assigned has elements of size %d but record has data_bits=%d", expected_data_bits, self->rec.data_bits);
        return -1;
    }

    // Check the type of the passed object.  Here we just refust anything that
    // is not a numpy array but we could check for other acceptable types
    // and when we go to write the record in a file, we could do something
    // different based on the type.
    if(Py_TYPE(to_assign) !=  &PyArray_Type){
        PyErr_SetString(PyExc_TypeError, "The data of a record must be a numpy array");
        return -1;
    }

    Py_INCREF(to_assign);
    self->data_array = to_assign;

    self->rec.data = PyArray_DATA((PyArrayObject *)self->data_array);

    return 0;
}
static PyObject *py_fst_record_richcompare(struct py_fst_record *self, PyObject *other, int op)
{
    if(Py_TYPE(other) != Py_TYPE(self)){
        PyErr_Format(PyExc_TypeError, "TypeError: can't compare %S with %S", Py_TYPE(self), Py_TYPE(other));
        return NULL;
    }

    switch(op){
        case Py_NE:
        case Py_EQ:
            break;
        case Py_LT:
        case Py_GT:
        case Py_LE:
        case Py_GE:
            Py_RETURN_NOTIMPLEMENTED;
    }

    int result = fst24_record_has_same_info(&self->rec, &((struct py_fst_record*)other)->rec);
    if(op == Py_NE){
        result = ! result;
    }

    if(result){
        Py_RETURN_TRUE;
    } else {
        Py_RETURN_FALSE;
    }
}
static PyObject *rmn_get_test_record(PyObject *self, PyObject * Py_UNUSED(args))
{
    struct py_fst_record *obj = (struct py_fst_record*)py_fst_record_type.tp_alloc(&py_fst_record_type, 0);
    fst_record test_record;
    test_record = default_fst_record;
    test_record.data = NULL;
    test_record.pack_bits = 32;
    test_record.dateo= 458021600;
    test_record.deet = 300;
    test_record.npas = 0;
    test_record.ip1  = 1;
    test_record.ip2  = 10;
    test_record.ip3  = 100;
    strcpy(test_record.typvar, "P");
    strcpy(test_record.nomvar, "WAVE");
    strcpy(test_record.etiket, "float");
    strcpy(test_record.grtyp, "X");
    test_record.ig1   = 0;
    test_record.ig2   = 0;
    test_record.ig3   = 0;
    test_record.ig4   = 0;
    test_record.data_type = FST_TYPE_REAL_IEEE;
    test_record.data_bits = 32;
    // test_record.metadata = Meta_NewObject(META_TYPE_FILE, NULL);
    obj->rec = test_record;
    return (PyObject *)obj;
}

/*******************************************************************************
 * Indexing functions.  These functions, along with the functions in indexing.c
 * serve to create a Pandas DataFrame whose rows are the information about a
 * record.
 *
 * To achieve this, we create a Python dictionnary whose keys are column names
 * and whose values are columns in the form of a numpy array.
 *
 * Improvements
 * - There are some inefficiencies with how the data is gathered then processed
 *   into a format for the string columns.  This could be improved by changing
 *   the RecordData struct and gathering code so that the is already in the
 *   memory layout required for the numpy array.
 * - There are two ways of getting numpy arrays of strings, one is the method
 *   currently used where all the strings are in a single block of memory and
 *   the other is to have the numpy array be an array of arbitrary objects
 *   (NPY_OBJECT) but then we would need to create individual python string
 *   objects each with their own memeory (the numpy array would be an array of
 *   pointers).  This is a choice to make.
 * - The python function takes only a single filename.  It could be made to
 *   take a Python list and we could do a link of all the files in that list.
 * - Since we don't know the number of records ahead of time, we can't allocate
 *   the RecordData struct without getting all the records.  I made a simple
 *   vector<fst_record> (in C++ parlance) to get an array of all the records
 *   Maybe there is a better way to use this vector to create the numpy arrays.
*******************************************************************************/
int make_1d_array_and_add_to_dict(PyObject *dict, const char *key, int nb_items, int type, void *data);
int make_1d_string_array_and_add_to_dict(PyObject *dict, const char *key, int nb_items, int max_str_length, char *data);
static PyObject *rmn_get_index_columns(PyObject *self, PyObject *args){

    PyObject *file_list = NULL;
    int ok = PyArg_ParseTuple(args, "O", &file_list);
    if(!ok){
        // Exception already set by PyArg_ParseTuple
        return NULL;
    }

    /*
     * Convert list of Python unicode strings into an array of char *
     */
    // TODO: Verify that the object is a list and throw exception otherwise
    Py_ssize_t nb_files = PyList_Size(file_list);
    const char *filenames[nb_files]; // TODO: Automatic arrays, some people don't like them and if they're too big they can blow the stack
    for(int i = 0; i < nb_files ; i++){
        PyObject *item = PyList_GetItem(file_list, i);
        if(item == NULL){
            Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): ERROR: OOPSIE: Better error handling is required to not leak memory (Phil)\n", __func__);
            return NULL;
        }
        const char *filename = PyUnicode_AsUTF8AndSize(item, NULL);
        if(filename == NULL){
            return NULL;
        }
        filenames[i] = filename;
    }

    /*
     * Get raw column data
     */
    RecordData *raw_columns = rmn_get_index_columns_raw(filenames, nb_files);
    if(raw_columns == NULL){
        PyErr_SetString(PyExc_RuntimeError, "Could not get column data");
        return NULL;
    }

    /*
     * Create a dictionnary where the keys are column names and the values
     * are numpy arrays created from the raw column data
     */
    Lib_Log(APP_LIBRMN, APP_DEBUG, "Creating numpy arrays and assigning them as keys in a dictionnary\n");
    PyObject *columns = PyDict_New();
    npy_intp dims[] = {raw_columns->nb_records};

    if(make_1d_string_array_and_add_to_dict(columns, "nomvar", raw_columns->nb_records, FST_NOMVAR_LEN, raw_columns->nomvar)){goto error;}
    if(make_1d_string_array_and_add_to_dict(columns, "typvar", raw_columns->nb_records, FST_TYPVAR_LEN, raw_columns->typvar)){goto error;}
    if(make_1d_string_array_and_add_to_dict(columns, "etiket", raw_columns->nb_records, FST_ETIKET_LEN, raw_columns->etiket)){goto error;}

    if(make_1d_array_and_add_to_dict(columns, "ni", raw_columns->nb_records, NPY_INT32, raw_columns->ni)){ goto error; }
    if(make_1d_array_and_add_to_dict(columns, "nj", raw_columns->nb_records, NPY_INT32, raw_columns->nj)){ goto error; }
    if(make_1d_array_and_add_to_dict(columns, "nk", raw_columns->nb_records, NPY_INT32, raw_columns->nk)){ goto error; }

    if(make_1d_array_and_add_to_dict(columns, "dateo", raw_columns->nb_records, NPY_INT32, raw_columns->dateo)){ goto error;}

    if(make_1d_array_and_add_to_dict(columns, "ip1", raw_columns->nb_records, NPY_INT32, raw_columns->ip1)){ goto error; }
    if(make_1d_array_and_add_to_dict(columns, "ip2", raw_columns->nb_records, NPY_INT32, raw_columns->ip2)){ goto error; }
    if(make_1d_array_and_add_to_dict(columns, "ip3", raw_columns->nb_records, NPY_INT32, raw_columns->ip3)){ goto error; }

    if(make_1d_array_and_add_to_dict(columns, "deet", raw_columns->nb_records, NPY_INT32, raw_columns->deet)){ goto error;}
    if(make_1d_array_and_add_to_dict(columns, "npas", raw_columns->nb_records, NPY_INT32, raw_columns->npas)){ goto error;}
    if(make_1d_array_and_add_to_dict(columns, "data_type", raw_columns->nb_records, NPY_INT32, raw_columns->data_type)){ goto error;}
    if(make_1d_array_and_add_to_dict(columns, "pack_bits", raw_columns->nb_records, NPY_INT32, raw_columns->pack_bits)){ goto error;}

    if(make_1d_string_array_and_add_to_dict(columns, "grtyp", raw_columns->nb_records, FST_GTYP_LEN, raw_columns->grtyp)){goto error;}

    if(make_1d_array_and_add_to_dict(columns, "ig1", raw_columns->nb_records, NPY_INT32, raw_columns->ig1)){ goto error; }
    if(make_1d_array_and_add_to_dict(columns, "ig2", raw_columns->nb_records, NPY_INT32, raw_columns->ig2)){ goto error; }
    if(make_1d_array_and_add_to_dict(columns, "ig3", raw_columns->nb_records, NPY_INT32, raw_columns->ig3)){ goto error; }
    if(make_1d_array_and_add_to_dict(columns, "ig4", raw_columns->nb_records, NPY_INT32, raw_columns->ig4)){ goto error; }

    if(make_1d_array_and_add_to_dict(columns, "file_index", raw_columns->nb_records, NPY_INT32, raw_columns->file_index)){ goto error; }

    // if(make_1d_array_and_add_to_dict(columns, "swa", raw_columns->nb_records, NPY_INT32, raw_columns->swa)){ goto error; }
    // if(make_1d_array_and_add_to_dict(columns, "lng", raw_columns->nb_records, NPY_INT32, raw_columns->lng)){ goto error; }
    // if(make_1d_array_and_add_to_dict(columns, "dltf", raw_columns->nb_records, NPY_INT32, raw_columns->dltf)){ goto error; }
    // if(make_1d_array_and_add_to_dict(columns, "dltc", raw_columns->nb_records, NPY_INT32, raw_columns->dltc)){ goto error; }

    // if(make_1d_array_and_add_to_dict(columns, "extra1", raw_columns->nb_records, NPY_INT32, raw_columns->extra1)){ goto error; }
    // if(make_1d_array_and_add_to_dict(columns, "extra2", raw_columns->nb_records, NPY_INT32, raw_columns->extra2)){ goto error; }
    // if(make_1d_array_and_add_to_dict(columns, "extra3", raw_columns->nb_records, NPY_INT32, raw_columns->extra3)){ goto error; }
    //if(make_1d_string_array_and_add_to_dict(columns, "path", raw_columns->nb_records, PATH_MAX+1, raw_columns->path)){ goto error; }

    free(raw_columns); // The struct contains a bunch of arrays but we don't want
                       // to free them since we gave them away to some numpy arrays
    Lib_Log(APP_LIBRMN, APP_DEBUG, "Handing control back to Python\n");
    return columns;
error:
    free(raw_columns);
    // TODO: Error handling for all the numpy arrays we created.
    // Doing DECREF(columns) will destroy the dictionnary, and every array
    // we added to the dictionnary will get DECREF'd by the destruction of the
    // dictionnary, and all the make_1d... destroy the array if they can't
    // add it to the dictionnary so I think we're good here.
    // What about the ones that we didn't get to yet, we need to free those
    // but not the ones that we gave to the dictionnary.
    // We'll have to think about that one.
    Py_XDECREF(columns);
    // All functions called here that could fail will set the exception.
    return NULL;
}

static PyObject * make_array_owning_data(int ndims, npy_intp *dims, int type, void *data){
    PyObject *array = PyArray_SimpleNewFromData(ndims, dims,NPY_INT32, data);
    if(array == NULL){
        return NULL;
    }
    PyObject *capsule = PyCapsule_New(data, "raw_columns ni", (PyCapsule_Destructor)free_capsule_ptr);
    if(capsule == NULL){
        Py_DECREF(array);
        return NULL;
    }
    if(PyArray_SetBaseObject((PyArrayObject*)array, capsule) < 0){
        Py_DECREF(array);
        Py_DECREF(capsule);
        return NULL;
    }

    return array;
}

int make_1d_string_array_and_add_to_dict(PyObject *dict, const char *key, int nb_items, int max_str_length, char *data)
{
    npy_intp dims[] = {nb_items};
    PyObject *array_1d = PyArray_New(&PyArray_Type, 1, dims, NPY_STRING, NULL, data, max_str_length, NPY_ARRAY_OWNDATA, NULL);
    if(array_1d == NULL){
        PyErr_Format(PyExc_RuntimeError, "Could not create numpy array for column '%s'", key);
        return 1;
    }
    if(PyDict_SetItemString(dict, key, array_1d)){
        Py_DECREF(array_1d);
        return 1;
    }
    return 0;
}

int make_1d_array_and_add_to_dict(PyObject *dict, const char *key, int nb_items, int type, void *data)
{
    npy_intp dims[] = {nb_items};
    PyObject *array_1d = make_array_owning_data(1, dims, NPY_INT32, data);
    if(array_1d == NULL){
        PyErr_Format(PyExc_RuntimeError, "Could not create numpy array for column '%s'", key);
        return 1;
    }
    if(PyDict_SetItemString(dict, key, array_1d)){
        Py_DECREF(array_1d);
        return 1;
    }
    return 0;
}




PyMODINIT_FUNC PyInit__rmn(void)
{
    /*
     * Important!  This macro must be called before any function of the
     * Numpy C API is called.  Otherwise, a segfault will occur when the first
     * of these functions is called.
     *
     * This macro does `return NULL;` if the import of numpy fails so we
     * put it at the start so there is nothing to Py_DECREF().
     */
    import_array();

    int err = py_rmn_create_exceptions();
    if(err){
        PyErr_SetString(PyExc_ImportError, "Error initializing exceptions types for rmn");
        return NULL;
    }

    /*
     * Create the actual python module object from the PyModuleDef
     */
    PyObject *m = PyModule_Create(&mymodulemodule);
    if(m == NULL){
        return NULL;
    }

    /*
     * Initialize type object and add to module for fst24_file
     */
    if(PyType_Ready(&py_fst24_file_type) < 0){
        return NULL;
    }

    Py_INCREF(&py_fst24_file_type);
    if(PyModule_AddObject(m, "fst24_file", (PyObject *)&py_fst24_file_type) < 0){
        Py_DECREF(&py_fst24_file_type);
        Py_DECREF(m);
        return NULL;
    }

    /*
     * Initialize type object and add to module for fst_query
     * The name given here is the one that this class is accessed with.  The
     * .tp_name in the PyTypeObject is the name you access the class with the
     * name below and ask it for what its name is.
     */
    if(PyType_Ready(&py_fst_query_type) < 0){
        return NULL;
    }

    Py_INCREF(&py_fst_query_type);
    if(PyModule_AddObject(m, "fst_query", (PyObject *)&py_fst_query_type) < 0){
        Py_DECREF(&py_fst_query_type);
        Py_DECREF(m);
        return NULL;
    }

    /*
     * Initialize type object and add to module for fst_record
     */
    if(PyType_Ready(&py_fst_record_type) < 0){
        return NULL;
    }

    Py_INCREF(&py_fst_record_type);
    if(PyModule_AddObject(m, "fst_record", (PyObject *)&py_fst_record_type) < 0){
        Py_DECREF(&py_fst_record_type);
        Py_DECREF(m);
        return NULL;
    }

    /*
     * Add the exception objects to the module
     */
    if(PyModule_AddObject(m, "FstFileError", (PyObject *)RpnExc_FstFileError) < 0){
        Py_DECREF(RpnExc_FstFileError);
        Py_DECREF(m);
        return NULL;
    }

    if(PyModule_AddObject(m, "InvalidFstDataTypeError", (PyObject *)RpnExc_InvalidFstDataTypeError) < 0){
        Py_DECREF(RpnExc_InvalidFstDataTypeError);
        Py_DECREF(m);
        return NULL;
    }

    return m;
}
