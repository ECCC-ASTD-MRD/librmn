#include <stdlib.h>
#include <stdio.h>
#include <Python.h>
// Protect ourselves against using deprecated API functions from earlier versions
#define NPY_NO_DEPRECATED_API NPY_1_9_API_VERSION
#include <numpy/arrayobject.h>
#include <rmn/fst24_file.h>
#include <stddef.h> // for offsetof
#include <structmember.h> // From Python
#include <sys/stat.h>

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

static PyModuleDef mymodulemodule = {
    PyModuleDef_HEAD_INIT,
    .m_name = "_librmn",
    .m_doc = "Example module that creates a Person type",
    .m_size = -1,
};

/*******************************************************************************
 * fst24_file declarations
*******************************************************************************/
struct fst24_file_container {
    PyObject_HEAD
    struct fst24_file_ *ref;
    PyObject *filename; // for debugging
    PyObject *options; // for debugging
};
static PyObject *py_fst24_file_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static void py_fst24_file_dealloc(struct fst24_file_container *self);
static int py_fst24_file_init(struct fst24_file_container *self, PyObject *args, PyObject *kwds);
static PyObject *py_fst24_file_str(struct fst24_file_container *self, PyObject *Py_UNUSED(args));
static PyObject *py_fst24_file_new_query(struct fst24_file_container *self, PyObject *args, PyObject *kwds);

static PyMemberDef py_fst24_file_member_def[] = {
    {
        .name = "filename",
        .type = T_OBJECT_EX,
        .offset = offsetof(struct fst24_file_container, filename),
        .flags = 0,
        .doc = "filename of the file"
    },
    {
        .name = "options",
        .type = T_OBJECT_EX,
        .offset = offsetof(struct fst24_file_container, options),
        .flags = 0,
        .doc = "Options"
    },
    {NULL},
};

static PyMethodDef py_fst24_file_method_defs[] = {
    {
        .ml_name = "new_query",
        .ml_flags = METH_VARARGS|METH_KEYWORDS,
        .ml_meth = (PyCFunction) py_fst24_file_new_query,
        .ml_doc  = "Return a query object for file",
    },
    {NULL, NULL, 0, NULL},
};

static PyTypeObject py_fst24_file_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_librmn.fst24_file",
    .tp_doc = "Python fst24_file object",
    .tp_basicsize = sizeof(struct fst24_file_container),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst24_file_new,
    .tp_dealloc = (destructor) py_fst24_file_dealloc,
    .tp_init = (initproc) py_fst24_file_init,
    .tp_str = (reprfunc) py_fst24_file_str,
    .tp_repr = (reprfunc) py_fst24_file_str,
    .tp_members = py_fst24_file_member_def,
    .tp_methods = py_fst24_file_method_defs,
};
/*******************************************************************************
 * fst_query declarations
*******************************************************************************/
struct fst_query_container {
    PyObject_HEAD
    fst_query *ref;
};

static PyObject *py_fst_query_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static PyObject *py_fst_query_iternext(struct fst_query_container *self);
static PyObject *py_fst_query_get_iter(struct fst_query_container *self);
static PyTypeObject py_fst_query_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_librmn.fst_query",
    .tp_doc = "Python fst query object",
    .tp_basicsize = sizeof(struct fst_query_container),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst_query_new,
    .tp_iternext = (iternextfunc) py_fst_query_iternext,
    .tp_iter = (getiterfunc) py_fst_query_get_iter,
    // TODO dealloc
};

/*******************************************************************************
 * fst_record declarations
*******************************************************************************/
struct py_fst_record {
    PyObject_HEAD
    fst_record rec;
    PyObject *data_array;
};

// For all the fields that are of basic type we can do it this way, but for the
// string types we have to implement properties
static PyMemberDef py_fst_record_members[] = {
    {
        .name = "ip3",
        .type = T_INT,
        .offset = offsetof(struct py_fst_record, rec.ip3),
        .flags = 0,
        .doc = "IP3 of this fst_record"
    },
    {.name = NULL},
};

static PyObject *py_fst_record_get_nomvar(struct py_fst_record *self);
static PyObject *py_fst_record_get_etiket(struct py_fst_record *self);
static PyObject * py_fst_record_get_data(struct py_fst_record *self);
static PyGetSetDef py_fst_record_properties[] = {
    {
        .name = "nomvar",
        .get = (getter) py_fst_record_get_nomvar,
        .doc = "Name of the variable for this record",
        .closure = NULL,
    },
    {
        .name = "etiket",
        .get = (getter) py_fst_record_get_etiket,
        .doc = "Label of the variable for this record",
        .closure = NULL,
    },
    {
        .name = "data",
        .get = (getter) py_fst_record_get_data,
        .doc = "The data of the record.  Read only as needed",
        .closure = NULL,
    },
    {NULL},
};

static PyObject * py_fst_record_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static void py_fst_record_dealloc(struct py_fst_record *self);
static PyObject * py_fst_record_str(struct py_fst_record *self);
static PyMethodDef py_fst_record_method_defs[] = {
    {NULL, NULL, 0, NULL},
};

static PyTypeObject py_fst_record_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_librmn.fst_record",
    .tp_doc = "Python fst_record object",
    .tp_basicsize = sizeof(struct py_fst_record),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst_record_new,
    .tp_str = (reprfunc) py_fst_record_str,
    .tp_members = py_fst_record_members,
    .tp_getset = py_fst_record_properties,
    .tp_methods = py_fst_record_method_defs,
    .tp_dealloc = (destructor) py_fst_record_dealloc,
    // .tp_dict = ...
};

/*******************************************************************************
 * Type object for custom exception
*******************************************************************************/

static PyObject * RpnExc_InvalidFstFileError;
static PyObject * RpnExc_InvalidFstDataTypeError;
static int py_rmn_create_exceptions()
{
    RpnExc_InvalidFstFileError = PyErr_NewExceptionWithDoc("rmn.InvalidFstFileError",
            "Invalid fst file", NULL, NULL);
    if(RpnExc_InvalidFstFileError == NULL){
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
    struct fst24_file_container *self = (struct fst24_file_container *) type->tp_alloc(type, 0);
    if(self == NULL) {
        return NULL;
    }

    self->filename = NULL;
    self->options = NULL;

    return (PyObject *) self;
}

// 
static int py_fst24_file_init(struct fst24_file_container *self, PyObject *args, PyObject *kwds){
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
    struct stat statbuf;
    if(stat(filename, &statbuf) != 0){
        PyErr_Format(PyExc_FileNotFoundError, "[Errno %d] %s: '%s'", errno, strerror(errno), filename);
        return -1;
    }

    struct fst24_file_ *ref = fst24_open(filename, options);
    fprintf(stderr, "%s(): Pointer returned by fst24_open(): %p\n", __func__, ref);

    if(ref == NULL){
        PyErr_Format(RpnExc_InvalidFstFileError, "file '%s' could not be opened as fst24_file", filename);
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
    PySys_WriteStderr("%s(): self=%p, self->ref=%p, self->filename=%S, self->options=%S\n",
            __func__, self, self->ref, self->filename, self->options);

    return 0;
}

int init_fst_record_from_args_and_keywords(fst_record *rec, PyObject *args, PyObject *kwds){

    *rec = default_fst_record;

    static char *kwlist[] = {"ip3", "nomvar", NULL};
    // Values must be initialized because if the keyword argumetn is not
    // specified the PyArg_ParseTupleAndKeywords will not change them
    // TODO : Look at default_fst_record to see what values represent "no value".
    char *nomvar = NULL;
    // PyArg_ParseTupleAndKeywords will set nomvar to point to the buffer of a
    // of some python object, which means it is tied to the lifetime of that
    // object.  Therefore we can't do what what we do with ip3.  Same for all
    // types where we pass the address of a pointer.
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "|$is", kwlist, &rec->ip3, &nomvar)){
        fprintf(stderr, "%s(): Only ip3 and nomvar criteria are supported for now as I implement the rest of the chain\n", __func__);
        return 1;
    }

    if(nomvar != NULL){
        strncpy(rec->nomvar, nomvar, sizeof(rec->nomvar));
    }

    return 0;
}

static PyObject *py_fst24_file_new_query(struct fst24_file_container *self, PyObject *args, PyObject *kwds){

    fst_record criteria;
    int err = init_fst_record_from_args_and_keywords(&criteria, args, kwds);
    if(err){
        return NULL;
    }

    fst_query *q = fst24_new_query(self->ref, &criteria, NULL);
    struct fst_query_container *py_q = (struct fst_query_container *) py_fst_query_new(&py_fst_query_type, NULL, NULL);
    py_q->ref = q;

    Py_INCREF((PyObject*)py_q);
    return (PyObject *)py_q;
}

static PyObject *py_fst24_file_str(struct fst24_file_container *self, PyObject *Py_UNUSED(args)){
    fprintf(stderr, "%s(): self = %p\n", __func__, self);
    return PyUnicode_FromFormat("fst24_file(filename=%S, options=%S)", self->filename, self->options);
}

static void py_fst24_file_dealloc(struct fst24_file_container *self){
    PySys_FormatStderr("Deallocating object %S\n", self);
    Py_XDECREF(self->filename);
    Py_XDECREF(self->options);
    if(self->ref != NULL){
        // TODO Error handling for this
        fst24_close(self->ref);
    }
    Py_TYPE(self)->tp_free((PyObject *)self);
}

/*******************************************************************************
 * fst_query implementations
 ******************************************************************************/
static PyObject *py_fst_query_new(PyTypeObject *type, PyObject *args, PyObject *kwds){
    struct fst_query_container *self = (struct fst_query_container *) type->tp_alloc(type, 0);
    if(self == NULL){
        return NULL;
    }

    self->ref = NULL;

    return (PyObject *)self;
}

static PyObject *py_fst_query_get_iter(struct fst_query_container *self){
    // This is more for when a container type wants to provide an iterator
    // but I think the query *is* the iterator.  If the thing already is
    // an iterator, this method should return the object instance itself.
    return (PyObject *)self;
}

static PyObject *py_fst_query_iternext(struct fst_query_container *self)
{
    fst_record result = default_fst_record;
    fst_query *query = self->ref;

    if(!fst24_find_next(query, &result)){
        // Iterators signal the end of iteration by raising this
        // exception type, this is not an error
        PyErr_SetString(PyExc_StopIteration, "No more results in query");
        return NULL;
    }

    struct py_fst_record *py_rec = (struct py_fst_record *) py_fst_record_new(&py_fst_record_type, NULL, NULL);
    py_rec->rec = result;

    return (PyObject *)py_rec;
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
    // TODO: Need to be careful when freeing the memory:
    // suppose the user has a record:
    // >>> my_record = <a record>  # refcount(my_record)=1
    // >>> data = my_record.data # refcount(my_record)=1, refcount(data)=2 (record has a reference, data is another)
    // >>> my_record = None # refcount(my_record)=0, refcount(data)=1
    // The user still has a reference to the numpy array whose data is rec.data
    // so rec.data should not be freed.

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

static PyObject *py_fst_record_get_nomvar(struct py_fst_record *self)
{
    // TODO : Get rid of magic number
    char buf[8];
    strncpy(buf, self->rec.nomvar, 8);
    char *p = buf+7;
    for(; p != buf; p--){
        if(*p != ' ' && *p != '\0'){
            break;
        }
    }
    *(p+1) = '\0';

    return PyUnicode_FromString(buf);
}

static PyObject *py_fst_record_get_etiket(struct py_fst_record *self)
{
    // TODO : Get rid of magic number
    char buf[16];
    strncpy(buf, self->rec.etiket, 16);
    char *p = buf+15;
    for(; p != buf; p--){
        if(*p != ' ' && *p != '\0'){
            break;
        }
    }
    *(p+1) = '\0';

    return PyUnicode_FromString(buf);
}

static void free_capsule_ptr(void *capsule) {
    void * ptr = PyCapsule_GetPointer(capsule, PyCapsule_GetName(capsule));
    fprintf(stderr, "%s(): Freeing memory at %p from numpy array\n", __func__, ptr);
    free(ptr);
};

static PyObject * py_fst_record_get_data(struct py_fst_record *self)
{
    if(self->data_array == NULL){
        // TODO: Look at the record to find out what the actual data_type is
        // right now so I'm just assuming it's float
        size_t thing_size = sizeof(float);
        fst24_read_record(&self->rec);

        // TODO: Consider if we want to have the shape be (ni, nj) if nk == 1,
        // otherwise we can just always return arrays of shape (ni, nj, nk).
        // TODO: Ensure data ordering is OK (C vs Fortran)
        int ndims = 3;
        // npy_intp dims[3] = {self->rec.ni, self->rec.nj, self->rec.nk};
        npy_intp dims[3] = {self->rec.nk, self->rec.nj, self->rec.ni};

        PyObject * array = PyArray_SimpleNewFromData(ndims, dims, NPY_FLOAT32, self->rec.data);
        if(array == NULL){
            return NULL;
        }

        // Documentation says that we should not set NPY_ARRAY_OWNDATA manually:
        // https://numpy.org/doc/stable/reference/c-api/array.html#c.NPY_ARRAY_OWNDATA
        // it says to use a certain test as an example, this test can be found
        // in the source code of numpy: numpy/_core/tests/test_mem_policy.py:78-100
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
    if(PyModule_AddObject(m, "InvalidFstFileError", (PyObject *)RpnExc_InvalidFstFileError) < 0){
        Py_DECREF(RpnExc_InvalidFstFileError);
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
