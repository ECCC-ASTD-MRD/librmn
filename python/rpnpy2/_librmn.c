#include <stdlib.h>
#include <stdio.h>
#include <Python.h>
#include <numpy/arrayobject.h>
#include <rmn/fst24_file.h>
#include <stddef.h> // for offsetof
#include <structmember.h> // From Python

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
    {NULL},
};

static PyObject * py_fst_record_new(PyTypeObject *type, PyObject *args, PyObject *kwds);
static PyObject * py_fst_record_str(struct py_fst_record *self);
static PyObject * py_fst_record_get_data(struct py_fst_record *self);
static PyMethodDef py_fst_record_method_defs[] = {
    {
        .ml_name = "get_data",
        .ml_doc = "Read and return the data of this record",
        .ml_flags = METH_NOARGS,
        .ml_meth = (PyCFunction) py_fst_record_get_data,
    },
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
    // .tp_dict = ...
};

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
    char *options;
    char *filename;
    // TODO: I think I have to free options and filename
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "ss", kwlist, &filename, &options)){
        // Current exception already set by function
        return -1;
    }

    struct fst24_file_ *ref = fst24_open(filename, options[0] == '\0' ? NULL : options);
    fprintf(stderr, "%s(): Pointer returned by fst24_open(): %p\n", __func__, ref);

    if(ref == NULL){
        PyErr_SetString(PyExc_RuntimeError, "Underlying fst24_open() call failed");
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

static PyObject *py_fst24_file_new_query(struct fst24_file_container *self, PyObject *args, PyObject *kwds){

    static char *kwlist[] = {"ip3", "nomvar", NULL};
    // Values must be initialized because if the keyword argumetn is not
    // specified the PyArg_ParseTupleAndKeywords will not change them
    // TODO : Look at default_fst_record to see what values represent "no value".
    int32_t ip3 = default_fst_record.ip3;
    char *nomvar = NULL;
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "|$is", kwlist, &ip3, &nomvar)){
        fprintf(stderr, "%s(): Only ip3 and nomvar criteria are supported for now as I implement the rest of the chain\n", __func__);
        return NULL;
    }

    fst_record criteria = default_fst_record;
    if(nomvar != NULL){
        strncpy(criteria.nomvar, nomvar, sizeof(criteria.nomvar));
    }

    if(ip3 != default_fst_record.ip3){
        criteria.ip3 = ip3;
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

static int *py_fst_record_init(PyObject *self, PyObject *args, PyObject *kwds)
{
    // TODO: Take reuse the keyword args from the init of the query

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

static PyObject * py_fst_record_get_data(struct py_fst_record *self)
{
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


    // TODO: Verify that NPY_OWNDATA is set so that the data of the
    // ndarray is freed when the array's refcount drops to 0.
    return PyArray_SimpleNewFromData(ndims, dims, NPY_FLOAT32, self->rec.data);
}


PyMODINIT_FUNC PyInit__librmn(void)
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

    return m;
}
