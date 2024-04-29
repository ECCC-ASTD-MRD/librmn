#include <stdlib.h>
#include <stdio.h>
#include <Python.h>
#include <rmn/fst24_file.h>
#include <stddef.h> // for offsetof
#include <structmember.h> // From Python

/*
 * Invent this indirection struct because I want to be able to do
 * >>> import rpnpy2
 * >>> filename = "..."
 * >>> options = ""
 * >>> f = rpnpy2.fst24_file(filename, options)
 * but after the new function has been called we already have memory allocated
 * and we can't modify that in the __init__ function which is the function
 * that gets the filename.
 *
 * It's always possible that the new function can receive argumetns but there
 * is also something with the tp_alloc thing that I'm not sure about.
 *
 * Plust it allows me to add some fields that would just be relevant to Python.
 */
struct fst24_file_container {
    PyObject_HEAD
    struct fst24_file_ *ref;
    PyObject *filename; // for debugging
    PyObject *options; // for debugging
};

struct fst_query_container {
    PyObject_HEAD
    fst_query *ref;
};

struct py_fst_record {
    PyObject_HEAD
    fst_record *rec;
};

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

static PyObject *py_fst_query_new(PyTypeObject *type, PyObject *args, PyObject *kwds){
    struct fst_query_container *self = (struct fst_query_container *) type->tp_alloc(type, 0);
    if(self == NULL){
        return NULL;
    }

    self->ref = NULL;

    return (PyObject *)self;
}

static PyObject *py_fst_query_print(struct fst_query_container *self, PyObject *Py_UNUSED(args))
{
    Py_RETURN_NONE;
}


static PyMethodDef py_fst_query_method_defs[] = {
    {
        .ml_name = "print",
        .ml_flags = METH_NOARGS,
        .ml_meth = (PyCFunction) py_fst_query_print,
        .ml_doc = "Print info on an FST query",
    },
    {NULL, NULL, 0, NULL},
};

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

    return PyUnicode_FromFormat("Nomvar = %s, ip3 = %d", result.nomvar, result.ip3);
}

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

static PyObject *py_fst24_file_new_query(struct fst24_file_container *self, PyObject *args, PyObject *kwds){

    static char *kwlist[] = {"ip3", "nomvar", NULL};
    // Values must be initialized because if the keyword argumetn is not
    // specified the PyArg_ParseTupleAndKeywords will not change them
    int32_t ip3 = -1;
    char *nomvar = NULL;
    if(!PyArg_ParseTupleAndKeywords(args, kwds, "|$is", kwlist, &ip3, &nomvar)){
        fprintf(stderr, "%s(): Only ip3 and nomvar criteria are supported for now as I implement the rest of the chain\n", __func__);
        return NULL;
    }

    fst_record criteria = default_fst_record;
    if(nomvar != NULL){
        strncpy(criteria.nomvar, nomvar, sizeof(criteria.nomvar));
    }

    if(ip3 != -1){
        criteria.ip3 = ip3;
    }

    fst_query *q = fst24_new_query(self->ref, &criteria, NULL);
    struct fst_query_container *py_q = (struct fst_query_container *) py_fst_query_new(&py_fst_query_type, NULL, NULL);
    py_q->ref = q;

    Py_INCREF((PyObject*)py_q);
    return (PyObject *)py_q;
}

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

static PyObject * py_fst_record_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    struct py_fst_record *self = (struct py_fst_record *) type->tp_alloc(type, 0);
    if(self == NULL){
        return NULL;
    }

    return (PyObject *)self;
}

static PyTypeObject py_fst_record_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_librmn.fst_record",
    .tp_doc = "Python fst_record object",
    .tp_basicsize = sizeof(struct py_fst_record),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst_record_new,
};

static PyModuleDef mymodulemodule = {
    PyModuleDef_HEAD_INIT,
    .m_name = "_librmn",
    .m_doc = "Example module that creates a Person type",
    .m_size = -1,
};

PyMODINIT_FUNC PyInit__librmn(void)
{

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
