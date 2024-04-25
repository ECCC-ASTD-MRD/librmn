#include <stdlib.h>
#include <stdio.h>
#include <Python.h>
#include <rmn/fst24_file.h>

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
    char *filename; // for debugging
    char *options; // for debugging
};

static PyObject *py_fst24_file_new(PyTypeObject *type, PyObject *args, PyObject *kwds){
    struct fst24_file_container *self = (struct fst24_file_container *) type->tp_alloc(type, 0);
    if(self == NULL) {
        return NULL;
    }

    return (PyObject *) self;
}

// 
static int py_fst24_file_init(struct fst24_file_container *self, PyObject *args, PyObject *kwds){
    static char *kwlist[] = {"filename", "options", NULL};
    char *options;
    char *filename;
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

    self->filename = filename;
    self->options = options;
    self->ref = ref;
    fprintf(stderr, "%s(): self=%p, self->ref=%p, self->filename=%s, self->options=%s\n", __func__, self, self->ref, self->filename, self->options);

    return 0;
}

static PyTypeObject py_fst24_file_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_librmn.fst24_file",
    .tp_doc = "Python fst24_file object",
    .tp_basicsize = sizeof(struct fst24_file_container),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst24_file_new,
    .tp_init = (initproc) py_fst24_file_init,
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

    if(PyType_Ready(&py_fst24_file_type) < 0){
        return NULL;
    }

    Py_INCREF(&py_fst24_file_type);
    if(PyModule_AddObject(m, "fst24_file", (PyObject *)&py_fst24_file_type) < 0){
        Py_DECREF(&py_fst24_file_type);
        Py_DECREF(m);
        return NULL;
    }

    return m;
}
