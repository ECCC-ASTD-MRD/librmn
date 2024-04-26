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

static PyTypeObject py_fst24_file_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "_librmn.fst24_file",
    .tp_doc = "Python fst24_file object",
    .tp_basicsize = sizeof(struct fst24_file_container),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_new = py_fst24_file_new,
    .tp_init = (initproc) py_fst24_file_init,
    .tp_str = (reprfunc) py_fst24_file_str,
    .tp_repr = (reprfunc) py_fst24_file_str,
    .tp_members = py_fst24_file_member_def,
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
