#include <stdlib.h>
#include <Python.h>

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

    return m;
}
