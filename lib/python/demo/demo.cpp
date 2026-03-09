#include <Python.h> // Required for Python C API
#include <string>

// C++ function to be wrapped
std::string hello() {
    return "Hello, World from C++!";
}

// Wrapper function exposed to Python
static PyObject* hello_world(PyObject* self, PyObject* args) {
    // Return a Python string object created from the C++ string
    return PyUnicode_FromString(hello().c_str());
}

// Method table: links the Python method name "hello" to the C++ function
static PyMethodDef Methods[] = {
    {"hello", hello_world, METH_NOARGS, "Prints a hello message from C++."},
    {NULL, NULL, 0, NULL}  /* Sentinel */
};

// Module definition structure
static struct PyModuleDef demomodule = {
    PyModuleDef_HEAD_INIT,
    "demo",   /* name of module */
    "A demo C++ extension module.", /* module documentation, may be NULL */
    -1,       /* size of per-interpreter state of the module,
                 or -1 if the module keeps state in global variables. */
    Methods
};

// Module initialization function (entry point for Python)
PyMODINIT_FUNC PyInit_demo(void) {
    return PyModule_Create(&demomodule);
}
