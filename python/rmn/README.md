# Python package and indexing for dataframes

## Importing the built module

The directory `${CMAKE_CURRENT_BINARY_DIR}` (ex. `build/python/rmn` is a pythons
package (a python package is a directory that contains an `__init__.py` file
and python modules, the `__init__.py` file may be empty but its presence is
important.

Since the directory itself is the package, it must be in one of Python's search
paths.  The simplest way to do this is using `PYTHONPATH`:
The python package is `rmn`: a directory with that name containing a file `__init__.py`.

```
export PYTHONPATH=${build-dir}/python${PYTHONPATH:+:${PYTHONPATH}}
```

## Extension modules

An extension module is a term meaning "A python module written in C".  This
package contains one python module `_rmn` built from `_rmn.c` (documentation
links for implementing such a module in the file itself).

## Dependency on Intel

The compiler runtime must be available to run the package.  If we are compiling
with Intel, then the Intel compiler must be loaded at runtime.

## Running tests

Two test files can be run for the Python part:
- `tests/test_rmn.py` tests the basic API (only parts of it are implemented in
  the python package since this is a work in progress).
- `tests/test_indexing.py` tests the indexing functionnality for creating a
  single Pandas data frame.
- `${CMAKE_CURRENT_BINARY_DIR}/test_indexing` is a pure C executable for testing
  the functions used for indexing.  Because Python catches `SIGINT`, if Python
  goes into one of our long running C functions and we press `C-c` during that
  time, our program will only get interrupted when control goes back to Python
  unless we add calls to `PyErr_CheckSignals` everywhere in our code to check
  if Python got a `SIGINT` and manually abort our computations.  Combine that
  with multithreading and it would add layer of complexity that is not productive
  for something that is an experiment.

# Notes

For checking performance, use `${CMAKE_CURRENT_BINARY_DIR}/test_indexing`.
That executable creates the data arrays that would become 1D numpy arrays
that would become the columns of a data frame.  All that is missing is creating
the numpy array objects to hold those already created arrays and creating a
data frame with them.  That part is basically instantaneous compared to the rest.
Plus you can `C-c` it.

## Number of threads

10 threads is where I got the best results but it seems to be more dependant on
other things.

## Single threaded

To run single threaded comment out the `#define MULTITHREAD_INDEXING` part in
`indexing.c`.

## Timings

Some `TApp_Timer`s are used for timings and are printed.


