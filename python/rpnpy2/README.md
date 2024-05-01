The python package is `rpnpy2`: a directory with that name containing a file `__init__.py`.

The directory containing `rpnpy2` (the directory that is a python package because
it contains an `__init__.py` file) must be contained in `PYTHONPATH`.

This is why `rpnpy2` is in `${CMAKE_BINARY_DIR}/python` and not directly in
`${CMAKE_BINARY_DIR}`.  We want to add to `PYTHONPATH` a directory that contains
just `rpnpy2`.

To make the package available to import, we do
```
export PYTHONPATH=${build-dir}/python${PYTHONPATH:+:${PYTHONPATH}}
```
a file `${CMAKE_BUILD_DIR}/python/rpnpy2/setup.sh` can be sourced to do this.

We also need whatever is required to load `librmn.so` into memory so if it was
compiled with Intel, that means we must load the Intel compiler (for `libifport.so`
and others).

A test file `${CMAKE_SOURCE_DIR}/tests/test_rpnpy.py` can be run to try the
each of the current features.  The CMake target `test_py` runs this file with
`PYTHONPATH` set but if the proper compiler libraries are loaded and the proper
directory is in `PYTHONPATH`, the file can be run on its own.

Python looks in our present working directory when it looks for modules and
packages so tests should not be run while our `PWD` is either
`${CMAKE_SOURCE_DIR}/python` it will find a package named `rpnpy2` but when the
`__init__.py` of that package attempts to `from ._librmn import *`, it will not
find it because that file is only in `${CMAKE_BINARY_DIR}/python/rpnpy2`.

Python may also looks in the directory of the file that it is running so the
test must also not be placed separate from the python package.
