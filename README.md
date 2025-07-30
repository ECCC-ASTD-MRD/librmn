rmnlib is a library of functions for numerical weather prediction used
primarily by Environment and Climate Change Canada.

Its main components are Standard RPN files and the EZ interpolator.


## Documentation
  * [Functions reference accessible from the Internet](https://science:science@collaboration.cmc.ec.gc.ca/science/si/eng/si/libraries/rmnlib/)
  * [More complete documentation on CMC Wiki](https://wiki.cmc.ec.gc.ca/wiki/Librmn)

## Getting the code

The project is hosted at https://gitlab.science.gc.ca/RPN-SI/librmn
The code is available via Git at the following locations:
  * On ECCC network: git@gitlab.science.gc.ca:RPN-SI/librmn.git
  * For users outside ECCC: https://github.com/ECCC-ASTD-MRD/librmn.git

`cmake_rpn` is included as a git submodule.  Please clone with the
`--recursive` option or run `git submodule update --init --recursive` in the
git repo after having cloned.


## Installation instructions

### Create a directory for compilation
```
mkdir $build_dir_path
cd $build_dir_path
```

### Build setup

Options to configure compilation must be added with the `-D` prefix when
calling the `cmake` command.

`CMAKE_BUILD_TYPE`
: `(Release|RelWithDebInfo|Debug)` Compilation mode. Default: `RelWithDebInfo`

`CMAKE_INSTALL_PREFIX`
: Directory path for installation (`make install`)

`COMPILER_SUITE`
: `(gnu|intel|xl|nvhpc|...)` Compiler suite to be used. On ECCC systems,
the compiler loaded will be used.  If the ECCC-specific environment variables are not
not found, the default is `gnu`.

`WITH_OMPI`
: `(yes|no)` Indicates whether OpenMP/MPI support is enabled.  Default: `yes`

### Compilation example
```
cmake \
    -DCMAKE_INSTALL_PREFIX=$install_dir_path \
    -DWITH_OMPI=no \
    $src_dir_path
make -j $a_resonable_number
make install
```

### Compilation in ECCC environment

CMake scripts from `cmake_rpn` will automatically detect the compiler loaded
via the `EC_ARCH` environment variable.  It is therefore not necessary to
explicitly specify the compiler suite to use (`-DCOMPILER_SUITE=...`).  
However, you must load the desired compiler before performing the build
configuration.

Source the right file from the `ECCI_ENV` variable, depending on the desired
architecture.  This will load the specified compiler, set the
`ECCI_DATA_DIR` variable for the test datasets, and set the
`EC_CMAKE_MODULE_PATH` variable for the cmake_rpn modules.

- Example for PPP5:

```
. $ECCI_ENV/latest/ppp5/inteloneapi-2022.1.2.sh
```

- Example for CMC network and gnu 11.4.0:

```
. $ECCI_ENV/latest/ubuntu-22.04-amd-64/gnu.sh
```

### Example of compiling the dev branch on a system outside ECCC:
```bash
git clone -b dev https://github.com/ECCC-ASTD-MRD/librmn.git
cd librmn
git submodule update --init --recursive
cd ..
mkdir librmn_build
cd librmn_build
cmake ../librmn -DCMAKE_INSTALL_PREFIX=~/opt/
make -j4 install
```

### Documentation

A `doc` target is created by cmake to generate documentation. This, however, requires
Doxygen and graphviz.
```
make doc
```

### Change of compiler

Since the library of functions contains a Fortran module, it must use the
same compiler that produced the library for the client applications.

So, you have to clean the compilation directory and re-execute the
compilation instructions specifying a different installation directory
(`-DCMAKE_INSTALL_PREFIX=$install_dir_path`).


## Example of use in a client application

We have developed a very simple example of an application using librmn.  It
can be used as a reference to build a CMake project that uses librmn.

It can be consulted at the following addresses:
- [https://gitlab.science.gc.ca/RPN-SI/librmn-client-example](https://gitlab.science.gc.ca/RPN-SI/librmn-client-example) (On ECCC network)
- [https://github.com/ECCC-ASTD-MRD/librmn-client-example](https://github.com/ECCC-ASTD-MRD/librmn-client-example) (Public)

The static version of the library will be the one used by default when
searching for the rmn package, unless specified. Examples:

```
find_package(rmn [version] REQUIRED)
find_package(rmn [version] REQUIRED COMPONENTS shared)
```
