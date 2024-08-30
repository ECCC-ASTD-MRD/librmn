# To use the installed version

Simply do `. ssmuse-sh -x <CMAKE_INSTALL_PREFIX>`.

- The `ssmuse` command adds the proper directory to `PYTHONPATH`
- `librmn.so` is installed with RPATHs for the intel runtime libraries
- `_rmn.so` is installed with a relative RPATH so that it finds the `librmn.so`   that it was built for.

# To use the build version

Using the version in the build directory saves time since we only need to run
`make _rmn` after modifying any of the code in librmn or the C extension before
we can try again.

- The intel compiler must be loaded
- The directory *containing* the package must be part of `PYTHONPATH`

Therefore, load the intel compiler the way you normally do, and do
```
export PYTHONPATH=<CMAKE_BINARY_DIR>/python${PYTHONPATH:+:${PYTHONPATH}
```
