import ctypes
import os
import logging

this_dir = os.path.dirname(__file__)
logging.error(f'this_dir={this_dir}')
# __file__ == <build-dir>/python/rmn/shared_lib.py
# librmn.so == <build-dir>/librmn.so
rel_build_dir = os.path.normpath(os.path.join(this_dir, '..', '..', 'librmn.so'))
logging.error(f'rel_build_dir={rel_build_dir}')
if os.path.isfile(rel_build_dir):
    librmn = ctypes.cdll.LoadLibrary(rel_build_dir)
else:
    # __file__ == <install-dir>/lib/python/rmn/shared_lib.py
    # librmn.so == <install-dir>/lib/librmn.so
    # (same relative positions as build dir but that is a coincidence)
    rel_install_dir = os.path.normpath(os.path.join(this_dir, '..', '..', 'librmn.so'))
    if os.path.isfile(rel_install_dir):
        librmn = ctypes.cdll.LoadLibrary(rel_install_dir)
    else:
        logging.warn("Loading librmn though LD_LIBRARY_PATH.  Loaded shared library compatibility not garanteed")
        librmn = ctypes.cdll.LoadLibrary("librmn.so")
