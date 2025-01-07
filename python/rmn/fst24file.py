import ctypes
import logging

from .sharedlib import librmn
from .fstrecord import fst_record, _get_default_fst_record
from .fstquery import fst_query

_fst24_open = librmn.fst24_open
_fst24_open.argtypes = (ctypes.c_char_p, ctypes.c_char_p)
_fst24_open.restype = ctypes.c_void_p

_fst24_close = librmn.fst24_close
_fst24_close.argtypes = (ctypes.c_void_p,) # comma to make it a tuple
_fst24_close.restype = ctypes.c_int

_fst24_new_query = librmn.fst24_new_query
_fst24_new_query.argtypes = (ctypes.c_void_p, ctypes.POINTER(fst_record), ctypes.c_void_p)
_fst24_new_query.restype = ctypes.c_void_p

_fst24_write = librmn.fst24_write
_fst24_write.argtypes = (ctypes.c_void_p, ctypes.POINTER(fst_record), ctypes.c_int)

class fst24_file(ctypes.Structure):
    def __init__(self, filename, options=""):
        # Use '*' to enforce that next arguments must be passed as
        # keyword args.
        self._c_ref = None
        self.filename = filename
        self.options = options

        self._c_ref = _fst24_open(filename.encode('utf-8'), options.encode('utf-8'))
        logging.debug(f"fst24_file.__init__(): self._c_ref={self._c_ref}")
        if self._c_ref is None:
            raise FstFileError("Could not open file")
    def close(self):
        # See test_create_file_with_data().  It makes sense in and of itself
        # to prevent double-closing a file but the test shows a situation where
        # double closing in one object can close the file encapsulated in
        # another object.
        logging.debug(f"Closing fst24_file {self.filename}(_c_ref={self._c_ref}, id={id(self)})")
        if self._c_ref is not None:
            _fst24_close(self._c_ref)
            self._c_ref = None
    def __enter__(self):
        logging.debug(f"Context Manager __enter__: fst24_file {self.filename}")
        return self
    def __exit__(self, exc_type, exc_value, exc_traceback):
        logging.debug(f"Context Manager __exit__: fst24_file {self.filename}")
        self.close()

    def new_query(self, **kwargs):
        criteria = _get_default_fst_record()
        for k,v in kwargs.items():
            setattr(criteria, k, v)
        c_query = _fst24_new_query(self._c_ref, ctypes.byref(criteria), 0)
        if c_query is None or c_query == (None,):
            raise FstFileError("_fst24_file_new_query failed, TODO Call App_ErorrGet()")
        return fst_query(c_query, self)

    def __iter__(self):
        criteria = _get_default_fst_record()
        c_query = _fst24_new_query(self._c_ref, ctypes.byref(criteria), 0)
        return fst_query(c_query, self)

    def write(self, record, rewrite):
        if not isinstance(record, fst_record):
            raise TypeError(f"First argumnent should be fst_record, not '{type(record)}'")
        result = _fst24_write(self._c_ref, ctypes.byref(record), 1 if rewrite else 0)
        if result != 1:
            raise FstFileError("Error calling C function fst24_write()")
    def __del__(self):
        logging.debug(f"__del__: fst24_file {self.filename} (id={id(self)}")
        self.close()

class FstFileError(Exception):
    pass
