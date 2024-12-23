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
        if self._c_ref != 0:
            _fst24_close(self._c_ref)
    def __enter__(self):
        return self
    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.close()
    def new_query(self, **kwargs):
        # TODO: This code is copied from the __init__ of fst_record
        #       figure something out to make this more elegant
        criteria = _get_default_fst_record()
        for k,v in kwargs.items():
            # TODO: This allowas setting and adding absolutely any attribute
            #       to a record so probably change this
            if k in ['typvar', 'grtyp', 'nomvar', 'etiket']:
                setattr(criteria, '_'+k, v.encode('utf-8'))
            else:
                setattr(criteria, k, v)
        c_query = _fst24_new_query(self._c_ref, ctypes.byref(criteria), 0)
        if c_query is None or c_query == (None,):
            raise FstFileError("_fst24_file_new_query failed, TODO Call App_ErorrGet()")
        # print(f"fst24_file.new_query(): c_query has type '{type(c_query)}'")
        # print(f"fst24_file.new_query(): c_query={c_query}")
        return fst_query(c_query, self._c_ref)
    def __iter__(self):
        criteria = _get_default_fst_record()
        c_query = _fst24_new_query(self._c_ref, ctypes.byref(criteria), 0)
        # print(f"fst24_file.__iter__(): c_query has type '{type(c_query)}'")
        # print(f"fst24_file.__iter__(): c_query={c_query}")
        return fst_query(c_query, self)

class FstFileError(Exception):
    pass
