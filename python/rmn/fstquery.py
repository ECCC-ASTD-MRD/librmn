import ctypes
from .fstrecord import fst_record, _get_default_fst_record
from .sharedlib import librmn

_fst24_find_next = librmn.fst24_find_next
_fst24_find_next.argtypes = (ctypes.c_void_p, ctypes.POINTER(fst_record))
_fst24_find_next.restype = ctypes.c_int

class fst_query:
    def __init__(self, _c_ref, _file_ref):
        # Queries should only be created using fst24_file.new_query.
        # Can we prevent users from creating queries directly?
        self._c_ref = _c_ref
        # Keep the file object alive as long as this query is alive
        self._file_ref = _file_ref

    def __iter__(self):
        return self

    def __next__(self):
        if self._file_ref.closed:
            raise ValueError(f"Query {self} references closed file")
        rec = _get_default_fst_record()
        ok = _fst24_find_next(self._c_ref, ctypes.byref(rec))
        if ok == 0:
            raise StopIteration("No more results in query")

        return rec



