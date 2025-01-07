import ctypes
from .fstrecord import fst_record, _get_default_fst_record
from ._sharedlib import librmn

_fst24_find_next = librmn.fst24_find_next
_fst24_find_next.argtypes = (ctypes.c_void_p, ctypes.POINTER(fst_record))
_fst24_find_next.restype = ctypes.c_int

class fst_query:
    """ Query object for RPN fst24 files.  This object implements
    the iterator protocol
    # Print all records matching criteria
    >>> q = f.new_query(**criteria)
    >>> for rec in q:
    >>>     print(rec)
    """

    def __init__(self, _c_ref, _file_ref):
        """ Queries should only be created by users through the
        new_query method of fst24_file."""
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



