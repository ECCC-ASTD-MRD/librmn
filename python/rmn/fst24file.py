import ctypes
import logging
import os
import sys
import numpy
from typing import Union, Generator, Optional, Iterable, Sequence

from ._sharedlib import librmn
from .fstrecord import fst_record, _get_default_fst_record
from .fstquery import fst_query
from .errors import FstFileError

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

_fst24_get_record_by_index = librmn.fst24_get_record_by_index
_fst24_get_record_by_index.argtypes = (ctypes.c_void_p, ctypes.c_int32, ctypes.POINTER(fst_record))
_fst24_get_record_by_index.restype = ctypes.c_int32

# PYTHON VERSION:
# 3.9+: Generator[fst_record, None, None]
# 3.8-: Iterable[fst_record]

class fst24_file(ctypes.Structure):
    """ Object encapsulating an RPN fst24 file
    >>> # Print all records of a file that have nomvar == "AL"
    >>> import rmn
    >>> with rmn.fst24_file(<filename>) as f:
    >>>     q = f.new_query(nomvar="AL")
    >>>     for rec in q:
    >>>         print(rec)
    """
    def __init__(self, filename: Union[bytes,str,os.PathLike] , options: str = ""):
        """ Opens an fst24 file `filename` with options """
        # Use '*' to enforce that next arguments must be passed as
        # keyword args.
        self._c_ref = None
        self.closed = False
        self.filename = filename
        self.options = options
        if isinstance(filename, str):
            _filename = filename.encode('utf-8')
        elif isinstance(filename, os.PathLike):
            _filename = filename.__fspath__()
        elif isinstance(filename, bytes):
            _filename = filename
        else:
            raise TypeError("Argument 'filename' should be of type 'str', 'bytes', or 'os.PathLike', not '{type(filename).__name__}'")

        self._c_ref = _fst24_open(_filename, options.encode('utf-8'))
        if self._c_ref is None:
            raise FstFileError(f"Could not open file '{filename}'")

    def close(self):
        """ Closes an fst24 file.

        Users should prefer to use context managers over calling this method
        directly.

        This method has no effect if the file is already closed"""
        # See test_create_file_with_data().  It makes sense in and of itself
        # to prevent double-closing a file but the test shows a situation where
        # double closing in one object can close the file encapsulated in
        # another object.
        logging.debug(f"Closing fst24_file {self.filename}(_c_ref={self._c_ref}, id={id(self)})")
        if not self.closed:
            res = _fst24_close(self._c_ref)
            if res == 0:
                raise FstFileError(f"Error closing file '{self.filename}': Error calling C function fst24_close(0x{self._c_ref:x})")

            self.closed = True
            self._c_ref = None

    def __enter__(self):
        """ Context manager enter method on fst24_file """
        logging.debug(f"Context Manager __enter__: fst24_file {self.filename}")
        return self
    def __exit__(self, exc_type, exc_value, exc_traceback):
        """ Context manager exit method of fst24_file"""
        logging.debug(f"Context Manager __exit__: fst24_file {self.filename}")
        self.close()

    def new_query(self, **kwargs):
        """ Returns a new query to find records matching the criteria received
        as arguments """
        criteria = _get_default_fst_record()
        for k,v in kwargs.items():
            if k.startswith('_'):
                raise ValueError("{k} cannot be used as an argument")
            setattr(criteria, k, v)
        c_query = _fst24_new_query(self._c_ref, ctypes.byref(criteria), 0)
        if c_query is None or c_query == (None,):
            raise FstFileError("_fst24_file_new_query failed, TODO Call App_ErorrGet()")
        return fst_query(c_query, self)

    def __iter__(self):
        """ Returns an iterator that will iterate on all records of the file """
        criteria = _get_default_fst_record()
        c_query = _fst24_new_query(self._c_ref, ctypes.byref(criteria), 0)
        return fst_query(c_query, self)

    def write(self, record: fst_record, rewrite: bool):
        """ Write a record to the file """
        if self.closed:
            raise ValueError("I/O operation on closed file")
        if not isinstance(record, fst_record):
            raise TypeError(f"First argumnent should be fst_record, not '{type(record)}'")

        # Note: Accessing the property causes the data to be read if there is
        # some data so this check is more important than just verifying.
        if record.data is None:
            raise ValueError("The record has no data")
        record_original_data = record.data
        data_to_write = record.data
        if not data_to_write.flags['F_CONTIGUOUS']:
            # Maybe this long warning could be reduced but at least it's clear.
            logging.warn(f"The data of the record is not Fortran contiguous.  It will need to be converted to write in the file.  Because this conversion may entail significant memory manipulation, consider using working arrays that are already in fortran order (ex: wk = np.empty((ni,nj,nk), order='F'), np.zeros((ni,nj,nk), order='F'))")
            data_to_write = numpy.asfortranarray(data_to_write)

        record.data = data_to_write
        result = _fst24_write(self._c_ref, ctypes.byref(record), (1 if rewrite else 0))
        record.data = record_original_data

        if result != 1:
            raise FstFileError("Error calling C function fst24_write()")
    def __del__(self):
        logging.debug(f"__del__: fst24_file {self.filename} (id={id(self)}")
        self.close()

    def get_record_by_index(self, index: int) -> fst_record:
        """ Get a record by its file index """
        if self.closed:
            raise ValueError("I/O operation on closed file")

        rec = fst_record()
        result = _fst24_get_record_by_index(self._c_ref, index, ctypes.byref(rec))

        return rec

    # NOTE: Using  `Generator[fst_record, None, None]` requires Python 3.9
    def get_records_by_index(self, indices: Sequence[int]) -> Iterable[fst_record]:
        yield from map(self.get_record_by_index, indices)

    def get_record_by_index_with_data(self, index: int) -> fst_record:
        """ This is a convenience function that gets a record using its index in
        the file.  Because the record is being accessed using said index, we
        assume that this function is called to get the data of the record.

        Only call this function if you want the data. """
        rec = self.get_record_by_index(index)

        rec.data

        return rec

    def get_records_by_index_with_data(self, indices: Sequence[int]) -> Iterable[fst_record]:
        yield from map(self.get_record_by_index_with_data, indices)

    @classmethod
    def get_records_with_data(cls, filename: Union[str, os.PathLike], indices: Sequence[int]) -> Iterable[fst_record]:
        """ This function will read all the records at the given indices in the
        given file *and their data*.  Only use this function if you really want
        all that data!

        This function is implemented as a generator so it will stop reading
        records as soon as they stop being consumed by the caller. """

        with cls(filename) as f:
            yield from f.get_records_by_index_with_data(indices)


