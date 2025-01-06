import ctypes
import numpy as np

from .sharedlib import librmn

def align_to_4(val):
    return (val+3) & 0xfffffffc

class fst_record(ctypes.Structure):
    _fields_ = (
        # The do-not-touch struct
        ('_version', ctypes.c_int32),
        ('_deleted', ctypes.c_int32),
        ('_handle', ctypes.c_int64),
        ('_alloc', ctypes.c_int64),
        ('_flags', ctypes.c_int32),
        ('_fst_version', ctypes.c_uint8),
        ('_num_search_keys', ctypes.c_uint8),
        ('_extended_meta_size', ctypes.c_uint16),
        ('_sorted_data_size', ctypes.c_size_t),
        ('_unpacked_data_size', ctypes.c_size_t),
        ('_stringified_meta', ctypes.c_void_p),
        # End do-not-touch

        ('_file', ctypes.c_void_p),
        ('_data', ctypes.c_void_p),
        ('metadata', ctypes.c_void_p),

        ('file_index', ctypes.c_int32),

        ('dateo', ctypes.c_int32),
        ('datev', ctypes.c_int32),

        ('data_type', ctypes.c_int32),
        ('data_bits', ctypes.c_int32),
        ('pack_bits', ctypes.c_int32),
        ('ni', ctypes.c_int32),
        ('nj', ctypes.c_int32),
        ('nk', ctypes.c_int32),
        ('num_meta_bytes', ctypes.c_int32),

        ('deet', ctypes.c_int32),
        ('npas', ctypes.c_int32),

        ('ip1', ctypes.c_int32),
        ('ip2', ctypes.c_int32),
        ('ip3', ctypes.c_int32),

        ('ig1', ctypes.c_int32),
        ('ig2', ctypes.c_int32),
        ('ig3', ctypes.c_int32),
        ('ig4', ctypes.c_int32),

        ('dummy', ctypes.c_int32),

        # Make these properties probably.
        # Note: fst_record.h has sizes like 'FST_TYPVAR_LEN + 1'
        #       but in fst_sz.h, the comments say that the FST_TYPVAR_LEN
        #       already contains space for the \0.
        ('_typvar', ctypes.c_char * align_to_4(3)),
        ('_grtyp', ctypes.c_char * align_to_4(2)),
        ('_nomvar', ctypes.c_char * align_to_4(5)),
        ('_etiket', ctypes.c_char * align_to_4(13))
    )

    def __init__(self, **kwargs):
        # Set values according to C macro `default_fst_record`.
        _set_default_values(ctypes.byref(self))

        for k,v in kwargs.items():
            setattr(self, k, v)

    @property
    def data(self):
        # ASSUME FLOATS FOR NOW
        if hasattr(self, "_data_array") and self._data is None:
            raise Exception("Shouldn't happen")

        if self._data is None:
            # If no data has been set on the record and there is no file to
            # read the data from, return None.  This access is not an error.
            if self._handle < 0:
                return None

            data_array = np.empty((self.ni, self.nj, self.nk), dtype='f', order='F')
            self._data = data_array.ctypes.data
            res = _fst24_read_record(ctypes.byref(self))

            self._data_array = data_array
        return self._data_array

    @data.setter
    def data(self, value):
        self._data_array = value
        self._data = self._data_array.ctypes.data

    @property
    def etiket(self):
        return self._etiket.decode().rstrip()
    @etiket.setter
    def etiket(self, value):
        self._etiket = value.encode('utf-8')

    @property
    def typvar(self):
        return self._typvar.decode().rstrip()
    @typvar.setter
    def typvar(self, value):
        self._typvar = value.encode('utf-8')

    @property
    def nomvar(self):
        return self._nomvar.decode().rstrip()
    @nomvar.setter
    def nomvar(self, value):
        self._nomvar = value.encode('utf-8')

    @property
    def grtyp(self):
        return self._grtyp.decode().rstrip()
    @grtyp.setter
    def grtyp(self, value):
        self._grtyp = value.encode('utf-8')

_fst24_read_record = librmn.fst24_read_record
_fst24_read_record.argtypes = (ctypes.POINTER(fst_record),)
_fst24_read_record.restype = ctypes.c_int

_get_default_fst_record = librmn.get_default_fst_record
_get_default_fst_record.argtypes = tuple()
_get_default_fst_record.restype = fst_record

_set_default_values = librmn.fst_record_set_default_values
_set_default_values.argtypes = (ctypes.POINTER(fst_record),)
_set_default_values.restype = ctypes.c_int
