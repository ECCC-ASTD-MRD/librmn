import ctypes
import numpy as np

from ._sharedlib import librmn

def align_to_4(val):
    return (val+3) & 0xfffffffc

class fst_record(ctypes.Structure):
    """ fst_record object
    Fields:
    - dateo: int           Origin Date timestamp
    - datev: int           Valid Date timestamp

    - data_type: int       Data type of elements. See FST_TYPE_* constants.
    - data_bits: int       Number of bits per input elements
    - pack_bits: int       Number of stored bits
    - ni: int              First dimension of the data field (number of elements)
    - nj: int              Second dimension of the data field (number of elements)
    - nk: int              Third dimension of the data field (number of elements)
    - num_meta_bytes: int  Size of the metadata in bytes

    - deet: int            Length of the time steps in seconds (deet)
    - npas: int            Time step number

    - ip1: int             Vertical level
    - ip2: int             Forecast hour
    - ip3: int             User defined identifier

    - ig1: int             First grid descriptor
    - ig2: int             Second grid descriptor
    - ig3: int             Third grid descriptor
    - ig4: int             Fourth grid descriptor

    - typvar: str          Type of field (forecast, analysis, climatology)
    - grtyp: str           Type of geographical projection
    - nomvar: str          Variable name
    - etiket: str          Label
    Create a record with data
    >>> rec = rmn.fst_record(
    >>>     dateo=1, datev=2,
    >>>     data_type=5, data_bits=32, pack_bits=32,
    >>>     ni=8, nj=9, nk=1,
    >>>     ip1=1,ip2=2,ip3=3,
    >>>     ig1=1, ig2=2, ig3=3, ig4=4,
    >>>     nomvar="RPN", typvar="Y", grtyp="X"
    >>> )
    >>> rec.deet = 0
    >>> rec.npas = 0
    >>> rec.etiket = "demo"
    >>> rec.data = np.random.random(rec.ni * rec.nj * rec.nk).reshape((rec.ni, rec.nj, rec.nk), order='F').astype('f')
    """
    # The presence of __slots__ prevents the creation of other attributes.
    # Now the only possible attributes are the ones declared in the _fields_
    # alist and in __slots__.
    __slots__ = ['_data_array']
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

        ('_dummy', ctypes.c_int32),

        ('_typvar', ctypes.c_char * align_to_4(3)),
        ('_grtyp', ctypes.c_char * align_to_4(2)),
        ('_nomvar', ctypes.c_char * align_to_4(5)),
        ('_etiket', ctypes.c_char * align_to_4(13))
    )

    def __init__(self, **kwargs):
        # Set values according to C macro `default_fst_record`.
        # _set_default_values(ctypes.byref(self))
        self._data_array = None
        for k,v in kwargs.items():
            if k.startswith('_'):
                raise ValueError("Attributes beginning with '_' should not be touched")
            setattr(self, k, v)

    def __new__(cls, *args, **kwargs):
        return _get_default_fst_record()

    @property
    def data(self):
        """ Property encapsulating the data of an fst record.  On access, the
        data is read from the file and stored in a numpy array. """
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
        if not isinstance(value, np.ndarray):
            raise ValueError(f"Expecting {np.ndarray.__name__}, got {type(value).__name__}")
        self._data_array = value
        self._data = self._data_array.ctypes.data

    @property
    def etiket(self):
        return self._etiket.decode().rstrip()

    @etiket.setter
    def etiket(self, value):
        if isinstance(value, str):
            self._etiket = value.encode('utf-8')
        elif isinstance(value, bytes):
            self._etiket = value
        else:
            raise TypeError(f"Expected str or bytes, not {type(value).__name__}")

    @property
    def typvar(self):
        return self._typvar.decode().rstrip()
    @typvar.setter
    def typvar(self, value):
        if isinstance(value, str):
            self._typvar = value.encode('utf-8')
        elif isinstance(value, bytes):
            self._typvar = value
        else:
            raise TypeError(f"Expected str or bytes, not {type(value).__name__}")

    @property
    def nomvar(self):
        return self._nomvar.decode().rstrip()
    @nomvar.setter
    def nomvar(self, value):
        if isinstance(value, str):
            self._nomvar = value.encode('utf-8')
        elif isinstance(value, bytes):
            self._nomvar = value
        else:
            raise TypeError(f"Expected str or bytes, not {type(value).__name__}")

    @property
    def grtyp(self):
        return self._grtyp.decode().rstrip()
    @grtyp.setter
    def grtyp(self, value):
        if isinstance(value, str):
            self._grtyp = value.encode('utf-8')
        elif isinstance(value, bytes):
            self._grtyp = value
        else:
            raise TypeError(f"Expected str or bytes, not {type(value).__name__}")

    def to_dict(self):
        """ Convert a record to dictionnary potentially for use as a row of
        a Pandas DataFrame. """
        return {
            'nomvar': self.nomvar,
            'typvar': self.typvar,
            'grtyp': self.grtyp,
            'etiket': self.etiket,
            'ni': self.ni,
            'nj': self.nj,
            'nk': self.nk,
            'dateo': self.dateo,
            'datev': self.datev,
            'npas': self.npas,
            'deet': self.deet,
            'ip1': self.ip1,
            'ip2': self.ip2,
            'ip3': self.ip3,
            'ig1': self.ig1,
            'ig2': self.ig2,
            'ig3': self.ig3,
            'ig4': self.ig4,
            'data_type': self.data_type,
            'data_bits': self.data_bits,
            'file_index': self.file_index,
        }

_fst24_read_record = librmn.fst24_read_record
_fst24_read_record.argtypes = (ctypes.POINTER(fst_record),)
_fst24_read_record.restype = ctypes.c_int

_get_default_fst_record = librmn.get_default_fst_record
_get_default_fst_record.argtypes = tuple()
_get_default_fst_record.restype = fst_record
