import ctypes
import numpy as np
import enum

from ._sharedlib import librmn
from .errors import FstFileError

class FstDataType(enum.IntEnum):
    # Paste the whole thing once, put the #define line before it's documentation
    # then select the whole thing and :s/#define \(.*\) \([0-9]\+\)/\1 (\2):/
    # Then select the whole thing again and do :s/\/\/!> //
    # Then add """ at the beginning and end of the whole thing to form one long
    # Python string
    """
    FST_TYPE_BINARY (0)
    Raw binary data. Its elements can have any size, and it is not subject to interpretation by the FST layer.
    Identified with X.

    FST_TYPE_REAL_OLD_QUANT (1)
    Real-valued data using the old quantification scheme.
    Identified with R.
    This quantification is lossy and not reversible (non-cyclic)
    If trying to store with [31-32] bits, automatically converted to FST_TYPE_REAL_IEEE

    FST_TYPE_UNSIGNED (2)
    Unsigned integer data

    FST_TYPE_CHAR (3)
    Characters (not compressed)

    FST_TYPE_SIGNED (4)
    Signed integer data

    FST_TYPE_REAL_IEEE (5)
    Real-valued data using IEEE format (no quantification), in 32 or 64 bits. Identified with E.
    When trying to store data with number of bits in the range [33-63], the original data size is
    preserved (either 32 or 64 bits).
    When trying to store 64-bit (double) data with 32 bits or less, it is first converted to 32-bit IEEE (float)
    When trying to store 32-bit (float) data with less than 32 bits, the extra bits are simply truncated from the
    mantissa (so don't go too low).

    FST_TYPE_REAL (6)
    Real-valued data using a new quantification scheme.
    *This is the recommended REAL type to use.*
    This quantification scheme is lossy, but reversible (cyclic)
    Depending on number of bits requested for storage, a conversion may be performed at write-time.
      if > 24 -> use FST_TYPE_REAL_IEEE with 32 bits
      if [17-23] -> use FST_TYPE_REAL_OLD_QUANT with that number of bits
      if < 16 -> quantify to 16, then truncate any extra bit from the new mantissa

    FST_TYPE_STRING (7)
    Characters (compressed)

    FST_TYPE_COMPLEX (8)
    Complex number (32 or 64 bits) """

    # Paste the whole thing again,
    # Delete all the documentation so we are left with only the defines
    # Select the defines and do :s/#define \(.*\) \([0-9]\+\)/    \1 = \2
    FST_TYPE_NONE = -1
    FST_TYPE_BINARY = 0
    FST_TYPE_REAL_OLD_QUANT = 1
    FST_TYPE_UNSIGNED = 2
    FST_TYPE_CHAR = 3
    FST_TYPE_SIGNED = 4
    FST_TYPE_REAL_IEEE = 5
    FST_TYPE_REAL = 6
    FST_TYPE_STRING = 7
    FST_TYPE_COMPLEX = 8

    FST_TYPE_TURBOPACK = 128
    # Copy the thing above, select and
    # :s/    \(.*\) =.*/    \1_TURBOPACK = \1 + FST_TYPE_TURBOPACK
    FST_TYPE_BINARY_TURBOPACK = FST_TYPE_BINARY + FST_TYPE_TURBOPACK
    FST_TYPE_REAL_OLD_QUANT_TURBOPACK = FST_TYPE_REAL_OLD_QUANT + FST_TYPE_TURBOPACK
    FST_TYPE_UNSIGNED_TURBOPACK = FST_TYPE_UNSIGNED + FST_TYPE_TURBOPACK
    FST_TYPE_CHAR_TURBOPACK = FST_TYPE_CHAR + FST_TYPE_TURBOPACK
    FST_TYPE_SIGNED_TURBOPACK = FST_TYPE_SIGNED + FST_TYPE_TURBOPACK
    FST_TYPE_REAL_IEEE_TURBOPACK = FST_TYPE_REAL_IEEE + FST_TYPE_TURBOPACK
    FST_TYPE_REAL_TURBOPACK = FST_TYPE_REAL + FST_TYPE_TURBOPACK
    FST_TYPE_STRING_TURBOPACK = FST_TYPE_STRING + FST_TYPE_TURBOPACK
    FST_TYPE_COMPLEX_TURBOPACK = FST_TYPE_COMPLEX + FST_TYPE_TURBOPACK

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

        ('_data_type', ctypes.c_int32),
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

        # Setting data depends on other attributes of the record so it must be
        # set after all the other attributes have been set.  If the user
        # provided an invalid value for data_bits, data_type,
        data_to_set_after = None
        if 'data' in kwargs:
            data_to_set_after = kwargs.get('data')
            del kwargs['data']

        for k,v in kwargs.items():
            if k.startswith('_'):
                raise ValueError(f"Attributes beginning with '_' should not be touched: '{k}'")
            setattr(self, k, v)

        if data_to_set_after is not None:
            self.data = data_to_set_after

    def __new__(cls, *args, **kwargs):
        return _get_default_fst_record()

    @property
    def data_type(self):
        return FstDataType(self._data_type)

    @data_type.setter
    def data_type(self, value):
        self._data_type = value

    @property
    def data(self):
        """ Property encapsulating the data of an fst record.  On access, the
        data is read from the file and stored in a numpy array. """
        if self._data is None:
            # If no data has been set on the record and there is no file to
            # read the data from, return None.  This access is not an error.
            if self._handle < 0:
                return None

            dtype = fst_type_to_numpy_type(self.data_type, self.data_bits)
            data_array = np.empty((self.ni, self.nj, self.nk), dtype=dtype, order='F')
            self._data = data_array.ctypes.data
            res = _fst24_read_record(ctypes.byref(self))
            if res != 1:
                raise FstFileError("Call to C function fst24_read_record failed")

            self._data_array = data_array
        return self._data_array

    @data.setter
    def data(self, value):
        if not isinstance(value, np.ndarray):
            raise TypeError(f"Expecting {np.ndarray.__name__}, got {type(value).__name__}")
        dtype = fst_type_to_numpy_type(self.data_type, self.data_bits)
        if value.dtype != dtype:
            # The argument value, is of the right type as in type(value) is
            # indeed np.ndarray but the value of value.dtype is not right
            # so I think a ValueError is more appropriate but this is debatable
            raise ValueError(f"Numpy data type for the data of this record should be '{dtype}', not '{value.dtype}'")
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
    def __str__(self):
        return f"fst_record(nomvar='{self.nomvar}', data_bits={self.data_bits}, data_type={self.data_type._name_},...)"

    def numpy_type(self):
        """ Return the appropriate numpy type for this record based on its
        data_type and data_bits attributes.
        >>> rec = rmn.fst_record(data_type=..., data_bits=...)
        >>> data_for_record = np.array((rec.ni, rec.nj, rec.nk), data_from_somewhere, dtype=rec.numpy_type)
        """
        return fst_type_to_numpy_type(self.data_type, self.data_bits)


def numpy_type_to_fst_type(array_or_dtype):
    """ Return possible data Fst data types for a given numpy array or type.
    The result is returned as tuple with the first element being the list of
    possible values for a record's data_type attribute and the second element
    is the value for a record's data_bits attribute """
    if isinstance(array_or_dtype, numpy.ndarray):
        numpy_type = array_or_dtype.dtype
    else:
        numpy_type = array_or_dtype
    real_types = (
        FstDataType.FST_TYPE_REAL,
        FstDataType.FST_TYPE_REAL_IEEE,
        FstDataType.FST_TYPE_REAL_OLD_QUANT,
        FstDataType.FST_TYPE_REAL_TURBOPACK,
        FstDataType.FST_TYPE_REAL_IEEE_TURBOPACK,
        FstDataType.FST_TYPE_REAL_OLD_QUANT_TURBOPACK,
    )
    uint_types = (
        FstDataTYpe.FST_TYPE_UNSIGNED,
        FstDataTYpe.FST_TYPE_UNSIGNED_TURBOPACK,
    )
    return {
        "float32": (real_types, 32),
        "float64": (real_types, 64),
        "uint32":  (uint_types, 32),
        "uint64":  (uint_types, 64),
    }[numpy_type]

def fst_type_to_numpy_type(rmn_type, nbits):
    """ Return the numpy data type for a given pair of (FstDataType, nbits)
    Use the `numpy_type` method of fst_record if you have an instance. """
    base_rmn_type = FstDataType(rmn_type &~ FstDataType.FST_TYPE_TURBOPACK)
    if base_rmn_type == FstDataType.FST_TYPE_UNSIGNED:
        if nbits == 32:
            return "uint32"
        elif nbits == 64:
            return "uint64"
        else:
            raise NotImplementedError(f"No numpy data type known for FST_TYPE_UNSIGNED with data_bits = {nbits}")
    elif base_rmn_type in [FstDataType.FST_TYPE_REAL_OLD_QUANT,
                      FstDataType.FST_TYPE_REAL_IEEE,
                      FstDataType.FST_TYPE_REAL]:
        if nbits == 32:
            return "float32"
        elif nbits == 64:
            return "float64"
        else:
            raise NotImplementedError(f"No numpy data type known for FST_TYPE_REAL with data_bits = {nbits}")
    else:
        raise NotImplementedError(f"The proper numpy data type is not known for {rmn_type._name_}")

_fst24_read_record = librmn.fst24_read_record
_fst24_read_record.argtypes = (ctypes.POINTER(fst_record),)
_fst24_read_record.restype = ctypes.c_int

_get_default_fst_record = librmn.get_default_fst_record
_get_default_fst_record.argtypes = tuple()
_get_default_fst_record.restype = fst_record
