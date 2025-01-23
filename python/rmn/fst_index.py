from ._sharedlib import librmn
from .fst24file import fst24_file

import ctypes
import os
import logging
import numpy as np
import pandas as pd
import json

FST_TYPVAR_LEN = 3
FST_NOMVAR_LEN = 5
FST_ETIKET_LEN = 13
FST_GTYP_LEN = 2
FST_PATH_LEN = 4096 # The value of PATH_MAX

class RecordData(ctypes.Structure):
    _pack_ = 1  # Match C struct packing
    _fields_ = [
        ("nb_records", ctypes.c_size_t),
        ("nomvar", ctypes.POINTER(ctypes.c_char)),
        ("typvar", ctypes.POINTER(ctypes.c_char)),
        ("etiket", ctypes.POINTER(ctypes.c_char)),
        ("ni", ctypes.POINTER(ctypes.c_uint32)),
        ("nj", ctypes.POINTER(ctypes.c_uint32)),
        ("nk", ctypes.POINTER(ctypes.c_uint32)),
        ("dateo", ctypes.POINTER(ctypes.c_uint32)),
        ("ip1", ctypes.POINTER(ctypes.c_uint32)),
        ("ip2", ctypes.POINTER(ctypes.c_uint32)),
        ("ip3", ctypes.POINTER(ctypes.c_uint32)),
        ("deet", ctypes.POINTER(ctypes.c_uint32)),
        ("npas", ctypes.POINTER(ctypes.c_uint32)),
        ("data_type", ctypes.POINTER(ctypes.c_uint32)),
        ("pack_bits", ctypes.POINTER(ctypes.c_uint32)),
        ("data_bits", ctypes.POINTER(ctypes.c_uint32)),
        ("grtyp", ctypes.POINTER(ctypes.c_char)),
        ("ig1", ctypes.POINTER(ctypes.c_uint32)),
        ("ig2", ctypes.POINTER(ctypes.c_uint32)),
        ("ig3", ctypes.POINTER(ctypes.c_uint32)),
        ("ig4", ctypes.POINTER(ctypes.c_uint32)),
        ("path", ctypes.POINTER(ctypes.c_char)),
        ("file_index", ctypes.POINTER(ctypes.c_uint32)),
    ]
    def __del__(self):
        """ See notes.py: We need to allocate memory in Python """
        print("Destructor")

librmn.rmn_get_index_columns_raw.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_size_t]
librmn.rmn_get_index_columns_raw.restype = ctypes.POINTER(RecordData)

def get_raw_data(filenames):
    if isinstance(filenames, (str, os.PathLike)):
        filenames = [filenames]

    filenames = [str(filename) for filename in filenames]
    filenames_bytes = [filename.encode("utf-8") for filename in filenames]

    filenames_array_type = ctypes.c_char_p * len(filenames_bytes)
    filenames_array = filenames_array_type(*filenames_bytes)

    return librmn.rmn_get_index_columns_raw(filenames_array, len(filenames_bytes))

def get_index_columns(filenames):
    """ Return columns for an index of FST standard files """

    raw_columns_ptr = get_raw_data(filenames)
    if not raw_columns_ptr:
        raise RuntimeError("Failed to get index columns from C function.")

    raw_columns = raw_columns_ptr.contents

    nb_records = raw_columns.nb_records

    logging.debug(f"Number of records: {nb_records}")

    columns = {}
    columns['typvar'] = _decode_fixed_length_strings(raw_columns.typvar, FST_TYPVAR_LEN, nb_records)
    columns['nomvar'] = _decode_fixed_length_strings(raw_columns.nomvar, FST_NOMVAR_LEN, nb_records)
    columns['etiket'] = _decode_fixed_length_strings(raw_columns.etiket, FST_ETIKET_LEN, nb_records)
    columns['grtyp'] = _decode_fixed_length_strings(raw_columns.grtyp, FST_GTYP_LEN, nb_records)
    columns['path'] = _decode_fixed_length_strings(raw_columns.path, FST_PATH_LEN, nb_records)

    columns['ni'] = _ptr_to_numpy(raw_columns.ni, np.uint32, nb_records)
    columns['nj'] = _ptr_to_numpy(raw_columns.nj, np.uint32, nb_records)
    columns['nk'] = _ptr_to_numpy(raw_columns.nk, np.uint32, nb_records)
    columns['dateo'] = _ptr_to_numpy(raw_columns.dateo, np.uint32, nb_records)
    columns['ip1'] = _ptr_to_numpy(raw_columns.ip1, np.uint32, nb_records)
    columns['ip2'] = _ptr_to_numpy(raw_columns.ip2, np.uint32, nb_records)
    columns['ip3'] = _ptr_to_numpy(raw_columns.ip3, np.uint32, nb_records)
    columns['deet'] = _ptr_to_numpy(raw_columns.deet, np.uint32, nb_records)
    columns['npas'] = _ptr_to_numpy(raw_columns.npas, np.uint32, nb_records)
    columns['data_type'] = _ptr_to_numpy(raw_columns.data_type, np.uint32, nb_records)
    columns['pack_bits'] = _ptr_to_numpy(raw_columns.pack_bits, np.uint32, nb_records)
    columns['data_bits'] = _ptr_to_numpy(raw_columns.data_bits, np.uint32, nb_records)
    columns['ig1'] = _ptr_to_numpy(raw_columns.ig1, np.uint32, nb_records)
    columns['ig2'] = _ptr_to_numpy(raw_columns.ig2, np.uint32, nb_records)
    columns['ig3'] = _ptr_to_numpy(raw_columns.ig3, np.uint32, nb_records)
    columns['ig4'] = _ptr_to_numpy(raw_columns.ig4, np.uint32, nb_records)
    columns['file_index'] = _ptr_to_numpy(raw_columns.file_index, np.uint32, nb_records)

    return columns

def get_index_data_frame(filenames):
    """ Return an index for a set of standard files as a Pandas DataFrame """
    return pd.DataFrame(get_index_columns(filenames))

def _decode_fixed_length_strings(ptr, length, nb_records):
    """
    Decodes a contiguous block of fixed-length strings.

    Args:
        ptr (POINTER(c_char)): Pointer to the byte block.
        length (int): Length of each string.
        nb_records (int): Number of records.

    Returns:
        Generator producing nb_records strings of length at most length
    """
    if not ptr:
        return [""] * nb_records

    np_array = _ptr_to_numpy(ptr, np.char, length * nb_records)
    slices = (bytes(s) for s in np.split(np_array, nb_records))
    return (s.decode('utf-8', errors='ignore').rstrip('\x00').strip() for s in slices)

def _ptr_to_numpy(ptr, dtype, count):
    """
    Converts a ctypes pointer to a NumPy array.

    Args:
        ptr: ctypes pointer to the data
        ctype: ctypes data type (e.g., ctypes.c_uint32)
        count: number of elements in the array

    Returns:
        np.ndarray: 1D numpy ndarray of shape (count,)
    """
    if not ptr:
        return np.array([], dtype=dtype)

    return np.ctypeslib.as_array(ptr, shape=(count,))  # Convert to NumPy array


def read_fst_data_at_index(path: str, index: int) -> np.ndarray:
    """
    Read data from a FST file at a specific record index.

    Args:
        path: Path to the FST file
        index: Record index to read

    Returns:
        numpy.ndarray: The data array with the correct shape and dtype.

    Raises:
        RuntimeError: If reading fails

    Note:
        use fst24_file.get_records_with_data(filename, indices):
    """
    with fst24_file(path) as f:
        # NOTE: The way the data is read, the numpy array is allocated in Python
        # and we tell the C function to put the data in the memory that is
        # managed by the numpy array.  That way we have nothing to worry about
        # in terms of memory allocation.
        return f.get_record_at_index(index).data

librmn.get_opdict_metadata.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_size_t]
librmn.get_opdict_metadata.restype = ctypes.c_int

def get_opdict_metadata(rpn_name: str) -> str:
    """
    Retrieves metadata for a given RPN name.

    Args:
        rpn_name (str): The RPN name.

    Returns:
        str: The metadata as a string.
    """
    result = ctypes.create_string_buffer(1000)
    returncode = librmn.get_opdict_metadata(rpn_name.encode("utf-8"), result, len(result))
    if returncode != 0:
        raise RuntimeError("Failed obtain metadata from op dictionnary")
    meta = result.value.decode("utf-8").strip()
    return json.loads(meta)

# def get_grid_identifier_expr(df: pl.DataFrame) -> pl.DataFrame:
#     """
#     Most performant grid identifier method
#     """
#     return df.with_columns(
#         pl.when(pl.col("nomvar").str.strip().is_in(["^>", ">>", "^^", "!!", "!!SF"]))
#         .then(pl.col("ip1").cast(str) + pl.col("ip2").cast(str))
#         .when(pl.col("nomvar").str.strip() == "HY")
#         .then(pl.lit(""))
#         .otherwise(pl.col("ig1").cast(str) + pl.col("ig2").cast(str))
#         .alias("grid")
#     )


# def get_parsed_etiket_expr(df: pl.DataFrame) -> pl.DataFrame:
#     import re
# 
#     def parse_etiket(etiket):
#         if not isinstance(etiket, str):
#             etiket = ""
# 
#         # Predefined regex patterns
#         patterns = [
#             # CMC patterns
#             (r"^(\w{2})(\w{5})([NPX])$", "2,5,1,0,D"),  # No ensemble
#             (r"^(\w{2})(\w{5})([NPX])(\d{3})$", "2,5,1,3,D"),  # 3-digit number ensemble
#             (r"^(\w{2})(\w{5})([NPX])(\d{4})$", "2,5,1,4,K"),  # 4-digit number ensemble
#             (r"^(\w{2})(\w{5})([NPX])([a-zA-Z]{3})$", "2,5,1,3,K"),  # 3-letter ensemble
#             (r"^(\w{2})(\w{5})([NPX])([a-zA-Z]{4})$", "2,5,1,4,K"),  # 4-letter ensemble
#             # Spooki patterns
#             (r"^(\w{2})(\w{6})([NPX])$", "2,6,1,0,D"),  # No ensemble
#             (r"^(\w{2})(\w{6})([NPX])(\d{3})$", "2,6,1,3,D"),  # 3-digit number ensemble
#             (r"^(\w{2})(\w{6})([NPX])ALL$", "2,6,1,3,D"),  # ALL ensemble
#             (r"^(\w{2})(\w{6})([NPX])(\d{2}[a-zA-Z])$", "2,6,1,3,D"),  # IIC ensemble
#         ]
# 
#         for pattern, format_str in patterns:
#             match = re.match(pattern, etiket)
#             if match:
#                 groups = match.groups()
#                 return {
#                     "label": groups[1] if len(groups) > 1 else "",
#                     "run": groups[0] if len(groups) > 0 else "",
#                     "implementation": groups[2] if len(groups) > 2 else "",
#                     "ensemble_member": groups[3] if len(groups) > 3 else "",
#                     "etiket_format": format_str,
#                 }
# 
#         # Fallback for unmatched patterns
#         if len(etiket) >= 2:
#             return {
#                 "label": etiket[2:],
#                 "run": etiket[:2],
#                 "implementation": "",
#                 "ensemble_member": "",
#                 "etiket_format": f"2,{len(etiket)-2},0,0,D",
#             }
# 
#         return {
#             "label": "",
#             "run": "",
#             "implementation": "",
#             "ensemble_member": "",
#             "etiket_format": "",
#         }
# 
#     # Create Polars expression to parse etiket
#     return df.with_columns(
#         [
#             pl.col("etiket")
#             .cast(pl.Utf8)  # Ensure 'etiket' is of string type
#             .fill_null("")  # Handle null values by replacing them with empty strings
#             .apply(lambda x: parse_etiket(x)["label"])
#             .alias("label"),
#             pl.col("etiket").cast(pl.Utf8).fill_null("").apply(lambda x: parse_etiket(x)["run"]).alias("run"),
#             pl.col("etiket")
#             .cast(pl.Utf8)
#             .fill_null("")
#             .apply(lambda x: parse_etiket(x)["implementation"])
#             .alias("implementation"),
#             pl.col("etiket")
#             .cast(pl.Utf8)
#             .fill_null("")
#             .apply(lambda x: parse_etiket(x)["ensemble_member"])
#             .alias("ensemble_member"),
#             pl.col("etiket")
#             .cast(pl.Utf8)
#             .fill_null("")
#             .apply(lambda x: parse_etiket(x)["etiket_format"])
#             .alias("etiket_format"),
#         ]
#     )


# def read_fst(
#     file_paths,  # Union[str, Path, List[str], List[Path], Generator]
#     *,
#     columns=None,  # Optional column selection
#     query=None,  # Optional query filtering
#     decode_metadata=True,
#     library="polars",  # 'polars' or 'pandas'
#     lazy=False,  # Enable lazy loading for Polars
# ) -> Union[pl.DataFrame, pl.LazyFrame, pd.DataFrame]:
#     """
#     Read FST files into Polars or Pandas DataFrame.
# 
#     Args:
#         file_paths: Path(s) to FST file(s)
#             - Single file path (str or Path)
#             - List of file paths
#             - Glob pattern (str or Path)
#             - Generator of paths
#         columns: Optional subset of columns to load
#         query: Optional query string for filtering records
#         library: 'polars' (default) or 'pandas'
#         lazy: Whether to return a LazyFrame (Polars only)
# 
#     Returns:
#         Polars DataFrame/LazyFrame or Pandas DataFrame with FST file metadata
#     """
#     # Expand file paths
#     if isinstance(file_paths, (str, Path)):
#         # Handle glob patterns
#         if any(char in str(file_paths) for char in ["*", "?", "[", "]"]):
#             file_paths = glob.glob(str(file_paths))
#         else:
#             file_paths = [file_paths]
# 
#     # Convert to list of strings, handling Path objects
#     file_paths = [str(path) for path in file_paths]
# 
#     # Get index columns
#     df = get_index_columns_raw_python(file_paths)
# 
#     if decode_metadata:
#         df = get_parsed_etiket_expr(df)
# 
#     # Optional column selection
#     if columns:
#         df = df.select(columns)
# 
#     
#     # Optional query filtering
#     if query:
#         if library == "polars":
#             df = df.filter(pl.sql_expr(query))
#         elif library == "pandas":
#             # Convert to Pandas first, then apply query
#             df = df.to_pandas()
#             df = df.query(query)
# 
#     # Return based on library and lazy loading
#     if library == "polars":
#         return df.lazy() if lazy else df
#     elif library == "pandas":
#         return df if isinstance(df, pd.DataFrame) else df.to_pandas()
#     else:
#         raise ValueError(f"Unsupported library: {library}. Choose 'polars' or 'pandas'.")


# Monkey-patch libraries
# pl.read_fst = read_fst
# pd.read_fst = lambda *args, **kwargs: read_fst(*args, library="pandas", **kwargs)


# def get_parsed_etiket(raw_etiket: str, etiket_format: str = ""):
#     """parses the etiket of a standard file to get label, run, implementation and ensemble member if available

#     :param raw_etiket: raw etiket before parsing
#     :type raw_etiket: str
#     :param etiket_format: flag with number of character in run, label, implementation and ensemble_member
#     :type etiket_format: str
#     :return: the parsed etiket, run, implementation, ensemble member and etiket_format
#     :rtype: str

#     >>> get_parsed_etiket('')
#     ('', '', '', '')
#     >>> get_parsed_etiket('R1_V710_N')
#     ('_V710_', 'R1', 'N', '')
#     """
#     import re

#     label = ""
#     run = None
#     implementation = None
#     ensemble_member = None

#     if etiket_format != "":
#         idx = etiket_format.split(",")
#         idx_run = int(idx[0])
#         idx_label = int(idx[0]) + int(idx[1])
#         idx_implementation = int(idx[0]) + int(idx[1]) + int(idx[2])
#         idx_ensemble = int(idx[0]) + int(idx[1]) + int(idx[2]) + int(idx[3])

#         run = raw_etiket[:idx_run]
#         label = raw_etiket[idx_run:idx_label]
#         implementation = raw_etiket[idx_label:idx_implementation]
#         ensemble_member = raw_etiket[idx_implementation:idx_ensemble]
#         return label, run, implementation, ensemble_member, etiket_format

#     # match_run = "[RGPEAIMWNC_][\\dRLHMEA_]"
#     match_run = "\\w{2}"
#     match_main_cmc = "\\S{5}"
#     match_main_spooki = "\\S{6}"
#     match_implementation = "[NPX]"
#     # match_ensemble_member = "\\w{3}"
#     match_ensemble_number3 = "\\d{3}"
#     match_ensemble_number4 = "\\d{4}"
#     match_ensemble_letter3 = "[a-zA-Z]{3}"
#     match_ensemble_letter4 = "[a-zA-Z]{4}"
#     match_ensemble_all = "ALL"
#     match_ensemble_iic = "\\d{2}[a-zA-Z]"
#     match_end = "$"

#     re_match_run_only = match_run + match_end
#     re_match_cmc_no_ensemble = match_run + match_main_cmc + match_implementation + match_end
#     re_match_cmc_ensemble_number3 = (
#         match_run + match_main_cmc + match_implementation + match_ensemble_number3 + match_end
#     )
#     re_match_cmc_ensemble_number4 = (
#         match_run + match_main_cmc + match_implementation + match_ensemble_number4 + match_end
#     )
#     re_match_cmc_ensemble_letter3 = (
#         match_run + match_main_cmc + match_implementation + match_ensemble_letter3 + match_end
#     )
#     re_match_cmc_ensemble_letter4 = (
#         match_run + match_main_cmc + match_implementation + match_ensemble_letter4 + match_end
#     )
#     re_match_spooki_no_ensemble = match_run + match_main_spooki + match_implementation + match_end
#     re_match_spooki_ensemble = match_run + match_main_spooki + match_implementation + match_ensemble_number3 + match_end
#     re_match_spooki_ensemble_all = match_run + match_main_spooki + match_implementation + match_ensemble_all + match_end
#     re_match_spooki_ensemble_iic = match_run + match_main_spooki + match_implementation + match_ensemble_iic + match_end

#     if re.match(re_match_cmc_no_ensemble, raw_etiket):
#         etiket_format = "2,5,1,0,D"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:7]
#         implementation = raw_etiket[7]
#     elif re.match(re_match_cmc_ensemble_number3, raw_etiket):
#         etiket_format = "2,5,1,3,D"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:7]
#         implementation = raw_etiket[7]
#         ensemble_member = raw_etiket[8:11]
#     elif re.match(re_match_cmc_ensemble_number4, raw_etiket):
#         etiket_format = "2,5,1,4,K"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:7]
#         implementation = raw_etiket[7]
#         ensemble_member = raw_etiket[8:12]
#     elif re.match(re_match_cmc_ensemble_letter3, raw_etiket):
#         etiket_format = "2,5,1,3,K"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:7]
#         implementation = raw_etiket[7]
#         ensemble_member = raw_etiket[8:11]
#     elif re.match(re_match_cmc_ensemble_letter4, raw_etiket):
#         etiket_format = "2,5,1,4,K"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:7]
#         implementation = raw_etiket[7]
#         ensemble_member = raw_etiket[8:12]
#     elif re.match(re_match_spooki_no_ensemble, raw_etiket):
#         etiket_format = "2,6,1,0,D"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:8]
#         implementation = raw_etiket[8]
#     elif re.match(re_match_spooki_ensemble, raw_etiket):
#         etiket_format = "2,6,1,3,D"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:8]
#         implementation = raw_etiket[8]
#         ensemble_member = raw_etiket[9:12]
#     elif re.match(re_match_spooki_ensemble_all, raw_etiket):
#         etiket_format = "2,6,1,3,D"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:8]
#         implementation = raw_etiket[8]
#         ensemble_member = raw_etiket[9:12]
#     elif re.match(re_match_run_only, raw_etiket):
#         etiket_format = "2,0,0,0,K"
#         run = raw_etiket[:2]
#     elif re.match(re_match_spooki_ensemble_iic, raw_etiket):
#         etiket_format = "2,6,1,3,D"
#         run = raw_etiket[:2]
#         label = raw_etiket[2:8]
#         implementation = raw_etiket[8]
#         ensemble_member = raw_etiket[9:12]
#     else:
#         if len(raw_etiket) >= 2:
#             label_len = len(raw_etiket) - 2
#             etiket_format = "2," + str(label_len) + ",0,0,D"
#             run = raw_etiket[:2]
#             label = raw_etiket[2:]
#         else:
#             label = raw_etiket
#     return label, run, implementation, ensemble_member, etiket_format


# Numpy dtype mappings for 32-bit FST data types
# FST_DATYP2NUMPY_LIST = {
#     0: np.uint32,  # binary, transparent
#     1: np.float32,  # floating point
#     2: np.uint32,  # unsigned integer
#     3: np.uint32,  # character (R4A in integer)
#     4: np.int32,  # signed integer
#     5: np.float32,  # IEEE floating point
#     6: np.float32,  # floating point (16 bit)
#     7: np.uint8,  # character string
#     8: np.complex64,  # complex IEEE
# }
# 
# # Numpy dtype mappings for 64-bit FST data types
# FST_DATYP2NUMPY_LIST64 = {
#     0: np.uint64,  # binary, transparent
#     1: np.float64,  # floating point
#     2: np.uint64,  # unsigned integer
#     3: np.uint64,  # character (R4A in integer)
#     4: np.int64,  # signed integer
#     5: np.float64,  # IEEE floating point
#     6: np.float64,  # floating point (16 bit)
#     8: np.complex128,  # complex IEEE
# }
# 
# 
# def get_record_dtype(data_type: int, pack_bits: int) -> np.dtype:
#     """Convert FST data type to numpy dtype."""
# 
#     # Remove turbopack and missing value flags for type checking
#     base_type = data_type & ~128
# 
#     # Select dtype mapping based on pack_bits
#     # TODO: Centralize logic: already coded for fst_record data accessor and getter
#     if pack_bits == 64:
#         dtype_map = FST_DATYP2NUMPY_LIST64
#     elif pack_bits <= 32:
#         dtype_map = FST_DATYP2NUMPY_LIST
#     else:
#         raise ValueError(f"Unsupported pack_bits value: {pack_bits}")
# 
#     try:
#         return dtype_map[base_type]
#     except KeyError:
#         raise ValueError(f"Unsupported FST data type: {data_type} (base type: {base_type})")
