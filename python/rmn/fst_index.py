from ._sharedlib import librmn
from .fst24file import fst24_file
from .fstrecord import fst_type_to_numpy_type

from functools import lru_cache
import ctypes
import os
import logging
from typing import Union
from contextlib import contextmanager
from pathlib import Path
import glob
import logging
import json
import numpy as np  # not part of the python standard library
import polars as pl  # not part of the python standard library
import pandas as pd  # not part of the python standard library
import dask  # not part of the python standard library
from dask import array as da  # not part of the python standard library
from threading import stack_size

stack_size(16 * 1024 * 1024)

FST_TYPVAR_LEN = 3
FST_NOMVAR_LEN = 5
FST_ETIKET_LEN = 13
FST_GTYP_LEN = 2
FST_PATH_LEN = 4096  # The value of PATH_MAX
CONVERT_ERROR = 0x01  # Update based on actual definition


def check_versions() -> None:
    """
    Check if installed package versions meet minimum requirements.
    Raises RuntimeError if any dependency version is too low.

    Minimum versions:
    - polars: 0.18.1
    - dask: 2020.12.0
    - numpy: 1.24.4
    - pandas: 1.3.0
    - pyarrow: 11.0.0
    """
    import importlib
    from packaging import version

    min_versions = {"polars": "0.18.1", "dask": "2020.12.0", "numpy": "1.24.4", "pandas": "1.3.0", "pyarrow": "11.0.0"}

    errors = []

    for package, min_version in min_versions.items():
        try:
            # Import the module
            module = importlib.import_module(package)

            # Get installed version
            installed_version = module.__version__

            # Compare versions
            if version.parse(installed_version) < version.parse(min_version):
                errors.append(
                    f"{package} version {installed_version} is lower than minimum required version {min_version}"
                )

        except ImportError:
            errors.append(f"{package} is not installed")
        except AttributeError:
            errors.append(f"Could not determine version for {package}")

    if errors:
        error_msg = "Version requirements not met:\n" + "\n".join(f"- {error}" for error in errors)
        raise RuntimeError(error_msg)

    return True


check_versions()
# This will use the root logger configuration
logger = logging.getLogger()


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
        """See notes.py: We need to allocate memory in Python"""
        print("Destructor")


librmn.rmn_get_index_columns_raw.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_size_t]
librmn.rmn_get_index_columns_raw.restype = ctypes.POINTER(RecordData)


librmn.newdate_.argtypes = [
    ctypes.POINTER(ctypes.c_int),
    ctypes.POINTER(ctypes.c_int),
    ctypes.POINTER(ctypes.c_int),
    ctypes.POINTER(ctypes.c_int),
]
librmn.newdate_.restype = ctypes.c_int


# int FstCanTranslateName(const char *varname);
librmn.FstCanTranslateName.argtypes = [ctypes.c_char_p]
librmn.FstCanTranslateName.restype = ctypes.c_int

# Define ConvertIPtoPK
# int ConvertIPtoPK(float *p1, int *kind1, float *p2, int *kind2, float *p3, int *kind3, int32_t ip1, int32_t ip2, int32_t ip3);
librmn.ConvertIPtoPK.argtypes = [
    ctypes.POINTER(ctypes.c_float),  # p1
    ctypes.POINTER(ctypes.c_int),  # kind1
    ctypes.POINTER(ctypes.c_float),  # p2
    ctypes.POINTER(ctypes.c_int),  # kind2
    ctypes.POINTER(ctypes.c_float),  # p3
    ctypes.POINTER(ctypes.c_int),  # kind3
    ctypes.c_int,  # ip1
    ctypes.c_int,  # ip2
    ctypes.c_int,  # ip3
]
librmn.ConvertIPtoPK.restype = ctypes.c_int

librmn.KindToString.argtypes = [ctypes.c_int, ctypes.POINTER(ctypes.c_char), ctypes.POINTER(ctypes.c_char)]
librmn.KindToString.restype = None


librmn.get_opdict_metadata.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_size_t]
librmn.get_opdict_metadata.restype = ctypes.c_int

# # void incdatr_c(int32_t *fdat1,int32_t *fdat2,double *fnhours);

# librmn.incdatr_c.argtypes = [
#     ctypes.POINTER(ctypes.c_int),
#     ctypes.POINTER(ctypes.c_int),
#     ctypes.POINTER(ctypes.c_double),
# ]
# librmn.incdatr_c.restype = None

# # uint32_t get_valid_date32(
# #     const int64_t origin_date,      //!< Start date of the run
# #     const int32_t timestep_size,    //!< Size of the timestep in seconds
# #     const int32_t timestep_num      //!< Timestep number
# # )
librmn.get_valid_date32.argtypes = [ctypes.c_int64, ctypes.c_int32, ctypes.c_int32]
librmn.get_valid_date32.restype = ctypes.c_uint32


def get_raw_data(filenames):
    if isinstance(filenames, (str, os.PathLike)):
        filenames = [filenames]

    filenames = [str(filename) for filename in filenames]
    filenames_bytes = [filename.encode("utf-8") for filename in filenames]

    filenames_array_type = ctypes.c_char_p * len(filenames_bytes)
    filenames_array = filenames_array_type(*filenames_bytes)

    return librmn.rmn_get_index_columns_raw(filenames_array, len(filenames_bytes))


def _get_index_columns(filenames):
    """Return columns for an index of FST standard files"""

    raw_columns_ptr = get_raw_data(filenames)
    if not raw_columns_ptr:
        raise RuntimeError("Failed to get index columns from C function.")

    raw_columns = raw_columns_ptr.contents

    nb_records = raw_columns.nb_records

    logger.debug(f"Number of records: {nb_records}")

    columns = {}
    columns["typvar"] = _decode_fixed_length_strings(raw_columns.typvar, FST_TYPVAR_LEN, nb_records)
    columns["nomvar"] = _decode_fixed_length_strings(raw_columns.nomvar, FST_NOMVAR_LEN, nb_records)
    columns["etiket"] = _decode_fixed_length_strings(raw_columns.etiket, FST_ETIKET_LEN, nb_records)
    columns["grtyp"] = _decode_fixed_length_strings(raw_columns.grtyp, FST_GTYP_LEN, nb_records)
    columns["path"] = _decode_fixed_length_strings(raw_columns.path, FST_PATH_LEN, nb_records)

    columns["ni"] = _ptr_to_numpy(raw_columns.ni, np.uint32, nb_records)
    columns["nj"] = _ptr_to_numpy(raw_columns.nj, np.uint32, nb_records)
    columns["nk"] = _ptr_to_numpy(raw_columns.nk, np.uint32, nb_records)
    columns["dateo"] = _ptr_to_numpy(raw_columns.dateo, np.uint32, nb_records)
    columns["ip1"] = _ptr_to_numpy(raw_columns.ip1, np.uint32, nb_records)
    columns["ip2"] = _ptr_to_numpy(raw_columns.ip2, np.uint32, nb_records)
    columns["ip3"] = _ptr_to_numpy(raw_columns.ip3, np.uint32, nb_records)
    columns["deet"] = _ptr_to_numpy(raw_columns.deet, np.uint32, nb_records)
    columns["npas"] = _ptr_to_numpy(raw_columns.npas, np.uint32, nb_records)
    columns["data_type"] = _ptr_to_numpy(raw_columns.data_type, np.uint32, nb_records)
    columns["pack_bits"] = _ptr_to_numpy(raw_columns.pack_bits, np.uint32, nb_records)
    columns["data_bits"] = _ptr_to_numpy(raw_columns.data_bits, np.uint32, nb_records)
    columns["ig1"] = _ptr_to_numpy(raw_columns.ig1, np.uint32, nb_records)
    columns["ig2"] = _ptr_to_numpy(raw_columns.ig2, np.uint32, nb_records)
    columns["ig3"] = _ptr_to_numpy(raw_columns.ig3, np.uint32, nb_records)
    columns["ig4"] = _ptr_to_numpy(raw_columns.ig4, np.uint32, nb_records)
    columns["file_index"] = _ptr_to_numpy(raw_columns.file_index, np.uint32, nb_records)

    return columns


def get_index_data_frame(filenames):
    """Return an index for a set of standard files as a Pandas DataFrame"""
    return pl.DataFrame(_get_index_columns(filenames))


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
    return (s.decode("utf-8", errors="ignore").rstrip("\x00").strip() for s in slices)


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


@contextmanager
def cached_fst24_file(path: str):
    """
    Thread-safe context manager for cached FST file opening.

    Args:
        path (str): Path to the FST file

    Yields:
        fst24_file: An opened file object
    """
    try:
        file = _cached_open_fst_file(path)
        yield file
    finally:
        # The file will be automatically managed by the LRU cache
        pass


@lru_cache(maxsize=10)
def _cached_open_fst_file(path: str) -> fst24_file:
    """
    Cached file opener with thread-local lock to ensure thread safety.

    Args:
        path (str): Path to the FST file

    Returns:
        fst24_file: An opened file object
    """
    return fst24_file(path)


def read_fst_data_at_index(path: str, index: int) -> np.ndarray:
    """
    Read data from a FST file at a specific record index, using cached file opening.

    Args:
        path: Path to the FST file
        index: Record index to read

    Returns:
        numpy.ndarray: The data array with the correct shape and dtype.
    """
    with cached_fst24_file(path) as f:
        return f.get_record_by_index(index).data


def remove_duplicate_rows(df: pl.DataFrame) -> pl.DataFrame:
    """
    Remove duplicate rows from a DataFrame, keeping the first occurrence.

    Args:
        df (pl.DataFrame): Input DataFrame
        columns (list): Columns to check for duplicates

    Returns:
        pl.DataFrame: DataFrame with duplicates removed
    """
    columns = [
        "nomvar",
        "typvar",
        "etiket",
        "ni",
        "nj",
        "nk",
        "dateo",
        "ip1",
        "ip2",
        "ip3",
        "deet",
        "npas",
        "datyp",
        "nbits",
        "grtyp",
        "ig1",
        "ig3",
        "ig4",
    ]
    initial_count = df.height

    # Keep only the first occurrence of each duplicate
    clean_df = df.unique(subset=columns, keep="first")

    if initial_count != clean_df.height:
        logger.warning("Found duplicate rows in dataframe!")

        # Identify the duplicate rows
        duplicates = df.filter(df.select(columns).is_duplicated())

        if "d" in duplicates.columns:
            duplicates = duplicates.drop("d")

        logger.info(f"Duplicate rows:\n{duplicates.select(columns)}")

    return clean_df


def read_fst(
    file_paths,  # Union[str, Path, List[str], List[Path], Generator]
    *,
    columns=None,  # Optional column selection
    query=None,  # Optional query filtering
    decode_metadata=False,
    library="polars",  # 'polars' or 'pandas'
    lazy=False,  # Enable lazy loading for Polars
) -> Union[pl.DataFrame, pl.LazyFrame, pd.DataFrame]:
    """
    Read FST files into Polars or Pandas DataFrame.

    Args:
        file_paths: Path(s) to FST file(s)
            - Single file path (str or Path)
            - List of file paths
            - Glob pattern (str or Path)
            - Generator of paths
        columns: Optional subset of columns to load
        query: Optional query string for filtering records
        library: 'polars' (default) or 'pandas'
        lazy: Whether to return a LazyFrame (Polars only)

    Returns:
        Polars DataFrame/LazyFrame or Pandas DataFrame with FST file metadata
    """
    # Expand file paths
    if isinstance(file_paths, (str, Path)):
        # Handle glob patterns
        if any(char in str(file_paths) for char in ["*", "?", "[", "]"]):
            file_paths = glob.glob(str(file_paths))
        else:
            file_paths = [file_paths]

    # Convert to list of strings, handling Path objects
    file_paths = [str(path) for path in file_paths]

    # Get index columns
    df = get_index_data_frame(file_paths)
    df = add_datev_expr(df)
    df = add_grid_identifier_expr(df)
    # Rename columns to match standard naming conventions
    df = df.rename({"data_type": "datyp", "pack_bits": "nbits"})

    # Start Generation Here
    logger.info("Detecting duplicates")
    # Remove duplicate rows
    df = remove_duplicate_rows(df)

    if decode_metadata:
        df = add_parsed_etiket_expr(df)
        df = add_unit_and_description_expr(df)
        df = add_date_of_observation_expr(df)
        df = add_date_of_validity_expr(df)
        df = add_forecast_hour_expr(df)
        df = add_data_type_str_expr(df)
        df = add_decoded_ips_from_ip123_expr(df)
        df = add_is_surface_column_expr(df)
        df = add_level_type_follows_topography_expr(df)
        df = add_level_sort_order_expr(df)
        df = add_interval_expr(df)
        df = add_meta_fields_exists_expr(df)
        df = add_vctype_expr(df)
        df = add_typvar_flags_expr(df)

    # Optional column selection
    if columns:
        # Validate that all requested columns exist in the DataFrame
        available_columns = set(df.columns)
        invalid_columns = [col for col in columns if col not in available_columns]

        if invalid_columns:
            logger.warning(f"Columns not found in DataFrame: {invalid_columns}")
            logger.info(f"Available columns: {sorted(available_columns)}")

            # Only select columns that exist
            columns = [col for col in columns if col in available_columns]
        df = df.select(columns)

    if query:
        # Optional query filtering
        if library == "polars":
            df = df.filter(pl.sql_expr(query))
        elif library == "pandas":
            # Convert Pandas query to Polars SQL expression
            polars_query = convert_pandas_query_to_polars_sql(query)
            df = df.filter(pl.sql_expr(polars_query))

    if "path" in df.columns and "file_index" in df.columns:
        df = df.sort(["path", "file_index"])

    df = add_dask_expr(df)

    if decode_metadata:
        df = df.select(
            [
                "nomvar",
                "typvar",
                "etiket",
                "ni",
                "nj",
                "nk",
                "dateo",
                "ip1",
                "ip2",
                "ip3",
                "deet",
                "npas",
                "datyp",
                "nbits",
                "grtyp",
                "ig1",
                "ig2",
                "ig3",
                "ig4",
                "datev",
                "grid",
                "label",
                "run",
                "implementation",
                "ensemble_member",
                "etiket_format",
                "unit",
                "description",
                "date_of_observation",
                "date_of_validity",
                "forecast_hour",
                "data_type_str",
                "level",
                "ip1_kind",
                "ip1_pkind",
                "ip2_dec",
                "ip2_kind",
                "ip2_pkind",
                "ip3_dec",
                "ip3_kind",
                "ip3_pkind",
                "surface",
                "follow_topography",
                "ascending",
                "interval_ip",
                "interval_low",
                "interval_high",
                "interval_kind",
                "vctype",
                "multiple_modifications",
                "zapped",
                "filtered",
                "interpolated",
                "unit_converted",
                "bounded",
                "missing_data",
                "ensemble_extra_info",
                "masks",
                "masked",
                "d",
            ]
        )
    else:
        # Select columns
        df = df.select(
            [
                "nomvar",
                "typvar",
                "etiket",
                "ni",
                "nj",
                "nk",
                "dateo",
                "ip1",
                "ip2",
                "ip3",
                "deet",
                "npas",
                "datyp",
                "nbits",
                "grtyp",
                "ig1",
                "ig2",
                "ig3",
                "ig4",
                "datev",
                "grid",
                "d",
            ]
        )
    # Return based on library and lazy loading

    if library == "polars":
        return df.lazy() if lazy else df
    elif library == "pandas":
        # Convert dask/polars 'd' column to a simple list before pandas conversion
        if "d" in df.columns:
            d_list = df.select("d").to_numpy().flatten().squeeze()
            df = df.drop("d")
            df = df if isinstance(df, pd.DataFrame) else df.to_pandas()
            df["d"] = d_list
        return df
    else:
        raise ValueError(f"Unsupported library: {library}. Choose 'polars' or 'pandas'.")


# Monkey-patch libraries
pl.read_fst = read_fst
pd.read_fst = lambda *args, **kwargs: read_fst(*args, library="pandas", **kwargs)


def convert_pandas_query_to_polars_sql(query: str) -> str:
    """
    Convert Pandas-style query to Polars SQL expression.

    Handles basic comparisons and logical operations.

    Args:
        query (str): Pandas-style query string

    Returns:
        str: Polars SQL-compatible expression
    """
    # Replace Python comparison operators with SQL-style
    replacements = [
        ("==", "="),  # Equality
        ("!=", "!="),  # Inequality
        ("&", " AND "),  # Logical AND
        ("|", " OR "),  # Logical OR
    ]

    # Apply replacements
    for py_op, sql_op in replacements:
        query = query.replace(py_op, sql_op)

    return query


def add_datev_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Calculate the date of validity from dateo, deet and npas.

    Args:
        df (pl.DataFrame): Input DataFrame with 'dateo', 'deet', and 'npas' columns

    Returns:
        pl.DataFrame: DataFrame with added 'datev' column
    """

    def calculate_datev(dateo, deet, npas):
        """
        Convert CMC timestamp to Unix timestamp in milliseconds using newdate_.

        Args:
            dateo (int): CMC date
            deet (int): Time step in seconds
            npas (int): Number of time steps

        Returns:
            int: Unix date or 0 for special cases
        """
        try:
            # Ensure inputs are integers
            dateo = int(dateo)
            deet = int(deet)
            npas = int(npas)

            # Handle special cases
            if dateo == 0 or dateo == 10101011:
                return 0

            # Call the C function
            try:
                datev = librmn.get_valid_date32(ctypes.c_int64(dateo), ctypes.c_int32(deet), ctypes.c_int32(npas))
                return datev
            except Exception as e:
                logger.error(f"Error in get_valid_date32: {e}")
                return 0

        except (TypeError, ValueError) as e:
            logger.error(f"Error in calculate_datev: {e}")
            return 0

    # Get deet and npas values once
    deet_val = int(df.select(pl.col("deet")).to_series()[0])
    npas_val = int(df.select(pl.col("npas")).to_series()[0])

    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            [
                pl.col("dateo")
                .cast(pl.Int64)
                .map_elements(lambda x: calculate_datev(x, deet_val, npas_val), return_dtype=pl.UInt32)
                .cast(pl.UInt32)
                .alias("datev")
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns(
            [
                pl.col("dateo")
                .cast(pl.Int64)
                .apply(lambda x: calculate_datev(x, deet_val, npas_val))
                .cast(pl.UInt32)
                .alias("datev")
            ]
        )


def add_grid_identifier_expr(df: pl.DataFrame) -> pl.DataFrame:

    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            pl.when(pl.col("nomvar").str.strip_chars().is_in(["^>", ">>", "^^", "!!", "!!SF"]))
            .then(pl.col("ip1").cast(str) + pl.col("ip2").cast(str))
            .when(pl.col("nomvar").str.strip_chars() == "HY")
            .then(pl.lit(""))
            .otherwise(pl.col("ig1").cast(str) + pl.col("ig2").cast(str))
            .alias("grid")
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns(
            pl.when(pl.col("nomvar").str.strip().is_in(["^>", ">>", "^^", "!!", "!!SF"]))
            .then(pl.col("ip1").cast(str) + pl.col("ip2").cast(str))
            .when(pl.col("nomvar").str.strip() == "HY")
            .then(pl.lit(""))
            .otherwise(pl.col("ig1").cast(str) + pl.col("ig2").cast(str))
            .alias("grid")
        )


def add_parsed_etiket_expr(df: pl.DataFrame) -> pl.DataFrame:
    import re

    def parse_etiket(etiket):
        if not isinstance(etiket, str):
            etiket = ""

        # Predefined regex patterns
        patterns = [
            # CMC patterns
            (r"^(\w{2})(\w{5})([NPX])$", "2,5,1,0,D"),  # No ensemble
            (r"^(\w{2})(\w{5})([NPX])(\d{3})$", "2,5,1,3,D"),  # 3-digit number ensemble
            (r"^(\w{2})(\w{5})([NPX])(\d{4})$", "2,5,1,4,K"),  # 4-digit number ensemble
            (r"^(\w{2})(\w{5})([NPX])([a-zA-Z]{3})$", "2,5,1,3,K"),  # 3-letter ensemble
            (r"^(\w{2})(\w{5})([NPX])([a-zA-Z]{4})$", "2,5,1,4,K"),  # 4-letter ensemble
            # Spooki patterns
            (r"^(\w{2})(\w{6})([NPX])$", "2,6,1,0,D"),  # No ensemble
            (r"^(\w{2})(\w{6})([NPX])(\d{3})$", "2,6,1,3,D"),  # 3-digit number ensemble
            (r"^(\w{2})(\w{6})([NPX])ALL$", "2,6,1,3,D"),  # ALL ensemble
            (r"^(\w{2})(\w{6})([NPX])(\d{2}[a-zA-Z])$", "2,6,1,3,D"),  # IIC ensemble
        ]

        for pattern, format_str in patterns:
            match = re.match(pattern, etiket)
            if match:
                groups = match.groups()
                return {
                    "label": groups[1] if len(groups) > 1 else "",
                    "run": groups[0] if len(groups) > 0 else "",
                    "implementation": groups[2] if len(groups) > 2 else "",
                    "ensemble_member": groups[3] if len(groups) > 3 else "",
                    "etiket_format": format_str,
                }

        # Fallback for unmatched patterns
        if len(etiket) >= 2:
            return {
                "label": etiket[2:],
                "run": etiket[:2],
                "implementation": "",
                "ensemble_member": "",
                "etiket_format": f"2,{len(etiket)-2},0,0,D",
            }

        return {
            "label": "",
            "run": "",
            "implementation": "",
            "ensemble_member": "",
            "etiket_format": "",
        }

    # Create Polars expression to parse etiket
    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            [
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .map_elements(lambda x: parse_etiket(x)["label"], return_dtype=pl.Utf8)
                .alias("label"),
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .map_elements(lambda x: parse_etiket(x)["run"], return_dtype=pl.Utf8)
                .alias("run"),
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .map_elements(lambda x: parse_etiket(x)["implementation"], return_dtype=pl.Utf8)
                .alias("implementation"),
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .map_elements(lambda x: parse_etiket(x)["ensemble_member"], return_dtype=pl.Utf8)
                .alias("ensemble_member"),
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .map_elements(lambda x: parse_etiket(x)["etiket_format"], return_dtype=pl.Utf8)
                .alias("etiket_format"),
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns(
            [
                pl.col("etiket").cast(pl.Utf8).fill_null("").apply(lambda x: parse_etiket(x)["label"]).alias("label"),
                pl.col("etiket").cast(pl.Utf8).fill_null("").apply(lambda x: parse_etiket(x)["run"]).alias("run"),
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .apply(lambda x: parse_etiket(x)["implementation"])
                .alias("implementation"),
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .apply(lambda x: parse_etiket(x)["ensemble_member"])
                .alias("ensemble_member"),
                pl.col("etiket")
                .cast(pl.Utf8)
                .fill_null("")
                .apply(lambda x: parse_etiket(x)["etiket_format"])
                .alias("etiket_format"),
            ]
        )


def add_forecast_hour_expr(df: pl.DataFrame) -> pl.DataFrame:
    return df.with_columns(
        pl.when(pl.col("deet") * pl.col("npas") > 0)
        .then(pl.col("deet") * pl.col("npas") / 3600.0)
        .otherwise(None)
        .alias("forecast_hour")
    )


def add_date_of_validity_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Calculate the date of validity from observation date and forecast parameters.

    Args:
        df (pl.DataFrame): Input DataFrame with 'date_of_observation', 'deet', and 'npas' columns

    Returns:
        pl.DataFrame: DataFrame with added 'date_of_validity' column
    """

    return df.with_columns(
        pl.when(pl.col("date_of_observation").is_not_null())
        .then(pl.col("date_of_observation") + (pl.col("deet") * pl.col("npas") * 1000).cast(pl.Duration("ms")))
        .otherwise(pl.lit(None))
        .alias("date_of_validity")
    )


def add_date_of_observation_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Calculate the date of observation from the CMC timestamp using newdate_ function.

    Args:
        df (pl.DataFrame): Input DataFrame with 'dateo' column

    Returns:
        pl.DataFrame: DataFrame with added 'date_of_observation' column
    """

    def convert_cmc_timestamp(dateo):
        """
        Convert CMC timestamp to Unix timestamp in milliseconds using newdate_.

        Args:
            dateo (int): CMC timestamp

        Returns:
            int: Unix timestamp in milliseconds or None
        """
        # Handle special cases
        if dateo == 0 or dateo == 10101011:
            return None

        try:
            # Prepare ctypes arguments
            true_date = ctypes.c_int(dateo)
            date_array = ctypes.c_int(0)
            time_array = ctypes.c_int(0)
            mode = ctypes.c_int(-3)  # Convert from CMC stamp to printable

            # Call newdate_ function
            result = librmn.newdate_(
                ctypes.byref(true_date), ctypes.byref(date_array), ctypes.byref(time_array), ctypes.byref(mode)
            )

            # Check if conversion was successful
            if result == 0:
                # Extract date components
                year = date_array.value // 10000
                month = (date_array.value % 10000) // 100
                day = date_array.value % 100

                # Extract time components
                hour = time_array.value // 1000000
                minute = (time_array.value % 1000000) // 10000
                second = (time_array.value % 10000) // 100

                # Create a datetime object
                from datetime import datetime, timezone

                dt = datetime(year, month, day, hour, minute, second, tzinfo=timezone.utc)

                # Convert to milliseconds since epoch
                return int(dt.timestamp() * 1000)

            return None

        except Exception as e:
            logger.error(f"Error converting CMC timestamp {dateo}: {e}")
            return None

    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            [
                pl.col("dateo")
                .cast(pl.Int64)
                .map_elements(convert_cmc_timestamp, return_dtype=pl.Int64)
                .cast(pl.Datetime(time_unit="ms"))
                .alias("date_of_observation")
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns(
            [
                pl.col("dateo")
                .apply(convert_cmc_timestamp, return_dtype=pl.Int64)
                .cast(pl.Datetime(time_unit="ms"))
                .alias("date_of_observation")
            ]
        )


def add_data_type_str_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Convert data type to a type letter representation.

    Args:
        df (pl.DataFrame): Input DataFrame with 'datyp' column

    Returns:
        pl.DataFrame: DataFrame with added 'data_type_str' column
    """
    # Mapping of base types to letters
    type_letters = ["X", "R", "I", "C", "S", "E", "F", "A", "Z"]

    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            pl.when(pl.col("datyp").is_not_null())
            .then(
                pl.col("datyp").map_elements(
                    lambda x: type_letters[x & 0x3F].lower() if (x & 128) == 128 else type_letters[x & 0x3F],
                    return_dtype=pl.Utf8,
                )
            )
            .otherwise(pl.lit(None))
            .alias("data_type_str")
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns(
            pl.when(pl.col("datyp").is_not_null())
            .then(
                pl.col("datyp").apply(
                    lambda x: type_letters[x & 0x3F].lower() if (x & 128) == 128 else type_letters[x & 0x3F]
                )
            )
            .otherwise(pl.lit(None))
            .alias("data_type_str")
        )


def add_typvar_flags_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Parse typvar flags from the second character of typvar.

    Args:
        df (pl.DataFrame): Input DataFrame with 'typvar' column

    Returns:
        pl.DataFrame: DataFrame with added typvar flag columns
    """

    def parse_typvar_flags(typvar: str):
        """
        Parse typvar flags for a single typvar string.

        Args:
            typvar (str): Typvar string to parse

        Returns:
            dict: Dictionary of boolean flags in a specific order
        """
        typvar2 = typvar[1] if len(typvar) == 2 else ""

        return {
            "multiple_modifications": typvar2 == "M",
            "zapped": typvar2 == "Z",
            "filtered": typvar2 == "F",
            "interpolated": typvar2 == "I",
            "unit_converted": typvar2 == "U",
            "bounded": typvar2 == "B",
            "missing_data": typvar2 in ["?", "H"],
            "ensemble_extra_info": typvar2 == "!",
            "masks": typvar[0] == "@" and typvar2 == "@",
            "masked": typvar2 == "@" and typvar[0] != "@",
        }

    try:
        return df.with_columns(
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["multiple_modifications"], return_dtype=pl.Boolean)
            .alias("multiple_modifications"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["zapped"], return_dtype=pl.Boolean)
            .alias("zapped"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["filtered"], return_dtype=pl.Boolean)
            .alias("filtered"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["interpolated"], return_dtype=pl.Boolean)
            .alias("interpolated"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["unit_converted"], return_dtype=pl.Boolean)
            .alias("unit_converted"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["bounded"], return_dtype=pl.Boolean)
            .alias("bounded"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["missing_data"], return_dtype=pl.Boolean)
            .alias("missing_data"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["ensemble_extra_info"], return_dtype=pl.Boolean)
            .alias("ensemble_extra_info"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["masks"], return_dtype=pl.Boolean)
            .alias("masks"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .map_elements(lambda x: parse_typvar_flags(x)["masked"], return_dtype=pl.Boolean)
            .alias("masked"),
        )
    except AttributeError:
        return df.with_columns(
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["multiple_modifications"])
            .cast(pl.Boolean)
            .alias("multiple_modifications"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["zapped"])
            .cast(pl.Boolean)
            .alias("zapped"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["filtered"])
            .cast(pl.Boolean)
            .alias("filtered"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["interpolated"])
            .cast(pl.Boolean)
            .alias("interpolated"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["unit_converted"])
            .cast(pl.Boolean)
            .alias("unit_converted"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["bounded"])
            .cast(pl.Boolean)
            .alias("bounded"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["missing_data"])
            .cast(pl.Boolean)
            .alias("missing_data"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["ensemble_extra_info"])
            .cast(pl.Boolean)
            .alias("ensemble_extra_info"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["masks"])
            .cast(pl.Boolean)
            .alias("masks"),
            pl.col("typvar")
            .cast(pl.Utf8)
            .fill_null("")
            .apply(lambda x: parse_typvar_flags(x)["masked"])
            .cast(pl.Boolean)
            .alias("masked"),
        )


def add_decoded_ips_from_ip123_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Decodes IP columns and adds decoded information to the DataFrame.
    Then, adds string representations for the kind columns using the add_kind_string_expr function.

    Parameters:
        df (pl.DataFrame): The input DataFrame with 'nomvar', 'ip1', 'ip2', 'ip3' columns.

    Returns:
        pl.DataFrame: The DataFrame with decoded IP columns and their string representations.
    """

    def get_decoded_ips_from_ip123(nomvar: str, ip1: int, ip2: int, ip3: int) -> dict:

        # Encode the variable name as bytes
        nomvar_bytes = nomvar.encode("utf-8")

        # Check if the name can be translated
        can_translate = librmn.FstCanTranslateName(nomvar_bytes)

        if not can_translate:
            # For variables that can't be translated, return raw values
            return {
                "level": None,
                "ip2_dec": None,
                "ip3_dec": None,
                "ip1_kind": None,
                "ip2_kind": None,
                "ip3_kind": None,
                "status_ip": 0,
            }

        # Prepare variables for ConvertIPtoPK
        p1 = ctypes.c_float()
        p2 = ctypes.c_float()
        p3 = ctypes.c_float()
        kind1 = ctypes.c_int()
        kind2 = ctypes.c_int()
        kind3 = ctypes.c_int()

        # Call ConvertIPtoPK
        status_ip = librmn.ConvertIPtoPK(
            ctypes.byref(p1),
            ctypes.byref(kind1),
            ctypes.byref(p2),
            ctypes.byref(kind2),
            ctypes.byref(p3),
            ctypes.byref(kind3),
            ip1,
            ip2,
            ip3,
        )

        # Check for errors
        if (kind1.value < 0 or kind2.value < 0 or kind3.value < 0) or (status_ip & CONVERT_ERROR):
            # Decode error somewhere
            return {
                "level": None,
                "ip2_dec": None,
                "ip3_dec": None,
                "ip1_kind": None,
                "ip2_kind": None,
                "ip3_kind": None,
                "status_ip": 0,
            }

        # Force modulo 32 on kinds

        kind1.value &= 0x1F
        kind2.value &= 0x1F
        kind3.value &= 0x1F

        return {
            "level": p1.value,
            "ip2_dec": p2.value,
            "ip3_dec": p3.value,
            "ip1_kind": kind1.value,
            "ip2_kind": kind2.value,
            "ip3_kind": kind3.value,
            "status_ip": status_ip,
        }

    try:
        # Try newer Polars API (1.x)
        df = df.with_columns(
            [
                pl.struct(["nomvar", "ip1", "ip2", "ip3"])
                .map_elements(
                    lambda row: get_decoded_ips_from_ip123(row["nomvar"], row["ip1"], row["ip2"], row["ip3"]),
                    return_dtype=pl.Struct(
                        [
                            pl.Field("level", pl.Float64),  # Changed from Float32
                            pl.Field("ip2_dec", pl.Float64),  # Changed from Float32
                            pl.Field("ip3_dec", pl.Float64),  # Changed from Float32
                            pl.Field("ip1_kind", pl.Int64),  # Changed from Int32
                            pl.Field("ip2_kind", pl.Int64),  # Changed from Int32
                            pl.Field("ip3_kind", pl.Int64),  # Changed from Int32
                            pl.Field("status_ip", pl.Int64),  # Changed from Int32
                        ]
                    ),
                )
                .alias("decoded_ips")
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        df = df.with_columns(
            [
                pl.struct(["nomvar", "ip1", "ip2", "ip3"])
                .apply(lambda row: get_decoded_ips_from_ip123(row["nomvar"], row["ip1"], row["ip2"], row["ip3"]))
                .alias("decoded_ips")
            ]
        )

    # Extract individual fields from the 'decoded_ips' struct into separate columns
    decoded_columns = [
        pl.col("decoded_ips").struct.field("level").cast(pl.Float32).alias("level"),
        pl.col("decoded_ips").struct.field("ip2_dec").cast(pl.Float32).alias("ip2_dec"),
        pl.col("decoded_ips").struct.field("ip3_dec").cast(pl.Float32).alias("ip3_dec"),
        pl.col("decoded_ips").struct.field("ip1_kind").cast(pl.Int32).alias("ip1_kind"),
        pl.col("decoded_ips").struct.field("ip2_kind").cast(pl.Int32).alias("ip2_kind"),
        pl.col("decoded_ips").struct.field("ip3_kind").cast(pl.Int32).alias("ip3_kind"),
    ]

    # Add the decoded columns to the DataFrame and drop the intermediate 'decoded_ips' column
    df = df.with_columns(decoded_columns).drop("decoded_ips")

    df = add_kind_string_expr(df, "ip1_kind", "ip1_pkind")
    df = add_kind_string_expr(df, "ip2_kind", "ip2_pkind")
    df = add_kind_string_expr(df, "ip3_kind", "ip3_pkind")

    return df


def add_kind_string_expr(df: pl.DataFrame, kind_col: str, new_col: str) -> pl.DataFrame:
    """
    Adds a new string column to the DataFrame based on the specified kind column.
    Replaces specific integer values with an empty string and transforms others
    using the lib.KindToString C function.

    Parameters:
        df (pl.DataFrame): The input DataFrame.
        kind_col (str): The name of the kind column to transform.
        new_col (str): The name of the new string column to create.

    Returns:
        pl.DataFrame: The DataFrame with the added string column.
    """
    replace_values = {-1, 3, 15, 17, 100}

    @lru_cache(maxsize=20)
    def kind_to_string(kind: int) -> str:
        """
        Transforms an integer kind value to its string representation using the C library.

        Parameters:
            kind (int): The kind value to transform.

        Returns:
            str: The first two characters of the string representation.
        """
        if kind in {-1, 3, 15, 17, 100} or kind is None:
            return None

        try:
            # Create two single-character buffers
            s1 = ctypes.create_string_buffer(1)
            s2 = ctypes.create_string_buffer(1)

            # Call the C function KindToString correctly
            librmn.KindToString(kind, s1, s2)

            # Decode the characters and concatenate
            decoded_kind = s1.value.decode("utf-8") + s2.value.decode("utf-8")
            return decoded_kind
        except Exception as e:
            logger.error(f"Error converting kind {kind}: {e}")
            return None

    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            pl.when(pl.col(kind_col).is_in(list(replace_values)) | pl.col(kind_col).is_null())
            .then(pl.lit(None))
            .otherwise(pl.col(kind_col).map_elements(kind_to_string, return_dtype=pl.Utf8))
            .alias(new_col)
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns(
            pl.when(pl.col(kind_col).is_in(list(replace_values)) | pl.col(kind_col).is_null())
            .then(pl.lit(None))
            .otherwise(pl.col(kind_col).apply(kind_to_string))
            .alias(new_col)
        )


def add_is_surface_column_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Adds an 'is_surface' boolean column to the DataFrame based on 'ip1_kind' and 'level'.

    Parameters:
        df (pl.DataFrame): The input DataFrame with 'nomvar', 'ip1', 'ip2', 'ip3', 'ip1_kind', and 'level' columns.

    Returns:
        pl.DataFrame: The DataFrame with the added 'is_surface' column.
    """
    # Define meter_levels as per the original function
    meter_levels = np.arange(0.0, 10.5, 0.5).tolist()  # [0.0, 0.5, 1.0, ..., 10.0]
    return df.with_columns(
        pl.when((pl.col("ip1_kind") == 5) & (pl.col("level") == 1.0))
        .then(True)
        .when((pl.col("ip1_kind") == 4) & (pl.col("level").is_in(meter_levels)))
        .then(True)
        .when((pl.col("ip1_kind") == 1) & (pl.col("level") == 1.0))
        .then(True)
        .otherwise(False)
        .alias("surface")
    )


def add_level_type_follows_topography_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Adds a boolean column 'level_type_follows_topography' to the DataFrame based on 'ip1_kind'.

    Parameters:
        df (pl.DataFrame): The input DataFrame with an 'ip1_kind' column.

    Returns:
        pl.DataFrame: The DataFrame with the added 'level_type_follows_topography' column.
    """
    return df.with_columns([pl.col("ip1_kind").is_in([1, 4, 5]).alias("follow_topography")])


def add_level_sort_order_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Adds a boolean column 'level_sort_order' to the DataFrame based on 'ip1_kind'.

    Parameters:
        df (pl.DataFrame): The input DataFrame with an 'ip1_kind' column.

    Returns:
        pl.DataFrame: The DataFrame with the added 'level_sort_order' column.
    """

    @lru_cache(maxsize=15)
    def get_level_sort_order(kind: int) -> bool:
        """returns the level sort order

        :param kind: level kind
        :type kind: int
        :return: True if the level type is ascending or False otherwise
        :rtype: bool
        """
        # order = {0:'ascending',1:'descending',2:'descending',4:'ascending',5:'descending',21:'ascending'}
        order = {0: True, 3: True, 4: True, 21: True, 100: True, 1: False, 2: False, 5: False, 6: False, 7: False}
        if kind in order.keys():
            return order[kind]

        return False

    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            [pl.col("ip1_kind").map_elements(get_level_sort_order, return_dtype=pl.Boolean).alias("ascending")]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns([pl.col("ip1_kind").apply(get_level_sort_order).alias("ascending")])


# def get_field_metadata_expr(df: pl.DataFrame) -> pl.DataFrame:


def get_optdict_metadata(rpn_name: str) -> str:
    """
    Retrieves metadata for a given RPN name.

    Args:
        rpn_name (str): The RPN name.

    Returns:
        str: The metadata as a string.
    """
    result = ctypes.create_string_buffer(1000)
    librmn.get_opdict_metadata(rpn_name.encode("utf-8"), result, len(result))
    meta = result.value.decode("utf-8").strip()
    return json.loads(meta)


def add_unit_and_description_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Retrieves unit and description for a given RPN name.
    """

    @lru_cache(maxsize=200)
    def get_unit_and_description(rpn_name: str) -> str:
        """
        Retrieves unit and description for a given RPN name.
        """
        meta_dict = get_optdict_metadata(rpn_name)

        return {"description": meta_dict["standard_name"], "unit": meta_dict["unit"]}

    try:
        # Try newer Polars API (1.x)
        df = df.with_columns(
            [
                pl.col("nomvar")
                .map_elements(
                    get_unit_and_description,
                    return_dtype=pl.Struct([pl.Field("description", pl.Utf8), pl.Field("unit", pl.Utf8)]),
                )
                .alias("field_metadata")
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        df = df.with_columns([pl.col("nomvar").apply(get_unit_and_description).alias("field_metadata")])

    decoded_columns = [
        pl.col("field_metadata").struct.field("unit").cast(pl.Utf8).alias("unit"),
        pl.col("field_metadata").struct.field("description").cast(pl.Utf8).alias("description"),
    ]

    # # Add the decoded columns to the DataFrame and drop the intermediate 'decoded_ips' column
    df = df.with_columns(decoded_columns).drop("field_metadata")

    return df


def add_interval_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Adds an interval column to the DataFrame based on 'ip1', 'ip2', 'ip3', 'ip1_kind', 'ip2_kind', 'ip3_kind'.
    """
    if df is None:
        return df

    def get_interval(
        ip1: int,
        ip2: int,
        ip3: int,
        level: float,
        ip1_kind: int,
        ip2_dec: float,
        ip2_kind: int,
        ip3_dec: float,
        ip3_kind: int,
    ) -> dict:
        if ip3 >= 32768:
            if (ip1 >= 32768) and (ip1_kind == ip3_kind):
                return {"ip": "ip1", "low": level, "high": ip3_dec, "kind": ip1_kind}
            elif (ip2 >= 32768) and (ip2_kind == ip3_kind):
                return {"ip": "ip2", "low": ip3_dec, "high": ip2_dec, "kind": ip2_kind}
            else:
                return {"ip": None, "low": None, "high": None, "kind": None}
        return {"ip": None, "low": None, "high": None, "kind": None}

    try:
        # Try newer Polars API (1.x)
        result = df.with_columns(
            [
                pl.struct(["ip1", "ip2", "ip3", "level", "ip1_kind", "ip2_dec", "ip2_kind", "ip3_dec", "ip3_kind"])
                .map_elements(
                    lambda row: get_interval(
                        row["ip1"],
                        row["ip2"],
                        row["ip3"],
                        row["level"],
                        row["ip1_kind"],
                        row["ip2_dec"],
                        row["ip2_kind"],
                        row["ip3_dec"],
                        row["ip3_kind"],
                    ),
                    return_dtype=pl.Struct(
                        [
                            pl.Field("ip", pl.Utf8),
                            pl.Field("low", pl.Float64),
                            pl.Field("high", pl.Float64),
                            pl.Field("kind", pl.Int64),
                        ]
                    ),
                )
                .alias("interval")
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        result = df.with_columns(
            [
                pl.struct(["ip1", "ip2", "ip3", "level", "ip1_kind", "ip2_dec", "ip2_kind", "ip3_dec", "ip3_kind"])
                .apply(
                    lambda row: get_interval(
                        row["ip1"],
                        row["ip2"],
                        row["ip3"],
                        row["level"],
                        row["ip1_kind"],
                        row["ip2_dec"],
                        row["ip2_kind"],
                        row["ip3_dec"],
                        row["ip3_kind"],
                    )
                )
                .alias("interval")
            ]
        )

    if result is None:
        return df

    decoded_columns = [
        pl.col("interval").struct.field("ip").cast(pl.Utf8).alias("interval_ip"),
        pl.col("interval").struct.field("low").cast(pl.Float32).alias("interval_low"),
        pl.col("interval").struct.field("high").cast(pl.Float32).alias("interval_high"),
        pl.col("interval").struct.field("kind").cast(pl.Int32).alias("interval_kind"),
    ]

    # Add the decoded columns to the DataFrame and drop the intermediate column
    result = result.with_columns(decoded_columns).drop("interval")

    return result


def add_meta_fields_exists_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Detect metadata fields for each unique grid group.

    Args:
        df (pl.DataFrame): Input DataFrame with 'grid' and 'nomvar' columns.

    Returns:
        pl.DataFrame: Original DataFrame with added metadata columns.
    """
    # First create the boolean metadata columns
    try:
        # Try newer Polars API (1.x)
        meta_results = df.group_by("grid").agg(
            [
                (pl.col("nomvar") == "!!").any().alias("toctoc"),
                (pl.col("nomvar") == "P0").any().alias("p0"),
                (pl.col("nomvar") == "E1").any().alias("e1"),
                (pl.col("nomvar") == "PT").any().alias("pt"),
                (pl.col("nomvar") == "HY").any().alias("hy"),
                (pl.col("nomvar") == "!!SF").any().alias("sf"),
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        meta_results = df.groupby("grid").agg(
            [
                (pl.col("nomvar") == "!!").any().alias("toctoc"),
                (pl.col("nomvar") == "P0").any().alias("p0"),
                (pl.col("nomvar") == "E1").any().alias("e1"),
                (pl.col("nomvar") == "PT").any().alias("pt"),
                (pl.col("nomvar") == "HY").any().alias("hy"),
                (pl.col("nomvar") == "!!SF").any().alias("sf"),
            ]
        )

    # Create vcode lists using Python operations
    # First get all unique grids
    all_grids = df.select("grid").unique()
    grid_list = all_grids.get_column("grid").to_list()

    # Create vcode lists for each grid
    vcode_lists = []
    for grid in grid_list:
        grid_ig1 = df.filter((pl.col("grid") == grid) & (pl.col("nomvar") == "!!")).select("ig1").to_series().to_list()

        if grid_ig1:
            vcode_lists.append([grid_ig1[0]])
        else:
            vcode_lists.append([-1])

    # Create vcode DataFrame with correct types from the start
    vcode_df = pl.DataFrame(
        {"grid": grid_list, "vcode": vcode_lists}, schema={"grid": pl.Utf8, "vcode": pl.List(pl.Int64)}
    )

    # Join everything together
    meta_results = meta_results.join(vcode_df, on="grid", how="left")

    # Join back to original DataFrame
    return df.join(meta_results, on="grid", how="left")


def add_vctype_expr(df: pl.DataFrame) -> pl.DataFrame:
    @lru_cache(maxsize=20)
    def get_vertical_coordinate_type(
        kind: int, toctoc: bool, p0: bool, pt: bool, hy: bool, vcode_value: int, check_p0: bool = True
    ) -> str:
        result = "UNKNOWN"

        if kind == 0:
            result = "METER_SEA_LEVEL"
        elif kind == 4:
            result = "METER_GROUND_LEVEL"
        elif kind == 2:
            result = "PRESSURE_2001"
        elif kind == 1:  # SIGMA
            if pt:
                result = "ETA_1002"
            elif hy:
                result = "HYBRID_NORMALIZED_1003"
            else:
                result = "SIGMA_1001"
        elif kind == 5:  # HYBRID
            if hy:
                result = "HYBRID_5001"

        if result == "UNKNOWN":
            if kind == 5:  # HYBRID:
                if toctoc and vcode_value == 5001:
                    result = "HYBRID_5001"
                elif toctoc and vcode_value == 5002:
                    result = "HYBRID_5002"  # HYBRID_STAGGERED
                elif toctoc and vcode_value == 5005:
                    result = "HYBRID_5005"
                elif toctoc and vcode_value == 5100:
                    result = "HYBRID_5100"
            elif kind == 1:  # SIGMA
                if toctoc and vcode_value == 1001:
                    result = "SIGMA_1001"
                elif toctoc and vcode_value == 1002:
                    result = "ETA_1002"
            elif kind == 2:
                if toctoc and vcode_value == 2001:
                    result = "PRESSURE_2001"

        if check_p0 and kind in [5, 1]:  # HYBRID or SIGMA
            if not p0:
                result = "UNKNOWN"

        return result

    def process_row(row):
        vcode_value = row["vcode"][0] if isinstance(row["vcode"], list) and row["vcode"] else -1
        return get_vertical_coordinate_type(
            kind=row["ip1_kind"],
            toctoc=row["toctoc"],
            p0=row["p0"],
            pt=row["pt"],
            hy=row["hy"],
            vcode_value=vcode_value,
            check_p0=True,
        )

    try:
        # Try newer Polars API (1.x)
        return df.with_columns(
            [
                pl.struct(["ip1_kind", "toctoc", "p0", "pt", "hy", "vcode"])
                .map_elements(process_row, return_dtype=pl.Utf8)
                .alias("vctype")
            ]
        )
    except AttributeError:
        # Fallback for older Polars API (0.x)
        return df.with_columns(
            [
                pl.struct(["ip1_kind", "toctoc", "p0", "pt", "hy", "vcode"])
                .apply(process_row)
                .cast(pl.Utf8)
                .alias("vctype")
            ]
        )
    # try:
    #     # Try newer Polars API (1.x)
    #     return df.with_columns(
    #         [
    #             pl.struct(["ip1_kind", "toctoc", "p0", "pt", "hy", "vcode"])
    #             .map_elements(process_row, return_dtype=pl.Utf8)
    #             .alias("vctype")
    #         ]
    #     )
    # except AttributeError:
    #     # Fallback for older Polars API (0.x)
    #     return df.with_columns([pl.col("nomvar").apply(process_row).cast(pl.Utf8).alias("vctype")])


def add_dask_expr(df: pl.DataFrame) -> pl.DataFrame:
    """
    Add a column 'd' containing dask arrays from FST file data.

    Args:
        df (pl.DataFrame): Input DataFrame with 'path' and 'file_index' columns.

    Returns:
        pl.DataFrame: DataFrame with added 'd' column containing dask arrays.
    """
    # Ensure required columns exist
    required_cols = ["path", "file_index", "ni", "nj", "nk", "datyp", "nbits"]
    if not all(col in df.columns for col in required_cols):
        missing = [col for col in required_cols if col not in df.columns]
        raise ValueError(f"Missing required columns: {missing}")

    # Get shapes for each row - only include nk if it's > 1
    shapes = []
    for row in df.select(["ni", "nj", "nk"]).rows():
        if row[2] == 1:  # if nk == 1
            shapes.append((row[0], row[1]))  # 2D shape
        else:
            shapes.append((row[0], row[1], row[2]))  # 3D shape

    # Create delayed objects and dask arrays for each row
    dask_arrays = []
    for (path, idx, datyp, nbits), shape in zip(df.select(["path", "file_index", "datyp", "nbits"]).rows(), shapes):
        # Create delayed object
        delayed_obj = dask.delayed(read_fst_data_at_index)(path, idx)
        # Convert to dask array with known shape
        array = da.from_delayed(delayed_obj, shape=shape, dtype=fst_type_to_numpy_type(datyp, nbits))
        dask_arrays.append(array)

    # Create Polars Series and add to DataFrame
    d = pl.Series("d", dask_arrays, dtype=pl.Object)
    return df.with_columns([d])
