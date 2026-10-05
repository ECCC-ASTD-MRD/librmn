"""Date encoding/decoding for RPN fst24 files.

The FST layer stores dates (dateo/datev) as encoded integers. This module
wraps the C ``newdate_c`` routine to convert between those integers and
Python/numpy date objects.

>>> import rmn
>>> enc = rmn.encode_date("2024-01-15 12:30:45")
>>> rmn.decode_date(enc)
numpy.datetime64('2024-01-15T12:30:45.000000000')
"""

from __future__ import annotations

import ctypes
from datetime import datetime
from typing import Union

import numpy as np

from ._sharedlib import librmn

newdate_c = librmn.newdate_c
newdate_c.restype = ctypes.c_int

DateInputType = Union[str, datetime, np.datetime64]


def encode_date(init_date: DateInputType) -> int:
    """Encode a date into the integer representation used by FST files.

    Args:
        init_date: The date to encode, as a string in the format
            ``"YYYY-MM-DD HH:MM:SS"`` (e.g. ``"2024-01-15 12:30:45"``),
            or as a ``datetime``/``numpy.datetime64`` object, which is
            converted to that string format first.

    Returns:
        The encoded date as an integer, suitable for use as the ``dateo``
        or ``datev`` field of an ``fst_record``.

    Raises:
        RuntimeError: If the underlying ``newdate_c`` call fails.
    """
    init_date = str(init_date)
    date = ctypes.c_int(int(init_date[:10].replace("-", "")))
    time = ctypes.c_int(int(init_date[11:19].replace(":", "") + "00"))
    datev = ctypes.c_int(0)
    mode = ctypes.c_int(3)

    rc = newdate_c(ctypes.byref(datev), ctypes.byref(date), ctypes.byref(time), ctypes.byref(mode))
    if rc != 0:
        raise RuntimeError(f"newdate_c failed: {rc}")
    return datev.value


def decode_date(enc_date: int) -> np.datetime64:
    """Decode an FST date integer into a ``numpy.datetime64``.

    Args:
        enc_date: An encoded date as produced by :func:`encode_date` (or
            read from the ``dateo``/``datev`` field of an ``fst_record``).

    Returns:
        The decoded date as a ``numpy.datetime64``.

    Raises:
        RuntimeError: If the underlying ``newdate_c`` call fails.
    """
    date = ctypes.c_int(0)
    time = ctypes.c_int(0)
    datev = ctypes.c_int(int(enc_date))
    mode = ctypes.c_int(-3)

    rc = newdate_c(ctypes.byref(datev), ctypes.byref(date), ctypes.byref(time), ctypes.byref(mode))
    if rc != 0:
        raise RuntimeError(f"newdate_c failed: {rc}")
    d = f"{date.value:08d}"
    t = f"{time.value:08d}"
    dt = datetime.strptime(d + t[:6], "%Y%m%d%H%M%S")
    date64 = np.datetime64(dt)
    return date64
