#!/usr/bin/env python3

from pathlib import Path
from typing import Any

import numpy as np
import rmn
from rmn import FstDataType

all_data: dict[Any, np.ndarray] = {}


def make_rec(data_type: int, data: np.ndarray, pack_bits=0):
    data_bits = data.dtype.itemsize * 8
    is_turbo = data_type >= 128
    if pack_bits <= 0:
        pack_bits = data_bits

    def prod(tup):
        p = tup[0]
        for val in tup[1:]:
            p *= val
        return p

    if data.ndim >= 3:
        ni, nj, nk = data.shape[:2] + (prod(data.shape[2:]),)
    elif data.ndim == 2:
        ni, nj, nk = data.shape + (1,)
    else:
        ni, nj, nk = data.shape + (1, 1)

    return rmn.fst_record(
        data=data,
        data_type=data_type,
        data_bits=data_bits,
        pack_bits=pack_bits,
        ni=ni,
        nj=nj,
        nk=nk,
        dateo=0,
        npas=0,
        deet=0,
        ip1=0,
        ip2=0,
        ip3=0,
        ig1=0,
        ig2=0,
        ig3=0,
        ig4=0,
        etiket=f"{data.dtype}{'.T' if is_turbo else ''}",
    )


def make_data():
    all_data[np.float64] = np.arange(32.0 * 60.0).reshape(32, 60).T * 0.11111111
    all_data[np.float32] = all_data[np.float64].astype(np.float32)

    all_data[np.int64] = np.arange(32**2).reshape(32, 32).T
    for t in [np.int32, np.int16, np.int8, np.uint64, np.uint32, np.uint16, np.uint8]:
        all_data[t] = all_data[np.int64].astype(t)

    for t in ["V2", "V4", "V5", "V6"]:
        all_data[t] = all_data[np.float32].T.view(dtype=t).T
        # v = all_data[t]
        # print(f"{v.shape}, {v.dtype.itemsize}\n{v[:5, :5]}")


float_types = [
    FstDataType.FST_TYPE_REAL,
    FstDataType.FST_TYPE_REAL_TURBOPACK,
    FstDataType.FST_TYPE_REAL_IEEE,
    FstDataType.FST_TYPE_REAL_IEEE_TURBOPACK,
    FstDataType.FST_TYPE_REAL_OLD_QUANT,
    FstDataType.FST_TYPE_REAL_OLD_QUANT_TURBOPACK,
]

int_types = [
    FstDataType.FST_TYPE_SIGNED,
    FstDataType.FST_TYPE_SIGNED_TURBOPACK,
    FstDataType.FST_TYPE_UNSIGNED,
    FstDataType.FST_TYPE_UNSIGNED_TURBOPACK,
]


def human_readable_bytes(num_bytes: int) -> str:
    units = ["B", "KB", "MB", "GB", "TB", "PB", "EB"]
    size = float(num_bytes)
    for unit in units:
        if size < 1024 or unit == units[-1]:
            return f"{size:6.2f} {unit}" if unit != "B" else f"{int(size)}  {unit}"
        size /= 1024

    return f"{num_bytes} B"


def run_test(format):
    print(f"Running test for {format}")
    filename = f"python_fst_types_{format}.fst"
    Path.unlink(Path(filename), missing_ok=True)

    cases = [
        (FstDataType.FST_TYPE_REAL, np.float64),
        (FstDataType.FST_TYPE_REAL_TURBOPACK, np.float64),
        (FstDataType.FST_TYPE_REAL_IEEE, np.float64),
        (FstDataType.FST_TYPE_REAL_IEEE_TURBOPACK, np.float64),
        (FstDataType.FST_TYPE_REAL_OLD_QUANT, np.float64),
        (FstDataType.FST_TYPE_REAL_OLD_QUANT_TURBOPACK, np.float64),
        # (FstDataType.FST_TYPE_SIGNED, np.int64),
        # (FstDataType.FST_TYPE_UNSIGNED, np.uint64),
        (FstDataType.FST_TYPE_REAL, np.float32),
        (FstDataType.FST_TYPE_REAL_TURBOPACK, np.float32),
        (FstDataType.FST_TYPE_REAL_IEEE, np.float32),
        (FstDataType.FST_TYPE_REAL_IEEE_TURBOPACK, np.float32),
        (FstDataType.FST_TYPE_REAL_OLD_QUANT, np.float32),
        (FstDataType.FST_TYPE_REAL_OLD_QUANT_TURBOPACK, np.float32),
        (FstDataType.FST_TYPE_SIGNED, np.int32),
        (FstDataType.FST_TYPE_UNSIGNED, np.uint32),
        (FstDataType.FST_TYPE_BINARY, "V4"),
        (FstDataType.FST_TYPE_BINARY, "V2"),
        (FstDataType.FST_TYPE_BINARY, "V6"),
        (FstDataType.FST_TYPE_BINARY, "V5"),
    ]

    with rmn.fst24_file(filename, f"{format}+r/w") as f:
        for type_fst, type_np in cases:
            r = make_rec(type_fst, all_data[type_np])
            f.write(r, rewrite=False)

        # Truncated
        for type_fst, type_np in cases:
            if type_fst not in float_types:
                continue

            for packed in [48, 32, 24, 16, 12]:
                if packed >= np.dtype(type_np).itemsize * 8:
                    continue
                if packed < 16 and type_fst in [
                    FstDataType.FST_TYPE_REAL_IEEE,
                    FstDataType.FST_TYPE_REAL_IEEE_TURBOPACK,
                ]:
                    continue

                r = make_rec(type_fst, all_data[type_np], pack_bits=packed)
                f.write(r, rewrite=False)

    np.set_printoptions(precision=1, linewidth=128)
    with rmn.fst24_file(filename) as f:
        q = f.new_query()
        for rec in q:
            if rec.data is None:
                raise ValueError("No data!")

            # print(f"{rec.nomvar} {rec.etiket} {rec.data_type} {rec.data_bits} {rec.pack_bits}")

            if rec.data_type in float_types:
                ref = all_data[np.float64] if rec.data_bits == 64 else all_data[np.float32]
                diff = np.linalg.norm(rec.data - ref) / np.linalg.norm(ref)
                diff_str = f"{diff:8.2e}"

                threshold = 0.0
                if rec.pack_bits < 32:
                    threshold = {
                        (FstDataType.FST_TYPE_REAL_IEEE, 24): 2e-5,
                        (FstDataType.FST_TYPE_REAL_IEEE, 16): 4e-3,
                        (FstDataType.FST_TYPE_REAL_OLD_QUANT, 24): 1e-7,
                        (FstDataType.FST_TYPE_REAL_OLD_QUANT, 16): 3e-5,
                        (FstDataType.FST_TYPE_REAL_OLD_QUANT, 12): 4e-4,
                        (FstDataType.FST_TYPE_REAL, 24): 5e-8,
                        (FstDataType.FST_TYPE_REAL, 16): 2e-5,
                        (FstDataType.FST_TYPE_REAL, 12): 3e-4,
                    }[(FstDataType(rec.data_type & ~128), rec.pack_bits)]

            elif rec.data_type in int_types:
                ref = all_data[np.int32]
                diff = (rec.data - ref.astype(rec.data.dtype)).sum()
                diff_str = f"{diff:8d}"

                threshold = 0
            elif rec.data_type == FstDataType.FST_TYPE_BINARY:
                # print(
                #     f"data shape: {rec.data.shape}, itemsize {rec.data.dtype.itemsize}, "
                #     f"({rec.data_bits} -> {rec.pack_bits})"
                # )
                ref = all_data["V4"]
                diff = (rec.data.T.view(dtype="int8").T - ref.T.view(dtype="int8").T).sum()
                diff_str = f"{diff:8d}"
                threshold = 0
            else:
                raise ValueError("Not implemented")

            # print(
            #     f"{rec.etiket:10s} {rec.data_type & ~128}{'*' if rec.data_type >= 128 else ' '} "
            #     f"{rec.data_bits} -> {rec.pack_bits} "
            #     f"data: ({diff_str}, {human_readable_bytes(rec.total_stored_bytes):>9s}) {np.ravel(rec.data)[:6]}"
            # )

            if diff > threshold:
                raise ValueError(
                    f"({format}) Data is different from what was stored! Diff = {diff} (expected max {threshold})"
                )


if __name__ == "__main__":
    make_data()
    run_test("XDF")
    run_test("RSF")
