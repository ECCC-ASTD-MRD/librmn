#!/usr/bin/env python3
from pathlib import Path

import numpy
import rmn


def make_record(ip1, etiket):
    dummy_data = numpy.arange(1.0)
    return rmn.fst_record(
        data=dummy_data,
        data_type=rmn.FstDataType.FST_TYPE_REAL,
        data_bits=64,
        pack_bits=64,
        ni=dummy_data.size,
        nj=1,
        nk=1,
        ip1=ip1,
        ip2=0,
        ip3=0,
        ig1=0,
        ig2=0,
        ig3=0,
        ig4=0,
        dateo=0,
        npas=0,
        deet=0,
        etiket=etiket,
    )


def run_test():
    base_name = "backend_name"
    filenames = [f"{base_name}_RSF.fst", f"{base_name}_XDF.fst"]

    # Start from scratch
    for f in filenames:
        Path.unlink(Path(f), missing_ok=True)

    # Single files: backend_name should match the backend used to create them
    for i, (filename, backend) in enumerate(zip(filenames, ["RSF", "XDF"])):
        with rmn.fst24_file(filename, f"R/W+{backend}") as f:
            f.write(make_record(i, f"file_{i}"), rewrite=False)
            if f.backend_name != backend:
                raise ValueError(f"Expected backend_name '{backend}', got '{f.backend_name}'")

    # Linked collection: only the first file is considered
    with rmn.fst24_file(filenames) as f:
        if f.backend_name != "RSF":
            raise ValueError(f"Expected backend_name 'RSF' for linked files, got '{f.backend_name}'")

    # Closed file: backend_name should be None
    f = rmn.fst24_file(filenames[0])
    f.close()
    if f.backend_name is not None:
        raise ValueError(f"Expected backend_name None for closed file, got '{f.backend_name}'")

    for f in filenames:
        Path.unlink(Path(f), missing_ok=True)


if __name__ == "__main__":
    run_test()
