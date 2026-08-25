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
    # Generate several files
    base_name = "open_close_link"
    filenames = [f"{base_name}{i}" for i in range(4)]

    # Start from scratch
    for f in filenames:
        Path.unlink(Path(f), missing_ok=True)

    for i, filename in enumerate(filenames):
        ftype = "RSF" if (i % 2 == 0) else "XDF"
        with rmn.fst24_file(filename, f"R/W+{ftype}") as f:
            f.write(make_record(i, f"file_{i}"), rewrite=False)
            f.write(make_record(i + 100, "oijoi"), rewrite=False)

    # Open them all at once
    with rmn.fst24_file(filenames) as f:
        count = 0
        for rec in f.new_query(etiket="file_~"):
            if rec.etiket.lower() != f"file_{rec.ip1}":
                raise ValueError(f"We have a wrong record: {rec}")
            count += 1
        if count != len(filenames):
            raise ValueError(f"Got the wrong number of records: {count}, expected {len(filenames)}")


if __name__ == "__main__":
    run_test()
