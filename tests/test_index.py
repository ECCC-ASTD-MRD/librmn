#!/usr/bin/env python3
import rmn
import os
import pandas
import timeit
import pathlib
print("done importing (cause importing pandas is the longest part of this script)")


# def get_index(filenames):
#     columns = rmn.get_index_columns(filenames)
#     return pandas.DataFrame(columns)
# 
filenames = list(pathlib.Path("/home/sici000/ci_data/libgeoref/grids/").glob('*.fstd'))

index_columns = rmn.get_index_data_frame(filenames)
print(index_columns)

# # filenames = [
# # "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052412_034",
# # "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052500_126",
# # "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052600_025",
# # "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052612_099",
# # "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052712_016",
# # "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052800_080"
# # ]
# print(f"PID={os.getpid()}")
# filenames = filenames[:]
# print(f"Number of files: {len(filenames)}")
# index = get_index(filenames)
# print(index)
# print(index['etiket'])
