import rmn
import os
import pandas
print("done importing (cause importing pandas is the longest part of this script)")

def get_index(filenames):
    columns = rmn.get_index_columns(filenames)
    return pandas.DataFrame(columns)
d = "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres"
filenames = [f"{d}/{f}" for f in os.listdir(d) if f.startswith('2024')]
print(len(filenames))
# filenames = [
# "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052412_034",
# "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052500_126",
# "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052600_025",
# "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052612_099",
# "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052712_016",
# "/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052800_080"
# ]
index = get_index(filenames)
print(index)
