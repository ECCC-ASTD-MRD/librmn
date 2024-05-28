import rmn
import pandas
print("done importing (cause importing pandas is the longest part of this script)")

def get_index(filename):
    columns = rmn.get_index_columns(filename)
    return pandas.DataFrame(columns)

print(get_index("/fs/site5/eccc/prod/ops/suites/gdps_20220621/g1/gridpt.usr/prog/pres/2024052100_144"))
