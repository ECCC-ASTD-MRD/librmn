#
# SUPER IMPORTANT NOTE!!!!!!
#
# To truly test the package, this file must be run with the rmn NOT in
# the current directory
#
# And our PWD must not be in the rmn package directory.
#
# This is because it changes how the relative imports work.
#

import numpy as np
import rmn
import sys

filename = "/fs/site5/eccc/cmd/w/spst900/spooki/spooki_dir/pluginsRelatedStuff/AbsoluteValue/testsFiles/AbsoluteValue_file2cmp.std"

f = rmn.fst24_file(filename=filename, options="")
q4 = f.new_query(ip3=0)
for record in q4:
    ip3 = record.ip3 # PyMemberDef
    nomvar = record.nomvar # PyGetSetDef
    etiket = record.etiket # PyGetSetDef
    print(f"PYTHON: Result from q3: {record}", file=sys.stderr) # .tp_str
    print(f"PYTHON: Record has ip3={ip3}, nomvar='{nomvar}', etiket='{etiket}'", file=sys.stderr); #.tp_getset
    print(record.data)

try:
    f2 = rmn.fst24_file(filename="/home/phc001/.profile", options="");
except rmn.InvalidFstFileError as e:
    print(f"Got exception as expected: {repr(e)}")

try:
    f2 = rmn.fst24_file(filename="", options="");
except FileNotFoundError as e:
    print(f"Got exception as expected: {type(e)}:{e}")

try:
    f2 = rmn.fst24_file(filename)
except TypeError as e:
    print(f"Got exception as expected: {repr(e)}")

print("===================== without options =========================")
f2 = rmn.fst24_file(filename=filename)

rec = rmn.fst_record(nomvar="<<", ip3=42)
rec.ip3 = 99
rec.ni = 4
rec.nj = 7
rec.nk = 1
print(rec)

try:
    rec.data = "not a numpy array"
except TypeError as e:
    print(f"Got an exception as expected: {repr(e)}")

# No idea if the NI, NJ, NK are in the right order
x = np.random.random(rec.nk * rec.nj * rec.ni).reshape((rec.nk, rec.nj, rec.ni))
rec.data = x

print(rec.data)





# r = rmn.fst_record()
# 
# print(f"rmn.fst_record: {r}")

