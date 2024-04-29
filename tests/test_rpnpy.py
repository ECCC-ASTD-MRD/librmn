
#
# SUPER IMPORTANT NOTE!!!!!!
#
# To truly test the package, this file must be run with the rpnpy2 NOT in
# the current directory
#
# And our PWD must not be in the rpnpy2 package directory.
#
# This is because it changes how the relative imports work.
#
import rpnpy2
import sys

filename = "/fs/site5/eccc/cmd/w/spst900/spooki/spooki_dir/pluginsRelatedStuff/AbsoluteValue/testsFiles/AbsoluteValue_file2cmp.std"

f = rpnpy2.fst24_file(filename=filename, options="")

# print(f.filename)
# 
# print(str(f))
# 
# q1 = f.new_query(nomvar="TT", ip3=0)
# q2 = f.new_query(ip3=0)
# print("=============================================", file=sys.stderr)
# q3 = f.new_query(nomvar="VV")
# for restult in q3:
#     print(f"Result from q3: {result}", file=sys.stderr)
print("=============================================", file=sys.stderr)
q4 = f.new_query(ip3=0)
for result in q4:
    ip3 = result.ip3
    nomvar = result.nomvar
    etiket = result.etiket
    print(f"PYTHON: Result from q3: {result}", file=sys.stderr) # .tp_str
    print(f"PYTHON: Record has ip3={ip3}, nomvar='{nomvar}', etiket='{etiket}'", file=sys.stderr); #.tp_getset




# r = rpnpy2.fst_record()
# 
# print(f"rpnpy2.fst_record: {r}")

