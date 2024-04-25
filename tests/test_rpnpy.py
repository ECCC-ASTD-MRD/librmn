
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

filename = "/fs/site5/eccc/cmd/w/spst900/spooki/spooki_dir/pluginsRelatedStuff/AbsoluteValue/testsFiles/AbsoluteValue_file2cmp.std"

f = rpnpy2.fst24_file(filename=filename, options="")

print(str(f))
