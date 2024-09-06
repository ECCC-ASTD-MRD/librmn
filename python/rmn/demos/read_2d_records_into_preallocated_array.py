import numpy as np
import rmn
import argparse

p = argparse.ArgumentParser()
p.add_argument('-f', '--filename', required=True)
args = p.parse_args()

f = rmn.fst24_file(filename=args.filename)
# Consume iterable into list
# - Need to know the number of records to create the numpy array that will
#   receive their data.
tt_records = list(f.new_query(nomvar='TT'))

# Create numpy array
# - Assume data is of type float32 (corresponding to dtype='f' for numpy)
# - Assume all records have the same ni,nj as the first one
cube = np.empty((tt_records[0].ni, tt_records[0].nj, len(tt_records)), dtype='f')

# Iterate over records
# - Using enumerate is the standard way to get an iteration index with your items
# - Using rec.data[:,:,0] is the standard way of turning a 3D array ni x nj x 1
#   into a 2D array ni x ni.
# - Any sorting order can be achieved by passing the proper key to sorted().
for i, rec in enumerate(sorted(tt_records, key=lambda r: r.npas)):
    print(f"Record:{rec} with data:")
    print(rec.data[:,:,0])
    cube[:,:,i] = rec.data[:,:,0]

print("3D numpy array containing all the slices:")
print(cube)
