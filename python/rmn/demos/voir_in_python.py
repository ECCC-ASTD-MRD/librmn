import rmn
import argparse

p = argparse.ArgumentParser()
p.add_argument('-f', '--filename', required=True)
args = p.parse_args()

# If there is a natural way to iterate over something, it should be available
# iterating over an fst file would naturally be iterating over all records
for rec in rmn.fst24_file(filename=args.filename):
    print(rec)
