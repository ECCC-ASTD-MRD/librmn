import rmn
import numpy as np
import argparse

p = argparse.ArgumentParser()
p.add_argument('-f', '--filename', required=False, default="my-fst-file.std")
args = p.parse_args()

# Create the record and assign its attributes one by one
rec_1 = rmn.fst_record()

rec_1.nomvar = 'UU'
rec_1.etiket = 'ASSIGN'

rec_1.ni = 100
rec_1.nj = 200
rec_1.nk = 1
rec_1.data_type = rmn.FstDataType.FST_TYPE_REAL    # The type constants haven't been exposed to Python
rec_1.data_bits = 32
rec_1.pack_bits = 32

rec_1.deet = 0
rec_1.npas = 0
rec_1.ip1 = 0
rec_1.ip2 = 0
rec_1.ip3 = 0
rec_1.ig1 = 0
rec_1.ig2 = 0
rec_1.ig3 = 0
rec_1.ig4 = 0

# Create a record with attributes given as keyword arguments to the constructor
rec_2 = rmn.fst_record(
        nomvar='VV', etiket='CONSTR',
        ni=100, nj=200, nk=1,
        data_type=5|128, data_bits=32, pack_bits=32,
        deet=0, npas=0,
        ip1=0, ip2=0, ip3=0,
        ig1=0, ig2=0, ig3=0, ig4=0
    )

# Both ways just set attributes.  A hybrid method can be used:
rec_3 = rmn.fst_record(
        ni=100, nj=200, nk=1,
        data_type=5, data_bits=32, pack_bits=32,
        deet=0, npas=0,
        ip1=0, ip2=0, ip3=0,
        ig1=0, ig2=0, ig3=0, ig4=0
    )
rec_3.nomvar = 'TT'
rec_3.etiket = 'HYBRID'

# All sorts of python techniques can be used to reuse values.  For example,
common = {'ni': 100, 'nj': 200, 'nk': 1, 'data_type': 5, 'data_bits': 32, 'pack_bits': 32}
ips = {'ip1': 0, 'ip2': 0, 'ip3': 0}
rec_4 = rmn.fst_record(nomvar='HU', etiket='REUSE', **common, **ips, deet=1, npas=8, ig1=0, ig2=0, ig3=0, ig4=0)
rec_5 = rmn.fst_record(nomvar='HY', etiket='REUSE', **common, **ips, deet=1, npas=9, ig1=0, ig2=0, ig3=0, ig4=0)

# Create an array of random numbers between 0 and 1 of type float32
# to be compatible with rec.data_type.  Not important for the demo but I haven't
# found a way to directly create an array of random floats, only a 1D array of
# random doubles (float64) that is reshaped and converted to floats (float32).
rec_1.data = np.random.random(rec_1.ni * rec_1.nj * rec_1.nk).reshape((rec_1.ni, rec_1.nj, rec_1.nk)).astype('f')
rec_2.data = np.random.random(rec_2.ni * rec_2.nj * rec_2.nk).reshape((rec_2.ni, rec_2.nj, rec_2.nk)).astype('f')
rec_3.data = np.random.random(rec_3.ni * rec_3.nj * rec_3.nk).reshape((rec_3.ni, rec_3.nj, rec_3.nk)).astype('f')
rec_4.data = np.random.random(rec_4.ni * rec_4.nj * rec_4.nk).reshape((rec_4.ni, rec_4.nj, rec_4.nk)).astype('f')
rec_5.data = np.random.random(rec_5.ni * rec_5.nj * rec_5.nk).reshape((rec_5.ni, rec_5.nj, rec_5.nk)).astype('f')

with rmn.fst24_file(filename=args.filename, options="R/W") as f:
    f.write(rec_1, rewrite=True)
    f.write(rec_2, rewrite=True)
    f.write(rec_3, rewrite=True)
    f.write(rec_4, rewrite=True)
    f.write(rec_5, rewrite=True)
