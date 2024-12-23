import sys
import os
import ctypesrmn as rmn
import numpy as np

sys.path.append(f"{os.getcwd()}/build/python")


input_file = "/home/sici000/ci_data/rpn-tools/stdfile.rpn"
invalid_file = "/home/sici000/.profile"

def create_record():
    rec = rmn.fst_record(
        dateo=1, datev=2,
        data_type=5, data_bits=32, pack_bits=32,
        ni=8, nj=9, nk=1,
        ip1=1,ip2=2,ip3=3,
        ig1=1, ig2=2, ig3=3, ig4=4,
        nomvar="RPN", typvar="Y", grtyp="X"
    )
    rec.deet = 0
    rec.npas = 0
    rec.etiket = "unittest"
    return rec

def create_record_with_data():
    rec = create_record()
    rec.data = np.random.random(rec.ni * rec.nj * rec.nk).reshape((rec.ni, rec.nj, rec.nk), order='F').astype('f')
    return rec
def test_iterate_whole_file():
    with rmn.fst24_file(filename=input_file, options="R/O") as f:
        nb_rec = 0
        for rec in f:
            print(f"rec.ni={rec.ni}, rec.etiket='{rec.etiket}'")
            nb_rec += 1
    print(nb_rec)
def test_create_file_with_data():
    to_write = create_record_with_data()
    try:
        os.remove("new_file.std")
    except:
        pass
    filename = f"new_file.std"
    with rmn.fst24_file(filename=filename, options="R/W") as f:
        f.write(to_write, rewrite=True)

test_create_file_with_data()
