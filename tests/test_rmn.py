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
def print_py(s):
    print(f"\033[1;32m\nPYTHON: {s}\033[0m")

import numpy as np
import os
# os.environ['APP_VERBOSE'] = 'fatal'
import rmn
import sys

filename = "/fs/site5/eccc/cmd/w/spst900/spooki/spooki_dir/pluginsRelatedStuff/AbsoluteValue/testsFiles/AbsoluteValue_file2cmp.std"
if not os.path.isfile(filename):
    raise RuntimeError(f"Test file does not exist")

def iterate_on_whole_file():
    for rec in rmn.fst24_file(filename=filename):
        print(rec)

def open_file():
    print_py("Create fst24_file")
    rmn.fst24_file(filename=filename, options="")

def create_query_and_iterate_on_records():
    print_py("Iterate over records of fst24_file")
    q4 = rmn.fst24_file(filename=filename).new_query(ip3=0)
    print(q4)
    for record in q4:
        ip3 = record.ip3 # PyMemberDef
        nomvar = record.nomvar # PyGetSetDef
        etiket = record.etiket # PyGetSetDef
        print(f"PYTHON: Result from q3: {record}", file=sys.stderr) # .tp_str
        print(f"PYTHON: Record has ip3={ip3}, nomvar='{nomvar}', etiket='{etiket}'", file=sys.stderr); #.tp_getset
        print(record.data)

def open_invalid_file():
    print_py("Attempt to open existing non-fst file")
    try:
        f2 = rmn.fst24_file(filename="/home/phc001/.profile", options="");
    except rmn.FstFileError as e:
        print(f"Got exception as expected: {repr(e)}")

def open_empty_filename():
    print_py("Attempt to open empty string filename")
    try:
        f2 = rmn.fst24_file(filename="", options="");
    except rmn.FstFileError as e:
        print(f"Got exception as expected: {type(e)}:{e}")

def open_file_bad_arguments():
    print_py("Attempt to create file object using non-keyword argumetns")
    try:
        f2 = rmn.fst24_file(filename)
    except TypeError as e:
        print(f"Got exception as expected: {repr(e)}")

def open_file_no_options():
    print_py("Open valid file without passing options argument")
    f2 = rmn.fst24_file(filename=filename)


def create_record_and_assign_data():
    print_py("Creating a record with data we created in Python")
    # rec = rmn.fst_record(nomvar="<<", ip3=42)
    rec = rmn.fst_record(dateo=1, datev=2, data_type=3, data_bits=4, pack_bits=8,
                         ni=8, nj=9, nk=1, ip1=1,ip2=2,ip3=3, ig1=-1, ig2=-2, ig3=-3, ig4=-4, nomvar="phil", typvar="Y", grtyp="X", etiket="VincentMagnoux")
    print(rec)

    # No idea if the NI, NJ, NK are in the right order
    x = np.random.random(rec.nk * rec.nj * rec.ni).reshape((rec.nk, rec.nj, rec.ni))
    # rec.etiket = "VincentMagnoux"
    # rec.data = x
    print(rec.data)
    return rec


def invalid_access_of_record_data():
    print_py("Attempting to obtain the data of a record that has no file or data")
    # When reading a file with the for loop with the query as an iterator, it looks
    # really cool to do `rec.data` to access the data of the record with the
    # property mechanism that hides getters and setters, and that the data is only
    # read on demand and only once.  However, in the context where a user is
    # creating a record with the intention of writing it in a file, accessing
    # `rec.data` when there is no file and no data to be read is not nice.
    new_rec = rmn.fst_record(nomvar="TT", ip3=82)

    # It makes sense that the user should not try to access the data of a record
    # that has no data, so I think what we want is simply for this line to throw
    # the right exception or it should return None.
    # But right now we get valueError because we try to create a numpy array with
    # negative dimensions
    new_rec.ni = new_rec.nj = new_rec.nk = 1
    print(new_rec)
    print(f"Data of record: {new_rec.data}")

def open_non_existant_file_without_W_option():
    print_py("Attempt to open non-existant file without 'W' option")
    try:
        f3 = rmn.fst24_file(filename="noexist", options="R")
    except rmn.FstFileError as e:
        print(f"Got exception as expected: {repr(e)}")

    f3 = rmn.fst24_file(filename="my_fst_file.fst", options="R/W")

def create_file_with_data(rsf=True):
    backend_type = 'rsf' if rsf else 'xdf'
    print_py(f"Create new {backend_type} file with data")
    filename = f"new_{backend_type}_file.fst"
    if os.path.isfile(filename):
        os.remove(filename)


    rec = rmn.get_test_record()
    rec.ni = rec.nj = 8
    rec.nk = 1

    rec.data = np.random.random(rec.nk * rec.nj * rec.ni).reshape((rec.nk, rec.nj, rec.ni))

    with rmn.fst24_file(filename=filename, options="R/W+RSF" if rsf else "R/W+XDF") as f:
        f.write(rec, rewrite=False)

    print("Successfully wrote record to file")

def attempt_to_write_non_record_to_file():
    print_py("attempt to write non-record into an fst24_file")
    filename = "new_fst_file.fst"
    if os.path.isfile(filename):
        os.remove(filename)

    f = rmn.fst24_file(filename=filename, options="R/W")

    try:
        f.write("this is not a record")
    except TypeError as e:
        print(f"Got exception as expected: {repr(e)}")

def test_record_attributes():
    print_py("Iterate over records of fst24_file")
    q4 = rmn.fst24_file(filename=filename).new_query(ip3=0)
    print(q4)
    for record in q4:
        print(f"PYTHON: dateo={repr(record.dateo)}")
        print(f"PYTHON: datev={repr(record.datev)}")
        print(f"PYTHON: data_type={repr(record.data_type)}")
        print(f"PYTHON: data_bits={repr(record.data_bits)}")
        print(f"PYTHON: pack_bits={repr(record.pack_bits)}")
        print(f"PYTHON: ni={repr(record.ni)}")
        print(f"PYTHON: nj={repr(record.nj)}")
        print(f"PYTHON: nk={repr(record.nk)}")
        print(f"PYTHON: deet={repr(record.deet)}")
        print(f"PYTHON: npas={repr(record.npas)}")
        print(f"PYTHON: ip1={repr(record.ip1)}")
        print(f"PYTHON: ip2={repr(record.ip2)}")
        print(f"PYTHON: ip3={repr(record.ip3)}")
        print(f"PYTHON: ig1={repr(record.ig1)}")
        print(f"PYTHON: ig2={repr(record.ig2)}")
        print(f"PYTHON: ig3={repr(record.ig3)}")
        print(f"PYTHON: ig4={repr(record.ig4)}")
        print(f"PYTHON: typvar={repr(record.typvar)}")
        print(f"PYTHON: grtyp={repr(record.grtyp)}")
        print(f"PYTHON: nomvar={repr(record.nomvar)}")
        print(f"PYTHON: etiket={repr(record.etiket)}")
        break

def get_record_by_index():
    file = rmn.fst24_file(filename=filename)
    query = file.new_query()
    next(query)
    rec = next(query)
    print(rec)
    file = None
    query = None
    dat = rec.data
    print(dat)


if __name__ == "__main__":
    # create_query_and_iterate_on_records()
    # open_empty_filename()
    # open_non_existant_file_without_W_option()
    # create_file_with_data(rsf=False)

    # # The created file with rsf must be inspected with a sufficiently recent
    # # version of voir.  Also, there are error messages that are printed
    # # (ERROR) META|Meta_Init: Unable to initialize profiles, CMCCONST and META_PROFPATH variable not defined
    # # (ERROR) META|Dict_Load: Environment variable CMCCONST not defined, source the CMOI base domain
    # # but the file was created successfully so these are not actually errors.
    # create_file_with_data(rsf=True)

    # attempt_to_write_non_record_to_file()
    # invalid_access_of_record_data()
    # test_record_attributes()
    # create_record_and_assign_data()
    # get_record_by_index()
    iterate_on_whole_file()

