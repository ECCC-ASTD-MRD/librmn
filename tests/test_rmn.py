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
import os
import ctypesrmn as rmn
import sys
import tempfile
import unittest

keep_tmpdir = False

# Don't print error messages from librmn since many of the tests cause errors
# on purpose to check that the python module raises exceptions.
os.environ['APP_VERBOSE'] = "SYSTEM"
print(f'Setting APP_VERBOSE to SYSTEM')

class TestRMNPackage(unittest.TestCase):
    def setUp(self):
        self.input_file = "/home/sici000/ci_data/rpn-tools/stdfile.rpn"
        self.invalid_file = "/home/sici000/.profile"

        if keep_tmpdir:
            self.tmpdir = tempfile.mkdtemp()
        else:
            self.tmpdir_obj = tempfile.TemporaryDirectory()
            self.tmpdir = self.tmpdir_obj.name

    def tearDown(self):
        if keep_tmpdir:
            print(f"Temporary files for {self._testMethodName} kept in {self.tmpdir}")
        else:
            self.tmpdir_obj.cleanup()

    def create_record(self):
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

    def create_record_with_data(self):
        rec = self.create_record()
        rec.data = np.random.random(rec.ni * rec.nj * rec.nk).reshape((rec.ni, rec.nj, rec.nk), order='F').astype('f')
        return rec

    def test_iterate_whole_file(self):
        with rmn.fst24_file(filename=self.input_file, options="R/O") as f:
            nb_rec = 0
            for rec in f:
                nb_rec += 1
        self.assertEqual(nb_rec, 161)

    def test_record_attribute_access(self):
        with rmn.fst24_file(filename=self.input_file, options="R/O") as f:
            rec = next(iter(f))
            self.assertEqual(rec.nomvar, '>>')
            self.assertEqual(rec.etiket, 'GEM_NEMO')
            self.assertEqual(rec.ni, 257)
            self.assertEqual(rec.nj, 1)
            self.assertEqual(rec.nk, 1)
            self.assertEqual(rec.dateo, 287576000)
            self.assertEqual(rec.ip1, 77343)
            self.assertEqual(rec.ip2, 96583)
            self.assertEqual(rec.ip3, 0)
            self.assertEqual(rec.ig1, 900)
            self.assertEqual(rec.ig2, 0)
            self.assertEqual(rec.ig3, 43200)
            self.assertEqual(rec.ig4, 43200)
            self.assertEqual(rec.deet, 0)
            self.assertEqual(rec.npas, 0)
            self.assertEqual(rec.data_type, 5)
            self.assertEqual(rec.grtyp, 'E')
            self.assertEqual(rec.typvar, 'X')
            self.assertEqual(rec.data_bits, 32)
            self.assertEqual(rec.pack_bits, 32)

    def test_iterate_on_query(self):
        from collections import defaultdict
        with rmn.fst24_file(filename=self.input_file, options="R/O") as f:
            q = f.new_query(nomvar='AL')
            self.assertEqual(len(list(q)), 15)

    def test_create_file(self):
        filename = f'{self.tmpdir}/new_file.std'
        with rmn.fst24_file(filename=filename, options='R/W') as f:
            pass
        assert(os.path.isfile(filename))

    def test_invalid_opens(self):

        def open_invalid_file():
            f = rmn.fst24_file(filename=self.invalid_file, options="");
        def open_empty_file():
            f = rmn.fst24_file(filename="", options="");
        def open_file_bad_arguments():
            f = rmn.fst24_file(self.input_file, options="");
        def open_noexist_without_RW():
            f = rmn.fst24_file(filename=f'{self.tmpdir}/noexist.std');

        self.assertRaises(rmn.FstFileError, open_invalid_file)
        self.assertRaises(rmn.FstFileError, open_empty_file)
        # self.assertRaises(TypeError, open_file_bad_arguments)
        self.assertRaises(rmn.FstFileError, open_noexist_without_RW)

    def test_create_record(self):
        rec = self.create_record()
        self.assertEqual(rec.ni, 8)

    def test_access_record_data(self):
        with rmn.fst24_file(filename=self.input_file, options="R/O") as f:
            q = f.new_query(nomvar='AL')
            rec = next(q)
            data = rec.data
            self.assertAlmostEqual(data[0,0,0], 0.7999902)
            self.assertAlmostEqual(data[-1,-1,0], 0.1699853)
            self.assertAlmostEqual(data[235,109,0], 0.1199822)

    def test_create_record_with_data(self):
        rec = self.create_record_with_data()
        self.assertIsInstance(rec.data, np.ndarray)
        self.assertEqual(rec.data.shape, (rec.ni, rec.nj, rec.nk))

    def test_access_record_data_of_record_without_data(self):
        rec = self.create_record()
        self.assertIs(rec.data, None)

    def test_create_file_with_data(self):
        to_write = self.create_record_with_data()
        filename = f"{self.tmpdir}/new_file.std"
        with rmn.fst24_file(filename=filename, options="R/W") as f:
            f.write(to_write, rewrite=True)

        with rmn.fst24_file(filename=filename, options="R/O") as f:
            read_from_file = next(iter(f))
            self.assertTrue(np.array_equal(to_write.data, read_from_file.data))

    def test_write_to_closed_file(self):
        with rmn.fst24_file(filename=f'{self.tmpdir}/write_to_closed.std', options='R/W') as f:
            g = f
        def write_to_closed_file():
            rec = self.create_record_with_data()
            g.write(rec, rewrite=True)
        self.assertRaises(rmn.FstFileError, write_to_closed_file)

#     def test_record_data_types(self):
#         rec = self.create_record()
#         data = np.random.random(rec.ni * rec.nj * rec.nk).reshape((rec.ni, rec.nj, rec.nk)).astype('f')
#         rec.data_type = 1
#         rec.data = data
#
#         rec.data_type = 5
#         rec.data = data
#
#         rec.data_type = 6
#         rec.data = data
#
#     def test_assign_invalid_data(self):
#         rec = self.create_record()
#         rec.data_type = 5
#         double_array = np.random.random(rec.ni * rec.nj * rec.nk).reshape((rec.ni, rec.nj, rec.nk))
#         single_array = double_array.astype('f')
#         def assign_double_array_to_record_of_single():
#             rec.data_bits = 32
#             rec.data = double_array
#         def assign_single_array_to_record_of_double():
#             rec.data_bits = 64
#             rec.data = single_array
#         def assign_non_array_to_record_data():
#             rec.data = [1,2,3,4]
#
#         self.assertRaises(TypeError, assign_double_array_to_record_of_single)
#         self.assertRaises(TypeError, assign_single_array_to_record_of_double)
#         self.assertRaises(TypeError, assign_non_array_to_record_data)
#
    def test_fst_record_to_dict(self):
        with rmn.fst24_file(filename=self.input_file) as f:
            it = f.__iter__()
            rec = next(it)
            self.assertEqual(rec.to_dict(), {
                "nomvar": ">>",
                "typvar": "X",
                "grtyp": "E",
                "etiket": "GEM_NEMO",
                "dateo": 287576000,
                "datev": 287576000,
                "deet": 0,
                "npas": 0,
                "ni": 257,
                "nj": 1,
                "nk": 1,
                "ip1": 77343,
                "ip2": 96583,
                "ip3": 0,
                "ig1": 900,
                "ig2": 0,
                "ig3": 43200,
                "ig4": 43200,
                "data_type": 5,
                "data_bits": 32,
                "file_index": 0
            })
            rec = next(it)
            self.assertEqual(rec.to_dict(), {
                "nomvar": "^^",
                "typvar": "X",
                "grtyp": "E",
                "etiket": "GEM_NEMO",
                "dateo": 287576000,
                "datev": 287576000,
                "deet": 0,
                "npas": 0,
                "ni": 1,
                "nj": 128,
                "nk": 1,
                "ip1": 77343,
                "ip2": 96583,
                "ip3": 0,
                "ig1": 900,
                "ig2": 0,
                "ig3": 43200,
                "ig4": 43200,
                "data_type": 5,
                "data_bits": 32,
                "file_index": 1
            })


if __name__ == "__main__":
    unittest.main()
