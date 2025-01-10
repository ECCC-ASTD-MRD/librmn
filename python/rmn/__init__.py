"""
Python bindings for the fst24 API of librmn

Print all records of a file
>>> import rmn
>>> with rmn.fst24_file(<filename>) as f:
>>>     for rec in q:
>>>         print(rec)

Print all records matching criteria
>>> import rmn
>>> with rmn.fst24_file(<filename>) as f:
>>>     for rec in f.new_query(**criteria):
>>>         print(rec)

Print data of records:
>>> import rmn
>>> with rmn.fst24_file(<filename>) as f:
>>>     for rec in f.new_query(**criteria):
>>>         print(rec.data)

Create a file with a single record with random data:
>>> rec = rmn.fst_record(
>>>     dateo=1, datev=2,
>>>     data_type=5, data_bits=32, pack_bits=32,
>>>     ni=8, nj=9, nk=1,
>>>     ip1=1,ip2=2,ip3=3,
>>>     ig1=1, ig2=2, ig3=3, ig4=4,
>>>     nomvar="RPN", typvar="Y", grtyp="X"
>>> )
>>> rec.deet = 0
>>> rec.npas = 0
>>> rec.etiket = "unittest"
>>> rec.data = np.random.random(rec.ni * rec.nj * rec.nk).reshape((rec.ni, rec.nj, rec.nk), order='F').astype('f')
>>> with rmn.fst24_file(<filename>, options="R/W") as f:
>>>     f.write(rec, rewrite=True)
"""
from .fst24file import fst24_file, FstFileError
from .fstrecord import fst_record, FstDataType
