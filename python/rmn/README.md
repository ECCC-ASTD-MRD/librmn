## Examples

See demos subdirectory for some files that expand on the APIs demonstrated
here.

### Opening and closing a file

```python
import rmn

f = rmn.fst24_file(filename="my_file")
```
File is closed when the object is destroyed which happens automatically when
there are no more references to the underlying object.

The file object implements the context manager functions so here the file would
be closed at the end of the `with` block.

```python
import rmn

with rmn.fst24_file(filename="my_file") as f:
    pass
```

The method `f.close()` can be called to close the file explictly but letting it
be closed automatically or using a context manager are preferred.

### Open for Writing or Creating a New File

This example creates an empty file named `my_file.std` in the current directory:
```python
import rmn

with rmn.fst24_file(filename="my_file.std", options="R/W") as f:
    pass
```

See later examples for creating records and writing them to files.

### Finding and Reading a Record

```python
import rmn

f = rmn.fst24_file(filename="my_file.std")
q = f.new_query(ip1=1001, ip2=1002, ip3=1003, nomvar='TT')

for record in q:
    print(record)
    print(record.data) # record.data is a numpy array of shape (record.ni, record.nj, record.nk)
```

Python concepts:
- `q` is of type `rmn.fst_query` which is a python [iterator](https://docs.python.org/3/tutorial/classes.html#iterators)
  which means we can do with everything that can be done with any
  other python iterator.
  - Iterate over it with a `for` loop as above,
  - Getting a single record can be done with `next(q)`,
  - Consuming the whole iterator into a list with `list(q)`,
  - Use `zip` with another iterable (possibly another query).
- The `data` attribute of `fst_record` is not an attribute but a Python property
  which means that it has an underlying getter and setter.  Because of this,
  reading the data from the file can be delayed until the first access and the
  subsequent accesses return the data that was read at the first access without
  doing a second read.
C/Fortran notes:
- No need to free the query, python's automatic memory management tools are used
  to make sure the underlying C function `fst24_query_free()` is called.

### Find several records at once

```python
many_records = list(rmn.fst24_file.new_query(ip1=8, nomvar='TT'))
```

### Working with several queries

```python
import rmn

f = rmn.fst24_file(filename="my-file.std")

q_a = f.new_query(etiket='LABEL_A')
q_b = f.new_query(etiket='LABEL_B')

for rec_a in q_a:
    print(rec_a)
    for _, rec_b in zip(range(3), q_b):
        print(rec_b)

# Want to do stuff with query A again?
q_a.rewind()
for rec in q_a:
    print(rec_a)
```

### Nested Queries

In this example, we get find the grid descriptors (tic-tic and tac-tac) for
every record in the file that has grid descriptors and is not itself a grid descriptor.
```python
import rmn

f = rmn.fst24_file(filename="my-file.std")

for rec in f:
    if rec.nomvar in ['>>', '^^']:
        continue

    if rec.grtyp in ['A', '...']:
        print(f"Record {rec} has doesn't have '>>','^^'")
        continue

    q_tic_tic = f.new_query(nomvar='>>', ip1=rec.ig1, ip2=rec.ig2, ip3=rec.ig3)
    tic_tic = next(q_tic_tic)  # Same as Fortran `call q_desc%find_next(tic_tic)`

    # Same as above but in one line
    tac_tac = next(f.new_query(nomvar='^^', ip1=rec.ig1, ip2=rec.ig2, ip3=rec.ig3))

    print("Record {rec}")
    print(f"    >>: {tic_tic})
    print(f"    >>: {tac_tac})
```

### Reading 2D slices into a pre-allocated 3D array

```python
import numpy as np
import rmn

f = rmn.fst24_file(filename='x.rmn')
tt_records = list(f.new_query(nomvar='TT'))

cube = np.empty((tt_records[0].ni, tt_records[0].nj, len(tt_records)), dtype='f')

for i, rec in enumerate(sorted(tt_records, key=lambda r: r.npas)):
    cube[:,:,i] = rec.data[:,:,0]

print(cube)
```
- Records can be ordered in the cube any way we want by changing the key
  argument to the sorted function.
- We consume the iterable query into a list to know the number of records
  to create a properly sized array.
- We assume for brevity that all the records have the same `ni`,`nj` as the
  first one.
- All records have data of shape `(ni,nj,nk)` but `nk` is usually 1.

### Creating a record and write it to a file

```python
import rmn
import numpy as np

# Attributes of record can all either be given to the constructor or assigned
# with normal attribute assingment
rec = rmn.fst_record(
        ni=100, nj=200, nk=1,
        data_type=5, data_bits=32, pack_bits=32,
        deet=0, npas=0,
        ip1=0, ip2=0, ip3=0,
        ig1=0, ig2=0, ig3=0, ig4=0
    )
rec.nomvar = 'TT'
rec.etiket = 'HYBRID'

rec.data = np.random.random(rec_2.ni * rec_2.nj * rec_2.nk).reshape((rec_2.ni, rec_2.nj, rec_2.nk)).astype('f')

# Write to file, options needs to be supplied and have "R/W" when because we
# are writing.
with rmn.fst24_file(filename="my-file.std", options="R/W") as f:
    f.write(rec_1, rewrite=True)
    # File is closed when exiting this block.
```
