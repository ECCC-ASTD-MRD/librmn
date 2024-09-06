# To discuss
## Named arguments to open file

The way I made it, the constructor of `fst24_file` accepts only keyword args.

I found it annoying sometimes because the first argument is obviously going to
be the filename.

```python
f = rmn.fst24_file("my_file.std")  # ERROR: This function takes only kwargs
f = rmn.fst24_file(filename="my_file.std") # OK
```

>>> Should I allow it to be a positional argument

## To get all records of a file

```python
f = rmn.fst24_file(filename="my_file.std)
q = f.new_query()
for rec in q:
    pass
```

The `fst24_file()` object could be given a simple `__iter__` method that would
do `return self.new_query()` allowing
```python
for rec in rmn.fst24_file(filename="my_file.std):
    pass
```
the file would be closed after the for loop because at the end of the for loop,
there will be no references to the unnamed object.

If there is a natural way to iterate over something it should be available.  Kind
of like how
```
for l in open("text_file.txt"):
    print(l)
```
iterates over the lines of the file.

>>> Do we want to be able to do this?  It's like 5 lines of code.

## Names of Types

Should the names be UpperCamelCase or do we stick with the exact C and Fortran
names?

```python
f = rmn.fst24_file(...) # Unclear if fst24_file() is a function or the
                        # constructor of a class
f = rmn.Fst24File(...)  # Gives an indication that fst24_file is a class
```
I'm OK with both

## Rewind

I feel like rewind is not super useful in Python.  If I want to go back, I can just do
```python
import rmn
f = fst24_file(filename="my_file.std")
records = list(fst24_file.new_query())
```
and I have my list of records that I can traverse any way I want.  I added the
rewind method to the query object because it took 5 minutes but I wonder what
the use case is.


# Todos

## Interface todos

## Non-interface

- The `__str__()` function is missing some commas and should be checked
- The shape given to the numpy array could be reversed
- The row/colum-majorness needs to be verified (ie do we pass `NPY_ARRAY_F_CONTIGUOUS`
  in the flags when creating the arrays
  (https://numpy.org/doc/stable/reference/c-api/array.html#creating-arrays)

## Interface-ish todo's

- Handle other data types than floats
  - Reading:
    - An if or switch-case to decide what the value of the argument `typenum`
      should be in the C function that creates the numpy array based on the
      value of `record.data_type`.
    - What happens if I get a record from a query and change its `data_type`
      attribute before I access `record.data`?  It's an obvious bad idea so
      not a big problem but if there is a clean way to prevent it, then we should
      do it.
  - Writing:  Assigning a numpy array of data to a python `rmn.fst_record`
    - Check that the array's `.dtype` has an acceptable value
    - Ensure that it fits with the record's `data_type` and `data_bits`
      attributes
  - What about the other types?  I know we can have strings and ints but I get
    the impression that nobody does those.  Maybe ints are possible but I think
    strings are an unused feature.  Creating a numpy array of strings can be
    done in one efficient way with one big block of memory if the max-length of
    the strings is known or simply (and less efficiently) with a numpy array of
    python objects each of which is a string.

