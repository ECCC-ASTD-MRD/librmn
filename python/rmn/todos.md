# Tasks
## Data types
### Do the mapping between the record `data_type` field and the numpy array types
#### Setting data
- Ensure the basic checks at the start of ` py_fst_record_set_data()` are done
  - Incoming is of type numpy array

- Ensure that previous data is dealt with appropriately
  - If it belongs to a numpy array, it should disappear on its own
  - If `self._data` is not `NULL` but does not belong to a numpy array
    consult Vincent.

- Ensure the type of the numpy array is compatible with the `self.data_type`
  - Copy the values of `FST_TYPE_REAL, ...`
  - Check `data_bits`
    - If it matches good
    - If it doesn't, then do `array.astype()` to convert

#### Getting the data

Do what `py_fst_record_get_data()` does
- Set data type of array receiving data according to `self.data_type` and
  `self.data_bits`.

# Evaluation

- Show Vincent my initialization function in `python_extras.c`.  All the values
  from the macro `default_fst_record` are duplicated but in Python I can't use
  the macro and I can't write a helper function that uses the macro because I
  need to set the values on the attributes of a record that has already been
  created on Python's heap.
- Expose the `FST_TYPE_X` constants.  They are macros but maybe we could
  change that to `const int FST_TYPE_REAL = 5` in a C file and
  `extern const int FST_TYPE_REAL` in an H file.
  For starters I can just copy the values.
- Consider deferring conversion using astype to when it is needed
