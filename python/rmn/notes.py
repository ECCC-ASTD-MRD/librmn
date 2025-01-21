# All the numpy arrays for numeric things are views into the big chunk
# of memory that is managed by this struct.  That means that our Pandas
# data frame that we make with this data will need that memory to continue
# existing.

# We can't even add a method that the user should call when they are done
# with the memory because this object is only a temporary object that
# exists only until the end of get_index_columns.  The user never sees
# this object.

# The better way to do things would be to create numpy arrays in Python
# and give the C function a pointer to the memory that the array manages
# and have it put data in that memory:

# int deposit_data_at_address(void *data_dest, ...){ put data at dest }
deposit_data_at_address = lib.deposit_data_at_address
deposit_data_at_address.argtypes = (ctypes.c_void_p,...)
deposit_data_at_address.restype = ctypes.c_int32

# Which can be used as
array = numpy.empty(<shape>)  # Allocates memory which is managed by the array
deposit_data_at_address(array.ctypes.data)

# OR WE CAN SET THE ARG DIFFERENTLY
ctypes_deposit_data_at_address.argtypes = (numpy.ctypeslib.ndpointer(dtype='f'), )
# and pass the array itself
ctypes_deposit_data_at_address(array)

# Either way our function gets the address of the block of memory managed
# by the numpy array.

# Then we wouldn't have to worry about allocation.

################################################################################

# I thought we couldn't know the number of records in a file without going
# through the whole file but it turns out that we can get the number of
# records in a file in constant time.  Which also means that getting the
# total number of records in a list of files is probably decently quick:
for filename in files
    with open(filename) as f:
        nb_records += f.get_number_of_records()

# although this does open and close all the files.

# We modify the function so that instead of doing
# raw_data = NewRecordData(nb_records);
# it just gets a pointer to an already allocated RecordData object. 
librmn.rmn_get_index_columns_raw.argtypes = [ctypes.POINTER(ctypes.c_char_p), ctypes.c_size_t, ctypes.POINTER(RecordData)]
librmn.rmn_get_index_columns_raw.restype = ctypes.POINTER(RecordData)

# We then create a RecordData object in Python
raw_data = RecordData()

# and allocate the memory for all the pointer fields:
ni = np.empty((nb_records,), dtype=np.int32)
raw_data.ni = ni.ctypes.data

nomvar = np.empty((nb_records * FST_NOMVAR_LEN,) dtype=np.char)
raw_data.nomvar = nomvar.ctypes.data

# Now we call our funcn
librmn.rmn_get_index_columns_raw(filenames_array, len(filenames), ctypes.byref(raw_data))

# After this function call, our numpy arrays contain data.  The numeric
# ones can be put in the dictionary directly, the string ones get
# converted and the result put into the dictionnary.

columns = {}
columns['ni'] = ni
columns['nomvar'] = _decode_fixed_lenght_strings(nomvar, FST_NOMVAR_LEN, nb_records)

# And now since all the memory was allocated in Python, we have nothing to
# worry about.


# Something like this:
def new_get_index_columns(filenames):
    if isinstance(filenames, (str, os.PathLike)):
        filenames = [filenames]

    filenames = [str(filename) for filename in filenames]
    filenames_bytes = [filename.encode("utf-8") for filename in filenames]

    filenames_array_type = ctypes.c_char_p * len(filenames_bytes)
    filenames_array = filenames_array_type(*filenames_bytes)

    # Get total number of records nb_records
    # Write a C function that takes filenames_array and returns the number
    # This potentially a mildly expensive operation

    # Allocate 22 arrays
    ni = np.empty((nb_records,), dtype=np.uint32)
    nomvar = np.empty((nb_records * FST_NOMVAR_LEN,), dtype=np.char)
    # ...

    # Create a raw_data and set the fields to be the pointers to the memory
    # managed by our arrays.
    raw_data = RecordData()
    raw_data.ni = ni.ctypes.data
    raw_data.nomvar = nomvar.ctypes.data
    # ...

    # Call the indexing function (assuming we have changed it to take a pointer
    # to RecordData as an argument
    result = librmn.get_index_columns_raw(filenames_array, nb_files, ctypes.byref(raw_data))

    # Now data has been deposited inside the arrays

    columns = {}
    columns['ni'] = ni
    columns['nomvar'] = _decode_fixed_length_strings(nomvar, FST_NOMVAR_LEN, nb_records)
    # ...

    return columns
