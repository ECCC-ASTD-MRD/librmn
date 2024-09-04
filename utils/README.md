
# A few utilities that provide librmn functionality in the form of executable programs.

### rsf_dump

Print a summary of the contents of an RSF file, without interpreting that content. Basically, it's a list of all records in the file, along with their size and some other metadata.

### sparse_concat

Concatenate a series of sparse files into a single one. This program takes into account the sparsity of the files, so if they have "holes" in them, these holes won't just be expanded and start taking up space. If there's only one file in the list, the program will just print the map of holes and data in the file.
