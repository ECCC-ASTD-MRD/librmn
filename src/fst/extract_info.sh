#!/usr/bin/env bash

function get_signature() {
   function_name=${1}
   file_name=${2}
   echo "$(
      sed -n '                                        # Get comment block before the function name
         /\/\/!.*$/, /^[^/[:space:]][^/]*'${function_name}'($/ p
         /^[^/[:space:]][^/]*'${function_name}'(/ q   # Stop looking after having found that function

         ' ${file_name} | tac | sed -e '/^$/,$d ' | tac
      )
$(    sed -n ' # Get function parameters, if they are on separate lines
         /^[^/[:space:]][^/]*'${function_name}'(/,/^\}$/ {
            /^[^/[:space:]][^/]*'${function_name}'(/ b       # Skip line with the function name (was already printed)
            p
         }

      ' ${file_name})
   " | \
      sed -e ' # Cut away everything that comes after the parameters
         s|^\([^/]*\)).*|\1);|
         /);/ q      # Stop looking after the first replacement
      '
}

# fnames="fst24_new_query"
function_fnames="
   fst24_is_open
   fst24_file_name
   fst24_is_valid
   fst24_open
   fst24_close
   fst24_flush
   fst24_get_num_records
   fst24_get_record_by_index
   fst24_print_summary
   fst24_write
   fst24_read
   fst24_new_query
   fst24_link
   fst24_unlink
   fst24_eof
"

query_fnames="
   fst24_find_next
   fst24_read_next
   fst24_find_all
   fst24_rewind_search
   fst24_query_is_valid
   fst24_query_free
"

echo -e "\n\nFILE\n\n"

for fname in ${function_fnames}; do
   get_signature ${fname} fst24_file.c
   echo
done

echo -e "\n\nQUERY\n\n"

for fname in ${query_fnames}; do
   get_signature ${fname} fst24_file.c
   echo
done

