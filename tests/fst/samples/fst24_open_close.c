
#include <rmn.h>

int main(void)
{
    remove("my_file.fst");


    fst_file* my_file = fst24_open("my_file.fst", "R/W");
  
    if (my_file == NULL) {
        // Deal with error
        return -1;
    }
  
    if (fst24_close(my_file) != TRUE) return -1;
}
