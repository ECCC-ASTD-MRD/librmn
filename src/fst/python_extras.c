#include "rmn/fst24_record.h"

fst_record get_default_fst_record(){
    // This function is used when implementation code wants a default fst_record
    //
    // Because `fst_record` is a ctypes structure, we can do
    //
    // >>> x = get_default_fst_record()
    //
    // Python will create an <class 'fst_record'> object and copy what this
    // function returns into it (if I understand correctly), and the variable
    // x will be a reference to that Python object.
    return default_fst_record;
}


int fst_record_set_default_values(fst_record *rec){
    // This function is necessary for the __init__ of the fst_record class.
    //
    // In __init__, we have an already allocated python object (that was
    // created by the __new__()) function and we want to set its values which
    // is why this function exists even though we have `get_default_fst_record`
    // above.
    //
    //     class fst_record(ctypes.Structure):
    //         _fields_ = ( ... )
    //         def __init__(self):
    //             fst_record_set_default_values(ctypes.byref(self))
    //
    rec->do_not_touch.version  = (FST24_VERSION_OFFSET_C + FST24_VERSION_COUNT);
    rec->do_not_touch.deleted  = 0;
    rec->do_not_touch.handle   = -1;
    rec->do_not_touch.alloc    = 0;
    rec->do_not_touch.flags    = 0x0;
    rec->do_not_touch.fst_version = 0;
    rec->do_not_touch.num_search_keys = 0;
    rec->do_not_touch.extended_meta_size = 0;
    rec->do_not_touch.stored_data_size = 0;
    rec->do_not_touch.unpacked_data_size = 0;
    rec->do_not_touch.stringified_meta = NULL;

    rec->file     = NULL;
    rec->data     = NULL;
    rec->metadata = NULL;

    rec->file_index = -1;

    rec->dateo     = -1;
    rec->datev     = -1;

    rec->data_type = -1;
    rec->data_bits = -1;
    rec->pack_bits = -1;
    rec->ni = -1;
    rec->nj = -1;
    rec->nk = -1;
    rec->num_meta_bytes = 0;

    rec->deet = -1;
    rec->npas = -1;

    rec->ip1 = -1;
    rec->ip2 = -1;
    rec->ip3 = -1;

    rec->ig1 = -1;
    rec->ig2 = -1;
    rec->ig3 = -1;
    rec->ig4 = -1;

    rec->dummy = 0;

    /* TODO Ask find out why we are't just doing rec->typvar[0] = '\0' */
    // rec->typvar = {' ' , ' ' , '\0', '\0'};
    // rec->grtyp  = {' ' , '\0', '\0', '\0'};
    // rec->nomvar = {' ' , ' ' , ' ' , ' ',
    //            '\0', '\0', '\0', '\0'};
    // rec->etiket = {' ' , ' ' , ' ' , ' ',
    //            ' ' , ' ' , ' ' , ' ',
    //            ' ' , ' ' , ' ' , ' ',
    //            '\0', '\0', '\0', '\0'};
    return 0;
}
