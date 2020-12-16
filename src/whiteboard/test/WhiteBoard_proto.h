int c_wb_get(
    //! [in] WhiteBoard in which to search
    WhiteBoard *wb,
    //! [in] Name of key (length MUST be supplied in nameLength)
    char *name,
    //! [in] Type represented by one character: R/I/L/C , key type real/inetger/logical/character
    char type,
    //! [in] Size in bytes of each element 4/8 for R/I/L, 1->WB_MAXSTRINGLENGTH for character strings
    int size,
    //! [out] Pointer to where data is written (everything considered as unsigned bytes)
    unsigned char *dest,
    //! [in] Number of elements that can be stored into value
    int nbelem,
    //! [in] Length of name
    int nameLength
);


int c_wb_put(
    //! [in,out] WhiteBoard in which to store.  If this is null, the WhiteBoard refrenced by DummyWhiteboardPtr is used.
    WhiteBoard *wb,
    //! [in] Name of key (length MUST be supplied in nameLength)
    char *name,
    //! [in] Type represented by one character: R/I/L/C , key type real/inetger/logical/character
    char type,
    //! [in] Size in bytes of each element 4/8 for R/I/L, 1->WB_MAXSTRINGLENGTH for character strings
    int size,
    //! [in] Pointer to data asssociated with key (everything considered as unsigned bytes)
    unsigned char *src,
    //! [in] Number of elements (0 means a scalar) (1 or more means an array)
    int nbelem,
    //! [in] Numeric representation of active flags
    int options,
    //! [in] Name Length
    int nameLength
);
