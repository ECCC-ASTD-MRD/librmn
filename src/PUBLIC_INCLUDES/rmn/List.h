#ifndef _List_h
#define _List_h

//! \file
//! Interface of doubly linked lists
//! \addtogroup genericDataStructures
//! @{


//! Doubly link list node
typedef struct TList {
    //! Node's data
    void * Data;
    //! Next node
    struct TList * Next;
    //! Previous node
    struct TList * Prev;
} TList;

typedef int (TList_CompareProc)(const void * const item1, const void * const item2);
typedef void (TList_FreeProc)(void * item);

TList * TList_Add(TList * const list, void * const item);
TList * TList_AddSorted(TList * const list, TList_CompareProc * const compare, void * const item);
TList * TList_Del(TList * list, const void * const item);
TList * TList_Find(const TList * const list, TList_CompareProc * const compare, const void * const item);
void TList_Clear(TList * list, TList_FreeProc * const freeItem);

//! @}

#endif
