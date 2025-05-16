#include <malloc.h>
#include "rmn/List.h"


//! \file
//! Implementation of a generic doubly linked list

//! \defgroup genericDataStructures Generic Data Structures
//! @{
//! An implementation of generic data structures
//!
//! Provided data structures:
//! - Doubly linked lists
//! - Quad trees
//! - Vectors


//! Add a node to a doubly linked list
TList * TList_Add(
    //! [inout] List to which to add an item
    TList * const list,
    //! [inout] Item to add
    void * const item
) {
    //! \return List's head (new node)
    //! - These lists are doubly linked
    //! - Adding will be done on the head of the list, which allows to use as a queue or even stack as well

    TList * const node = (TList*)malloc(sizeof(TList));
    if (node) {
        node->Next = list;
        node->Prev = NULL;
        node->Data = item;

        if (list) list->Prev = node;
    }

    return node;
}


//! \copydoc TList_Add
TList * TList_Push(
    TList * const list,
    void * const item
) {
    return TList_Add(list, item);
}


//! \copydoc TList_Add
TList * TList_Queue(
    TList * const list,
    void * const item
) {
    return TList_Add(list, item);
}


//! Remove the node at the head of the list
TList * TList_Pop(
    //! [inout] List from which to remove the first item
    TList * const list
) {
    //! \return New list head

    TList * const node = list;

    if (node && node->Next) {
        node->Next->Prev = NULL;
    }
    return node->Next;
}


//! Remove last item from list
void * TList_Dequeue(
    //! [inout] List from which to remove the last item
    TList * const list
) {
    //! \return Last item

    TList * node = list;
    void * item = NULL;

    while (node) {
        if (!node->Next) {
            node->Prev->Next = NULL;
            item = node->Data;
            free(node);
            break;
        }
        node = node->Next;
    }
    return item;
}


//! Add a node to the list in an ordered way
TList * TList_AddSorted(
    //! [inout] List to which to add the item
    TList * const list,
    //! [in] Comparison function
    TList_CompareProc * const compare,
    //! [in] Item to add
    void * const item
) {
    //! \return List's head

    TList * head = list;
    TList * node = (TList*)calloc(1, sizeof(TList));
    if (node) {
        node->Data = item;
        TList * prev = NULL;

        TList * insert = list;
        while (insert) {
            if (compare(insert->Data, item) >= 0) {
                node->Next = insert;
                node->Prev = insert->Prev;
                if (insert->Prev) {
                    insert->Prev->Next = node;
                } else {
                    head = node;
                }
                insert->Prev = node;
                break;
            }
            prev = insert;
            insert = insert->Next;
        }

        if (!insert) {
            if (prev) {
                prev->Next = node;
                node->Prev = prev;
            } else {
                head = node;
            }
        }
    }

    return head;
}


//! Delete a node from the list
TList * TList_Del(
    //! [inout] List form which to remove an item
    TList * list,
    //! [in] Item to remove from the list
    const void * const item
) {

    TList * node = list;
    while (node) {
        if (node->Data == item) {
            if (node->Prev)
                node->Prev->Next = node->Next;
            if (node->Next)
                node->Next->Prev = node->Prev;
            if (node == list) {
                list = list->Next;
            }
            free(node);
            break;
        }
        node = node->Next;
    }
    return list;
}


//! Find the node corresponding to the provided item
TList * TList_Find(
    //! [in] List in which to search
    TList * const list,
    //! [in] Comparison function. Must return 1 when items match.
    TList_CompareProc * const compare,
    //! [in] Item to search for
    const void * const item
) {
    //! \return Item's node if found, NULL otherwise

    if (compare && list && item) {
        TList * node = list;
        while (node) {
            if (compare(node->Data, item)) {
                return node;
            }
            node = node->Next;
        }
    }
    return NULL;
}


//! Free a list
void TList_Clear(
    //! [inout] List to be freed
    TList * list,
    //! [in] Optional function to free each node's item. Provide NULL if not required.
    TList_FreeProc * const freeItem
) {
    while (list) {
        TList * tmp = list;
        list = list->Next;
        if (freeItem) freeItem(tmp->Data);
        free(tmp);
    }
}

//! @}
