#ifndef _QTree_h
#define _QTree_h

#include "Vector.h"

//! \addtogroup genericDataStructures
//! @{

#define QTREE_INFINITE 0xFFFFFFFF
#define QTREE_SIZEINCR 5

typedef struct TPoint2D {
    double X;
    double Y;
} TPoint2D;


//! Quad tree data
typedef struct TQTreeData {
    char * Ptr; //!< Data pointer
    TPoint2D Pos; //!< XY data position
} TQTreeData;


//! Quad tree
typedef struct TQTree {
    struct TQTree * Parent; //!< Pointer to parent cell
    struct TQTree * Childs[4]; //!< Array of the sub quad pointers, handy for iterations
    TQTreeData * Data; //!< Data payload

    TPoint2D BBox[2]; //!< South West XY position limit of the quad bounding box
    int NbData; //!< Number of data in the payload
    int Size; //!< Current size of data payload
} TQTree;


//! Quad tree iterator
typedef struct TQTreeIterator {
    struct TQTree * Node; //!< Next iteration restart node
    unsigned long long Path; //!< Current node path (3 last bits are parsed childs left shifted as we go down)
} TQTreeIterator;

typedef void (QTree_ParseProc) (void * const Data);

TQTree * QTree_New(double X0,double Y0,double X1,double Y1,TQTree *Parent);
void     QTree_Free(TQTree *Parent);
TQTree * QTree_Add(TQTree* restrict Node,double X,double Y,unsigned int MaxDepth,void* restrict Data);
int      QTree_AddData(TQTree* const restrict Node,double X,double Y,void *Data);
void     QTree_Del(TQTree* restrict Node);
void     QTree_DelData(TQTree* const Node);
TQTree * QTree_Find(TQTree * const restrict Node, const double X, const double Y);
void     QTree_Parse(TQTree* restrict Node,QTree_ParseProc *Proc,unsigned Depth);
void     QTree_Neighbors(TQTree * const Node, TQTree ** const Neighbors, const int NbNeighbors);
TQTree * QTree_Iterate(TQTree * const restrict Node, TQTreeIterator * const Iter);
TQTree * QTree_IterateFilled(TQTree * const restrict Node, TQTreeIterator * const Iter);
TQTreeIterator* QTree_IteratorNew(void);

// Helper function for M grids (Triangle meshes)
TQTree * QTree_AddTriangle(
    TQTree * const restrict Node,
    const Vect2d corners[3],
    const unsigned int MaxDepth,
    void * const restrict Data
);

//! Get one data item stored in a quad tree node
static inline char * QTree_GetData(
    //! [in] Quad tree
    TQTree * const restrict Node,
    //! [in] Data item index
    const int Index
) {
    if (Node && Node->Data && Index < Node->NbData && Index >= 0) {
        return Node->Data[Index].Ptr;
    }
    return NULL;
}


//! @}

#endif
