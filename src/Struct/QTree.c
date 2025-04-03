#include <malloc.h>
#include "QTree.h"

//! \file
//! Implementation of a quad tree
//! \addtogroup genericDataStructures
//! @{
//!
//! Quad tree numbering is:
//! |     |     |
//! | :-: | :-: |
//! |  0  |  1  |
//! |  2  |  3  |


// Vertice codes for Cohen–Sutherland bbox intersection algorithm check
#define CS_INSIDE 0x0;             // 0000
#define CS_LEFT   0x1;             // 0001
#define CS_RIGHT  0x2;             // 0010
#define CS_BOTTOM 0x4;             // 0100
#define CS_TOP    0x8;             // 1000
//! Predicate to check intersection
#define CS_Intersect(A, B) (!(A & B))


//! Check if a position is inside the bounding box of a TQTree object
static inline int QTree_Inside(
    //! [in] Quad tree
    const TQTree * const Node,
    //! [in] X position
    const double X,
    //! [in] Y position
    const double Y
) {
    //! \return 1 if inside, 0 otherwise
    return X >= Node->BBox[0].X && X < Node->BBox[1].X && Y >= Node->BBox[0].Y && Y < Node->BBox[1].Y;
}


//! Find the node of a tree which contains a given position in it's bounding box
static inline TQTree * QTree_FindChild(
    //! [in] Quad tree
    const TQTree * const restrict Node,
    //! [in] X position
    const double X,
    //! [in] Y position
    const double Y
) {
    //! \return Node containing the given position or NULL if not in the bounding box

    int cidx = 0;
    for (; cidx < 4; cidx++) {
        if (QTree_Inside(Node->Childs[cidx++], X, Y)) break;
    }

    return cidx < 4 ? Node->Childs[cidx - 1] : NULL;
}


//! Create and initialize all sub-quads objects of a quad
static inline int QTree_Split(
    //! [inout] Quad tree
    TQTree * const restrict Node,
    //! [in] Center
    const TPoint2D * const restrict Center
) {
    //! \return Non zero on success, 0 otherwise

    Node->Childs[0] = QTree_New(Node->BBox[0].X, Node->BBox[0].Y, Center->X, Center->Y, Node);
    Node->Childs[1] = QTree_New(Center->X, Node->BBox[0].Y, Node->BBox[1].X, Center->Y, Node);
    Node->Childs[2] = QTree_New(Node->BBox[0].X, Center->Y, Center->X, Node->BBox[1].Y, Node);
    Node->Childs[3] = QTree_New(Center->X, Center->Y, Node->BBox[1].X, Node->BBox[1].Y, Node);

    return Node->Childs[0] && Node->Childs[1] && Node->Childs[2] && Node->Childs[3];
}


//! Add data to a node
int QTree_AddData(
    //! [inout] Quad tree
    TQTree * const restrict Node,
    //! [in] X data position
    const double X,
    //! [in] Y data position
    const double Y,
    //! [in] Data to link to the node
    void * const Data
) {
    //! \return 1 on success, 0 otherwise

    if (Data) {
        Node->NbData++;
        if (Node->NbData>Node->Size) {
            Node->Size += QTREE_SIZEINCR;
            if (!(Node->Data = (TQTreeData*)realloc(Node->Data, Node->Size * sizeof(TQTreeData)))) {
                return 0;
            }
        }
        Node->Data[Node->NbData - 1].Ptr = Data;
        Node->Data[Node->NbData - 1].Pos.X = X;
        Node->Data[Node->NbData - 1].Pos.Y = Y;
    }

    return 1;
}


//! Delete the data information of a quad tree node
void QTree_DelData(
    //! [inout] Quad tree
    TQTree * const Node
) {
    Node->NbData = 0;
    Node->Size = 0;

    if (Node->Data) {
        free(Node->Data);
        Node->Data = NULL;
    }
}


//! Create and initialize a new quad tree
TQTree * QTree_New(
    //! [in] South West X bounding box limit for the quad
    const double X0,
    //! [in] South West Y bounding box limit for the quad
    const double Y0,
    //! [in] North East X bounding box limit for the quad
    const double X1,
    //! [in] North East Y bounding box limit for the quad
    const double Y1,
    //! [in] Parent of the new node
    TQTree * const Parent
) {
    //! \return New Quad tree node

    TQTree * const node = (TQTree*)malloc(sizeof(TQTree));

    if (node) {
        node->Size = 0;
        node->NbData = 0;
        node->Data = NULL;

        node->Parent = Parent;
        node->Childs[0] = NULL;
        node->Childs[1] = NULL;
        node->Childs[2] = NULL;
        node->Childs[3] = NULL;

        node->BBox[0].X = X0;
        node->BBox[0].Y = Y0;
        node->BBox[1].X = X1;
        node->BBox[1].Y = Y1;
    }

    return node;
}


//! Recursively free a quad tree
void QTree_Free(
    //! [inout] Quad tree
    TQTree * const Node
) {
    if (Node->Childs[0]) QTree_Free(Node->Childs[0]);
    if (Node->Childs[1]) QTree_Free(Node->Childs[1]);
    if (Node->Childs[2]) QTree_Free(Node->Childs[2]);
    if (Node->Childs[3]) QTree_Free(Node->Childs[3]);

    QTree_DelData(Node);
    free(Node);
}


//! Add some data to a quad tree (usually a head quad at level=1...)
TQTree * QTree_Add(
    //! [inout] Quad tree
    TQTree * const restrict Node,
    //! [in] X data position
    const double X,
    //! [in] Y data position
    const double Y,
    //! [in] Maximum tree depth
    const unsigned int MaxDepth,
    //! [in] Data to link to the node
    void * const restrict Data
) {
    //! \return Node to which the data was added

    int ok = 1;
    TQTree * new = Node;

    if (!(MaxDepth - 1)) {
        // We've hit maximum depth, add to this node
        ok = QTree_AddData(Node, X, Y, Data);
    } else {
        if (Node->Childs[0]) {
            // This is a not a leaf, find sub node recursively
            new = QTree_FindChild(Node, X, Y);
            new = QTree_Add(new, X, Y, MaxDepth - 1, Data);
        } else {
            if (!Node->NbData) {
                // Empty leaf, use it
                ok = QTree_AddData(Node, X, Y, Data);
            } else {
                // Check if it's same coordinate to avoid infinite recursion
                if (Node->Data[0].Pos.X == X && Node->Data[0].Pos.Y == Y) {
                    ok = QTree_AddData(Node, X, Y, Data);
                } else {
                    TPoint2D center;
                    // Leaf node but there is data already stored here, split.
                    center.X = Node->BBox[0].X+(Node->BBox[1].X-Node->BBox[0].X)*0.5;
                    center.Y = Node->BBox[0].Y+(Node->BBox[1].Y-Node->BBox[0].Y)*0.5;

                    if (!QTree_Split(Node, &center)) {
                        return NULL;
                    }

                    // Reassign previous node data into splitted node
                    new = QTree_FindChild(Node, Node->Data[0].Pos.X, Node->Data[0].Pos.Y);
                    new->Data = Node->Data;
                    new->NbData = Node->NbData;
                    Node->Data = NULL;
                    Node->NbData  = 0;
                    Node->Size = 0;

                    // Assign new data
                    new = QTree_FindChild(Node, X, Y);
                    new = QTree_Add(new, X, Y, MaxDepth - 1, Data);
                }
            }
        }
    }

    return ok ? new : NULL;
}


//! Cohen–Sutherland algorithm to compute the bit code for a point (X, Y) using the clip rectangle bounded diagonally by (X0, Y0), and (X1, Y1)
int CS_Code(
    //! [in] X position
    const double X,
    //! [in] Y position
    const double Y,
    //! [in] South West X bounding box limit for the quad
    const double X0,
    //! [in] South West Y bounding box limit for the quad
    const double Y0,
    //! [in] North East X bounding box limit for the quad
    const double X1,
    //! [in] North East Y bounding box limit for the quad
    const double Y1
) {
    //! \return Cohen-Sutherland bit code

    int code = CS_INSIDE;                   // Initialised as being inside of clip window

    if      (X < X0) { code |= CS_LEFT; }     // Left of clip window
    else if (X > X1) { code |= CS_RIGHT; }    // Right of clip window

    if      (Y < Y0) { code |= CS_BOTTOM; }   // Below the clip window
    else if (Y > Y1) { code |= CS_TOP; }      // Above the clip window

    return code;
}


//! Add some triangle data to a quad tree (usually a head quad at level=1...)
TQTree * QTree_AddTriangle(
    //! [inout] Quad tree
    TQTree * const restrict Node,
    //! [in] Triangle's corners (X, Y)
    const Vect2d corners[3],
    //! [in] Maximum tree depth
    const unsigned int MaxDepth,
    //! [in] Data to link
    void * const restrict Data
) {
    //! \return Node to which the triangle data was added

    //! \note
    //! The triangle is inserted in any leaf nodes intersected by the triangle, this implies
    //! some possible duplication but it's the most effective way.

    // Get Cohen-Sutherland code value for each triangle endpoint
    const int cs[3] = {
        CS_Code(corners[0][0], corners[0][1], Node->BBox[0].X, Node->BBox[0].Y, Node->BBox[1].X, Node->BBox[1].Y),
        CS_Code(corners[1][0], corners[1][1], Node->BBox[0].X, Node->BBox[0].Y, Node->BBox[1].X, Node->BBox[1].Y),
        CS_Code(corners[2][0], corners[2][1], Node->BBox[0].X, Node->BBox[0].Y, Node->BBox[1].X, Node->BBox[1].Y)
    };

    // If any segment intersects this node's bbox
    if (CS_Intersect(cs[0], cs[1]) || CS_Intersect(cs[1], cs[2]) || CS_Intersect(cs[2], cs[0])) {
        if (!(MaxDepth - 1)) {
            // We've hit maximum depth, add to this node
            QTree_AddData(Node, corners[0][0], corners[0][1], Data);
        }  else {
            // If there's no child yet at this node and depth
            if (!Node->Childs[0]) {
                // Split node in 4.
                TPoint2D center;
                center.X = Node->BBox[0].X + (Node->BBox[1].X - Node->BBox[0].X) * 0.5;
                center.Y = Node->BBox[0].Y + (Node->BBox[1].Y - Node->BBox[0].Y) * 0.5;
                if (!QTree_Split(Node, &center)) return NULL;
            }

            // Recursively dig into the tree
            QTree_AddTriangle(Node->Childs[0], corners, MaxDepth - 1, Data);
            QTree_AddTriangle(Node->Childs[1], corners, MaxDepth - 1, Data);
            QTree_AddTriangle(Node->Childs[2], corners, MaxDepth - 1, Data);
            QTree_AddTriangle(Node->Childs[3], corners, MaxDepth - 1, Data);
        }
    }

    return Node;
}


//! Find all neighbors of a quad cell
void QTree_Neighbors(
    //! [in] Quad tree
    TQTree * const Node,
    //! [out] Neighbors
    TQTree ** const Neighbors,
    //! [in] Number of neightbors to find (4 or 8)
    const int NbNeighbors
) {

    //! Neighbor numbering is as follow:
    //! |     |     |     |
    //! | :-: | :-: | :-: |
    //! |  4  |  0  |  5  |
    //! |  3  |  X  |  1  |
    //! |  7  |  2  |  6  |

    for(int n = 0; n < NbNeighbors; n++) Neighbors[n] = NULL;

    const double dx = (Node->BBox[1].X - Node->BBox[0].X) * 0.5;
    const double dy = (Node->BBox[1].Y - Node->BBox[0].Y) * 0.5;

    // Find the tree's root
    TQTree * root = NULL;
    TQTree * node = Node;
    while((node = node->Parent)) { root = node; }

    // Find neighbors
    Neighbors[0] = QTree_Find(root, Node->BBox[0].X + dx, Node->BBox[0].Y - dy);
    Neighbors[1] = QTree_Find(root, Node->BBox[1].X + dx, Node->BBox[0].Y + dy);
    Neighbors[2] = QTree_Find(root, Node->BBox[0].X + dx, Node->BBox[1].Y + dy);
    Neighbors[3] = QTree_Find(root, Node->BBox[0].X - dx, Node->BBox[0].Y + dy);

    // If corners asked
    if (NbNeighbors == 8) {
        Neighbors[4] = QTree_Find(root, Node->BBox[0].X - dx, Node->BBox[0].Y - dy);
        Neighbors[5] = QTree_Find(root, Node->BBox[1].X + dx, Node->BBox[0].Y - dy);
        Neighbors[6] = QTree_Find(root, Node->BBox[1].X + dx, Node->BBox[1].Y + dy);
        Neighbors[7] = QTree_Find(root, Node->BBox[0].X - dx, Node->BBox[0].Y + dy);
    }
}


//! Delete(free) all nodes of a quad tree
void QTree_Del(
    //! [inout] Quad tree
    TQTree * const restrict Node
) {
    if (Node) {
        for(int c = 0; c < 4; c++) {
            if (Node->Childs[c]) {
                QTree_Del(Node->Childs[c]);
                free(Node->Childs[c]);
                Node->Childs[c] = NULL;
            }
        }
        QTree_DelData(Node);
    }
}


//! Find the leaf node of a given position
TQTree * QTree_Find(
    //! [in] Quad tree
    TQTree * const restrict Node,
    //! [in] X position
    const double X,
    //! [in] Y position
    const double Y
) {
    //! \return Child node

    TQTree * node = Node;

    if (Node->Childs[0]) {
        // Navigate through the quad tree recursively to find the terminal quad
        // which bounding box contains the XY position X, Y.
        int cidx = 0;
        int found = 0;
        while (cidx < 4 && !(found = QTree_Inside(Node->Childs[cidx++], X, Y)));
        if (!found) return NULL;

        node = QTree_Find(Node->Childs[cidx - 1], X, Y);
    } else {
        // This node is empty
        if (0 && !Node->Data) {
            // Go up and pick first parent's child having data
            int cidx = 0;
            while (cidx < 4 && !Node->Parent->Childs[cidx++]->Data);
            node = Node->Parent->Childs[cidx - 1];
        }
    }

    return node;
}


//! Create a new iterator
TQTreeIterator * QTree_IteratorNew(void) {
    //! \return New iterator

    return (TQTreeIterator*)calloc(1, sizeof(TQTreeIterator));
}


//! Iterate over the leaf nodes of a quad tree
TQTree * QTree_Iterate(
    //! [in] Quad tree in which to iterate. Must be provided for the first call only; provide NULL after
    TQTree * const restrict Node,
    //! [inout] Quad tree iterator
    TQTreeIterator * const Iter
) {
    //! \return Next leaf node; each call will return the next leaf

    // On other than first iteration, use iterator restart point
    TQTree * node = !Node ? Node : Iter->Node;

    if (node) {
        if (node->Childs[0]) {                               // If this node has childs and not all are visited
            unsigned char idx = Iter->Path & 0x7;                               // Get the next child index (3 first bit)
            if ((idx = Iter->Path & 0x7) < 4) {
                Iter->Path++;                                  // Increment child number
                Iter->Path <<= 3;                                // Push on path for next level down
                Iter->Node = node;                               // Keep this node as the iterator restart
                node = QTree_Iterate(node->Childs[idx], Iter);    // Look deeper for a leaf
            }
            if (idx == 3 && !node->Childs[0]) {                 // If we were on the last leaf of the previous node
                while((Iter->Path & 0x7) == 4) {                   // Go up to the last unprocessed branch
                    Iter->Path >>= 3;                             // Pop the child index
                    Iter->Node = Iter->Node->Parent;              // Set iterator restart to parent
                }
            }
        } else {
            Iter->Path >>= 3;                                   // This is a leaf, return it and pop the path
        }
    }

    return node;
}


//! Iterate over the leaf filled nodes (with data) of a quad tree
TQTree * QTree_IterateFilled(
    //! [in] Quad tree in which to iterate. Must be provided for the first call only; provide NULL after
    TQTree * const restrict Node,
    //! [inout] Quad tree iterator
    TQTreeIterator * const Iter
) {
    //! \return Next leaf node; each call will return the next leaf
    TQTree * node;
    while ((node = QTree_Iterate(Iter->Node ? NULL : Node, Iter)) && !node->NbData);

    return node;
}


//! Apply a function to each node of a quad tree
void QTree_Parse(
    //! [inout] Quad tree to traverse
    TQTree * const restrict Node,
    //! [in] Function to apply to each node
    QTree_ParseProc * const fnct,
    //! [in] Current depth; start with 0
    const unsigned int Depth
) {
    //! If no function is provided (fnct == NULL), information about each node is printed

    if (Node) {
        if (fnct) {
            for(int d = 0; d < Node->NbData; d++) {
                fnct(Node->Data[d].Ptr);
            }
        } else {
            int d = Depth;
            while(d--) fprintf(stdout, "   ");
            fprintf(stdout, "Level %i, BBox [%f, %f - %f, %f], Data %i\n",
                Depth, Node->BBox[0].X, Node->BBox[0].Y, Node->BBox[1].X, Node->BBox[1].Y, Node->NbData);
        }
        for(int c = 0; c < 4; c++) {
            QTree_Parse(Node->Childs[c], fnct, Depth + 1);
        }
    }
}


//! @}
