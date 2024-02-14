#include <malloc.h>
#include "QTree.h"

/**----------------------------------------------------------------------------
 * @brief  Check if a XY data position is inside the bounding box of a TQTree object
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    @param[in] X          X data position
 *    @param[in] Y          Y data position
 *    
 *    @return               0(False) if outside, not 0(True) if inside
 */
static inline int QTree_Inside(TQTree * const Node,double X,double Y) {

  return(X>=Node->BBox[0].X && X<Node->BBox[1].X && Y>=Node->BBox[0].Y && Y<Node->BBox[1].Y );
}

/**----------------------------------------------------------------------------
 * @brief  Find the node of a tree which contains the data XY position in it's bounding box
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    @param[in] X          X data position
 *    @param[in] Y          Y data position
 *    
 *    @return               pointer to the node
 */
static inline TQTree* QTree_FindChild(const TQTree* const restrict Node,double X,double Y) {

  int cidx=0;

  while(!QTree_Inside(Node->Childs[cidx++],X,Y));

  return(Node->Childs[cidx-1]);
}

/**----------------------------------------------------------------------------
 * @brief  Create and initialize all sub-quads objects of a quad
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    @param[in] Center     Point2D object pointer XY center
 *    
 *    @return               False on failed allocation 
 * 
 *    @remark : 
 *    Box numbering is
 *         ----- 
 *       | 0 | 1 |
 *       |---+---| 
 *       | 2 | 3 |  
 *         ----- 
 */
static inline int QTree_Split(TQTree* const restrict Node,const TPoint2D* const restrict Center) {
   
   Node->Childs[0] = QTree_New(Node->BBox[0].X,Node->BBox[0].Y, Center->X, Center->Y,Node);
   Node->Childs[1] = QTree_New(Center->X,Node->BBox[0].Y,Node->BBox[1].X, Center->Y,Node); 
   Node->Childs[2] = QTree_New(Node->BBox[0].X,Center->Y,Center->X, Node->BBox[1].Y,Node);
   Node->Childs[3] = QTree_New(Center->X,Center->Y,Node->BBox[1].X, Node->BBox[1].Y,Node);

   return(Node->Childs[0] && Node->Childs[1] && Node->Childs[2] && Node->Childs[3]);
}

/**----------------------------------------------------------------------------
 * @brief  Add data information into a node
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    @param[in] X          X data position
 *    @param[in] Y          Y data position
 *    @param[in] Data       Data pointer
 *    
 *    @return               False on failed allocation
 */
int QTree_AddData(TQTree* const restrict Node,double X,double Y,void *Data) {

  if (Data) {
     Node->NbData++;
     if (Node->NbData>Node->Size) {
        Node->Size+=QTREE_SIZEINCR;
        if (!(Node->Data=(TQTreeData*)realloc(Node->Data,Node->Size*sizeof(TQTreeData)))) {
           return(0);
        }
     }
     Node->Data[Node->NbData-1].Ptr=Data;
     Node->Data[Node->NbData-1].Pos.X=X;
     Node->Data[Node->NbData-1].Pos.Y=Y;
  }
  
  return(1);
}

/**----------------------------------------------------------------------------
 * @brief  Delete the data information of a QTree node
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    
 *    @return               False on failed allocation
 */

void QTree_DelData(TQTree* const Node) {

   Node->NbData=0;
   Node->Size=0;
 
   if (Node->Data) {
      free(Node->Data);
      Node->Data=NULL;
   }
}

/**----------------------------------------------------------------------------
 * @brief  Create and initialize a new TQTree object
 * @date   November 2008
 *    @param[in] X0          South West X bounding box limit for the quad
 *    @param[in] Y0          South West Y bounding box limit for the quad
 *    @param[in] X1          North East X bounding box limit for the quad
 *    @param[in] Y1          North East Y bounding box limit for the quad
 *    
 *    @return                New and initialized TQTree object pointer
 */
TQTree* QTree_New(double X0,double Y0,double X1,double Y1,TQTree *Parent) {

   TQTree *node=NULL;
   
   if ((node=(TQTree*)malloc(sizeof(TQTree)))) {

      node->Size      = 0;
      node->NbData    = 0;
      node->Data      = NULL;

      node->Parent    = Parent;
      node->Childs[0] =
      node->Childs[1] =
      node->Childs[2] =
      node->Childs[3] = NULL;

      node->BBox[0].X = X0;
      node->BBox[0].Y = Y0;
      node->BBox[1].X = X1;
      node->BBox[1].Y = Y1;
   }
   
   return(node);
}

/**----------------------------------------------------------------------------
 * @brief  Free a QTree node
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    
 */
void QTree_Free(TQTree *Node) {
   
   if (Node->Childs[0]) QTree_Free(Node->Childs[0]);
   if (Node->Childs[1]) QTree_Free(Node->Childs[1]);
   if (Node->Childs[2]) QTree_Free(Node->Childs[2]);
   if (Node->Childs[3]) QTree_Free(Node->Childs[3]);
   
   QTree_DelData(Node);
   free(Node);
}

/**----------------------------------------------------------------------------
 * @brief  Add some data to a TQTree object(usually a head quad at level=1...)
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    @param[in] X          X data position
 *    @param[in] Y          Y data position
 *    @param[in] MaxDepth   Maximum depth of tree
 *    @param[in] Data       External data pointer
 *    
 *    @return               A TQTree object pointer(Could be NULL if a problem arise)
 */
TQTree* QTree_Add(TQTree* restrict Node,double X,double Y,unsigned int MaxDepth,void* restrict Data) {

   int      ok=1;
   TPoint2D center;
   TQTree  *new=Node;

   if (!(MaxDepth--)) {
       // We've hit maximum depth, add to this node
       ok=QTree_AddData(Node,X,Y,Data);
   } else {
      if (Node->Childs[0]) {

         // This is a not a leaf, find sub node recursively
         new = QTree_FindChild(Node,X,Y);
         new = QTree_Add(new,X,Y,MaxDepth,Data);

      } else {

         if (!Node->NbData) {
            // Empty leaf, use it
            ok=QTree_AddData(Node,X,Y,Data);
         } else {
            // Check if it's same coordinate to avoid infinite recursion
            if (Node->Data[0].Pos.X==X && Node->Data[0].Pos.Y==Y) {
               ok=QTree_AddData(Node,X,Y,Data);
            } else {
            
               // Leaf node but there is data already stored here, split.
               center.X = Node->BBox[0].X+(Node->BBox[1].X-Node->BBox[0].X)*0.5;
               center.Y = Node->BBox[0].Y+(Node->BBox[1].Y-Node->BBox[0].Y)*0.5;

               if (!QTree_Split(Node,&center)) {
                  return(NULL);
               }
                     
               // Reassign previous node data into splitted node
               new = QTree_FindChild(Node,Node->Data[0].Pos.X,Node->Data[0].Pos.Y);
               new->Data=Node->Data;
               new->NbData=Node->NbData;
               Node->Data=NULL;
               Node->NbData=0;
               Node->Size=0;
               
               // Assign new data
               new = QTree_FindChild(Node,X,Y);
               new = QTree_Add(new,X,Y,MaxDepth,Data);
            }
         }
      }
   }

   return(ok?new:NULL);
}

// Vertice codes for Cohen–Sutherland bbox intersection algorithm check
#define CS_INSIDE 0x0;             // 0000
#define CS_LEFT   0x1;             // 0001
#define CS_RIGHT  0x2;             // 0010
#define CS_BOTTOM 0x4;             // 0100
#define CS_TOP    0x8;             // 1000
#define CS_Intersect(A,B) (!(A&B)) // Do theses code intersect

/**----------------------------------------------------------------------------
 * @brief  Cohen–Sutherland algorithm to compute the bit code for a point (X,Y) 
 *         using the clip rectangle bounded diagonally by (X0,Y0), and (X1,Y1)
 * @date   November 2008
 *    @param[in] X          X position
 *    @param[in] Y          Y position
 *    @param[in] X0         South West X bounding box limit for the quad
 *    @param[in] Y0         South West Y bounding box limit for the quad
 *    @param[in] X1         North East X bounding box limit for the quad
 *    @param[in] Y1         North East Y bounding box limit for the quad
 *    
 *    @return               code
 */
int CS_Code(double X, double Y,double X0,double Y0,double X1,double Y1) {
   
   int code=CS_INSIDE;                     // Initialised as being inside of clip window

   if      (X<X0) { code |= CS_LEFT; }     // Left of clip window 
   else if (X>X1) { code |= CS_RIGHT; }    // Right of clip window
            
   if      (Y<Y0) { code |= CS_BOTTOM; }   // Below the clip window           
   else if (Y>Y1) { code |= CS_TOP; }      // Above the clip window
            
   return(code);
}

/**----------------------------------------------------------------------------
 * @brief  Add some triangle data to a TQTree object(usually a head quad at level=1...)
 * @date   July 2015
 *    @param[in] Node       TQTree object pointer
 *    @param[in] T          Triangle's corners (X,Y)
 *    @param[in] MaxDepth   Maximum depth of tree
 *    @param[in] Data       External data pointer
 *    
 *    @return               A TQTree object pointer(Could be NULL if a problem arise)
 * 
 *    @remarks
 *    - We insert the triangle in any leaf nodes intersected by the triangle, this implies
 *      some possible duplication but it's the most effective way.
 */
TQTree* QTree_AddTriangle(TQTree* restrict Node,Vect2d T[3],unsigned int MaxDepth,void* restrict Data) {

   TPoint2D center;
   int      cs[3];
   TQTree  *new=Node;
      
   // Get Cohen-Sutherland code value for each triangle endpoint
   cs[0]=CS_Code(T[0][0],T[0][1],Node->BBox[0].X,Node->BBox[0].Y,Node->BBox[1].X,Node->BBox[1].Y);
   cs[1]=CS_Code(T[1][0],T[1][1],Node->BBox[0].X,Node->BBox[0].Y,Node->BBox[1].X,Node->BBox[1].Y);
   cs[2]=CS_Code(T[2][0],T[2][1],Node->BBox[0].X,Node->BBox[0].Y,Node->BBox[1].X,Node->BBox[1].Y);
      
   // If any segment intersects this node's bbox
   if (CS_Intersect(cs[0],cs[1]) || CS_Intersect(cs[1],cs[2]) || CS_Intersect(cs[2],cs[0])) {
      
      if (!(MaxDepth--)) {
         // We've hit maximum depth, add to this node
         if (QTree_AddData(Node,T[0][0],T[0][1],Data)) {
            new=Node;
         }
      }  else {
         // If there's no child yet at this node and depth
         if (!Node->Childs[0]) {
            // Split node in 4.
            center.X = Node->BBox[0].X+(Node->BBox[1].X-Node->BBox[0].X)*0.5;
            center.Y = Node->BBox[0].Y+(Node->BBox[1].Y-Node->BBox[0].Y)*0.5;

            if (!QTree_Split(Node,&center)) {
               return(NULL);
            }
         }

         // Recursively dig into the tree
         QTree_AddTriangle(Node->Childs[0],T,MaxDepth,Data);
         QTree_AddTriangle(Node->Childs[1],T,MaxDepth,Data);
         QTree_AddTriangle(Node->Childs[2],T,MaxDepth,Data);
         QTree_AddTriangle(Node->Childs[3],T,MaxDepth,Data);
      }
   }

   return(new);
}

/**----------------------------------------------------------------------------
 * @brief  Find all neighbors of a quad cell
 * @date   July 2015
 *    @param[in]  Node       TQTree object pointer
 *    @param[out] Neighbors  Neighbors list
 *    @param[in]  Nb         Number of neightbors to find (4 or 8 for corners)
 *    
 *    @remarks 
 *   - The neighbor numbering is as follow
 *
 *     4   0   5
 *        --- 
 *     3 |   | 1
 *        --- 
 *     7   2   6
 */
void QTree_Neighbors(TQTree* Node,TQTree** Neighbors,int Nb) {

   TQTree *root,*node;
   double  dx,dy;
   int     d=0;;
   
   if (Nb!=4 || Nb!=8) {
      for(int n=0;n<Nb;n++) Neighbors[n]=NULL;
   }
   
   dx=(Node->BBox[1].X-Node->BBox[0].X)*0.5;
   dy=(Node->BBox[1].Y-Node->BBox[0].Y)*0.5;

   // Climb back up the tree
   root=NULL;
   node=Node;
   while((node=node->Parent)) { d++; root=node; }      
         
   // Find neighbors
   Neighbors[0]=QTree_Find(root,Node->BBox[0].X+dx,Node->BBox[0].Y-dy);
   Neighbors[1]=QTree_Find(root,Node->BBox[1].X+dx,Node->BBox[0].Y+dy);
   Neighbors[2]=QTree_Find(root,Node->BBox[0].X+dx,Node->BBox[1].Y+dy);
   Neighbors[3]=QTree_Find(root,Node->BBox[0].X-dx,Node->BBox[0].Y+dy);
   
   // If corners asked
   if (Nb==8) {
      Neighbors[4]=QTree_Find(root,Node->BBox[0].X-dx,Node->BBox[0].Y-dy);
      Neighbors[5]=QTree_Find(root,Node->BBox[1].X+dx,Node->BBox[0].Y-dy);
      Neighbors[6]=QTree_Find(root,Node->BBox[1].X+dx,Node->BBox[1].Y+dy);
      Neighbors[7]=QTree_Find(root,Node->BBox[0].X-dx,Node->BBox[0].Y+dy);      
   }
}

/**----------------------------------------------------------------------------
 * @brief  Delete(free) all nodes of a TQTree object
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    
 */
void QTree_Del(TQTree* restrict Node) {
   
   int c;
   
   if (Node) {

      for(c=0;c<4;c++) {
         if (Node->Childs[c]) {
            QTree_Del(Node->Childs[c]);
            free(Node->Childs[c]); 
            Node->Childs[c]=NULL;
         }
      }
      QTree_DelData(Node);
   }
}

/**----------------------------------------------------------------------------
 * @brief  Find the leaf node for an abitrary XY positione
 * @date   November 2008
 *    @param[in] Node       TQTree object pointer
 *    @param[in] X          X position
 *    @param[in] Y          Y position
 *    
 *    @return               Child node
 */
TQTree* QTree_Find(TQTree* restrict Node,double X,double Y) {

   int     cidx=0,f;
   TQTree* node=Node;

   if (Node->Childs[0]) {

      // Navigate through the quad tree recursively to find the terminal quad
      // which bounding box contains the XY position X,Y.
      while (cidx<4 && !(f=QTree_Inside(Node->Childs[cidx++],X,Y)));
      if (!f) 
         return(NULL);

      node=QTree_Find(Node->Childs[cidx-1],X,Y);
   } else {
      // This node is empty
      if (0 && !Node->Data) {
         // Go up and pick first parent's child having data
         while (cidx<4 && !Node->Parent->Childs[cidx++]->Data);
         node=Node->Parent->Childs[cidx-1];
      }    
   }

   return(node);
}

/**----------------------------------------------------------------------------
 * @brief  Initialiaze a new iterator
 * @date   November 2014
 * 
 *    @return               TQTree iterator
 */
TQTreeIterator* QTree_IteratorNew(void) {
   
   TQTreeIterator *iter;
   
   iter=(TQTreeIterator*)calloc(1,sizeof(TQTreeIterator));
   return(iter);
}

/**----------------------------------------------------------------------------
 * @brief  Parse only leaf nodes. Each call will return the next leaf
 * @date   November 2014
 *    @param[in] Node       TQTree object pointer which we want to iterate into, fisrt call only, NULL thereafter
 *    @param[in] Iter       Tree iterator
 *    
 *    @return               Next leaf node
 */
TQTree* QTree_Iterate(TQTree* restrict Node,TQTreeIterator *Iter) {
   
   unsigned char idx;
   
   // On other than first iteration, use iterator restart point
   if (!Node) {
      Node=Iter->Node;
   }
   
   if (Node) {
      if (Node->Childs[0]) {                               // If this node has childs and not all are visited
         idx=Iter->Path&0x7;                               // Get the next child index (3 first bit)
         
         if ((idx=Iter->Path&0x7)<4) {
            Iter->Path++;                                  // Increment child number
            Iter->Path<<=3;                                // Push on path for next level down
            Iter->Node=Node;                               // Keep this node as the iterator restart
            Node=QTree_Iterate(Node->Childs[idx],Iter);    // Look deeper for a leaf
         } 
         if (idx==3 && !Node->Childs[0]) {                 // If we were on the last leaf of the previous node
            while((Iter->Path&0x7)==4) {                   // Go up to the last unprocessed branch
               Iter->Path>>=3;                             // Pop the child index
               Iter->Node=Iter->Node->Parent;              // Set iterator restart to parent
            }
         }
      } else {
         Iter->Path>>=3;                                   // This is a leaf, return it and pop the path
      }
   }
 
   return(Node);
}

/**----------------------------------------------------------------------------
 * @brief  Parse only leaf nodes, containg data payload. Each call will return the next leaf
 * @date   November 2014
 *    @param[in] Node       TQTree object pointer which we want to iterate into, fisrt call only, NULL thereafter
 *    @param[in] Iter       Tree iterator
 *    
 *    @return               Next leaf node
 */
TQTree* QTree_IterateFilled(TQTree* restrict Node,TQTreeIterator *Iter) {

   TQTree *node;
   
   while((node=QTree_Iterate(Iter->Node?NULL:Node,Iter)) && !node->NbData);
   
   return(node);
}

/**----------------------------------------------------------------------------
 * @brief  Parse all nodes af the QTree
 * @date   November 2014
 *    @param[in] Node       TQTree object pointer which we want to parse
 *    @param[in] Proc       Proc to apply to all nodes data (typedef QTree_ParseProc)
 *    @param[in] Depth      Use 0 for starter
 *    
 *    @return               Next leaf node
 */
void QTree_Parse(TQTree* restrict Node,QTree_ParseProc *Proc,unsigned Depth) {
   
   int c=0,d;
   
   if (Node) {
      
      if (Proc) {
         for(d=0;d<Node->NbData;d++) {
            Proc(Node->Data[d].Ptr);
         }
      } else {
         d=Depth;
         while(d--) fprintf(stdout,"   ");
         fprintf(stdout,"Level %i, BBox [%f,%f - %f,%f], Data %i\n",Depth,Node->BBox[0].X,Node->BBox[0].Y,Node->BBox[1].X,Node->BBox[1].Y,Node->NbData);
      }
      for(c=0;c<4;c++) {
         QTree_Parse(Node->Childs[c],Proc,Depth+1);
      }
   }
}
