#include <malloc.h>

#include "rmn/List.h"

/**----------------------------------------------------------------------------
 * @brief  Add a node to the a doubly linked list
 * @date   Aout 1998
 *    @param[in]   List           List to add a node to
 *    @param[in]   Data           Pointer to associated data to add
 *   
 *    @return                     Pointer to the list head (new node)
 *
 * @remark
 *   - This is a doubly linked list
 *   - Adding will be done on the head of the list, which allows to use as a queue or even stack as well
 */
TList* TList_Add(TList *List,void *Data) {

   TList *node=(TList*)malloc(sizeof(TList));

   if (node) {
      node->Next=List;
      node->Prev=NULL;
      node->Data=Data;

      if (List)
         List->Prev=node;
   }
   
   return(node);
}

TList* TList_Push(TList *List,void *Data) {

   return (TList_Add(List,Data));
}

TList* TList_Pop(TList *List) {
   
   TList *node=List;

   if (node && node->Next) {
      node->Next->Prev=NULL;
   }
   return(node->Next);
}

TList* TList_Queue(TList *List,void *Data) {

   return (TList_Add(List,Data));
}

void* TList_Dequeue(TList *List) {
   
   TList *node=List;
   void *data=NULL;

   while(node) {
      if (!node->Next) {
         node->Prev->Next=NULL;
         data=node->Data;
         free(node);
         break;
      }
      node=node->Next;
   }
   return(data);
}

/**----------------------------------------------------------------------------
 * @brief  Add a node to the list in an ordonated way
 * @date   Mai 2014 
 *    @param[in]   List           List to add a node to
 *    @param[in]   Proc           Pointer to comparison function
 *    @param[in]   Data           Pointer to associated data to add
 *   
 *    @return                     Pointer to the list head
 *
 */
TList* TList_AddSorted(TList *List,TList_CompareProc *Proc,void *Data) {

   TList *insert,*prev=NULL,*node=(TList*)calloc(1,sizeof(TList));

   if (node) {
      node->Data=Data;      
      
      insert=List;
      
      while (insert) {
         if (Proc(insert->Data,Data)>=0) {
            node->Next=insert;
            node->Prev=insert->Prev;
            if (insert->Prev) {
               insert->Prev->Next=node;
            } else {
               List=node;               
            }  
            insert->Prev=node;
            break;
         }
         prev=insert;
         insert=insert->Next;
      }

      if (!insert) {
         if (prev) {
            prev->Next=node;
            node->Prev=prev;           
         } else {
            List=node;
         }
      }
   }
   
   return(List);
}

/**----------------------------------------------------------------------------
 * @brief  Delete a node from the list
 * @date   Aout 1998
 *    @param[in]   List           List to delete the node from
 *    @param[in]   Data           Pointer to data of the node to delete
 *   
 *    @return                     Pointer to the list head
 */
TList* TList_Del(TList *List,void *Data) {

  TList *node=List;

  while(node) {
      if (node->Data==Data) {
         if (node->Prev)
            node->Prev->Next=node->Next;
         if (node->Next)
            node->Next->Prev=node->Prev;
         if (node==List) {
            List=List->Next;
         }
         free(node);
         
         break;
      }
      node=node->Next;
   }
   return(List);
}

/**----------------------------------------------------------------------------
 * @brief  Rechercher un noeud contenant la donnee specifie
 * @date   Aout 1998 
 *    @param[in]   List           List to be searched
 *    @param[in]   Proc           Pointer to comparison function (must return 1 if found)
 *    @param[in]   Data           Pointer to data to be found
 *   
 *    @return                     Pointer on the found node or NULL
 */
TList* TList_Find(TList *List,TList_CompareProc *Proc,void *Data) {

   while(List) {
      if (!Proc || Proc(List->Data,Data)) {
         return(List);
      }
      List=List->Next;
   }
   return(NULL);
}

/**----------------------------------------------------------------------------
 * @brief  Free a list
 * @date   Aout 1998 
 *    @param[in]   List           List to be freed
 *    @param[in]   Proc           Pointer to function to free the nodes associated data
 */
void TList_Clear(TList *List,TList_FreeProc *Proc) {

   TList *tmp;

   while(List) {
      tmp=List;
      List=List->Next;
      if (Proc)
         Proc(tmp->Data);
      free(tmp);
   }
}
