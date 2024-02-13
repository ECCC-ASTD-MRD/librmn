#ifndef _List_h
#define _List_h

typedef struct TList {
   void         *Data;
   struct TList *Next;
   struct TList *Prev;
} TList;

typedef int (TList_CompareProc)(void *Data0,void *Data1);
typedef int (TList_FreeProc)(void *Data0);

TList* TList_Add(TList *List,void *Data);
TList* TList_AddSorted(TList *List,TList_CompareProc *Proc,void *Data);
TList* TList_Del(TList *List,void *Data);
TList* TList_Find(TList *List,TList_CompareProc *Proc,void *Data);
void   TList_Clear(TList *List,TList_FreeProc *Proc);

#endif
