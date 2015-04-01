#include <rpnmacros.h>
#include <ezscint.h>
#include <ez_funcdef.h>

int c_ez_findgrid(int grid_index, _Grille *gr)
  {
  wordint gdrow, gdcol, index_found;
  wordint found, end_reached, next_index;
  _Grille *refgd;
    
  if (grid_index == -1)
    {
    return -1;
    }
  
  refgd = gr_list[grid_index];
  found = -1;
  index_found = -1;
  end_reached = -1;

  if (gr == NULL)
     {
     fprintf(stderr, "gr = NULL!\n");
     found = -1;
     return -1;
     }
  while (found == -1 && end_reached == -1)
    {
    if (gr->grtyp[0] == refgd->grtyp[0] &&
        gr->ni == refgd->ni &&  gr->nj == refgd->nj &&
        gr->fst.ig[IG1] == refgd->fst.ig[IG1] && gr->fst.ig[IG2] == refgd->fst.ig[IG2] &&
        gr->fst.ig[IG3] == refgd->fst.ig[IG3] && gr->fst.ig[IG4] == refgd->fst.ig[IG4])
      {
      found = 1;
      index_found = refgd->index;
      }
    else
      {
      if (refgd->next_gd == -1)
        {
        end_reached = 1;
        }
      else
        {
        c_gdkey2rowcol(refgd->next_gd, &gdrow, &gdcol);
        refgd = &Grille[gdrow][gdcol];
/*        next_index = refgd->next_gd;*/
        }
      }
    }  
  
  if (found == -1)
    {
    return -1;
    }
  else  
    {
    return index_found;
    }
  }

void dump_gr_list()
  {
  int i, gd_row, gd_col;
  _Grille *gr;
  
  for (i=0; i < chunks[cur_log_chunk]; i++)
    {
    if (gr_list[i] != NULL)
      {
      gr = gr_list[i];
      printf("%d %d -> ", i, gr->index);
      while (gr->next_gd != -1)
        {
        printf("%d ->", gr->next_gd);
        c_gdkey2rowcol(gr->next_gd, &gd_row, &gd_col);
        gr = &Grille[gd_row][gd_col];
        }
      printf("\n");
      }
    }
  }
  
