/* 
 * Copyright (C) 2025  Recherche en Prevision Numerique
 *                     Environnement Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 */

// even terms followed by odd terms after forward transform (in place)
// even terms followed by odd terms before inverse transform (in place)
void fwd_1d_lgt53(int *x, int n);
void inv_1d_lgt53(int *x, int n);

// multi level 1D transform, same layout as fwd_1d_lgt53/inv_1d_lgt53 (in place)
void fwd_1d_lgt53_n(int *x, int n, int levels);
void inv_1d_lgt53_n(int *x, int n, int levels);

// even/odd pairs with possible even term alone at end (in place)
void fwd_1d_lgt53_asis(int *x, int n);
void inv_1d_lgt53_asis(int *x, int n);

// explicit split into even and odd arrays
void fwd_1d_lgt53_split(int *x, int *e, int *o, int n);
void fwd_1d_lgt53_split_c(int *x, int *e, int *o, int n);
void fwd_1d_lgt53_split_simd(int *x, int *e, int *o, int n);

void inv_1d_lgt53_split(int *x, int *e, int *o, int n);
void inv_1d_lgt53_split_c(int *x, int *e, int *o, int n);
void inv_1d_lgt53_split_simd(int *x, int *e, int *o, int n);

// 4 quadrants, 2D extension of fwd_1d_lgt53/inv_1d_lgt53 (in place)
void fwd_2d_lgt53(int *x, int lni, int ni, int nj);
void inv_2d_lgt53(int *x, int lni, int ni, int nj);

// multi level 2D transform, same layout as fwd_2d_lgt53/inv_2d_lgt53 (in place)
void fwd_2d_lgt53_n(int *x, int lni, int ni, int nj, int levels);
void inv_2d_lgt53_n(int *x, int lni, int ni, int nj, int levels);
