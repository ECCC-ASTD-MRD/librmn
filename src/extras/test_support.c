//
// Copyright (C) 2025  Environnement Canada
//
// This is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This software is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details .
//
// Author:
//     M. Valin,   Recherche en Prevision Numerique, 2025
//
// functions needed for some of the tests

// recurrent scaling of floating point values for some exponent range tests
// must be in another file to inhibit optimizer for subnormal test
void scale_floats_recurrent(float *f, int n , float fact){
  int i ;
  for(i=1 ; i<n ; i++) f[i] = f[i-1] * fact ;
}
