/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <rmnlib.h>
#include <rpnmacros.h>


#define elementCount  2000
#define elementCount_4 8000
#define maxBit       64



ftnfloat f77name(second)();

static double powerOf2s[maxBit];

void f77name(mainrle)()
{

  word bits[maxBit];
  word i, k, j;
  int arrayOfInt1[elementCount_4], arrayOfInt2[elementCount_4];
  int arrayOfInt3[elementCount_4], arrayOfInt4[elementCount_4];
  int numOfBitPerToken;
  int tempInteger;
  int packedElementCount;
  int isError;
  ftnfloat startTime, switchTime, endTime;
  int repeatTestTimes = 100;
  int stride;
  int maxStride = 4;
  int minTokenBit = 3, maxTokenBit = 31;
  int maxInteger=1, minInteger=1;


 
  /******************************************
   *                                        *
   *  generate number pool                  *
   *                                        *
   *****************************************/
  k       = 1;
  bits[0] = k;
  for( i = 1; i < maxBit; i++)
    {
      k += k;
      bits[i] = k;
    };
  

  /******************************************
   *                                        *
   *  generate 2's power                    *
   *                                        *
   *****************************************/
  powerOf2s[0] = 1.0;
  for ( i = 1; i < maxBit; i++)
    {
      powerOf2s[i] = 2.0 *powerOf2s[i-1];      
    };






  /*******************************************
   *                                         *
   *    body of test                         *
   *                                         *
   ******************************************/
 for (stride = 1; stride <= maxStride; stride++)
 {
   printf("\n\n ============== \n\n");
   printf(" stride is : %d ", stride );
   printf("\n\n ============== \n\n");

  for ( numOfBitPerToken = minTokenBit; numOfBitPerToken <= maxTokenBit; numOfBitPerToken++)
    {


      for ( i = 0; i < elementCount*stride ; i++)
        {
          arrayOfInt1[i] = -1;
          arrayOfInt2[i] = -1;
          arrayOfInt3[i] = -1;
          arrayOfInt4[i] = -1;
        };/* for */

      /*********************************
       *                               *
       * initialize integer arrays     *
       *                               *
       ********************************/
      i = 0;
      j = 0;
      while ( i < elementCount*stride )
        {

          arrayOfInt1[i] = bits[j%(numOfBitPerToken-1)];
              
          if ( ( arrayOfInt1[i] <= 32 ) && ( arrayOfInt1[i]*stride+i < elementCount*stride ) 
               )
            {          
              for ( k= i; k <= i+arrayOfInt1[i]*stride; k+=stride)
                {
                  arrayOfInt1[k] = arrayOfInt1[i];
                  
                };
              i+=arrayOfInt1[i]*stride;
            };/* if */
          i+=stride;
          j++;
        };/* while */
     
      arrayOfInt1[5*stride] = 0;
      arrayOfInt1[6*stride] = powerOf2s[numOfBitPerToken] - 1 - 3;
      
      minInteger = 0;
      maxInteger = powerOf2s[numOfBitPerToken] - 1 - 3;
      
      for ( i = 0; i < elementCount*stride ; i+=stride)
        {
          
          arrayOfInt3[i] = arrayOfInt1[i];
          
         
        };/* for */



     

      /*************************************
       *                                   *
       * pack and unpack                   *
       *                                   *
       ************************************/
      startTime = f77name(second)();
     
      for ( i = 1; i <= repeatTestTimes; i++)
        {
          packedElementCount = compact_rle( arrayOfInt1, arrayOfInt2, arrayOfInt2,
                                            maxInteger, minInteger, 
                                            elementCount, numOfBitPerToken, 128, stride, 1);
        };
     
     
      switchTime = f77name(second)();
      for ( i = 1; i <= repeatTestTimes; i++)
        {
         unpackWrapper(arrayOfInt4, arrayOfInt2, arrayOfInt2, stride, &stride );
         
          /*
          packedElementCount = compact_rle( arrayOfInt4, arrayOfInt2, arrayOfInt2, 
                                            maxInteger, minInteger, 
                                            elementCount, numOfBitPerToken, 128, stride, 2);
                                            */                                
        };
      
      endTime = f77name(second)();

      /*****************************************
       *                                       *
       * check for error and report result     *
       *                                       *
       ****************************************/
 
      isError = 0;
      for ( i = 0; i < elementCount*stride; i+=stride)
        {
          
          
          if (arrayOfInt1[i] != arrayOfInt4[i] )
            {
              isError = 1;
            };
        };

      if ( isError )
        {
          printf("\n NBITS: %d \t RLE_PACK : %f \t\t RLE_UNPACK : %f \t\t ERROR", 
                 numOfBitPerToken, (switchTime-startTime)/repeatTestTimes, (endTime-switchTime)/repeatTestTimes);
        }
      else
        {
          printf("\n NBITS: %d \t RLE_PACK : %6e \t\t RLE_UNPACK : %6e ", 
                 numOfBitPerToken, (switchTime-startTime)/repeatTestTimes, 
                 (endTime-switchTime)/repeatTestTimes);
        };
      

    };/* for, numOfBitPerToken */
 };/*for, stride*/
 printf("\n");
}
