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

#include <rmnlib.h>
#include <stdlib.h>
#include "mymacrosIEEE.h"
#define myElementCount 2100
#define myElementCount2 6300
#define strideElementCount 6300
#define veryLargeElementCount 4194305
static int elementCount = myElementCount;

static double powerOf2s[33];



void IEEEtransform1(int bitSizeOfToken, int mantisaSize, int expoAlignment, FLOAT_4_8 *inArrayFloat, 
                   FLOAT_4_8 *outArrayFloat, int floatCount, int stride)
{


  int i;
  ALL_FLOAT floatTemplate;

  if ( bitSizeOfToken == 32)
    {
     for ( i = 0; i < floatCount*stride; i+=stride)
        {
          outArrayFloat[i] = inArrayFloat[i];
        };
    }
  else
    {
      for ( i = 0; i < floatCount*stride; i+=stride)
        {
          floatTemplate.X = inArrayFloat[i];
          if (floatTemplate.M.expo - expoAlignment < 0 )
            floatTemplate.X = 0;
          if (mantisaSize < 24 )
            floatTemplate.M.mantis = floatTemplate.M.mantis >> ( 24 - mantisaSize );

          outArrayFloat[i] = floatTemplate.X;
      
        };
    };

}





void f77name(cmainieee)()
{
  int i;

  FLOAT_4_8 arrayOfFloat0[myElementCount];
  FLOAT_4_8 arrayOfFloat[myElementCount2], arrayOfFloatTest[strideElementCount];
  FLOAT_4_8 arrayOfFloat1[myElementCount2], arrayOfFloat2[strideElementCount]; 
  FLOAT_4_8 arrayOfFloat3[myElementCount2], arrayOfFloat4[strideElementCount]; 
  FLOAT_4_8 arrayOfFloat5[myElementCount2], arrayOfFloat6[myElementCount2], arrayOfFloat7[myElementCount2];


  FLOAT_4_8 arrayOfFloat1_IEEE[myElementCount2], arrayOfFloat2_IEEE[myElementCount2];
  FLOAT_4_8 arrayOfFloat3_IEEE[myElementCount2];
  ftnword   ftnBitSizeOfIEEE, ftnBitSizeOfIBM, ftnElementCount, ftnCorrectElement, ftnErrorCode;



  word arrayOfInt1[myElementCount2], arrayOfInt2[myElementCount2];
  word arrayOfInt3[myElementCount2], arrayOfInt4[myElementCount2];
  word arrayOfInt5[myElementCount2], arrayOfInt6[myElementCount2], arrayOfInt7[myElementCount2];
  

  /***************************
    need to switch between 4 and 32 to check its viability
    *****************************/
  int bitSizeOfInt;
  int mBitSizeOfInt;
  int off_set = 1;
  int stride = 1;
  int opCode = FLOAT_PACK;
  int opCode1 = FLOAT_UNPACK; 
  ftnword *unpackedFloat1, *unpackedFloat2, *unpackedFloat3, *unpackedFloat4,*unpackedFloat5, *unpackedFloat6;

  FLOAT_4_8 missingTag = 9999.0000;
  FLOAT_4_8 tempFloat;
  FLOAT_4_8 maxError, maxDifference;
  int notStablized = 0;
  int strideNotWorking;
  int testError;
  int bitSizeOfExpo;
  int mantisaSize;
  int expoAlignment;
  FLOAT_4_8 powerOf2s[64];



  bitSizeOfInt  = okBit;
  mBitSizeOfInt = -okBit;






  powerOf2s[0] = 1.0;
  for ( i = 1; i < 64; i++)
    {
      powerOf2s[i] = 2.0 *powerOf2s[i-1];      
    };



  /*******************************************
   *                                         *
   *  transform IBM format into IEEE         *
   *                                         *
   ******************************************/
  ftnBitSizeOfIEEE = 4;
  ftnBitSizeOfIBM  = 4;
  ftnElementCount  = myElementCount2;
 





if ( geneticOn == 1 )
{
  /******************************
    genetic test
    ****************************/
  bitSizeOfInt = 16;
  bitSizeOfExpo = 7;
  mantisaSize = 9;

  printf("\n IEEE block test starts(float) \n" );  
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;

  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
    };  
 

#if defined (_FLOAT1)
  f77name(fl1_ie3)(arrayOfFloat1, arrayOfFloat1_IEEE, &ftnBitSizeOfIBM, &ftnBitSizeOfIEEE, 
                   &ftnElementCount, &ftnCorrectElement, &ftnErrorCode);
#else
  for ( i = 0; i < elementCount ; i++ )
    arrayOfFloat1_IEEE[i] = arrayOfFloat1[i];
#endif

  
  for ( bitSizeOfInt = 16; bitSizeOfInt < 33; bitSizeOfInt++)
    {

      if ( bitSizeOfInt < 21 )
        {
          bitSizeOfExpo = 5;
        }
      else if ( bitSizeOfInt < 25 )
        {
          bitSizeOfExpo = 6;
        }
      else if ( bitSizeOfInt < 29 )
        {
          bitSizeOfExpo = 7;
          
        }
      else 
        {
          bitSizeOfExpo = 8;
        }  


      mantisaSize = bitSizeOfInt - bitSizeOfExpo - 1;
      expoAlignment = 126 - (powerOf2s[bitSizeOfExpo] - 1);



      unpackedFloat1 = compact_IEEEblock_FLOAT_4_8(arrayOfFloat1_IEEE, &arrayOfInt1[0], &arrayOfInt1[4], 
                                                   elementCount, bitSizeOfInt, 
                                                   bitSizeOfExpo, 0, 1, FLOAT_PACK, 0, &missingTag);
      
      unpackedFloat2 = compact_IEEEblock_FLOAT_4_8(arrayOfFloat2_IEEE, &arrayOfInt1[0], &arrayOfInt1[4], 
                                                   elementCount, bitSizeOfInt, 
                                                   bitSizeOfExpo, 0, 1, FLOAT_UNPACK, 0, &missingTag);

     
#if defined(_FLOAT1)
      f77name(ie3_fl1)(arrayOfFloat2_IEEE, arrayOfFloat2, &ftnBitSizeOfIEEE, &ftnBitSizeOfIBM, 
                       &ftnElementCount, &ftnCorrectElement, &ftnErrorCode);
     

      
      IEEEtransform1(bitSizeOfInt, mantisaSize, expoAlignment, arrayOfFloat1_IEEE, arrayOfFloat3_IEEE, 
                     elementCount, stride); 
#else
      for ( i = 0; i < elementCount ; i++ )
	arrayOfFloat2[i] = arrayOfFloat2_IEEE[i];
#endif

#if defined(_FLOAT1)
      f77name(ie3_fl1)(arrayOfFloat3_IEEE, arrayOfFloat3, &ftnBitSizeOfIEEE, &ftnBitSizeOfIBM, 
                       &ftnElementCount, &ftnCorrectElement, &ftnErrorCode);
#else
      for ( i = 0; i < elementCount ; i++ )
	arrayOfFloat3[i] = arrayOfFloat3_IEEE[i];
#endif
                    



                                
  

      testError = 0;
      for ( i = 0; i < elementCount*stride; i+=stride)
        {
          if ( arrayOfFloat2[i] != arrayOfFloat3[i] )
            {
              testError = 1;
              printf("\t %d \t %f \t %f \n",  i, arrayOfFloat2[i], arrayOfFloat3[i]);
            }
        };

      if (testError == 1 )
        {
          printf(" bitSizeOfToken: %d \t bitSizeOfExpo: %d \t test not passed \n", 
                 bitSizeOfInt, bitSizeOfExpo);
        }
      else
        {
          printf(" bitSizeOfToken: %d \t bitSizeOfExpo: %d \t test pased \n", 
                 bitSizeOfInt, bitSizeOfExpo);
        };
    };
  printf("\n IEEE block test finishes(float)  \n");
  printf("\n ===================================================== \n\n\n");

};













}/* end of program */














