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
#include <stdio.h>
#include "mymacrosDouble.h"
#define myElementCount 21
#define myElementCount2 63
#define strideElementCount 63

static int elementCount = myElementCount;

static double powerOf2s[33];


void f77name(dcmain)()
{
  int i;

  FLOAT_4_8 arrayOfFloat0[myElementCount];
  FLOAT_4_8 arrayOfFloat[myElementCount2], arrayOfFloatTest[strideElementCount];
  FLOAT_4_8 arrayOfFloat1[myElementCount2], arrayOfFloat2[strideElementCount]; 
  FLOAT_4_8 arrayOfFloat3[myElementCount2], arrayOfFloat4[strideElementCount]; 
  FLOAT_4_8 arrayOfFloat5[myElementCount2], arrayOfFloat6[myElementCount2], arrayOfFloat7[myElementCount2];

  float arrayOfFloat1_f[myElementCount2], arrayOfFloat2_f[strideElementCount]; 
  float arrayOfFloat3_f[myElementCount2], arrayOfFloat4_f[strideElementCount]; 
  float arrayOfFloat5_f[myElementCount2], arrayOfFloat6_f[myElementCount2];

  INT_32 arrayOfInt1[myElementCount2], arrayOfInt2[myElementCount2];
  word arrayOfInt3[myElementCount2], arrayOfInt4[myElementCount2];
  word arrayOfInt5[myElementCount2], arrayOfInt6[myElementCount2], arrayOfInt7[myElementCount2];
  


  int bitSizeOfInt;
  int mBitSizeOfInt;
  int off_set = 1;
  int stride = 2;
  int opCode = FLOAT_PACK;
  int opCode1 = FLOAT_UNPACK; 
  ftnword *unpackedFloat1, *unpackedFloat2, *unpackedFloat3, *unpackedFloat4,*unpackedFloat5, *unpackedFloat6;

  FLOAT_4_8 missingTag = 9999.0000;
  float float_missingTag = 9999.0000;
  FLOAT_4_8 tempFloat;
  FLOAT_4_8 maxError, maxDifference;
  int notStablized = 0;
  int strideNotWorking;


  setvbuf( stdout, NULL, _IONBF, 0 );
  bitSizeOfInt  = okBit;
  mBitSizeOfInt = -okBit;






if ( stabilityOn == 1 )
{

  /****************************************************
    stability test
    **************************************************/
  
  printf("\n stability test on double starts\n");

  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfFloat2[0] = 0.0;
  arrayOfFloat2[1] = 0.0;
  arrayOfFloat3[0] = 0.0;
  arrayOfFloat3[1] = 0.0;
  arrayOfFloat4[0] = 0.0;
  arrayOfFloat4[1] = 0.0;
  arrayOfInt1[0] = 0;
  arrayOfInt1[1] = 0;
  arrayOfInt2[0] = 0;
  arrayOfInt2[1] = 0;
  arrayOfInt3[0] = 0;
  arrayOfInt3[1] = 0;
 
  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat2[i] = 0.0;
      arrayOfFloat3[i] = 0.0;
      arrayOfFloat4[i] = 0.0;
      arrayOfInt1[i] = 0;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };


  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);


#if defined (NEC64)
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
#else
  unpackWrapper(arrayOfFloat2_f, arrayOfInt1, arrayOfInt1, 1, &missingTag);
  for ( i = 0; i < elementCount; i++)
    {
     arrayOfFloat2[i] = arrayOfFloat2_f[i];  
    };
#endif


  unpackedFloat3 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt2[0], &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);



#if defined (NEC64)  
  unpackedFloat4 = compact_FLOAT_4_8(arrayOfFloat3, &arrayOfInt2[0], &arrayOfInt2[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
#else 
unpackWrapper(arrayOfFloat3_f, arrayOfInt2, arrayOfInt2, 1, &missingTag);
  for ( i = 0; i < elementCount; i++)
    {
     arrayOfFloat3[i] = arrayOfFloat3_f[i];  
    };
#endif

  unpackedFloat5 = compact_FLOAT_4_8(arrayOfFloat3, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);


#if defined (NEC64)  
  unpackedFloat6 = compact_FLOAT_4_8(arrayOfFloat4, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
#else 
  unpackWrapper(arrayOfFloat4_f, arrayOfInt3, arrayOfInt3, 1, &missingTag);
  for ( i = 0; i < elementCount; i++)
    {
     arrayOfFloat4[i] = arrayOfFloat4_f[i];  
    };
#endif            

  /*
  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);



  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);



  unpackedFloat3 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt2[0], &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);



  
  unpackedFloat4 = compact_FLOAT_4_8(arrayOfFloat3, &arrayOfInt2[0], &arrayOfInt2[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
 


  unpackedFloat5 = compact_FLOAT_4_8(arrayOfFloat3, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);



  unpackedFloat6 = compact_FLOAT_4_8(arrayOfFloat4, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
                                 */
  notStablized = 0;
 
  for ( i = 0; i < elementCount; i++)
  {
    if (( arrayOfInt1[i] != arrayOfInt2[i] ) || ( arrayOfInt1[i] != arrayOfInt3[i] ))
      {
        /*
        notStablized = 1;
        */
        printf(" %d \t %8.8x \t %8.8x \t %8.8x  \n", i, arrayOfInt1[i], 
               arrayOfInt2[i], arrayOfInt3[i]); 
               
      }
  };
  /*
  printf("\n i, arrayOfFloat1[i], arrayOfFloat2[i], arrayOfFloat3[i], arrayOfFloat4[i] \n");
  */
  for ( i = 0; i < elementCount; i++)
  {
    
    if (( arrayOfFloat2[i] == arrayOfFloat3[i] ) && ( arrayOfFloat2[i] == arrayOfFloat4[i] ))
      {
        /* result is stablized */
      }
    else /* instablility resulted */
      {
        
        printf(" %d \t %f \t %f \t %f \t %f \n", i, arrayOfFloat1[i], arrayOfFloat2[i], 
               arrayOfFloat3[i], arrayOfFloat4[i]);
               
        notStablized = 1;
      };
  };
  
  if (notStablized)
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
      
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
      
    };

  printf("\n stability test end\n");
  printf("\n\n\n\n\n ================================================================== \n\n\n\n\n");
  

};



if ( strideTestOn == 1 )
{

  /**************************************
    test of stride
    *************************************/

  printf("\n stride test on double starts\n");
  stride = 3;

  arrayOfFloat[0] = 0.0;
  arrayOfFloat[1] = arrayOfFloat[0];
  arrayOfFloat[2] = arrayOfFloat[0];

  arrayOfFloat[stride] = 0.999999;
  arrayOfFloat[stride+1] = arrayOfFloat[stride];
  arrayOfFloat[stride+2] = arrayOfFloat[stride];
  
  for ( i = 2*stride; i < strideElementCount ; i+=stride )
    { 
      arrayOfFloat[i] = ((i/stride+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat[i+1] = arrayOfFloat[i];
      arrayOfFloat[i+2] = arrayOfFloat[i];
    };
  

  
  for ( i = 0; i < 21; i++ )
    {
      arrayOfInt1[i] = 0;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };
  


  unpackedFloat1 = compact_FLOAT_4_8(&arrayOfFloat[0], &arrayOfInt1[0], &arrayOfInt1[4],elementCount, 
                                 bitSizeOfInt, 0, stride, 1, 0, &missingTag);
                                

  unpackedFloat2 = compact_FLOAT_4_8(&arrayOfFloat[1], &arrayOfInt2[0], &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, stride, 1, 0, &missingTag);
  unpackedFloat3 = compact_FLOAT_4_8(&arrayOfFloat[2], &arrayOfInt3[0], &arrayOfInt3[4],elementCount, 
                                 bitSizeOfInt, 0, stride, 1, 0, &missingTag);
 


#if defined (NEC64)
  unpackedFloat4 = compact_FLOAT_4_8(&arrayOfFloat4[0],  &arrayOfInt1[0], &arrayOfInt1[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
  unpackedFloat5 = compact_FLOAT_4_8(&arrayOfFloat4[1],  &arrayOfInt2[0], &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
  unpackedFloat6 = compact_FLOAT_4_8(&arrayOfFloat4[2],  &arrayOfInt3[0], &arrayOfInt3[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
#else
  unpackWrapper(&arrayOfFloat4_f[0],  arrayOfInt1, arrayOfInt1, stride, &missingTag);
  unpackWrapper(&arrayOfFloat4_f[1],  arrayOfInt2, arrayOfInt2, stride, &missingTag);
  unpackWrapper(&arrayOfFloat4_f[2],  arrayOfInt3, arrayOfInt3, stride, &missingTag);
  for ( i = 0; i < strideElementCount; i+=stride)
    {
      arrayOfFloat4[i]   = arrayOfFloat4_f[i];
      arrayOfFloat4[i+1] = arrayOfFloat4_f[i+1];
      arrayOfFloat4[i+2] = arrayOfFloat4_f[i+2];
    };
#endif 
  /*
  unpackedFloat4 = compact_FLOAT_4_8(&arrayOfFloat4[0],  &arrayOfInt1[0], &arrayOfInt1[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
                                 
  
  unpackedFloat5 = compact_FLOAT_4_8(&arrayOfFloat4[1],  &arrayOfInt2[0], &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
  unpackedFloat6 = compact_FLOAT_4_8(&arrayOfFloat4[2],  &arrayOfInt3[0], &arrayOfInt3[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
                                 */                            
 
  strideNotWorking = 0;

  for ( i = 0; i < elementCount ; i++ )
    {
      if ((arrayOfInt1[i] == arrayOfInt2[i] ) && ( arrayOfInt1[i] == arrayOfInt3[i] ) )
        {
          /* stride works in the packed side */
        }
      else
        {
          strideNotWorking = 1;
          
        };
    };


  for ( i = 0; i < strideElementCount ; i+=stride )
    { 
      if ((arrayOfFloat4[i] == arrayOfFloat4[i+1] ) && ( arrayOfFloat4[i] == arrayOfFloat4[i+2] ) ) 
        {
          /* stride works in the unpacked side */
        }
      else
        {
          strideNotWorking = 1;
              
        }
    }
  if (strideNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");

    };


  printf("\n stride test on double end\n");
  printf("\n\n\n\n\n ========================================================  \n\n\n\n\n");

};







if ( errorTestOn  == 1 )
{

  /**********************
    error test
    ********************/
  bitSizeOfInt = 4;
  printf("\n error test starts on double with bitSizeOfInt : %d\n", bitSizeOfInt);

  arrayOfFloat[0] = 0.0;
  arrayOfFloat[1] = 0.999999;

  for ( i = 2; i < elementCount; i++ )
    {
      arrayOfFloat[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat4[i] = 0.0;
      arrayOfFloat5[i] = 0.0;
      arrayOfInt1[i] = 0;
      arrayOfInt2[i] = 0;
    };

  for ( i = 0; i < elementCount; i++ )
    {
      arrayOfFloat4[i] = 0.0;
      arrayOfFloat5[i] = 0.0;
      arrayOfInt1[i] = 0;
      arrayOfInt2[i] = 0;
    };

  
  arrayOfFloat[19] = missingTag;
  arrayOfFloat[15] = 7.5; 
  unpackedFloat1 = compact_FLOAT_4_8(&arrayOfFloat[0],  arrayOfInt1, &arrayOfInt1[4],elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 1, &missingTag );
#if defined (NEC64)
  unpackedFloat4 = compact_FLOAT_4_8(&arrayOfFloat4[0],  arrayOfInt1, &arrayOfInt1[4],elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 1, &missingTag );
#else
  unpackWrapper(arrayOfFloat4_f, arrayOfInt1, arrayOfInt1, 1, &float_missingTag);
  for ( i = 0; i < elementCount; i++)
    {
      arrayOfFloat4[i] = arrayOfFloat4_f[i];
    };
#endif


  arrayOfFloat[15] = 6.0; 
  unpackedFloat2 = compact_FLOAT_4_8(&arrayOfFloat[0],  arrayOfInt2, &arrayOfInt2[4],elementCount, 

                                 bitSizeOfInt, 0, 1, 1, 1, &missingTag );
#if defined (NEC64)                   
  unpackedFloat5 = compact_FLOAT_4_8(&arrayOfFloat5[0],  arrayOfInt2, &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 1, &missingTag );
#else
  
  unpackWrapper(arrayOfFloat5_f, arrayOfInt2, arrayOfInt2, 1, &float_missingTag);  
  
  for ( i = 0; i < elementCount; i++)
    {
      arrayOfFloat5[i] = arrayOfFloat5_f[i];
    };
#endif

  powerOf2s[0] = 1.0;
  for ( i = 1; i < 32; i++)
    {
      powerOf2s[i] = 2.0 *powerOf2s[i-1];      
    };
  maxError = 8 / powerOf2s[bitSizeOfInt-1];
  maxDifference = arrayOfFloat4[0] - arrayOfFloat5[0];
  if (maxDifference < 0 )
    maxDifference = maxDifference * -1.0;
 
  for ( i = 0; i < elementCount; i++ )
    { 
      tempFloat = arrayOfFloat4[i] - arrayOfFloat5[i];
      if (tempFloat < 0 )
        tempFloat = tempFloat * -1.0;
      if ( tempFloat > maxDifference )
        {
          maxDifference = tempFloat;
        }; 
      printf( "%f \t %f \t %f \n", arrayOfFloat[i], arrayOfFloat4[i], arrayOfFloat5[i]);
    }


  if ((arrayOfFloat4[19] == 9999.0) && (arrayOfFloat5[19] == 9999.0) ) 
    {
      
     printf("\n\t\t ********** Passed ********** \n"); 
    }
  else
    {
      
      printf("\n\t\t ********** Fail ********** \n"); 
      exit ( 0 );
    };


  printf("\n error test on double end \n");
  printf("\n\n\n\n\n =============================================================== \n\n\n\n\n");
 
}/* error test */



if ( indexTestOn == 1 )
{
  /******************************
    over_under indexing test starts
    ****************************/

  printf("\n over indexing test starts on double \n" );  
  elementCount = 10;
  bitSizeOfInt = 4;
  arrayOfFloat1[0] = 0.0;
  arrayOfInt1[0]   = -1.0;
  arrayOfFloat3[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfInt1[1]   = -1.0;
  arrayOfFloat3[1] = 0.999999;


  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat3[i] = arrayOfFloat1[i];
      arrayOfInt1[i]   = -1.0;
    };  

  arrayOfFloat1[0] = 1000.0;
  arrayOfFloat1[9] = 1000.0;
  arrayOfFloat3[0] = 1000.0;
  arrayOfFloat3[9] = 1000.0;

  unpackedFloat1 = compact_FLOAT_4_8(&arrayOfFloat1[1], &arrayOfInt1[1], &arrayOfInt1[1], elementCount-2, 
                                 bitSizeOfInt, 120, 1, FLOAT_PACK, 0, &missingTag);
 
#if defined (NEC64)
  unpackedFloat2 = compact_FLOAT_4_8(&arrayOfFloat1[1], &arrayOfInt1[1], &arrayOfInt1[1], elementCount-2, 
                                 bitSizeOfInt, 120, 1, FLOAT_UNPACK, 0, &missingTag);
#else
  /*
    unpackWrapper(&arrayOfFloat1[1], &arrayOfInt1[1], &arrayOfInt1[1], 1, &missingTag);
    */
  unpackedFloat2 = compact_FLOAT_4_8(&arrayOfFloat1[1], &arrayOfInt1[1], &arrayOfInt1[1], elementCount-2, 
                                     bitSizeOfInt, 120, 1, FLOAT_UNPACK, 0, &missingTag);
#endif
  printf("\n i, originaldouble, \t packedInt, \t unpackeddouble\n");
  /*
  printf("\n i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]\n");
  */
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d  \t %f \t %8.8x \t %f \n", i,  arrayOfFloat3[i], arrayOfInt1[i], arrayOfFloat1[i]);
    /*
    printf("%d \t %8.8x \t %f \t %f \n", i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]);
    */
  };

  if ( (arrayOfFloat1[0] != arrayOfFloat3[0]) || (arrayOfFloat1[9] != arrayOfFloat3[9]) )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");

    };
  printf("\n end of over indexing test on double \n");
  printf("\n ===================================================== \n\n\n");

};

}/* end of program */














