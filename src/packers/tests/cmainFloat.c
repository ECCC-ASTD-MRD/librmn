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
#include "mymacrosFloat.h"
#define myElementCount 21
#define myElementCount2 72
#define myElementCount3 80
#define strideElementCount 63
#define veryLargeElementCount 194305
static int elementCount = myElementCount;

static double powerOf2s[33];


void f77name(cmain)()
{
  int i;
  int position_i;

  FLOAT_4_8 arrayOfFloat0[myElementCount];
  FLOAT_4_8 arrayOfFloat[myElementCount2], arrayOfFloatTest[strideElementCount];
  FLOAT_4_8 arrayOfFloat1[myElementCount2], arrayOfFloat2[myElementCount2]; 
  FLOAT_4_8 arrayOfFloat3[myElementCount2], arrayOfFloat4[myElementCount2]; 
  FLOAT_4_8 arrayOfFloat5[myElementCount2], arrayOfFloat6[myElementCount2], arrayOfFloat7[myElementCount2];
 
#if !defined (NEC)

  float arrayOfFloat1_large[veryLargeElementCount], arrayOfFloat2_large[veryLargeElementCount];
  word  arrayOfInt1_large[veryLargeElementCount];
  
#endif

  INT_32 arrayOfInt1[myElementCount3], arrayOfInt2[myElementCount3];
  word arrayOfInt3[myElementCount3], arrayOfInt4[myElementCount3];
  word arrayOfInt5[myElementCount3], arrayOfInt6[myElementCount3], arrayOfInt7[myElementCount3];
  

  /***************************
    need to switch between 4 and 32 to check its viability
    *****************************/
  int bitSizeOfInt;
  int mBitSizeOfInt;
  int off_set = 1;
  int stride = 2;
  int opCode = FLOAT_PACK;
  int opCode1 = FLOAT_UNPACK; 
  ftnword *unpackedFloat1, *unpackedFloat2, *unpackedFloat3, *unpackedFloat4,*unpackedFloat5, *unpackedFloat6;

  FLOAT_4_8 missingTag = 9999.0000;
  FLOAT_4_8 tempFloat;
  FLOAT_4_8 maxError, maxDifference;
  int notStablized = 0;
  int strideNotWorking;
  int testNotWorking;

  int myI;
  int ii;
  int myJ;
  int myK;
  float mySeed;
  int j, k;



  setvbuf( stdout, NULL, _IONBF, 0 );
  bitSizeOfInt  = okBit;
  mBitSizeOfInt = -okBit;

if ( geneticOn == 1 )
{

  /******************************
    genetic test
    ****************************/
  printf("\n ===================================================== \n");
  printf("\n genetic test starts at token size: %d  \n", bitSizeOfInt );  
  /*
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  
  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
    };
  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[3], elementCount, 
                                 bitSizeOfInt, 24, 1, FLOAT_PACK, 0, &missingTag);
  
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[3], elementCount, 
                                 bitSizeOfInt, 24, 1, FLOAT_UNPACK, 0, &missingTag);    
                                 */                             
  
  myI = 6;
  myJ = 4;
  myK = 3;
  mySeed = 1.0;
  printf("Debug avant la boucle mySeed=%f \t 1.0=%f\n",mySeed,1.0);
  ii = 0;

  for ( i = 1; i <= myI; i++)
    {
      for ( j = 1; j <= myJ; j++)
        { 
          for ( k = 1; k <= myK; k++)
            {  
              arrayOfFloat1[ii] = mySeed*1000 + i*100 + j*10 + k;
              mySeed = mySeed + 1;
              ii++;
            };
        };  
    };
   
  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[3], myElementCount2, 
                                     bitSizeOfInt, 24, 1, FLOAT_PACK, 0, &missingTag);
  
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[3], myElementCount2, 
                                     bitSizeOfInt, 24, 1, FLOAT_UNPACK, 0, &missingTag);        
                                     
  

                                 
  printf("\n i, originalFloat, \t packedInt, \t unpackedFloat\n");
  /*
  printf("\n i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]\n");
  */
  for ( i = 0; i < myElementCount2; i++)
  {
    printf("%d  \t %f \t %8.8x \t %f \n", i,  arrayOfFloat1[i], arrayOfInt1[i], arrayOfFloat2[i]);
    /*
    printf("%d \t %8.8x \t %f \t %f \n", i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]);
    */
  };
  printf("\n ********** for visual verification purpose only ************* \n");
  printf("\n end of genetic test  \n");
  printf("\n ===================================================== \n\n\n");

};


if ( geneticOn == 1 )
{

  int myTempElementCount   = 7;
  int myTempBitSizeOfToken = 16;
  int myTempOffset         = 120;
  int myTempStride         = 1;
  int packCode             = FLOAT_PACK;
  int unpackCode           = FLOAT_UNPACK;
  
  
  /******************************
    16 bitSizeOfToken with odd count of floating point test
    ****************************/
 
 
  printf("\n ===================================================== \n");
  printf("\n test starts at token size: 16  and elementCount: 4 stride: 2\n");  
 
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfFloat2[0] = -1.0;
  arrayOfFloat2[1] = -1.0;
  arrayOfInt1[0] = -1.0;
  arrayOfInt1[1] = -1.0;
  
  for ( i = 2; i < 7 ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfInt1[i] = -1.0;
      arrayOfFloat2[i] = -1.0;
    };

  arrayOfFloat1[7] = 1000.0;
  arrayOfFloat1[8] = 1000.0;
  arrayOfFloat2[7] = 1000.0;
  arrayOfFloat2[8] = 1000.0;
  arrayOfInt1[7] = - 1.0;
  arrayOfInt1[8] = - 1.0;
  

                
  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[3], 4, 
                                     16, 24, 2, FLOAT_PACK, 0, &missingTag);

  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[3], 4, 
                                     16, 24, 2, FLOAT_UNPACK, 0, &missingTag);    
                                                           
 

                                 
  printf("\n i, originalFloat, \t packedInt, \t unpackedFloat\n");

  for ( i = 0; i < 9; i++)
  {
    printf("%d  \t %f \t %8.8x \t %f \n", i,  arrayOfFloat1[i], arrayOfInt1[i], arrayOfFloat2[i]);

  };

  if ( (arrayOfFloat2[1] == -1.0) && (arrayOfFloat2[3] == -1.0) && (arrayOfFloat2[5] == -1.0) &&
       (arrayOfFloat1[7] == arrayOfFloat2[7]) && (arrayOfFloat2[7] == 1000.0) )
    {
      printf("\n ****************** Passed ****************\n");
    }
  else
    {
      printf("\n ****************** Failed ****************\n");  
      exit ( 0 );
    };

  printf("\n end of test  \n");
  printf("\n ===================================================== \n\n\n");

};


if ( token4BitTestOn == 1 )
{
  /******************************
    4 bit token test
    ****************************/

  printf("\n 4 bits token test starts \n");  
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfFloat2[0] = 0.0;
  arrayOfFloat2[1] = 0.0;
  arrayOfInt1[0] = 0;
  arrayOfInt1[1] = 0;

  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat2[i] = 0.0;
      arrayOfInt1[i] = 0;
    };  

  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 4, 0, 1, FLOAT_PACK, 0, &missingTag);
  
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 4, 0, 1, FLOAT_UNPACK, 0, &missingTag);
  printf("\n i, originalFloat, \t packedInt, \t unpackedFloat\n");
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d  \t %f \t %8.8x \t %f \n", i,  arrayOfFloat1[i], arrayOfInt1[i], arrayOfFloat2[i]);
  };
  printf("\n end of 4 bits token test \n");
  printf("\n ===================================================== \n\n\n");

};


if ( token16BitTestOn == 1  )
{

  printf("\n 16 bit test starts\n");  
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfFloat2[0] = 0.0;
  arrayOfFloat2[1] = 0.999999;
  arrayOfFloat3[0] = 0.0;
  arrayOfFloat3[1] = 0.999999;

  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat2[i] = arrayOfFloat1[i];
      arrayOfFloat3[i] = arrayOfFloat1[i];
    };  


  
  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 16, 12, 1, FLOAT_PACK, 0, &missingTag);
 
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat4, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 16, 12, 1, FLOAT_UNPACK, 0, &missingTag);  



  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt2[0], &arrayOfInt2[4], elementCount, 
                                 16, 0, 1, FLOAT_PACK, 0, &missingTag);
 
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat5, &arrayOfInt2[0], &arrayOfInt2[4], elementCount, 
                                 16, 0, 1, FLOAT_UNPACK, 0, &missingTag);  

                               
 
  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 16, 16, 1, FLOAT_PACK, 0, &missingTag);
  
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat6, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 16, 16, 1, FLOAT_UNPACK, 0, &missingTag);  
  
                                 
  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat3, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 16, 20, 1, FLOAT_PACK, 0, &missingTag);
  
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat7, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 16, 20, 1, FLOAT_UNPACK, 0, &missingTag);  
                               
  printf("\n offset = \t12 \t\t 0 \t\t 16 \t\t 20 \n");
  printf("\n i, arrayOfFloat4[i], arrayOfFloat5[i], arrayOfFloat6[i], arrayOfFloat7[i]\n");
  for ( i = 0; i < elementCount; i++)
  {
    if ( ( arrayOfFloat4[i] == arrayOfFloat5[i] ) && 
         ( arrayOfFloat4[i] == arrayOfFloat6[i] ) && 
         ( arrayOfFloat4[i] == arrayOfFloat7[i] ) )
      {
        /* everything is fine */
        printf(" %d \t %f \t same_value\n", i, arrayOfFloat4[i]); 
      }
    else
      {
        printf("%d \t %f \t %f \t %f \t %f \n", i, arrayOfFloat4[i], arrayOfFloat5[i], 
               arrayOfFloat6[i], arrayOfFloat7[i]);
      };

  };

  printf("\n end of 16 bit test \n");
  printf("\n ===================================================== \n\n\n");

};


if ( token24BitTestOn == 1 )
{
  /******************************
    24 bit token test
    ****************************/

  printf("\n 24 bits token test starts \n");  
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfFloat2[0] = 0.0;
  arrayOfFloat2[1] = 0.0;
  arrayOfInt1[0] = 0;
  arrayOfInt1[1] = 0;

  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat2[i] = 0.0;
      arrayOfInt1[i] = 0;
    };  

  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 24, 0, 1, FLOAT_PACK, 0, &missingTag);
  
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 24, 0, 1, FLOAT_UNPACK, 0, &missingTag);
  printf("\n i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]\n");
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t %8.8x \t %f \t %f \n", i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]);
  };
  printf("\n end of 24 bits token test \n");
  printf("\n ===================================================== \n\n\n");

};




if ( token32BitTestOn == 1 )
{
  /******************************
    32 bit token test
    ****************************/

  printf("\n 32 bits token test starts \n");  
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfFloat2[0] = 0.0;
  arrayOfFloat2[1] = 0.0;
  arrayOfInt1[0] = 0;
  arrayOfInt1[1] = 0;

  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat2[i] = 0.0;
      arrayOfInt1[i] = 0;
    };  

  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 32, 0, 1, FLOAT_PACK, 0, &missingTag);
  
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 32, 0, 1, FLOAT_UNPACK, 0, &missingTag);
  printf("\n i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]\n");
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t %8.8x \t %f \t %f \n", i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]);
  };
  printf("\n end of 32 bits token test \n");
  printf("\n ===================================================== \n\n\n");

};



if ( stabilityOn == 1 )
{
  printf("\n\n\n\n\n ===================================================== \n\n\n\n\n");
  /****************************************************
    stability test
    **************************************************/
  
  printf("\n stability test starts\n");

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
 
  bitSizeOfInt = 15;

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


  unpackWrapper(arrayOfFloat2, arrayOfInt1, arrayOfInt1, 1, &missingTag);
  /*
  unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
                                 */


  unpackedFloat3 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt2[0], &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);



  unpackWrapper(arrayOfFloat3, arrayOfInt2, arrayOfInt2, 1, &missingTag);
  /*
  unpackedFloat4 = compact_FLOAT_4_8(arrayOfFloat3, &arrayOfInt2[0], &arrayOfInt2[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
                                 */


  unpackedFloat5 = compact_FLOAT_4_8(arrayOfFloat3, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 0, &missingTag);


  unpackWrapper(arrayOfFloat4, arrayOfInt3, arrayOfInt3, 1, &missingTag);
  /*
  unpackedFloat6 = compact_FLOAT_4_8(arrayOfFloat4, &arrayOfInt3[0], &arrayOfInt3[4], elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
                                 */                             
  notStablized = 0;
  /*
  printf("\n i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i] \n");
  */
  /*
  for ( i = 0; i < elementCount; i++)
  {
    if (( arrayOfInt1[i] != arrayOfInt2[i] ) || ( arrayOfInt1[i] != arrayOfInt3[i] ))
      {
        notStablized = 1;
        
        printf(" %d \t %8.8x \t %8.8x \t %8.8x  \n", i, arrayOfInt1[i], 
               arrayOfInt2[i], arrayOfInt3[i]); 
               
      }
  };
  */
  /*
  printf("\n i, arrayOfFloat1[i], arrayOfFloat2[i], arrayOfFloat3[i], arrayOfFloat4[i] \n");
  */
  for ( i = 0; i < elementCount; i++)
  {
    
    if (( arrayOfFloat2[i] == arrayOfFloat3[i] ) && ( arrayOfFloat3[i] == arrayOfFloat4[i] ))
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
  if ( notStablized )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };    


  printf("\n stability test on float end \n");
  printf("\n\n\n\n\n ================================================================== \n\n\n\n\n");
  

};



if ( strideTestOn == 1 )
{

  /**************************************
    test of stride
    *************************************/
  printf("\n ================================================================== \n"); 
  printf("\n stride test on float starts\n");
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
  
  unpackWrapper(&arrayOfFloat4[0], arrayOfInt1, arrayOfInt1, stride, &missingTag);
  unpackWrapper(&arrayOfFloat4[1], arrayOfInt2, arrayOfInt2, stride, &missingTag);
  unpackWrapper(&arrayOfFloat4[2], arrayOfInt3, arrayOfInt3, stride, &missingTag);  
  /*
  unpackedFloat4 = compact_FLOAT_4_8(&arrayOfFloat4[0],  &arrayOfInt1[0], &arrayOfInt1[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
  unpackedFloat5 = compact_FLOAT_4_8(&arrayOfFloat4[1],  &arrayOfInt2[0], &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
  unpackedFloat6 = compact_FLOAT_4_8(&arrayOfFloat4[2],  &arrayOfInt3[0], &arrayOfInt3[4],elementCount, 
                                 bitSizeOfInt, 0, stride, FLOAT_UNPACK, 0, &missingTag);
                                 */                            
 
  strideNotWorking = 0;
  /*
  printf("\n i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i] \n");
  */
  for ( i = 0; i < elementCount ; i++ )
    {
      if ((arrayOfInt1[i] == arrayOfInt2[i] ) && ( arrayOfInt1[i] == arrayOfInt3[i] ) )
        {
          /* stride works in the packed side */
        }
      else
        {
          strideNotWorking = 1;
          /*
          printf( "%d \t %8.8x \t %8.8x \t %8.8x\n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
          */
        };
    };

    

  /*
  printf("\n i, arrayOfFloat[i], arrayOfFloat4[i], arrayOfFloat4[i+1], arrayOfFloat4[i+2] \n");
  */
  for ( i = 0; i < strideElementCount ; i+=stride )
    { 
      if ((arrayOfFloat4[i] == arrayOfFloat4[i+1] ) && ( arrayOfFloat4[i] == arrayOfFloat4[i+2] ) ) 
        {
          /* stride works in the unpacked side */
        }
      else
        {
          strideNotWorking = 1;
          /*      
          printf(" %d \t %f \t %f \t %f \t %f \n", i, arrayOfFloat[i], arrayOfFloat4[i], 
                 arrayOfFloat4[i+1], arrayOfFloat4[i+2]);
                 */     
        }
    }
  if ( strideNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };    



  printf("\n stride test on float end\n");
  printf("\n\n\n\n\n ========================================================  \n\n\n\n\n");

};







if ( errorTestOn  == 1 )
{

  /**********************
    error test
    ********************/
  bitSizeOfInt = 4;
  printf("\n error test starts on float with bitSizeOfInt : %d\n\n", bitSizeOfInt);

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

  unpackWrapper(arrayOfFloat4, arrayOfInt1, arrayOfInt1, 1, &missingTag);
  /*
  unpackedFloat4 = compact_FLOAT_4_8(&arrayOfFloat4[0],  arrayOfInt1, &arrayOfInt1[4],elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 1, &missingTag );
                                 */
  arrayOfFloat[15] = 6.0; 
  unpackedFloat2 = compact_FLOAT_4_8(&arrayOfFloat[0],  arrayOfInt2, &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, 1, 1, 1, &missingTag );


  unpackWrapper(arrayOfFloat5, arrayOfInt2, arrayOfInt2, 1, &missingTag);
  /*
  unpackedFloat5 = compact_FLOAT_4_8(&arrayOfFloat5[0],  arrayOfInt2, &arrayOfInt2[4],elementCount, 
                                 bitSizeOfInt, 0, 1, FLOAT_UNPACK, 1, &missingTag );
                                 */
  
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
 /*
 printf(" Max_Allowable_Error, Max_Differnece ( Valid only WHEN packedIntTokenSize = 4 )\n");
 printf(" %f \t      %f \n", maxError, maxDifference );
 */

 if ((arrayOfFloat4[19] == 9999.0) && (arrayOfFloat5[19] == 9999.0) ) 
   {
     printf("\n\t\t ********** Passed ********** \n"); 
               
   }
 else
   {
     printf("\n\t\t ********** Fail ********** \n"); 
     exit ( 0 );
   };


  printf("\n end of error test on float \n");
  printf("\n\n\n\n\n =============================================================== \n\n\n\n\n");
 
}/* error test */












if ( inPlaceTestOn == 1 )
{
  /**********************************
    in place test
    **********************************/

  printf("\n start of in place test, plus interface test  \n");


  if ( testOfDoubleInPlace == 1 )
    {
      opCode = 5;
      opCode1 = 6;
    }
  else
    {
      opCode = 1;
      opCode1 = 2;
    };
  stride = 1;

  arrayOfFloat0[0] = 0.0;
  arrayOfFloat0[1] = 0.999999;
  arrayOfFloat1[0] = 0.0;
  arrayOfFloat1[1] = 0.999999;

  for ( i = 2; i < elementCount; i++ )
    {
      arrayOfFloat0[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat1[i] = arrayOfFloat0[i];
    };

  for ( i = 0; i < elementCount; i++ )
    {
      arrayOfFloat2[i] = 0.0;
      arrayOfFloat3[i] = 0.0;
      arrayOfInt1[i] = 0;
    };


{
  ftnword *ftn_arrayOfInt1;
  ftnword ftn_elementCount = elementCount;
  ftnword ftn_off_set      = off_set;
  ftnword ftn_mBitSizeOfInt= mBitSizeOfInt;
  ftnword ftn_stride       = stride;
  ftnword ftn_opCode       = opCode;
  ftnword ftn_opCode1      = opCode1;

  ftn_arrayOfInt1 = (ftnword *)malloc(sizeof(ftnword)*elementCount);
  for ( i = 0; i < elementCount; i++ )
    {
      ftn_arrayOfInt1[i] = arrayOfInt1[i];
    };



if ( 0 == 0 )
{   
  /* regular pack */
  f77name(xxpak)(arrayOfFloat1, ftn_arrayOfInt1, &ftn_elementCount, &ftn_off_set, 
                 &ftn_mBitSizeOfInt, &ftn_stride, &ftn_opCode);
}

if ( 0 == 0 )
{
  /* regular pack, regular unpack */
  f77name(xxpak)(arrayOfFloat3, ftn_arrayOfInt1, &ftn_elementCount, &ftn_off_set, 
                 &ftn_mBitSizeOfInt, &ftn_stride, &ftn_opCode1);
  printf("\n after regular pack/unpack \n");
}
 
if ( 0 == 0 )
{
  /* inline pack */
  f77name(xxpak)(arrayOfFloat0, arrayOfFloat0, &ftn_elementCount, &ftn_off_set, 
                 &ftn_mBitSizeOfInt, &ftn_stride, &ftn_opCode);
  printf("\n after inline pack \n");
}

if ( 0 == 0 )
{  
  /*  inline pack, regular unpack */
  f77name(xxpak)(arrayOfFloat2, arrayOfFloat0, &ftn_elementCount, &ftn_off_set, 
                 &ftn_mBitSizeOfInt, &ftn_stride, &ftn_opCode1);
}

if ( 0 == 0 )
{                              
  /* inline pack, inline unpack */
  f77name(xxpak)(arrayOfFloat0, arrayOfFloat0, &ftn_elementCount, &ftn_off_set, 
                 &ftn_mBitSizeOfInt, &ftn_stride, &ftn_opCode1);
}                 


}/* local scope */

  printf("\n i, arrayOfFloat0[i], arrayOfFloat2[i], arrayOfFloat3[i] after unpacking \n");
  for ( i = 0; i < elementCount; i++ )
    { 
      if ( ( arrayOfFloat0[i] == arrayOfFloat2[i] )&&(arrayOfFloat0[i]==arrayOfFloat3[i]))
        {
          /* inline pack successful */
        }
      else
        {
          printf(" %d \t %f \t %f \t %f \n", i, arrayOfFloat0[i], arrayOfFloat2[i], arrayOfFloat3[i]);
        };
    }  


  printf("\n end of in place test \n");
  printf("\n =============================================================== \n\n\n");


}/* inplace pack and unpack */



if ( indexTestOn == 1 )
{
  /******************************
    over_under indexing test starts
    ****************************/

  printf("\n over indexing test starts on floats with 4 bit packed token \n" );  
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

  /*
    unpackWrapper(&arrayOfFloat[1], &arrayOfInt1[1], &arrayOfInt1[1], 1, &missingTag);
  */
  
  unpackedFloat2 = compact_FLOAT_4_8(&arrayOfFloat1[1], &arrayOfInt1[1], &arrayOfInt1[1], elementCount-2, 
                                     bitSizeOfInt, 120, 1, FLOAT_UNPACK, 0, &missingTag);
                                 
  printf("\n i, originalFloat, \t packedInt, \t unpackedFloat\n");
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
  printf("\n over indexing test on float end \n");
  printf("\n ===================================================== \n\n\n");

};




if ( indexTestOn == 1 )
{
  /******************************
    over_under indexing test of 16 bits packed token starts
    ****************************/

  printf("\n over indexing test starts on floats with 16 bits packed token \n" );  
  elementCount = 10;
  bitSizeOfInt = 16;
  arrayOfFloat1[0] = 0.0;
  arrayOfInt1[0]   = -1.0;
  arrayOfFloat3[0] = -1.0;
  arrayOfFloat1[1] = 0.999999;
  arrayOfInt1[1]   = -1.0;
  arrayOfFloat3[1] = -1.0;


  for ( i = 2; i < elementCount ; i++ )
    {
      arrayOfFloat1[i] = ((i+1.0) / (elementCount + 10.0) - 0.5);
      arrayOfFloat3[i] = -1.0;
      arrayOfInt1[i]   = -1.0;
    };  

  arrayOfFloat1[0] = 1000.0;
  arrayOfFloat1[9] = 1000.0;
  arrayOfFloat1[10] = 1000.0;
  arrayOfFloat3[0] = 1000.0;
  arrayOfFloat3[9] = 1000.0;
  arrayOfFloat3[10] = 1000.0;
  arrayOfInt1[10] = - 1.0 ;
 
  unpackedFloat1 = compact_FLOAT_4_8(&arrayOfFloat1[1], &arrayOfInt1[1], &arrayOfInt1[1], elementCount-2, 
                                 bitSizeOfInt, 120, 1, FLOAT_PACK, 0, &missingTag);


  unpackedFloat2 = compact_FLOAT_4_8(&arrayOfFloat3[1], &arrayOfInt1[1], &arrayOfInt1[1], elementCount-2, 
                                     bitSizeOfInt, 120, 1, FLOAT_UNPACK, 0, &missingTag);
                                     
  printf("\n i, originalFloat, \t packedInt, \t unpackedFloat\n");
  /*
  printf("\n i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]\n");
  */
  for ( i = 0; i < elementCount+1; i++)
  {
    printf("%d  \t %f \t %8.8x \t %f \n", i,  arrayOfFloat1[i], arrayOfInt1[i], arrayOfFloat3[i]);
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
  printf("\n over indexing test on float with 16 bits packed token end \n");
  printf("\n ===================================================== \n\n\n");

};













#if !defined (NEC)

if ( largeTestOn == 1 )
{
  /******************************
    test of using the High, Low field in the header
    ****************************/
  bitSizeOfInt = 27;

  printf("\n test of using the High, low field in the header at token size: %d \t elementCount : %d\n",
         bitSizeOfInt, veryLargeElementCount );  
  arrayOfFloat1_large[0] = 0.0;
  arrayOfFloat1_large[1] = 0.999999;

  for ( i = 2; i < veryLargeElementCount ; i++ )
    {
      arrayOfFloat1_large[i] = ((i+1.0) / (veryLargeElementCount + 10.0) - 0.5);
      
     
    };  

  unpackedFloat1 = compact_FLOAT_4_8(arrayOfFloat1_large, &arrayOfInt1_large[0], &arrayOfInt1_large[4],
                                     veryLargeElementCount, bitSizeOfInt, 0, 1, FLOAT_PACK, 0, 
                                     &missingTag);
  
  unpackWrapper( arrayOfFloat2_large, arrayOfInt1_large, arrayOfInt1_large, 1, &missingTag);
  /*
    unpackedFloat2 = compact_FLOAT_4_8(arrayOfFloat2, &arrayOfInt1[0], &arrayOfInt1[4], veryLargeElementCount, 
    bitSizeOfInt, 0, 1, FLOAT_UNPACK, 0, &missingTag);
  */
  printf("\n i, originalFloat, \t packedInt, \t unpackedFloat\n");
  /*
  printf("\n i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]\n");
  */
  testNotWorking = 0;
  maxDifference = 0;
  for ( i = 1; i < veryLargeElementCount; i++)
  {
     if ( arrayOfFloat1_large[i] != arrayOfFloat2_large[i] )
        { 
	  tempFloat = arrayOfFloat1_large[i] - arrayOfFloat2_large[i];
	  if (tempFloat < 0 )
	    tempFloat = tempFloat * -1.0;
	  if ( tempFloat > maxDifference )
	    {
	      maxDifference = tempFloat;
	      position_i = i;
            printf("%d  \t %8e \t %8.8x \t %8e %8e \n", i,  arrayOfFloat1_large[i], arrayOfInt1_large[i], 
	    arrayOfFloat2_large[i],maxDifference);
	    };  
/*          printf("%d  \t %8e \t %8.8x \t %8e %8e \n", i,  arrayOfFloat1_large[i], arrayOfInt1_large[i], 
	    arrayOfFloat2_large[i],maxDifference); */
          testNotWorking = 1;    
        };
    
    /*
    printf("%d \t %8.8x \t %f \t %f \n", i, arrayOfInt1[i], arrayOfFloat1[i], arrayOfFloat2[i]);
    */
  };
  printf("MaxDifference= %8e pour i=%d \n",maxDifference,position_i);
  if (maxDifference < 1.0e-07) testNotWorking=0;
  /*
  i = veryLargeElementCount-1;


  printf("%d  \t %f \t %8.8x \t %f \n", i,  arrayOfFloat1_large[i], arrayOfInt1_large[i], 
         arrayOfFloat2_large[i]);
         */
  /*

  for ( i = 0; i < veryLargeElementCount; i++)
    {
      if ( arrayOfFloat1_large[i] != arrayOfFloat2_large[i] )
        { 
          testNotWorking = 1;
         
        };
    };
    */
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };      


  printf("\n test on using the High and Low bit field in the header ends \n");
  printf("\n ===================================================== \n\n\n");

};

#endif

}/* end of program */














