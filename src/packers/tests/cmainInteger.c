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


#include "rmnlib.h"
#include <stdlib.h>
#include <stdio.h>

#define myElementCount 2100
#define myElementCount2 6300
#define strideElementCount 6300
#define maxBit            64


static double powerOf2s[65];

int  compact_integer( void *unpackedArrayOfInt, void *packedHeader, void *packedArrayOfInt, 
                       int elementCount, int bitSizeOfPackedToken, int off_set, 
                       int stride, int opCode);
int  compact_short( void *unpackedArrayOfShort, void *packedHeader, void *packedArrayOfInt, 
                       int elementCount, int bitSizeOfPackedToken, int off_set, 
                       int stride, int opCode);
                      

void f77name(cmainii)()
{
  int i, j, k;
  int elementCount = 8;
  int longElementCount = 40;
  int strideNotWorking;

  word arrayOfInt1[myElementCount2], arrayOfInt2[myElementCount2];
  word arrayOfInt3[myElementCount2], arrayOfInt4[myElementCount2];
  word arrayOfInt5[myElementCount2], arrayOfInt6[myElementCount2], arrayOfInt7[myElementCount2];
  unsigned short arrayOfShort1[myElementCount2], arrayOfShort3[myElementCount2];
  unsigned char arrayOfChar1[myElementCount2], arrayOfChar3[myElementCount2];  
  ftnword arrayOfFtnword1[myElementCount2], arrayOfFtnword2[myElementCount2], arrayOfFtnword3[myElementCount2];
  word arrayOfHeader[4];
  int arrayOfSignedInt1[myElementCount2], arrayOfSignedInt2[myElementCount2];
  int arrayOfSignedInt3[myElementCount2];

  /***************************
    need to switch between 4 and 32 to check its viability
    *****************************/
  int bitSizeOfInt;
  int mBitSizeOfInt;
  int off_set = 1;
  int stride = 2;
  int opCode = FLOAT_PACK;
  int opCode1 = FLOAT_UNPACK; 

  int returnBitSizeOfToken;
  int isOverIndex;
  int testNotWorking;

  word bits[maxBit]; 
  int numOfBitPerToken, randomBitReduction, shiftRequired;
  word chopOffMask;
  int minInt;
  int missingValueTag = -9999;


  setvbuf( stdout, NULL, _IONBF, 0 );
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

    

















  printf("\n ===================================================== \n");
  printf("\n unsigned test with header and varying SHIFT and STRIDE starts \n");  

  stride = 0;
  for ( k = 2; k <= 32; k++)
    {

      stride = (stride+1) % 4;
      if (stride == 0 )
        {
          stride++;
        };

      for ( i = 0; i < longElementCount*stride; i++)
        {
          arrayOfInt1[i] = -1;
          arrayOfInt2[i] = -1;
          arrayOfInt3[i] = -1;
        };

      i = 0;
      j = 0;
      while ( i < longElementCount*stride )
        {
          arrayOfInt1[i] = bits[j%(k+1)];
          i += stride;
          j++;
        };



      if ( k < 5)
        {
          randomBitReduction = 1;
        }
      else if ( k < 10 )
        {
          randomBitReduction = 2;  
        }
      else if ( k < 15 )
        {
          randomBitReduction = 3;          
        }
      else if ( k < 20 )
        {
          randomBitReduction = 8;          
        }
      else if ( k < 25 )
        {
          randomBitReduction = 12;          
        }
      else if ( k <= 32 )
        {
          randomBitReduction = 18;          
        };
      numOfBitPerToken = k - randomBitReduction;
      
     
      returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfInt2, arrayOfInt2, 
                                              longElementCount, numOfBitPerToken, 128, stride, 1);
      
      unpackWrapper(arrayOfInt3, arrayOfInt2, arrayOfInt2, stride, &missingValueTag);
      /*
      returnBitSizeOfToken = compact_integer( arrayOfInt3, arrayOfInt2, arrayOfInt2, 
                                              longElementCount, numOfBitPerToken, 128, stride, 2);    
                                              */                                      
      shiftRequired =(arrayOfInt2[0] & 0x00000fc0)>>6; 
     

      chopOffMask = (-1) << shiftRequired;

      /*
      printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");
      */
      testNotWorking = 0;
      if ( k < 32 )
        {
          minInt = 1;
        }
      else
        {
          minInt = 0;
        };
      for ( i = 0; i < longElementCount*stride; i+=stride)
        {/*
          printf("==%d \t\t %d \t\t %8.8x \t %d \t %d \n", i, 
                 arrayOfInt1[i], arrayOfInt2[i], (((arrayOfInt1[i]-1) & chopOffMask)+1), arrayOfInt3[i]);
                 */
          if ( (((arrayOfInt1[i]-minInt) & chopOffMask)+minInt) != arrayOfInt3[i])
            { /*
              printf("i: %d \n", i);
              */
              
              printf("%d \t\t %d \t\t %8.8x \t %d \t %d \n", i, 
                     arrayOfInt1[i], arrayOfInt2[i], (((arrayOfInt1[i]-minInt) & chopOffMask)+minInt), arrayOfInt3[i]);
              
              testNotWorking = 1; 
            };
            
         
        };
      if ( testNotWorking )
        {

          printf(" requiredBitSize: %d \t givenBitSize: %d \t shiftRequired: %d \t stride: %d \t ERROR \n",
                 k, numOfBitPerToken, shiftRequired, stride ); 
          exit ( 0 );
        }
      else
        {
          printf(" requiredBitSize: %d \t givenBitSiz: %d \t shiftRequired : %d \t stride: %d \n",
                 k, numOfBitPerToken, shiftRequired, stride );  

        };
    };





  printf("\n ===================================================== \n");
  printf("\n Signed test with header and varying SHIFT and STRIDE starts \n");  

  stride = 0;
  for ( k = 2; k <= 32; k++)
    {

      stride = (stride+1) % 4;
      if (stride == 0 )
        {
          stride++;
        };

      for ( i = 0; i < longElementCount*stride; i++)
        {
          arrayOfSignedInt1[i] = -1;
          arrayOfSignedInt2[i] = -1;
          arrayOfSignedInt3[i] = -1;
        };


      i = 0;
      j = 0;
      while ( i < longElementCount*stride )
        {
          arrayOfSignedInt1[i] = bits[j%(k-1)];
          i += stride;
          j++;
        };
      arrayOfSignedInt1[0] = -1;
      minInt = -1;


      if ( k < 5)
        {
          randomBitReduction = 1;
        }
      else if ( k < 10 )
        {
          randomBitReduction = 2;  
        }
      else if ( k < 15 )
        {
          randomBitReduction = 3;          
        }
      else if ( k < 20 )
        {
          randomBitReduction = 8;          
        }
      else if ( k < 25 )
        {
          randomBitReduction = 12;          
        }
      else if ( k <= 32 )
        {
          randomBitReduction = 18;          
        };
      numOfBitPerToken = k - randomBitReduction;
      
     
      returnBitSizeOfToken = compact_integer( arrayOfSignedInt1, arrayOfHeader, arrayOfSignedInt2, 
                                              longElementCount, numOfBitPerToken, 128, stride, 3);

      returnBitSizeOfToken = compact_integer( arrayOfSignedInt3, arrayOfHeader, arrayOfSignedInt2, 
                                              longElementCount, numOfBitPerToken, 128, stride, 4);    
      shiftRequired =(arrayOfHeader[0] & 0x00000fc0)>>6; 
     

      chopOffMask = (-1) << shiftRequired;

      /*
      printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");
      */
      testNotWorking = 0;

      for ( i = 0; i < longElementCount*stride; i+=stride)
        {/*
          printf("==%d \t\t %d \t\t %8.8x \t %d \t %d \n", i, 
                 arrayOfInt1[i], arrayOfInt2[i], (((arrayOfInt1[i]-1) & chopOffMask)+1), arrayOfInt3[i]);
                 */
          if ( (((arrayOfSignedInt1[i]-minInt) & chopOffMask)+minInt) != arrayOfSignedInt3[i])
            { /*
              printf("i: %d \n", i);
              */
              
              printf("%d \t\t %d \t\t %8.8x \t %d \t %d \n", i, 
                     arrayOfSignedInt1[i], arrayOfSignedInt2[i], 
                     (((arrayOfSignedInt1[i]-minInt) & chopOffMask)+minInt), arrayOfSignedInt3[i]);
              
              testNotWorking = 1; 
            };
            
         
        };
      if ( testNotWorking )
        {

          printf(" requiredBitSize: %d \t givenBitSize: %d \t shiftRequired: %d \t stride: %d \t ERROR \n",
                 k, numOfBitPerToken, shiftRequired, stride ); 
          exit ( 0 );
        }
      else
        {
          printf(" requiredBitSize: %d \t givenBitSiz: %d \t shiftRequired : %d \t stride: %d \n",
                 k, numOfBitPerToken, shiftRequired, stride );  
        };
    };




  /******************************
    bit mask test ( + )
    ****************************/
 
  printf("\n ===================================================== \n");
  printf("\n unsigned test specific to using bit mask starts \n");  
  elementCount = 6;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  arrayOfInt1[0] = 8500;
  arrayOfInt1[1] = 1945;
  arrayOfInt1[2] = -1;  
  arrayOfInt1[3] = -1;
  arrayOfInt1[4] = 270;
  arrayOfInt1[5] = 40;
  
  returnBitSizeOfToken = compact_integer( arrayOfInt1, NULL, arrayOfInt2, 
                                    elementCount, 14, 0, 1, 1);
  
  returnBitSizeOfToken = compact_integer( arrayOfInt3, NULL, arrayOfInt2, 
                                    elementCount, 14, 0, 1, 2);
  
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");
  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
    {
       if ( ( arrayOfInt1[i] != arrayOfInt3[i] ) && ( i != 2 ) && ( i != 3 ) )
         testNotWorking = 1; 
       if ( ( arrayOfInt3[2] != 16383 ) && ( arrayOfInt3[3] != 16383 ) )
         testNotWorking = 1;
      printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
    };
  
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  
    
  printf("\n test ends  \n");




  /******************************
    bit mask test ( +/- )
    ****************************/
 
  printf("\n ===================================================== \n");
  printf("\n signed test specific to using bit mask starts \n");  
  elementCount = 6;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  arrayOfInt1[0] = 8100;
  arrayOfInt1[1] = 1945;
  arrayOfInt1[2] = -1;  
  arrayOfInt1[3] = -1;
  arrayOfInt1[4] = 270;
  arrayOfInt1[5] = 40;
  
  returnBitSizeOfToken = compact_integer( arrayOfInt1, NULL, arrayOfInt2, 
                                    elementCount, 14, 0, 1, 3);
  
  returnBitSizeOfToken = compact_integer( arrayOfInt3, NULL, arrayOfInt2, 
                                    elementCount, 14, 0, 1, 4);
  
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");
  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
    {
      if ( arrayOfInt1[i] != arrayOfInt3[i] )
       testNotWorking = 1; 
      printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
    };
  
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  
    
  printf("\n test ends  \n");


  /******************************
    unsigned test ( + )
    ****************************/
 
  printf("\n ===================================================== \n");
  printf("\n unsigned test starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  
  
  arrayOfInt1[7] = 8;
  
  returnBitSizeOfToken = compact_integer( arrayOfInt1, NULL, arrayOfInt2, 
                                    elementCount, 4, 0, 1, 1);
  
  returnBitSizeOfToken = compact_integer( arrayOfInt3, NULL, arrayOfInt2, 
                                    elementCount, 4, 0, 1, 2);
  
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");
  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
  {
    if ( arrayOfInt1[i] != arrayOfInt3[i] )
      testNotWorking = 1;
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
  };
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  

  printf("\n unsigned test ends  \n");


  /******************************
    unsigned test ( + )
    ****************************/
 
  printf("\n ===================================================== \n");
  printf("\n unsigned test ( plus autodetection ) starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  
  
  arrayOfInt1[7] = 8;
  
  returnBitSizeOfToken = compact_integer( arrayOfInt1, NULL, arrayOfInt2, 
                                    elementCount, -1, 0, 1, 1);

  returnBitSizeOfToken = compact_integer( arrayOfInt3, NULL, arrayOfInt2, 
                                    elementCount, returnBitSizeOfToken, 0, 1, 2);
  
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");

  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
  {
    if ( arrayOfInt1[i] != arrayOfInt3[i] )
      testNotWorking = 1;
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
  };
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  


  printf("\n unsigned test ends  \n");



  /******************************
    genetic test( + / - )
    ****************************/
  
  printf("\n ===================================================== \n");
  printf("\n signed test ( + / - & autodetection )starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  arrayOfInt1[3] = -3;

  returnBitSizeOfToken = compact_integer( arrayOfInt1, NULL, arrayOfInt2, 
                                    elementCount, -1, 0, 1, 3);

  returnBitSizeOfToken = compact_integer( arrayOfInt3, NULL, arrayOfInt2, 
                                    elementCount, returnBitSizeOfToken, 0, 1, 4);
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");  

  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
  {
    if ( arrayOfInt1[i] != arrayOfInt3[i] )
      testNotWorking = 1;
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
  };
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  
 
  printf("\n signed test end \n");
  






  printf("\n ===================================================== \n");
  printf("\n over_under indexing test starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = -1;
      arrayOfInt3[i] = i;
    };  
 
 

  returnBitSizeOfToken = compact_integer( &arrayOfInt1[1], NULL, arrayOfInt2, 
                                    elementCount-2, 4, 0, 1, 1);

  returnBitSizeOfToken = compact_integer( &arrayOfInt1[1], NULL, arrayOfInt2, 
                                    elementCount-2, 4, 0, 1, 2);

  isOverIndex = 0;
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");  
  for ( i = 0; i < elementCount; i++)
  {
    if ( arrayOfInt3[i] != arrayOfInt1[i])
      {
        isOverIndex = 1;
      };
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt3[i], arrayOfInt2[i], arrayOfInt1[i]);
  };

  if ( isOverIndex )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };
  printf("\n over_under indexing test end   \n");






  /******************************
    genetic test( + only ) plus offset == 4
    ****************************/
  /* 
  printf("\n ===================================================== \n");
  printf("\n genetic test ( + only ) and offset == 4 starts \n");  

  elementCount = 16;
  stride       = 1 ;
  for ( i = 0; i < elementCount*stride; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfInt2, 
                                    elementCount, 4, 4, 1, 1);

  returnBitSizeOfToken = compact_integer( arrayOfInt3, arrayOfInt2, 
                                    elementCount, 4, 4, 1, 2);
  
  
  printf("\n i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3\n");
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t %8.8x \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
  };
  printf("\n end of genetic test  \n");
  */




  /******************************
    stride test( + only ) plus offset == 4
    ****************************/
  
  printf("\n ===================================================== \n");
  stride       = 3 ;
  printf("\n stride test ( offset == 4, stride == %d) starts \n", stride);  

  elementCount = 16;


  for ( i = 0; i < elementCount*stride ; i++ )  
    {
      arrayOfInt1[i] = 0;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  for ( i = 0; i < elementCount*stride ; i+=stride )
    {
      arrayOfInt1[i] = i/stride;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfHeader, arrayOfInt2, 
                                    elementCount, 4, 4, stride, 1);

  returnBitSizeOfToken = compact_integer( arrayOfInt3, arrayOfHeader, arrayOfInt2, 
                                    elementCount, 4, 4, stride, 2);
  
  printf("\n i, originalArray[i], unpackedArray[i]\n");
  
  strideNotWorking = 0;
  for ( i = 0; i < elementCount*stride; i+=stride)
  {
    printf(" %d \t %d \t\t %d \n", i, arrayOfInt1[i], arrayOfInt3[i]);
    if ( arrayOfInt3[i] != arrayOfInt1[i])
      {
        strideNotWorking = 1;
      };    
    
  };

  if ( strideNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };

  printf("\n stride test ends  \n");

  printf("\n ===================================================== \n");
  

  /******************************
    interface test
    ****************************/
  
  /*
  printf("\n interface test starts \n");  
  

  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfFtnword1[i] = i;
      arrayOfFtnword2[i] = -1;
      arrayOfFtnword3[i] = 0;
      
    };  


  {
  ftnword ftn_elementCount = elementCount;
  ftnword ftn_off_set      = 0;
  ftnword ftn_bitSizeOfInt = -4;
  ftnword ftn_nj           = 1;
  ftnword ftn_opCode       = 1;
  ftnword ftn_opCode1      = 2;
  
  f77name(iipak)(arrayOfFtnword1, arrayOfFtnword2, &ftn_elementCount, ftn_nj, 
                 &ftn_bitSizeOfInt, &ftn_off_set, &ftn_opCode);
  
  f77name(iipak)(arrayOfFtnword3, arrayOfFtnword2, &ftn_elementCount, &ftn_nj, 
                 &ftn_bitSizeOfInt,&ftn_off_set, &ftn_opCode1);
                 
  }
  
  printf("\n i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3\n");
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t %8.8x \t %8.8x \t %d \n", i, arrayOfFtnword1[i], arrayOfFtnword2[i], arrayOfFtnword3[i]);
  };
  printf("\n end of interface of  test  \n");
  
  printf("\n ===================================================== \n");
  */
  /******************************
    in place test
    ****************************/
  /*
  printf("\n ===================================================== \n");
  printf("\n inplace plus interface test starts \n");  
  
  elementCount = 16;

  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfFtnword1[i] = i;
      arrayOfFtnword2[i] = 0;
      arrayOfFtnword3[i] = 0;
    };  
  {
    ftnword ftn_elementCount = elementCount;
    ftnword ftn_off_set      = 0;
    ftnword ftn_bitSizeOfInt = -4;
    ftnword ftn_stride       = 1;
    ftnword ftn_opCode       = 1;
    ftnword ftn_opCode1      = 2;

    f77name(iipak)(arrayOfFtnword1, arrayOfFtnword2, &ftn_elementCount, &ftn_stride,
                 &ftn_bitSizeOfInt, &ftn_off_set, &ftn_opCode);
    f77name(iipak)(arrayOfFtnword1, arrayOfFtnword1, &ftn_elementCount, &ftn_stride, 
                 &ftn_bitSizeOfInt,&ftn_off_set, &ftn_opCode);
  
    printf("\n i, arrayOfFtnword1[i] \n");
    for ( i = 0; i < elementCount; i++)
      {
        printf("%d \t %8.8x \n", i, arrayOfFtnword1[i] );
      };


    f77name(iipak)(arrayOfFtnword3, arrayOfFtnword1, &ftn_elementCount, &ftn_stride, 
                   &ftn_bitSizeOfInt, &ftn_off_set, &ftn_opCode1);
    f77name(iipak)(arrayOfFtnword1, arrayOfFtnword1, &ftn_elementCount,  &ftn_stride,
                   &ftn_bitSizeOfInt, &ftn_off_set, &ftn_opCode1);

  }

  printf("\n i, arrayOfFtnword1[i], arrayOfFtnword2[i], arrayOfFtnword3\n");
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t %8.8x \t %8.8x \t %d \n", i, arrayOfFtnword1[i], arrayOfFtnword2[i], arrayOfFtnword3[i]);
  };
  printf("\n end of genetic test  \n");
  printf("\n ===================================================== \n\n\n");
  */
  


  /******************************
    unsigned test with header, no shift
    ****************************/
 
  /*
  printf("\n unsigned test with Header, no shift starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  
  
  arrayOfInt1[7] = 8;
  
  returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 4, 128, 1, 1);
  unpackWrapper(arrayOfInt3, arrayOfInt2, arrayOfInt2, 1, &missingValueTag);
 
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");
  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
  {
    if ( arrayOfInt1[i] != arrayOfInt3[i] )
      testNotWorking = 1;
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
  };
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  

  printf("\n unsigned test with Header, no shift  ends  \n");
  */

  /******************************
    signed test with Header
    ****************************/
  /*
  printf("\n ===================================================== \n");
  printf("\n signed test with Header starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  arrayOfInt1[3] = -3;

  returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 4, 128, 1, 3);
  unpackWrapper(arrayOfInt3, arrayOfInt2, arrayOfInt2, 1, &missingValueTag);

  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");  
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
  };
  printf("\n signed test with Header ends \n");


  printf("\n ===================================================== \n");
  
  printf("\n unsigned test with Header, shift by one,  starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  
  
  arrayOfInt1[7] = 8;
  
  returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 3, 128, 1, 1);
 
  returnBitSizeOfToken = compact_integer( arrayOfInt3, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 3, 128, 1, 2);
  
  testNotWorking = 0;
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
    if ( (arrayOfInt1[i]&0x0000000e) != arrayOfInt3[i])
      {
        testNotWorking = 1;
      };
  };

  if ( testNotWorking )
    {
      printf("\n TEST FAILED \n");
    }
  else
    {
      printf("\n TEST PASSED \n");
    };
  printf("\n unsigned test with Header, shift by one,  ends  \n");

  */

  /******************************
    signed test with Header
    ****************************/
  /*
  printf("\n ===================================================== \n");
  printf("\n signed test with Header, shift by 1,  starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  arrayOfInt1[3] = -3;

  returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 3, 128, 1, 3);

  returnBitSizeOfToken = compact_integer( arrayOfInt3, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 3, 128, 1, 4);
  
  testNotWorking = 0;
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");  
  for ( i = 0; i < elementCount; i++)
  {
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
    if ( (((arrayOfInt1[i]+3)&0x0000000e)-3) != arrayOfInt3[i])
      {
        testNotWorking = 1;
      };
  };

  if ( testNotWorking )
    {
      printf("\n TEST FAILED \n");
    }
  else
    {
      printf("\n TEST PASSED \n");
    };


  printf("\n signed test with Header, shift by 1,  ends \n");


  */
  /******************************
    boundary test with Header
    ****************************/
 

  printf("\n boundary signed test with Header starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfInt1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfInt3[i] = 0;
    };  

  arrayOfInt1[3] = 0x7fffffff;
  arrayOfInt1[4] = -1*arrayOfInt1[3];
  returnBitSizeOfToken = compact_integer( arrayOfInt1, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 32, 128, 1, 3);

  returnBitSizeOfToken = compact_integer( arrayOfInt3, arrayOfInt2, arrayOfInt2, 
                                    elementCount, 32, 128, 1, 4);
  
  printf("\n i, originalArray[i] \t\t packedArray[i] \t unpackedArray[i]\n"); 
 
  testNotWorking = 0;
  for ( i = 0; i < elementCount*2; i++)
  {
    if ( arrayOfInt1[i] != arrayOfInt3[i] )
      testNotWorking = 1;
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfInt1[i], arrayOfInt2[i], arrayOfInt3[i]);
  };
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  

  printf("\n boundary signed test with Header ends \n");
  printf("\n ============================================\n\n\n\n");

  printf("\n ===================================================== \n");
  printf("\n unsigned short test ( plus autodetection ) starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfShort1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfShort3[i] = 0;
    };  
  
  arrayOfShort1[7] = 8;
  
  returnBitSizeOfToken = compact_short( arrayOfShort1, NULL, arrayOfInt2, 
                                    elementCount, 16, 0, 1, 5);
  printf("\n  returnBitSizeOfToken = %d\n", returnBitSizeOfToken);                                

  returnBitSizeOfToken = compact_short( arrayOfShort3, NULL, arrayOfInt2, 
                                    elementCount, returnBitSizeOfToken, 0, 1, 6);
  
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");

  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
  {
    if ( arrayOfShort1[i] != arrayOfShort3[i] )
      testNotWorking = 1;
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfShort1[i], arrayOfInt2[i], arrayOfShort3[i]);
  };
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
     /* exit ( 0 ); */
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  


  printf("\n unsigned short test ends  \n");
  
  printf("\n ===================================================== \n");
  printf("\n unsigned char test ( plus autodetection ) starts \n");  
  elementCount = 8;
  for ( i = 0; i < elementCount ; i++ )
    {
      arrayOfChar1[i] = i;
      arrayOfInt2[i] = 0;
      arrayOfChar3[i] = 0;
    };  
  
  arrayOfChar1[7] = 8;
  
  returnBitSizeOfToken = compact_char( arrayOfChar1, NULL, arrayOfInt2, 
                                    elementCount, 8, 0, 1, 9);
  printf("\n  returnBitSizeOfToken = %d\n", returnBitSizeOfToken);                                

  returnBitSizeOfToken = compact_char( arrayOfChar3, NULL, arrayOfInt2, 
                                    elementCount, returnBitSizeOfToken, 0, 1, 10);
  
  
  printf("\n i, originalArray[i], packedArray[i], unpackedArray[i]\n");

  testNotWorking = 0;
  for ( i = 0; i < elementCount; i++)
  {
    if ( arrayOfChar1[i] != arrayOfChar3[i] )
      testNotWorking = 1;
    printf("%d \t\t %d \t %8.8x \t %d \n", i, arrayOfChar1[i], arrayOfInt2[i], arrayOfChar3[i]);
  };
  if ( testNotWorking )
    {
      printf("\n\t\t ********** Fail ********** \n");
      exit ( 0 );
    }
  else
    {
      printf("\n\t\t ********** Passed ********** \n");
    };  


  printf("\n unsigned char test ends  \n");

}/* end of program */














