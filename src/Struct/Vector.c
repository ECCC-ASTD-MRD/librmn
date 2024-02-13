#include <math.h>
#include "Vector.h"

/**----------------------------------------------------------------------------
 * @brief  Normalizes a vector of double
 * @date   January 2000 
 *    @param[inout]   V          Vector
 */
void Vect_Normalize(Vect3d V){

   double norm;

   norm=1.0/sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);

   if (norm==0.0) {
      V[0]=V[1]=V[2]=0.0;
   } else {
      V[0]*=norm;
      V[1]*=norm;
      V[2]*=norm;
   }
}

/**----------------------------------------------------------------------------
 * @brief  Normalizes a vector of floats
 * @date   January 2000 
 *    @param[inout]   V          Vector
 */
void Vect3f_Normalize(Vect3f V){

   double norm;

   norm=1.0/sqrt(V[0]*V[0]+V[1]*V[1]+V[2]*V[2]);

   if (norm==0.0) {
      V[0]=V[1]=V[2]=0.0;
   } else {
      V[0]*=norm;
      V[1]*=norm;
      V[2]*=norm;
   }
}

/**----------------------------------------------------------------------------
 * @brief  Calculates a double vector cross product
 * @date   January 2000  
 *    @param[out]  V1           result vector
 *    @param[in]   V2           first vector
 *    @param[in]   V3           second vector
 */
void Vect_CrossProduct(Vect3d V1,Vect3d V2,Vect3d V3){

   V1[0]=V2[1]*V3[2]-V2[2]*V3[1];
   V1[1]=V2[2]*V3[0]-V2[0]*V3[2];
   V1[2]=V2[0]*V3[1]-V2[1]*V3[0];
}

/**----------------------------------------------------------------------------
 * @brief  Calculates a float vector cross product
 * @date   January 2000  
 *    @param[out]  V1           result vector
 *    @param[in]   V2           first vector
 *    @param[in]   V3           second vector
 */
void Vect3f_CrossProduct(Vect3f V1,Vect3f V2,Vect3f V3){

   V1[0]=V2[1]*V3[2]-V2[2]*V3[1];
   V1[1]=V2[2]*V3[0]-V2[0]*V3[2];
   V1[2]=V2[0]*V3[1]-V2[1]*V3[0];
}

/**----------------------------------------------------------------------------
 * @brief  Calculates vector's norm to a plan
 * @date   January 2000  
 *    @param[out]  V1           result vector
 *    @param[in]   V2           first vector
 *    @param[in]   V3           second vector
 */
void Vect_Normal(Vect3d V1,Vect3d V2,Vect3d V3){

   double norm;

   V1[0]=V2[1]*V3[2]-V2[2]*V3[1];
   V1[1]=V2[2]*V3[0]-V2[0]*V3[2];
   V1[2]=V2[0]*V3[1]-V2[1]*V3[0];

   norm=1.0/sqrt(V1[0]*V1[0]+V1[1]*V1[1]+V1[2]*V1[2]);

   V1[0]*=norm;
   V1[1]*=norm;
   V1[2]*=norm;
}

/**----------------------------------------------------------------------------
 * @brief  Calculates intersection of a vector into a plan
 * @date   January 2000  
 *    @param[in]   Dir         Vector
 *    @param[out]  Pixel       Intersection coordinate
 *    @param[in]   R           Plan distance
 * 
 *    @return                  intersection validity (no intersection:0, else:1) 
 */
int Vect_InterPlane(Vect3d Dir,Vect3d Pix,double R){

   double t;

   if (Dir[2]!=0) {
      t=(Pix[2]-R)/Dir[2];

      Pix[0]-=Dir[0]*t;
      Pix[1]-=Dir[1]*t;
    //Pix[2]-=Dir[2]*t;

      return 1;
   } else {
      Pix[0]=999.0;
      Pix[1]=999.0;
    //Pix[2]=999.0;

      return 0;
   }
}


/**----------------------------------------------------------------------------
 * @brief  Calculates intersection of a vector with a sphere
 * @date   January 2000  
 *    @param[in]   Dir         Vector
 *    @param[in]   A           Quadratic constant factor x2
 *    @param[out]  Pixel       Intersection coordinate
 *    @param[in]   R           Sphere radius
 * 
 *    @return                  intersection validity (no intersection:0, else:1) 
 */
int Vect_InterSphere(Vect3d Dir,double A,Vect3d Pix,double R){

   double b,c,t,delta;

   b=2*(Dir[0]*Pix[0]+Dir[1]*Pix[1]+Dir[2]*Pix[2]);
   c=Pix[0]*Pix[0]+Pix[1]*Pix[1]+Pix[2]*Pix[2]-R;

   delta=b*b-2*A*c;

   if (delta>0) {
      t=(b+sqrt(delta))/A;

      Pix[0]-=Dir[0]*t;
      Pix[1]-=Dir[1]*t;
    //Pix[2]-=Dir[2]*t;

      return 1;
   } else {
      Pix[0]=999.0;
      Pix[1]=999.0;
    //Pix[2]=999.0;

      return 0;
   }
}
