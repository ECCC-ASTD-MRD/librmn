#ifndef _Vector_h
#define _Vector_h

#include <sys/types.h>

//! \addtogroup genericDataStructures
//! @{

//! Get the biggest of two numbers
#define VECT_MAX(A, B) (A < B ? B : A)
//! Average two numbers
#define VECT_MID(A, B) (A + B) * 0.5
//! Get the smallest of two numbers
#define VECT_MIN(A, B) (A < B ? A : B)

//! Add 2 3D vectors
#define Vect_Add(v3dOut, v3dIn1, v3dIn2) v3dOut[0] = v3dIn1[0] + v3dIn2[0]; v3dOut[1] = v3dIn1[1] + v3dIn2[1]; v3dOut[2] = v3dIn1[2] + v3dIn2[2];
//! Assign the value of a 3D vector to another
#define Vect_Assign(v3dOut, v3dIn1) v3dOut[0] = v3dIn1[0]; v3dOut[1] = v3dIn1[1]; v3dOut[2] = v3dIn1[2];
//! Set each component of a 3D vector to 0
#define Vect_Clear(v3d) v3d[0] = 0.0; v3d[1] = 0.0; v3d[2] = 0.0;
//! Compute the dot product of 2 3D vectors
#define Vect_DotProduct(v3dIn1, v3dIn2) (v3dIn1[0] * v3dIn2[0] + v3dIn1[1] * v3dIn2[1] + v3dIn1[2] * v3dIn2[2])
//! Assign each component of a 3D vector
#define Vect_Init(v3d, I, J, K) v3d[0] = I; v3d[1] = J; v3d[2] = K;
//! Set a 3D vector to the maximum of each component of the 2 3D input vectors
#define Vect_Max(v3dOut, v3dIn1, v3dIn2) v3dOut[0] = VECT_MAX(v3dIn1[0], v3dIn2[0]); v3dOut[1] = VECT_MAX(v3dIn1[1], v3dIn2[1]); v3dOut[2] = VECT_MAX(v3dIn1[2], v3dIn2[2])
//! Set a 3D vector to the average of each component of the 2 3D input vectors
#define Vect_Mid(v3dOut, v3dIn1, v3dIn2) v3dOut[0] = VECT_MID(v3dIn1[0], v3dIn2[0]); v3dOut[1] = VECT_MID(v3dIn1[1], v3dIn2[1]); v3dOut[2] = VECT_MID(v3dIn1[2], v3dIn2[2])
//! Set a 3D vector to the minimum of each component of the 2 3D input vectors
#define Vect_Min(v3dOut, v3dIn1, v3dIn2) v3dOut[0] = VECT_MIN(v3dIn1[0], v3dIn2[0]); v3dOut[1] = VECT_MIN(v3dIn1[1], v3dIn2[1]); v3dOut[2] = VECT_MIN(v3dIn1[2], v3dIn2[2])
//! Compute the norm of the 3D vector
#define Vect_Norm(v3d) (sqrt(v3d[0] * v3d[0] + v3d[1] * v3d[1] + v3d[2] * v3d[2]))
//! Check if each component of tree the vector is equal to 0
#define Vect_Null(v3d) (v3d[0] == 0.0 && v3d[1] == 0.0 && v3d[2] == 0.0)
//! Substract v3dIn2 from v3dIn1 giving v3dOut
#define Vect_Substract(v3dOut, v3dIn1, v3dIn2) v3dOut[0] = v3dIn1[0] - v3dIn2[0]; v3dOut[1] = v3dIn1[1] - v3dIn2[1]; v3dOut[2] = v3dIn1[2] - v3dIn2[2];

#define Vect_Weight(v3dIn1, v3dIn2) VECT_MAX(VECT_MAX(fabs(v3dIn2[0] - v3dIn1[0]), fabs(v3dIn2[1] - v3dIn1[1])), fabs(v3dIn2[2] - v3dIn1[2]))
//! Check if two 3D vectors are equal
#define Vect_Equal(v3dIn1, v3dIn2) (v3dIn1[0] == v3dIn2[0] && v3dIn1[1] == v3dIn2[1] && v3dIn1[2] == v3dIn2[2])
//! Multiplied two 3D vectors
#define Vect_Mul(v3dOut, v3dIn1, v3dIn2) v3dOut[0] = v3dIn1[0] * v3dIn2[0]; v3dOut[1] = v3dIn1[1] * v3dIn2[1]; v3dOut[2] = v3dIn1[2] * v3dIn2[2]
//! Compute the squared distance between two 3D vectors
#define Vect_Dist2(v3dIn1, v3dIn2) ((v3dIn1[0] - v3dIn2[0]) * (v3dIn1[0] - v3dIn2[0]) + (v3dIn1[1] - v3dIn2[1]) * (v3dIn1[1] - v3dIn2[1]) + (v3dIn1[2] - v3dIn2[2]) * (v3dIn1[2] - v3dIn2[2]))
//! Compute the distance between two 3D vectors
#define Vect_Dist(v3dIn1, v3dIn2) sqrt(Vect_Dist2(v3dIn1, v3dIn2))

//! Add a scalar to each component of a 3D vector
#define Vect_SAdd(v3dOut, v3dIn, SC)      v3dOut[0] = v3dIn[0] + SC; v3dOut[1] = v3dIn[1] + SC; v3dOut[2] = v3dIn[2] + SC;
//! Substract a scalar from each component of a 3D vector
#define Vect_SSubstrac(v3dOut, v3dIn, SC) v3dOut[0] = v3dIn[0] - SC; v3dOut[1] = v3dIn[1] - SC; v3dOut[2] = v3dIn[2] - SC;
//! Multiply each component of a 3D vector by a scalar
#define Vect_SMul(v3dOut, v3dIn, SC)      v3dOut[0] = v3dIn[0] * SC; v3dOut[1] = v3dIn[1] * SC; v3dOut[2] = v3dIn[2] * SC;
//! Devide each component of a 3D vector by a scalar
#define Vect_SDiv(v3dOut, v3dIn, SC)      v3dOut[0] = v3dIn[0] / SC; v3dOut[1] = v3dIn[1] / SC; v3dOut[2] = v3dIn[2] / SC;

#define Vect_Interp(V, V1, V2, R) Vect_Substract(V, V2, V1); Vect_SMul(V, V, R); Vect_Add(V, V1, V)
#define Vect_InterpC(V, V1, V2, R)  Vect_Substract(V, V2, V1);V[0]=V[0]>2?-(4-V[0]):(V[0]<-2?4+V[0]:V[0]);Vect_SMul(V, V, R);Vect_Add(V, V1, V)

//! 4D vector of double
typedef double   Vect4d[4];
//! 3D vector of double
typedef double   Vect3d[3];
//! 2D vector of double
typedef double   Vect2d[2];
//! 4D vector of float
typedef float    Vect4f[4];
//! 3D vector of float
typedef float    Vect3f[3];
//! 2D vector of float
typedef float    Vect2f[2];
//! 4D vector of 32 bit integer
typedef int32_t  Vect4i[4];
//! 3D vector of 32 bit integer
typedef int32_t  Vect3i[3];
//! 2D vector of 32 bit integer
typedef int32_t  Vect2i[2];

void Vect3f_CrossProduct(Vect3f v3dOut, const Vect3f v3dIn1, const Vect3f v3dIn2);
void Vect3f_Normalize(Vect3f vect);

void Vect_Normalize(Vect3d vect);
void Vect_Normal(Vect3d v3dOut, Vect3d v3dIn1, Vect3d v3dIn2);
void Vect_CrossProduct(Vect3d v3dOut, const Vect3d v3dIn1, const Vect3d v3dIn2);
int  Vect_InterPlane(const Vect3d vect, Vect3d intersectPos, const double dist);
int  Vect_InterSphere(const Vect3d vect, const double quadConstFact, Vect3d intersectPos, const double radius);

//! @}

#endif
