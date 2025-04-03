#include <math.h>
#include "Vector.h"


//! \file
//! Implementation of vector functions
//! \addtogroup genericDataStructures
//! @{


//! Normalize a vector of double
void Vect_Normalize(
    //! [inout] Vector
    Vect3d vect
) {
    const double norm = 1.0 / sqrt(vect[0] * vect[0] + vect[1] * vect[1] + vect[2] * vect[2]);

    if (norm == 0.0) {
        vect[0] = vect[1] = vect[2] = 0.0;
    } else {
        vect[0] *= norm;
        vect[1] *= norm;
        vect[2] *= norm;
    }
}


//! Normalize a vector of floats
void Vect3f_Normalize(
    //! [inout] Vector
    Vect3f vect
) {
    const double norm = 1.0 / sqrt(vect[0] * vect[0] + vect[1] * vect[1] + vect[2] * vect[2]);

    if (norm == 0.0) {
        vect[0] = vect[1] = vect[2] = 0.0;
    } else {
        vect[0] *= norm;
        vect[1] *= norm;
        vect[2] *= norm;
    }
}


//! Calculate a double vector cross product
void Vect_CrossProduct(
    //! [out] Result vector
    Vect3d vOut,
    //! [in] First input vector
    const Vect3d vIn1,
    //! [in] Second input vector
    const Vect3d vIn2
) {
    vOut[0] = vIn1[1] * vIn2[2] - vIn1[2] * vIn2[1];
    vOut[1] = vIn1[2] * vIn2[0] - vIn1[0] * vIn2[2];
    vOut[2] = vIn1[0] * vIn2[1] - vIn1[1] * vIn2[0];
}


//! Calculate a float vector cross product
void Vect3f_CrossProduct(
    //! [out] Result vector
    Vect3f vOut,
    //! [in] First input vector
    const Vect3f vIn1,
    //! [in] Second input vector
    const Vect3f vIn2
){
    vOut[0] = vIn1[1] * vIn2[2] - vIn1[2] * vIn2[1];
    vOut[1] = vIn1[2] * vIn2[0] - vIn1[0] * vIn2[2];
    vOut[2] = vIn1[0] * vIn2[1] - vIn1[1] * vIn2[0];
}


//! Calculate vector's norm to a plane
void Vect_Normal(
    //! [out] Result vector
    Vect3d vOut,
    //! [in] First input vector
    Vect3d vIn1,
    //! [in] Second input vector
    Vect3d vIn2
) {
    vOut[0] = vIn1[1] * vIn2[2] - vIn1[2] * vIn2[1];
    vOut[1] = vIn1[2] * vIn2[0] - vIn1[0] * vIn2[2];
    vOut[2] = vIn1[0] * vIn2[1] - vIn1[1] * vIn2[0];

    const double norm = 1.0 / sqrt(vOut[0] * vOut[0] + vOut[1] * vOut[1] + vOut[2] * vOut[2]);

    vOut[0] *= norm;
    vOut[1] *= norm;
    vOut[2] *= norm;
}


//! Calculate intersection of a vector into a plane
int Vect_InterPlane(
    //! [in] Vector
    const Vect3d vect,
    //! [inout] Intersection position
    Vect3d intersectPos,
    //! [in] Plane distance
    const double dist
) {
    //! \return 1 if the vector intersects the plane, 0 otherwise

    if (vect[2] != 0) {
        const double t = (intersectPos[2] - dist) / vect[2];

        intersectPos[0] -= vect[0] * t;
        intersectPos[1] -= vect[1] * t;

        return 1;
    } else {
        intersectPos[0] = 999.0;
        intersectPos[1] = 999.0;

        return 0;
    }
}


//! Calculate intersection of a vector with a sphere
int Vect_InterSphere(
    //! [in] Vector
    const Vect3d vect,
    //! [in] Quadratic constant factor x2
    const double quadConstFact,
    //! [inout] Intersection position
    Vect3d intersectPos,
    //! [in] Sphere radius
    const double radius
) {
    //! \return 1 if the vector intersects the sphere, 0 otherwise

    const double b = 2 * (vect[0] * intersectPos[0] + vect[1] * intersectPos[1] + vect[2] * intersectPos[2]);
    const double c = intersectPos[0] * intersectPos[0] + intersectPos[1] * intersectPos[1] + intersectPos[2] * intersectPos[2] - radius;
    const double delta = b * b - 2 * quadConstFact * c;

    if (delta > 0) {
        const double t = (b + sqrt(delta)) / quadConstFact;

        intersectPos[0] -= vect[0] * t;
        intersectPos[1] -= vect[1] * t;

        return 1;
    } else {
        intersectPos[0] = 999.0;
        intersectPos[1] = 999.0;

        return 0;
    }
}

//! @}
