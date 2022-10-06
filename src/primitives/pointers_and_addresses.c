/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2005  Meteorological Research Branch
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

#include <stdint.h>
#include <rmn/rpnmacros.h>


//! Union allowing to store an address and return it to caller using a 64 bit
//! integer. This should work on any platform.
typedef union {
    long long address_in_64bit;
    void *ptr;
} fakeptr;

/*
 FORTRAN callable subroutines / function
 this includes a NON TRANSPARENT replacement for the LOC() extension in FORTRAN
 addresses are stored using a 64 bit integer so the interface remains the same
 on 32 bit and 64 bit systems (length of a Cray style pointer can be 32 or 64 bits)

 integer *8 address
 integer *8 get_address_from
 external get_address_from
 external some_subroutine
 real value
 real array(dimensions)
 real crayarray
 pointer(crayptr,crayarray(*))

 address=get_address_from(array) ! put address of array into a 64 bit integer
 
 call set_content_of_location(address,10,12.5)   ! set array(10) = 12.5
 call get_content_of_location(address,1,value)   ! value = array(1) 

 call make_cray_pointer(crayptr,address) ! set Cray style pointer crayptr to address of array

 call pass_address_to(address,some_subroutine) ! call some_subroutine with array as argument

 subroutine some_subroutine(array)
 real array(*)
 ...
 return
 end
*/

//! similar to the FORTRAN LOC() intrinsic
long long f77name(get_address_from)(void *addr)
{
    fakeptr myptr;

    myptr.address_in_64bit = 0;
    // Put address into union
    myptr.ptr = addr;
    // return 64 bit long long to caller
    return(myptr.address_in_64bit);
}


void f77name(make_cray_pointer)(void **addr, long long *c)
{
    fakeptr myptr;
    // Get 64 bit long long containing address from caller and store it into union
    myptr.address_in_64bit = *c;
    // Return address to caller (addr is a Cray style pointer)
    *addr = myptr.ptr;
}


void f77name(pass_address_to)(long long *c, int *funct())
{
    fakeptr myptr;

    // get long long containing address in union from caller
    myptr.address_in_64bit = *c;
    // Call call_back specified by user with address as only argument
    (void) *funct(myptr.ptr);
}


// Indexing of location is done in base 1 (Fortran like)
void f77name(set_content_of_location)(
    long long * const location,
    const int32_t * const indx,
    const int32_t * const value
) {
    typedef union {
        long long address_in_64bit;
        int32_t *ptr;
    } U_ptr;

    U_ptr myptr;

    myptr.address_in_64bit = 0;
    // Put long long contaning address into union
    myptr.address_in_64bit = *location;
    // set content of index address to value
    myptr.ptr[(*indx)-1] = *value;
}


void f77name(get_content_of_location)(long long *location, int32_t *indx, int32_t *value)
// Indexing of location is done in base 1 (Fortran like)
{
    typedef union {
    long long address_in_64bit;
    int32_t *ptr;
    } U_ptr;

    U_ptr myptr;

    myptr.address_in_64bit = 0;
    // Put long long contaning address into union
    myptr.address_in_64bit = *location;
    // get content of index address into value
    *value = myptr.ptr[(*indx)-1];
}
