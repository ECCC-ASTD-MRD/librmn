VERTICAL INTERPOLATION PACKAGE 
 
LAYOUT OF THE SOFTWARE: 
The heart of the software consists of two classes:  the verticalInterpolation class where the interpolation is actually performed (using the 1-D library), and the verticalGrid class which defines the co-ordinates for the interpolation and is input for the verticalInterpolation class.
 
Finally, there is the ViConstants_f90.h file which groups together constants of the package, including error codes. 
 
In addition to the two classes mentioned above, each of the public methods of these classes has a normal function interface to it (indicated by its 'Ifc' suffix) to facilitate library maintenance.  These Ifc functions are the ones that are publicly available. 
 
For more information, see the HTML introduction and F90 program example.  The suffixes, 'Ifc' and '_X', in that documentation are valid if the published interface (ViIfc.ftn90) is used.  The suffixes should be dropped if the vertical interpolation package is called directly. 
 
Author (code and documentation):  Jeffrey W. Blezius
