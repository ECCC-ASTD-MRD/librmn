#ifdef WIN32
/*------------------------------------------------------------------
*  This function approximates the UNIX truncate function declared
* in <unistd.h>
*/
#include <windows.h>
void truncate(const char *filename, long size)
{
	HANDLE f = CreateFile(filename, GENERIC_WRITE,
				0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if(f == INVALID_HANDLE_VALUE)
		return;

	SetFilePointer(f, size, NULL, FILE_BEGIN);

	if(GetLastError() == NO_ERROR)
		SetEndOfFile(f);

	CloseHandle(f);
}
#endif

