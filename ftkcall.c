/* FORTRAN/TK
   Callrexx wrapping function
   (calls a function at a given offset)
   by Robin Haberkorn                   */

//Definition of the function pointer (PFN):

//#define APIENTRY  _System

#ifdef  __OS2__
#define APIENTRY __syscall
#else   __WIN__
#define APIENTRY __stdcall
#endif

typedef int (APIENTRY _PFN)();
typedef _PFN *PFN;

//typedef int (*PFN)(long, unsigned long, long, long, long);

int callrexx(PFN addr, long name, unsigned long numargs, long args, long queuename, long retstr)
{
 return((*addr)(name, numargs, args, queuename, retstr));
};
