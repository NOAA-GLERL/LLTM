
#  Detect the O/S and set a flag accordingly. This flag is used for
#  conditional compilation of some files
#  This is a simplification of stuff I found here:
#    http://stackoverflow.com/questions/714100/os-detecting-makefile
#
ifeq ($(OS),Windows_NT)
   OSFLAG = -D __windows__
	ThisOS := Windows
else
	UNAME_S := $(shell uname -s)
   ifeq ($(UNAME_S),linux)
      OSFLAG = -D __linux__
	endif
endif

CC = gfortran
ifeq ($(ThisOS), Windows)
  # CFLAGS = -Wall -Wno-unused-function -Wno-target-lifetime -Wno-surprising -fcheck=all -g -O1 -static
   #CFLAGS = -Wall -Wmaybe-unitialized -g -O1 -static # jak used this for testing
#   CFLAGS = -Wall -Wno-unused-function -Wno-target-lifetime -Wno-surprising -static
else
#   CFLAGS = -g -O1 -Wall
   CFLAGS = -fcheck=all -g -O1
endif


#
#   The executable
#
lltm: lltm.o gl_constants.o glshfs_util.o  glshfs_structureddata.o    \
                  glerldatatypesandunits.o cpp_util.o glshfs_global.o  \
					 dailydatastructures.o glshfs_structureddata.o
	$(CC) $(CFLAGS) -o lltm lltm.o gl_constants.o glshfs_util.o            \
				 glshfs_structureddata.o glerldatatypesandunits.o cpp_util.o  \
				 dailydatastructures.o glshfs_global.o

#
#   Specific blocks for object code
#
lltm.o: lltm.f90  gl_constants.o glshfs_util.o  glshfs_structureddata.o    \
                  glerldatatypesandunits.o cpp_util.o glshfs_global.o      \
			      dailydatastructures.o
	$(CC) $(CFLAGS) -c lltm.f90

   
#
#  The common/utility stuff (data structures, file read/write, etc.)
#  This stuff is mainly to fulfill various dependencies that LBRM has from GLSHFS.
#  Would be nice to reduce/eliminate LBRM dependencies in the future.
#
dailydatacollections.o: dailydatacollections.f90 glshfs_util.o cpp_util.o glshfs_global.o glerldatatypesandunits.o   \
                        gl_constants.o dailydatastructures.o
	$(CC) $(CFLAGS) -c dailydatacollections.f90

dailydatastructures.o: dailydatastructures.f90  glshfs_util.o cpp_util.o glshfs_global.o glerldatatypesandunits.o
	$(CC) $(CFLAGS) -c dailydatastructures.f90

gl_constants.o: gl_constants.f90
	$(CC) $(CFLAGS) $(OSFLAG) -c gl_constants.f90

glerldatatypesandunits.o: glerldatatypesandunits.f90
	$(CC) $(CFLAGS) -c glerldatatypesandunits.f90						 


glshfs_structureddata.o: glshfs_structureddata.f90  glerldatatypesandunits.o                                     
	$(CC) $(CFLAGS) -c glshfs_structureddata.f90  
#
#  Miscellaneous utility routines
#
#  Note that the source code for glshfs_util and glshfs_global include 
#  pre-processor directives that are conditional on the current O/S.  
#  The gfortran compiler will only respond to pre-processor directives if 
#  the both the file extension is F90 and the compile command specifies 
#  the file with extension of F90 (UPPERCASE F IS REQUIRED). 
#  I have seen discussions on the web that indicate this is a common
#  convention (Intel compiler, etc).
#
glshfs_util.o:  glshfs_util.F90 cpp_util.o
	$(CC) $(CFLAGS) $(OSFLAG) -c glshfs_util.F90

glshfs_global.o: glshfs_global.F90 glshfs_util.o cpp_util.o gl_constants.o
	$(CC) $(CFLAGS) $(OSFLAG) -c glshfs_global.F90





#
#  Build special C++ routines that fill gaps in Fortran functionality.
#
cpp_util.o: cpp_util.cpp
	gcc $(OSFLAG) -c -g -O1 cpp_util.cpp

#
#  clean everything up
#         
clean:
	$(RM) *.mod
	$(RM) *.o
 
