# IMPORTANT
# Before compile, see readme.txt for instructions 


include ./makefile_config

#----Compilacao em linux/unix
BIN=./bin/
DEL=\rm
SHARED=./shared

# executavel

NC2GRIB2   =$(BIN)mpas_nc2grib2.x
TEST       =$(BIN)test.x
# Programas fontes

NC2GRIB2F =./src/mpas_nc2grib2.f90
MPASCONVF1 =./src/mpas_nc2grads.f90
GRIB_PARAMETERS=./src/mgrib_tables.f90
MGRIB      =./src/mgrib_interface.f90
MGRADSF    =$(SHARED)/grdlib/mgrads.f90
# Externos
STRINGFLIBF=$(SHARED)/f90lib/stringflib.f90
DATELIBF   =$(SHARED)/f90lib/datelib.f90
METLIB     =$(SHARED)/metlib/metlib.f90
MSORT      =$(SHARED)/f90lib/msort.f90
XMLPARSEF  =$(SHARED)/xml_fortran/xmlparse.f90
OBJ= stringflib.o datelib.o mgrads.o metlib.o mgrib_tables.o msort.o mgrib_interface.o xmlparse.o

all:   $(NC2GRIB2) #$(TEST)

$(NC2GRIB2) : $(NC2GRIB2F) $(OBJ) mgrib_interface.o msort.o stringflib.o datelib.o xmlparse.o
	mkdir -p $(BIN)
	$(F90) -o $@ $(NC2GRIB2F) $(OBJ) -I$(NFDIR)/include -L$(NFDIR)/lib -lnetcdff -I$(ECCODES_DIR)/include -L$(ECCODES_DIR)/lib -Wl,-rpath,$(ECCODES_DIR)/lib -leccodes_f90 -leccodes

stringflib.o : $(STRINGFLIBF) 
	$(F90) -c   $(STRINGFLIBF) 
datelib.o : $(DATELIBF) stringflib.o
	$(F90) -c   $(DATELIBF) 
mgrads.o : $(MGRADSF) stringflib.o datelib.o
	$(F90) -c $(MGRADSF) stringflib.o datelib.o
metlib.o : $(METLIB)
	$(F90) -c $(METLIB)
mgrib_tables.o : $(GRIB_PARAMETERS) stringflib.o mgrib_interface.o xmlparse.o
	$(F90) -c $(GRIB_PARAMETERS)
msort.o : $(MSORT)
	$(F90) -c $(MSORT)
mgrib_interface.o : $(MGRIB) msort.o
	$(F90) -c   $(MGRIB) -I$(ECCODES_DIR)/include -L$(ECCODES_DIR)/lib -Wl,-rpath,$(ECCODES_DIR)/lib -leccodes_f90 -leccodes
xmlparse.o	: $(XMLPARSEF)
	$(F90) -c $(XMLPARSEF)
clean:
	rm  -r *.o *.mod *.a
