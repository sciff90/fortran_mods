#2DMHD Makefile
#Define Objects

OBJ = obj
SRC = src
MOD = mod
PLOTS = plots
DATA = data
EXEC = xVector_Mods

OBJECTS = ${OBJ}/vector_operations.o\
					${OBJ}/main.o



#Define Compiler

#############ifort############
COMP = ifort

#Define Switches
#SWITCH = -O2 -module ${MOD}
SWITCH = -g -debug-parameters -module ${MOD} -check all

#Define Libraries
#LIB_DIR = -L lib/ifort
#LIB = -llapack -lblas

##############################


#############gfortran############
#COMP = gfortran

#Define Switches
#SWITCH = -O2 -J ${MOD}
#SWITCH = -g -J ${MOD}

#Define Libraries

#LIB_DIR = -L lib/gfortran
#LIB = -llapack -lblas

##############################


#Makefile
all: DIR $(OBJECTS)
	$(COMP) $(SWITCH) ${OBJECTS} -o ${EXEC} $(LIB_DIR) $(LIB)

${OBJ}/vector_operations.o: ${SRC}/vector_operations.f90
	$(COMP) -c $(SWITCH) ${SRC}/vector_operations.f90 -o ${OBJ}/vector_operations.o

${OBJ}/main.o: ${SRC}/main.f90 ${OBJ}/vector_operations.o
	$(COMP) -c $(SWITCH) ${SRC}/main.f90 -o ${OBJ}/main.o

DIR:
	mkdir -p $(OBJ) $(DATA) $(PLOTS) $(MOD)
clean:
	rm -rf ${OBJ}

#Cleaning Everything
cleanest: clean
	rm -rf ${EXEC}
	rm -rf ${DATA}
	rm -rf ${PLOTS}
	rm -rf ${MOD}

# End of the makefile

