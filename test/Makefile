# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

#######################################################################
# Make file for test purposes only
# Project is build using STACK
#######################################################################
# DIRECTORIES
EXAMPSDIR       = ../examps
# TEST FILES
JAVASUTFILES    = $(wildcard $(EXAMPSDIR)/*/*.java $(EXAMPSDIR)/*/*/*.java)
#Alternative: use make recursively - make file for SUTs in their examps directory
# See GNU Make manual in particular
# pages 31-32 of section 4.5 and 
# paragraph 5.7 starting on page 50.
TSTDIR          = .
JAVATSTFILES    = $(wildcard $(TSTDIR)/*/*.java)

#dependencies for java need to be closed
# A.java needs B.java
# B.java needs C.java
# javac needs A.java, B.java, and C.java as arguments

%.class : %.java
#Both work, but what is exactly the difference?
#	-javac -classpath $(dir $@) $^
	-javac -sourcepath $(dir $@) $^

.PHONY      : test
test : $(patsubst %.java,%.class, $(JAVASUTFILES)) $(patsubst %.java,%.class, $(JAVATSTFILES))

.PHONY      : testCopyright
testCopyright : $(TSTDIR)/copyright/Copyright.class
	java -cp copyright Copyright .

#######################################################################
.PHONY      : clean
clean :
	-@del $(subst /,\,$(patsubst %.java,%.class, $(JAVASUTFILES)))
	-@del $(subst /,\,$(patsubst %.java,%.class, $(JAVATSTFILES)))
#######################################################################