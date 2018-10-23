# Makefile for generating the LSD R package
#
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
R_FILES := $(wildcard R/*.R)
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES)

ifeq ($(OS),Windows_NT) 
	RM = rm -rf
	CP = cp -f
	CD = cd
else
	RM = rm -rf
	CP = cp -f
	CD = cd
endif

.PHONY: all update tarball install clean

all: update tarball install clean

tarball: ../$(PKG_NAME)_$(PKG_VERSION).tar.gz
../$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	$(CD) ../ && R CMD build pkg

install: ../$(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL ../$(PKG_NAME)_$(PKG_VERSION).tar.gz

update DESCRIPTION NAMESPACE: $(R_FILES)
	Rscript update.R

clean:
	$(RM) ../$(PKG_NAME)_*.tar.gz
	$(RM) man/
