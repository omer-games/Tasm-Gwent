@echo off
tasm /zi /ml %1.asm
tlink /v %1.obj
