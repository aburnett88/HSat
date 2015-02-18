#!/bin/bash          
grep -n -r '^.\{80\}' tests-src/*
grep -n -r '^.\{80\}' src/*
grep -n -r '^.\{80\}' benchs-src/*
hlint src/*
hlint tests-src/*
hlint benchs-src/*
