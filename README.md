# FINITE FIELDS : galois.hs
Utility Calculator for Finite Fields written in Haskell.

- Performs 4 arithmetic operations on 2 values within a given finite field
- Calculates addition and multiplication tables
- Users can either input their own irreducible polynomial or ask the program to identify one


Current Limitations:
 - Output is presented in a terminal window (no GUI). Therefore tables wider than terminal width are not formatted correctly. 

# To run

prerequisites : ghci, CPimport.txt in the same directory as galois.hs

> runghc galois.hs

or simply compile it with

> ghc galois.hs
