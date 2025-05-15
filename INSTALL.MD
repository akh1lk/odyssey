Run the Following Commands: 

#To Install Necessary Packages
opam install ANSITerminal
opam install qcheck 
opam install bisect_ppx

#To Build the Program
dune build

#To execute the program
dune exec bin/main.exe

#To Test the Program
dune test