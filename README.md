# A deformation finder for wait-free implementability task

This is a proof-of-concept code for the discovery of deformation
that certificates the existence of a wait-free protocol for a distributed task
given as a carrier map.

## Disclaimer/No warranty.

This software is a pre-alpha version and is provided "as is" without any
warranty of any kind, either expressed or implied.


## Compilation and usage

### Requirement

- [OCaml](http://ocaml.org/) version>=4.04.0

### Compilation
- Just type `make'.

### Usage
- To just find one:

    % ./cont.opt.exe CMaps/butterfly3.map

- To let it be talkative a bit:

    % ./cont.opt.exe -verbose CMaps/butterfly3.map

- To let it be more talkative (debug mode):

    % ./cont.opt.exe -debug CMaps/butterfly3.map

  + verbose&debug mode is most talkative...

    % ./cont.opt.exe -verbose -debug CMaps/butterfly3.map

- To show the usage:

    % ./cont.opt.exe --help


### Notice
- This is an experimental software. It can potentially contain
unidentified bugs and it conducts few integrity checks on the input.
  + Especially, it is your responsibility to check if the input
  task specification file is topologically well-formed, namely,
  whether it is a chromatic rigid carrier map satisfying the
  link-connectivity condition.
