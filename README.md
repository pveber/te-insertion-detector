# te-insertion-detector

## Installation

Install instructions are the same than for the [bistro library](https://github.com/pveber/bistro), which this tools relies on. You can follow the [manual](http://bistro.readthedocs.io/en/latest/getting-started.html) to install `opam`, `bistro` and `docker`. Once this is done, simply type :
```
opam pin add -y biocaml https://github.com/biocaml/biocaml.git
opam pin add -y pipes https://github.com/pveber/pipes.git
opam pin add -y bistro --dev-repo
opam pin add -y te-insertion-detector https://github.com/pveber/te-insertion-detector.git
```
A `te-insertion-detector` program should now be available.

## Usage

Type `te-insertion-detector --help` to get a description of command-line options. A typical invocation should look like this:
```sh
$ te-insertion-detector pipeline --fq1 reads_1.fq.gz --fq2 reads_2.fq.gz --genome droSim1 --te-list TEs.fa --mem 8 --np 8 --outdir results
```
