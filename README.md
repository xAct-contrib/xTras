xTras
=====

[*xTras*](http://www.xact.es/xtras/) is a field-theory inspired add-on package for [*xAct*](http://www.xact.es/), 
the state-of-the-art tensor calculus package for Mathematica.

Some of the extra functionality it brings is:

* Generating a list of all contractions of a given tensorial expression.
* Generating lists of dimensional dependent identities (e.g. the vanishing of the Weyl tensor in D=3).
* Making any tensorial expression traceless.
* Computing equations of motion of monomials of Riemann tensors.
* Perturbations around AdS spaces.
* Killing vectors.
* Easy access to the database of the Invar package.
* Young symmetrizers and projectors.
* Solving linear systems of tensorial equations, both for constant variables (SolveConstants) 
  and for tensors (SolveTensors).


Installation
------------

#### Installing the package in Mathematica

If you just want to run the *xTras* package as a end user, download the compiled zip file from 
http://www.xact.es/xtras, unzip, and follow the instructions.

#### Configuring the project in Wolfram Workbench

If you want to set up *xTras* in Workbench to edit the code, you'll have to take a few extra steps.
This assumes you already have a Workbench environment up and running. 

1. Download and install the complete [*xAct*](http://www.xact.es/) bundle where Mathematica can find it, 
   but remove or rename the `xAct/xTras/` subdirectory.
2. Checkout or download the *xTras* source, and import it in Workbench.
3. Edit the file `buildfiles/main.xml` such that properties in the section `Machine-specific config variables`
   match your setup.

Running notebooks for testing code is the same as usual, but deploying and building the documentation is not,
due to *xTras*' unusual structure (it's a sub-package for *xAct*, which isn't supported out-of-the-box in Workbench).
Deploying can be done by running the `deploy` target in the `buildfiles/main.xml` file, and building the 
documentation by running the `documentation` target. The complete project can by build by running the 
`all` target.

The unit tests can be run by running the `tests/TestSuite.mt` as a Mathematica Test.


What's what
-----------

The Workbench project is structured into the following directories:

    buildfiles/       Ant files for building the docs, changelog, and the distributable zip.
    notebooks/        Mathematica *.nb files to run and / or test code.
    tests/            Unit test files.
    xAct/             The actual code of the package.
    xTras/            Documentation source.
