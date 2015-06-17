{ mkDerivation, base, containers, diagrams-contrib, diagrams-lib
, diagrams-pgf, filepath, stdenv
}:
mkDerivation {
  pname = "Plot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base containers diagrams-contrib diagrams-lib diagrams-pgf filepath
  ];
  license = stdenv.lib.licenses.unfree;
}
