{ mkDerivation, base, bytestring, pcre, stdenv }:
mkDerivation {
  pname = "regex";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring ];
  librarySystemDepends = [ pcre ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
