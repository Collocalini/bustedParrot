{ mkDerivation, aeson, base, bytestring, containers, deepseq
, directory, filepath, heist, JuicyPixels, lens, lens-aeson, mtl
, parsec, stdenv, text, time, xmlhtml
}:
mkDerivation {
  pname = "databaseHandling";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers deepseq directory filepath heist
    JuicyPixels lens lens-aeson mtl parsec text time xmlhtml 
    alex c2hs cpphs doctest happy  
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
