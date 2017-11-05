{ mkDerivation, array, base, BoundedChan, bytestring, cereal
, containers, hashable, heaps, monadLib, psqueues, QuickCheck
, random, SHA, stdenv, tasty, tasty-ant-xml, tasty-quickcheck, time
, unix
}:
mkDerivation {
  pname = "hans";
  version = "3.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base BoundedChan bytestring cereal containers hashable heaps
    monadLib psqueues random SHA time unix
  ];
  testHaskellDepends = [
    base bytestring cereal QuickCheck tasty tasty-ant-xml
    tasty-quickcheck
  ];
  description = "Network Stack";
  license = stdenv.lib.licenses.bsd3;
}
