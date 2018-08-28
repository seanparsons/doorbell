{ mkDerivation, async, base, blaze-builder, bytestring, file-embed
, http-client, http-client-tls, http-types, old-locale, process
, resourcet, stdenv, temporary, text, time, transformers
}:
mkDerivation {
  pname = "doorbell";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base blaze-builder bytestring file-embed http-client
    http-client-tls http-types old-locale process resourcet temporary
    text time transformers
  ];
  license = stdenv.lib.licenses.mpl20;
}
