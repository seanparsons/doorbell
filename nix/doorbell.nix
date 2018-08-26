{ mkDerivation, async, base, blaze-builder, bytestring, http-client
, http-client-tls, http-types, mpg123, old-locale, process
, resourcet, rtl_433, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "doorbell";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base blaze-builder bytestring http-client http-client-tls
    http-types old-locale process resourcet text time transformers
  ];
  license = stdenv.lib.licenses.mpl20;
}
