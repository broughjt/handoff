# This is a manual closure of the toml-lib dependencies. We add `tzdata` so the
# `gregor` library can use it.
{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  racket,
}:

let
  github =
    owner: repo: rev: hash:
    fetchFromGitHub { inherit owner repo rev hash; };

  toml-racket =
    github "toml-racket" "toml-racket" "c86fd8295470d1c6f51b19054812a6be47dac4c9"
      "sha256-5Bu3Ct1Aindeg1Qjty1P9ZF6k/XO/ULhgLMJkoPTTxk=";
  gregor =
    github "97jaz" "gregor" "f56215db229ef2e33670f55d08c0330d8f85de23"
      "sha256-4TIeinXk7ak7sbT2lwfWYdwIwFD9S7whBrR2KEajW30=";
  memoize =
    github "jbclements" "memoize" "c9691ed9ba2604de223b6ba3dd08d17f740bc8cd"
      "sha256-HLAnsZe+BACkBQUYpgid2HKcHe4y8E0QqsYcZuvFaR0=";
  parsack =
    github "stchang" "parsack" "57b21873e8e3eb7ffbdfa253251c3c27a66723b1"
      "sha256-nWpLRsynxy2G3vAxflGkl6LfMiVIRpujAQ/LTGuBDfk=";

  # Package name and the directory holding its info.rkt.
  packages = {
    toml-lib = "${toml-racket}/toml-lib";
    gregor-lib = "${gregor}/gregor-lib";
    memoize-lib = "${memoize}/memoize-lib";
    parsack-lib = "${parsack}/parsack-lib";
    tzinfo = github "97jaz" "tzinfo" "2f812283d9c90040aecb3c7e2ed2edf93a3720de"
      "sha256-vvb3EZHFysa/2OiTat+i8zuALxiCPHNNaWCGlyPF6gk=";
    cldr-core = github "97jaz" "cldr-core" "c9b80777c422c3b104bb85052d74a2dc1535a3c3"
      "sha256-Tpk6uYWz4//C+/n50wsLiD16rwOim85R/Ykrtcoa1+8=";
    cldr-bcp47 = github "97jaz" "cldr-bcp47" "823fc1a530f1a0ec4de59f5454c1a17f20c5a5d6"
      "sha256-YY5q44IQ1cNX4wk8Yt7B+z2uvfy+xMSl5tTDs+1RBlA=";
    cldr-numbers-modern = github "97jaz" "cldr-numbers-modern" "625428099b3f8cd264955a283dddc176a6080ba1"
      "sha256-RDa1d4sSyfyuNgz2dJdu2f1XGiO4cPOkaseZ7q2cLJU=";
    cldr-dates-modern = github "97jaz" "cldr-dates-modern" "c36282917247f6a069e553535f4619007cd7b6e5"
      "sha256-byD2ubs543P9512lKD1JKB1ppyzjKzoWnuW8JPspa7M=";
    tzdata = github "97jaz" "tzdata" "22a9a3c3dc506a08d7132a4761f21155d004ee25"
      "sha256-2alu1uc8wEsb/wyj0ekNyFly7MLdZpCohEU6vi/LWgE=";
    cldr-localenames-modern = github "97jaz" "cldr-localenames-modern" "f9f3e8d9245764a309542816acf40fe147b473a3"
      "sha256-fZ1fnkslpZuicJgMh6/aLd4rPov7lvJr6ulDWpTMpKg=";
  };
in

stdenvNoCC.mkDerivation {
  name = "handoff-racket-dependencies";
  dontUnpack = true;
  nativeBuildInputs = [ racket ];

  buildPhase = ''
    runHook preBuild
    export HOME=$TMPDIR PLTADDONDIR=$out
    mkdir packages
    ${lib.concatStrings (
      lib.mapAttrsToList (name: source: ''
        cp -r ${source} packages/${name}
      '') packages
    )}
    chmod -R u+w packages
    raco pkg install --batch --no-docs --scope user --deps fail --copy packages/*
    runHook postBuild
  '';

  dontInstall = true;
}
