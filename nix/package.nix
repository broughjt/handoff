{
  lib,
  stdenvNoCC,
  makeWrapper,
  removeReferencesTo,
  installAgentSkills,
  racket,
  racketDependencies,
  bash,
  git,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "handoff";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../handoff
      ../workflows
      ../HANDOFF.md
      ../skills
    ];
  };

  nativeBuildInputs = [
    racket
    makeWrapper
    removeReferencesTo
    installAgentSkills
  ];
  nativeCheckInputs = [
    bash
    git
  ];

  buildPhase = ''
    runHook preBuild
    export HOME=$TMPDIR PLTADDONDIR=${racketDependencies}
    raco make -v handoff/main.rkt
    mkdir build
    raco exe -o build/handoff handoff/main.rkt
    raco distribute dist build/handoff
    runHook postBuild
  '';

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    export LC_ALL=C.UTF-8
    raco test handoff/tests
    runHook postCheck
  '';

  # The script calls git commands and runs user checks under bash. It finds
  # reusable workflows in $XDG_DATA_HOME/handoff/workflows/ and then each
  # $XDG_DATA_DIRS/handoff/workflows/, and HANDOFF.md, the concepts the skills
  # share, at the first handoff/HANDOFF.md in the same directories, so the
  # wrapper puts the package's own share directory first among
  # $XDG_DATA_DIRS. installAgentSkills has already created $out in preInstall,
  # so dist is copied into it rather than to it.
  installPhase = ''
    runHook preInstall
    cp -r dist/. $out
    mkdir -p $out/share/handoff
    cp -r workflows $out/share/handoff/workflows
    cp HANDOFF.md $out/share/handoff/HANDOFF.md
    wrapProgram $out/bin/handoff \
      --prefix PATH : ${
        lib.makeBinPath [
          bash
          git
        ]
      } \
      --prefix XDG_DATA_DIRS : $out/share
    runHook postInstall
  '';

  # raco distribute makes the executable self-contained, but compiled code keeps
  # source paths into Racket and its add-on directory, and the launcher keeps
  # its default collection paths. Nothing reads them at run time, so remove them
  # to avoid including the full distribution, which includes GTK.
  postFixup = ''
    find $out -type f -exec remove-references-to -t ${racket} -t ${racketDependencies} {} +
  '';
  disallowedReferences = [
    racket
    racketDependencies
  ];

  # Run the stripped executable, which would fail if it still needed Racket.
  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    export HOME=$TMPDIR
    # The handoff repository sits inside a project repository, as the script requires.
    git init -q $TMPDIR/project
    repository=$TMPDIR/project/.scratch/handoff
    mkdir -p $repository/arc
    git init -q $repository
    touch $repository/arc/start.toml
    printf '[[check]]\nid = "x"\nclaim = "holds"\ncommand = "true"\n' > $repository/arc/checks.toml
    $out/bin/handoff --repository $repository check
    $out/bin/handoff workflow list | grep -qx 'name: agent-implements'
    test "$($out/bin/handoff guide)" = $out/share/handoff/HANDOFF.md
    test -f $out/share/skills/handoff/handoff-start/SKILL.md
    runHook postInstallCheck
  '';

  passthru.skills = "${finalAttrs.finalPackage}/share/skills/handoff";

  meta.mainProgram = "handoff";
})
