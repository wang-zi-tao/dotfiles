{
  stdenv,
  fetchgit,
  lib,
  python312,
  python312Packages,
  fetchPypi,
  makeWrapper,
  pdm,
}:
stdenv.mkDerivation rec {
  pname = "vectorcode";
  version = "0.5.5";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-Pj5EJeOfmgJjekDsSGiHdJKyGigEWg+0VZZJRqhmius=";
  };
  python_pkgs = python312.withPackages (
    ps: with ps; [
      ipython
      chromadb
      sentence-transformers
      pathspec
      tabulate
      shtab
      numpy
      psutil
      httpx
      tree-sitter
      # tree-sitter-language-pack
      pygments
      transformers
      wheel
      pygls
      lsprotocol
      # mcp
      pydantic
    ]
  );
  nativeBuildInputs = [
    python312
    python312Packages.pdm-backend
  ];
  buildInputs = [ ];

  installPhase = ''
    # ls
    mkdir $out
    # cp -r $src/* $out
    # echo >> $out/_version.py << EOF
    #     __version__ = "${version}"
    # EOF
    # wrapProgram $out/bin/vectorcode \
    #   --set PYTHONPATH $python_pkgs/site-packages
  '';

  meta = with lib; {
    description = "";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
