pkgs: prev: {
  mcp-neovim-server = pkgs.buildNpmPackage {
    pname = "mcp-neovim-server";
    version = "9076bb";
    src = pkgs.fetchFromGitHub {
      owner = "bigcodegen";
      repo = "mcp-neovim-server";
      rev = "9076bbb34a08f44a743ad66c78638ef22da58ab0";
      sha256 = "sha256-rSnizEKhuvHSxwmOG/V+QIaAx7TCN1lGUiP28usaeng=";
    };
    npmDepsHash = "sha256-vqRPSO8Oji0HvTMBDUXrhQxe+M6cfFpALnqsBfrctPQ=";
  };

  mcp-open-websearch = pkgs.buildNpmPackage {
    pname = "mcp-open-websearch";
    version = "1.2.5";
    src = pkgs.fetchFromGitHub {
      owner = "Aas-ee";
      repo = "open-webSearch";
      rev = "v1.2.5";
      sha256 = "sha256-ICT8pTwIL20/Yz6vz9cF4gwMqbNFS9uScB/Gt5qQais=";
    };
    npmDepsHash = "sha256-HHsWdhqGlIKyTNL2Jd0RnJmSJC8+T4Iz79oUTUGvt8M=";
  };

  mcp-obsidian = pkgs.buildNpmPackage {
    pname = "mcp-obsidian";
    version = "487625a";
    src = pkgs.fetchFromGitHub {
      owner = "bitbonsai";
      repo = "mcpvault";
      rev = "487625aa38302fa5faaacb51ced949ff9c71880a";
      sha256 = "sha256-QpFhAEzfrHtsKGKROBJfNgFSbV3B5ZwbqEYTh2PrH9c=";
    };
    npmDepsHash = "sha256-cKEzttAbFBPZ7dhNs4JIcltkIftU2Y5PuxiCFCm14ew=";
  };

  git-mcp-server = pkgs.buildNpmPackage {
    pname = "git-mcp-server";
    version = "v2.10.3";
    src = pkgs.fetchgit {
      url = "https://github.com/wang-zi-tao/git-mcp-server";
      rev = "ae406188a021f560e469ab77e823eebccd75b417";
      sha256 = "sha256-j1T+pim9FKmBQAd/aj3FCMzyUBhhGkMuKGpKD7wmbjE=";
    };
    npmDepsHash = "sha256-5Gw7tOLPl7t8b+TGCM+qAKhZ+sTavuDSb8J4Hlw1PF8=";

    nativeBuildInputs = [ pkgs.bun ];
    buildInputs = [ pkgs.bun ];
  };
}
