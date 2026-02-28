{
  config,
  pkgs,
  ...
}: {
  programs = {
    diff-highlight = {
      enable = true;
      enableGitIntegration = true;
    };

    difftastic.enable = true;

    git = {
      enable = true;
      ignores = [
        # Emacs temporary files
        "flycheck_*"
        "*~"
        ''\#*\#''
        ''.\#*''

        # QEMU disks
        "*.qcow2"

        # Python
        "__pycache__/"
        "*.py[codz]"
        "*$py.class"

        # Rust (rustfmt)
        "*.rs.bk"

        # Linux
        ".fuse_hidden*"
        ".nfs*"
        "nohup.out"

        # macOS
        ".DS_Store"

        # Agents
        ".serena/"
      ];
      settings.alias = rec {
        s = "status";
        d = "diff";
        c = "diff --cached";
        a = "add";
        co = "checkout";
        lg = "log --color --graph --pretty=format:'%Cred%h%Creset %C(bold magenta)%G?%Creset%C(yellow)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --abbrev-commit --date-order";
        lga = lg + " --all";
        ff = "merge --ff-only";
      };
      settings = {
        core = {
          safecrlf = false;
          commentchar = ";";
          hooksPath = "~/.config/git/hooks";
        };
        merge.conflictstyle = "diff3";
        fetch.prune = true;
        pull.ff = "only";
        transfer.fsckObjects = true;
        gpg.ssh.allowedSignersFile = "";
      };
    };
  };

  home.packages = [
    pkgs.cocogitto # Conventional Commits TUI, <https://docs.cocogitto.io/>
  ];
  home.file =
    {
      ".config/git/hooks".source = ./hooks;

      ".ssh/config.d/github.com".text = ''
        Host github.com
          Hostname github.com
          HostKeyAlias github.com
          UserKnownHostsFile ~/.ssh/known_hosts.d/github.com
          User git

        Host gist.github.com
          Hostname gist.github.com
          HostKeyAlias github.com
          UserKnownHostsFile ~/.ssh/known_hosts.d/github.com
          User git
      '';

      ".ssh/known_hosts.d/github.com".text = ''
        github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl
        github.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCj7ndNxQowgcQnjshcLrqPEiiphnt+VTTvDP6mHBL9j1aNUkY4Ue1gvwnGLVlOhGeYrnZaMgRK6+PKCUXaDbC7qtbW8gIkhL7aGCsOr/C56SJMy/BCZfxd1nWzAOxSDPgVsmerOBYfNqltV9/hWCqBywINIR+5dIg6JTJ72pcEpEjcYgXkE2YEFXV1JHnsKgbLWNlhScqb2UmyRkQyytRLtL+38TGxkxCflmO+5Z8CSSNY7GidjMIZ7Q4zMjA2n1nGrlTDkzwDCsw+wqFPGQA179cnfGWOWRVruj16z6XyvxvjJwbz0wQZ75XK5tKSb7FNyeIEs4TT4jk+S4dhPeAUC5y+bDYirYgM4GC7uEnztnZyaVWQ7B381AK4Qdrwt51ZqExKbQpTUNn+EjqoTwvqNj4kqx5QUCI0ThS/YkOxJCXmPUWZbhjpCg56i+2aB6CmK2JGhn57K5mj0MNdBXA4/WnwH6XoPWJzK5Nyu2zB3nAZp+S5hpQs+p1vN1/wsjk=
        github.com ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEmKSENjQEezOmxkZMy7opKgwFB9nkt5YRrYMjNuG5N87uRgg6CLrbo5wAdT/y6v0mKV0U2w0WZ2YB/++Tpockg=
      '';
    }
    // (
      if pkgs.stdenv.isDarwin
      then {
        # For whatever reason this doesn’t happen on Darwin:
        ".gitconfig".text = config.xdg.configFile."git/config".text;
      }
      else {}
    );
}
